import { useMemo, useState } from 'react'
import { PARTICIPANTS, COLORS, MFR_LOGOS, carBadgeUrl, type Driver } from '../data'
import type { AppData } from '../App'

type SortKey = 'avg' | 'form' | 'car'
const SORTS: { key: SortKey; label: string }[] = [
  { key: 'avg', label: 'Season avg' },
  { key: 'form', label: 'Last 5' },
  { key: 'car', label: 'Car #' },
]

function hideOnError(e: React.SyntheticEvent<HTMLImageElement>) {
  e.currentTarget.style.display = 'none'
}

export default function Drivers({ data }: { data: AppData }) {
  const { drivers, stats, scores } = data
  const { cars, usedBy, players, remaining, completed } = stats
  const [focus, setFocus] = useState<string | null>(null)
  const [sort, setSort] = useState<SortKey>('avg')

  const sorted = useMemo(() => {
    const val = (car: number) => {
      const c = cars[car]
      if (sort === 'car') return -car
      if (!c) return -Infinity
      return sort === 'form' ? (c.form ?? -Infinity) : c.avg
    }
    return [...drivers].sort((a, b) => {
      const ia = cars[a.car_number]?.active === false ? 1 : 0
      const ib = cars[b.car_number]?.active === false ? 1 : 0
      return ia - ib || val(b.car_number) - val(a.car_number)
    })
  }, [drivers, cars, sort])

  const usedDone = (p: string, car: number) => {
    const race = usedBy[p].get(car)
    return race !== undefined && completed.includes(race)
  }

  const counted = useMemo(() => {
    if (!focus) return new Set<number>()
    const available = drivers
      .filter(d => cars[d.car_number]?.active !== false && !usedDone(focus, d.car_number))
      .sort((a, b) => (cars[b.car_number]?.avg ?? 0) - (cars[a.car_number]?.avg ?? 0))
    return new Set(available.slice(0, remaining).map(d => d.car_number))
  }, [focus, drivers, cars, usedBy, completed, remaining])

  const pickPoints = (p: string, car: number) => {
    const race = usedBy[p].get(car)
    if (race === undefined) return null
    const pts = scores.find(s => s.participant === p && s.race_number === race)?.points
    return pts ?? (completed.includes(race) ? 0 : '…')
  }

  const focusStat = focus ? players.find(p => p.participant === focus) : null

  const card = (d: Driver) => {
    const c = cars[d.car_number]
    const inactive = c?.active === false
    const dim = inactive || (focus ? usedDone(focus, d.car_number) : false)
    const wontFit = focus && !dim && !counted.has(d.car_number)
    return (
      <div key={d.car_number} className={`driver-card${dim ? ' dim' : ''}`}>
        <div className="driver-card-badges">
          {PARTICIPANTS.map(p => {
            const pts = pickPoints(p, d.car_number)
            return pts === null
              ? <span key={p} className="pick-badge empty" title={`${p}: available`} />
              : <span key={p} className="pick-badge" style={{ background: COLORS[p] }} title={`${p}: ${pts} pts`}>{pts}</span>
          })}
        </div>
        <div className="driver-card-img">
          <div className="driver-card-fallback">#{d.car_number}</div>
          {d.headshot_url && <img className="driver-card-photo" src={d.headshot_url} alt="" onError={hideOnError} />}
          <div className="driver-card-number"><img src={carBadgeUrl(d.car_number)} alt={`#${d.car_number}`} onError={hideOnError} /></div>
          {MFR_LOGOS[d.manufacturer] && (
            <div className="driver-card-mfr"><img src={MFR_LOGOS[d.manufacturer]} alt={d.manufacturer} onError={hideOnError} /></div>
          )}
          <div className="driver-card-overlay">
            <div className="driver-card-name">{d.driver}</div>
            <div className="driver-card-team">{d.team}</div>
          </div>
        </div>
        <div className="driver-card-info">
          {inactive ? <span className="muted">Inactive since R{c?.lastRace}</span> : c ? (
            <>
              <span>{c.avg.toFixed(1)} <small>avg</small></span>
              <span className={c.form != null && c.form > c.avg + 3 ? 'pos' : c.form != null && c.form < c.avg - 3 ? 'neg' : ''}>
                {c.form != null ? c.form.toFixed(0) : '—'} <small>L5</small>
              </span>
            </>
          ) : <span className="muted">No races yet</span>}
        </div>
        {wontFit && <div className="driver-card-flag">won't fit</div>}
      </div>
    )
  }

  return (
    <>
      <div className="chips">
        <button className={`chip${focus === null ? ' active' : ''}`} onClick={() => setFocus(null)}>All</button>
        {PARTICIPANTS.map(p => (
          <button key={p} className={`chip${focus === p ? ' active' : ''}`} style={{ '--c': COLORS[p] } as React.CSSProperties}
            onClick={() => setFocus(focus === p ? null : p)}>{p}</button>
        ))}
      </div>

      {focusStat && (
        <div className="focus-banner" style={{ borderColor: COLORS[focusStat.participant] }}>
          <div><b>{focusStat.poolSize}</b> drivers left for <b>{remaining}</b> races</div>
          <div>Remaining potential <b className="pos">+{focusStat.potential}</b> → projected <b className="gold">{focusStat.projected}</b></div>
          {focusStat.dropped > 0 && <div className="muted">{focusStat.dropped} lowest-average driver{focusStat.dropped > 1 ? 's' : ''} won't fit (missed weeks)</div>}
        </div>
      )}

      <div className="card">
        <div className="card-header">
          <span>Driver Pool</span>
          <div className="sort-toggle">
            {SORTS.map(s => (
              <button key={s.key} className={sort === s.key ? 'active' : ''} onClick={() => setSort(s.key)}>{s.label}</button>
            ))}
          </div>
        </div>
        <div className="card-body">
          <div className="driver-grid">{sorted.map(card)}</div>
        </div>
        <div className="footnote">
          Top badges show the points each player scored with that car (empty = still available).
          {focus ? ` ${focus}'s used cars are dimmed.` : ''} L5 is the last-5-race average, shaded when 3+ points off the season average.
          Cars that miss 3 straight races are marked inactive and left out of projections.
        </div>
      </div>
    </>
  )
}
