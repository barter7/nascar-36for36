import { useMemo, useState } from 'react'
import { PARTICIPANTS, COLORS } from '../data'
import type { AppData } from '../App'
import { Avatar, CarBadge } from '../components'

type SortKey = 'avg' | 'form' | 'car'

function lastName(name: string) {
  const parts = name.split(' ')
  const suffix = /^(jr\.?|sr\.?|ii|iii)$/i.test(parts[parts.length - 1] ?? '')
  return parts.slice(suffix ? -2 : -1).join(' ')
}

export default function Drivers({ data }: { data: AppData }) {
  const { drivers, stats, scores } = data
  const { cars, usedBy, players, remaining, completed } = stats
  const [focus, setFocus] = useState<string | null>(null)
  const [sort, setSort] = useState<SortKey>('avg')

  const rows = useMemo(() => {
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

  const focusStat = focus ? players.find(p => p.participant === focus) : null
  const counted = useMemo(() => {
    if (!focus) return new Set<number>()
    const done = new Set(completed)
    const available = drivers
      .filter(d => cars[d.car_number]?.active !== false)
      .filter(d => { const r = usedBy[focus].get(d.car_number); return r === undefined || !done.has(r) })
      .sort((a, b) => (cars[b.car_number]?.avg ?? 0) - (cars[a.car_number]?.avg ?? 0))
    return new Set(available.slice(0, remaining).map(d => d.car_number))
  }, [focus, drivers, cars, usedBy, completed, remaining])

  const pointsFor = (p: string, car: number) => {
    const race = usedBy[p].get(car)
    if (race === undefined) return null
    return { race, pts: scores.find(s => s.participant === p && s.race_number === race)?.points }
  }

  const header = (key: SortKey, label: string, title?: string) => (
    <th className={`sortable${sort === key ? ' sorted' : ''}`} onClick={() => setSort(key)} title={title}>{label}</th>
  )

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
        <div className="card-header"><span>Driver Pool</span><span className="card-sub">{focus ? `${focus}'s used cars dimmed` : 'where each car has been used'}</span></div>
        <div className="table-scroll">
          <table className="drivers">
            <thead>
              <tr>
                {header('car', 'Driver')}
                {header('avg', 'Avg', 'Season average points')}
                {header('form', 'L5', 'Average over the last 5 races')}
                {PARTICIPANTS.map(p => <th key={p} style={{ color: COLORS[p] }}>{p.slice(0, 2)}</th>)}
              </tr>
            </thead>
            <tbody>
              {rows.map(d => {
                const c = cars[d.car_number]
                const inactive = c?.active === false
                const usedByFocus = focus ? usedBy[focus].has(d.car_number) && completed.includes(usedBy[focus].get(d.car_number)!) : false
                const dropped = focus && !usedByFocus && !inactive && !counted.has(d.car_number)
                return (
                  <tr key={d.car_number} className={usedByFocus || inactive ? 'dim' : ''}>
                    <td className="left">
                      <div className="driver-cell">
                        <Avatar driver={d} car={d.car_number} size={30} />
                        <div>
                          <div className="nowrap">
                            <CarBadge car={d.car_number} height={14} />{' '}
                            <span className="full-name">{d.driver}</span><span className="short-name">{lastName(d.driver)}</span>
                          </div>
                          <div className="sub">
                            {inactive ? `inactive since R${c?.lastRace}` : d.team}
                            {dropped && <span className="tag">won't fit</span>}
                          </div>
                        </div>
                      </div>
                    </td>
                    <td className="strong">{c ? c.avg.toFixed(1) : '—'}</td>
                    <td className={c?.form != null && c.form > c.avg + 3 ? 'pos' : c?.form != null && c.form < c.avg - 3 ? 'neg' : ''}>
                      {c?.form != null ? c.form.toFixed(0) : '—'}
                    </td>
                    {PARTICIPANTS.map(p => {
                      const u = pointsFor(p, d.car_number)
                      return (
                        <td key={p} className="used-cell">
                          {u ? (
                            <span className="used-chip" style={{ '--c': COLORS[p] } as React.CSSProperties}>
                              R{u.race}<small>{u.pts ?? '…'}</small>
                            </span>
                          ) : <span className="muted">·</span>}
                        </td>
                      )
                    })}
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
        <div className="footnote">Avg includes every race the car ran, whoever drove it. L5 is shaded when it's 3+ points above or below the season average. Cars that miss 3 straight races are marked inactive and left out of projections.</div>
      </div>
    </>
  )
}
