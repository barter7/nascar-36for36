import { useMemo, useState } from 'react'
import { PARTICIPANTS, COLORS } from '../data'
import type { AppData } from '../App'
import { valueOverAvg } from '../stats'
import { Avatar, CarBadge, PlayerName, signed, valueClass } from '../components'

export default function Weekly({ data }: { data: AppData }) {
  const { stats, scores, results, picks, raceLabel, driverFor, lastPicked } = data
  const { completed, cars, usedBy } = stats
  const [selected, setSelected] = useState<number | null>(null)
  const race = selected ?? completed[completed.length - 1]
  const idx = completed.indexOf(race)

  const rows = useMemo(() => PARTICIPANTS.map(p => {
    const s = scores.find(x => x.participant === p && x.race_number === race)
    const pick = picks.find(x => x.participant === p && x.race_number === race)
    return { p, s, pick }
  }).sort((a, b) => (b.s?.points ?? -1) - (a.s?.points ?? -1)), [scores, picks, race])

  const topCars = useMemo(() => {
    const pool = new Set(data.drivers.map(d => d.car_number))
    return results
      .filter(r => r.race_number === race && pool.has(r.car_number))
      .sort((a, b) => b.points - a.points)
      .slice(0, 5)
      .map(r => ({
        r,
        pickedBy: PARTICIPANTS.filter(p => picks.some(x => x.participant === p && x.race_number === race && x.car_number === r.car_number)),
        available: PARTICIPANTS.filter(p => {
          const usedAt = usedBy[p].get(r.car_number)
          return usedAt === undefined || usedAt >= race
        }),
      }))
  }, [results, picks, usedBy, race, data.drivers])

  if (!race) return <div className="loading">No races run yet.</div>
  const topPts = rows[0]?.s?.points ?? 0

  return (
    <>
      <div className="race-nav">
        <button disabled={idx <= 0} onClick={() => setSelected(completed[idx - 1])}>‹</button>
        <select value={race} onChange={e => setSelected(Number(e.target.value))}>
          {[...completed].reverse().map(r => <option key={r} value={r}>{raceLabel(r)}</option>)}
        </select>
        <button disabled={idx >= completed.length - 1} onClick={() => setSelected(completed[idx + 1])}>›</button>
      </div>

      <div className="card">
        <div className="card-header"><span>{raceLabel(race)}</span><span className="card-sub">Our picks</span></div>
        <div className="recap">
          {rows.map(({ p, s, pick }) => {
            const car = s?.car_number ?? pick?.car_number
            const v = s ? valueOverAvg(s, cars) : null
            const missed = !pick && race <= (lastPicked[p] || 0)
            return (
              <div key={p} className={`recap-row${s && s.points === topPts && topPts > 0 ? ' top' : ''}`}>
                <div className="recap-player" style={{ borderColor: COLORS[p] }}><PlayerName name={p} /></div>
                {car != null ? <Avatar driver={driverFor(car)} car={car} /> : <span className="avatar empty">{missed ? '😢' : '—'}</span>}
                <div className="recap-main">
                  {car != null ? (
                    <>
                      <div className="recap-driver"><CarBadge car={car} height={16} /> {s?.driver || driverFor(car)?.driver || ''}</div>
                      <div className="recap-detail">
                        {s && s.finish_pos > 0 ? <>P{s.finish_pos}{s.start_pos ? ` from P${s.start_pos}` : ''}</> : s ? '' : 'Did not race'}
                        {s?.stage_pts ? ` · ${s.stage_pts} stage` : ''}
                        {s?.laps_led ? ` · ${s.laps_led} led` : ''}
                      </div>
                    </>
                  ) : <div className="recap-detail">{missed ? 'Missed pick' : 'No pick entered'}</div>}
                </div>
                <div className="recap-pts">
                  <div className="pts">{s?.points ?? 0}</div>
                  {v != null && <div className={`val ${valueClass(v)}`}>{signed(v, 1)}</div>}
                </div>
              </div>
            )
          })}
        </div>
      </div>

      <div className="card">
        <div className="card-header"><span>Best Picks This Week</span><span className="card-sub">top-scoring pool cars</span></div>
        <div className="table-scroll">
          <table className="best-picks">
            <tbody>
              {topCars.map(({ r, pickedBy, available }) => (
                <tr key={r.car_number}>
                  <td className="left nowrap"><CarBadge car={r.car_number} height={18} /> {r.driver}</td>
                  <td className="muted nowrap">P{r.finish_pos}</td>
                  <td className="strong">{r.points}</td>
                  <td className="left">
                    <div className="dots">
                      {PARTICIPANTS.map(p => (
                        <span key={p} title={pickedBy.includes(p) ? `${p} picked it` : available.includes(p) ? `${p} had it available` : `${p} already used it`}
                          className={`dot ${pickedBy.includes(p) ? 'picked' : available.includes(p) ? 'avail' : 'used'}`}
                          style={{ '--c': COLORS[p] } as React.CSSProperties}>{p.slice(0, 2)}</span>
                      ))}
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        <div className="legend">
          <span><i className="dot picked" style={{ '--c': '#aaa' } as React.CSSProperties} /> Picked it</span>
          <span><i className="dot avail" style={{ '--c': '#aaa' } as React.CSSProperties} /> Had it available</span>
          <span><i className="dot used" /> Already used</span>
        </div>
      </div>
    </>
  )
}
