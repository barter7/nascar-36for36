import { useState, useMemo } from 'react'
import { PARTICIPANTS, COLORS, carBadgeUrl, MFR_LOGOS, type Driver, type Score, type Schedule, type Result, type picksToLong } from '../data'

interface Props {
  drivers: Driver[]; picksLong: ReturnType<typeof picksToLong>;
  scores: Score[]; schedule: Schedule[]; results: Result[]; completedRaces: number[];
}

export default function DriversUsed({ drivers, picksLong, scores, schedule, results, completedRaces }: Props) {
  const [selected, setSelected] = useState('Mike')

  const avgByCar = useMemo(() => {
    const map: Record<number, { total: number; count: number }> = {}
    for (const r of results) {
      if (!map[r.car_number]) map[r.car_number] = { total: 0, count: 0 }
      map[r.car_number].total += r.points
      map[r.car_number].count++
    }
    const out: Record<number, number> = {}
    for (const [car, a] of Object.entries(map)) out[Number(car)] = a.total / a.count
    return out
  }, [results])

  const usedCars = useMemo(() => {
    return picksLong.filter(p => p.participant === selected).map(p => p.car_number)
  }, [picksLong, selected])

  const usedData = useMemo(() => {
    return picksLong
      .filter(p => p.participant === selected)
      .map(p => {
        const sc = scores.find(s => s.participant === selected && s.race_number === p.race_number)
        const track = schedule.find(s => s.race_num === p.race_number)?.track_short || `R${p.race_number}`
        const d = drivers.find(d => d.car_number === p.car_number)
        const avg = avgByCar[p.car_number]
        const value = sc != null && avg != null ? sc.points - avg : null
        return { ...p, points: sc?.points, track, driver: d, avg, value }
      })
      .sort((a, b) => {
        if (a.value == null && b.value == null) return a.race_number - b.race_number
        if (a.value == null) return 1
        if (b.value == null) return -1
        return b.value - a.value
      })
  }, [picksLong, scores, schedule, drivers, selected, avgByCar])

  const remaining = 36 - completedRaces.length

  const rosPotential = useMemo(() => {
    const usedInCompleted = new Set(
      picksLong.filter(p => p.participant === selected && completedRaces.includes(p.race_number)).map(p => p.car_number)
    )
    const unused = drivers
      .filter(d => !usedInCompleted.has(d.car_number))
      .map(d => ({ car: d.car_number, avg: avgByCar[d.car_number] ?? 0 }))
      .sort((a, b) => b.avg - a.avg)
    const kept = new Set(unused.slice(0, remaining).map(u => u.car))
    const total = unused.slice(0, remaining).reduce((a, u) => a + u.avg, 0)
    const droppedCount = Math.max(0, unused.length - remaining)
    return { kept, total, droppedCount }
  }, [picksLong, drivers, avgByCar, selected, completedRaces, remaining])

  const available = useMemo(() => {
    return drivers
      .filter(d => !usedCars.includes(d.car_number))
      .map(d => ({ ...d, avg: avgByCar[d.car_number] ?? 0, counted: rosPotential.kept.has(d.car_number) }))
      .sort((a, b) => b.avg - a.avg)
  }, [drivers, usedCars, avgByCar, rosPotential])

  return (
    <>
      <div className="participant-buttons">
        {PARTICIPANTS.map(p => (
          <button key={p} className="participant-btn"
            style={{ background: COLORS[p], opacity: selected === p ? 1 : 0.5 }}
            onClick={() => setSelected(p)}>
            {p}
          </button>
        ))}
      </div>
      <div className="two-col">
        <div className="card">
          <div className="card-header">{selected} — Drivers Used (by Value Gained)</div>
          <div className="card-body">
            <div className="driver-grid">
              {usedData.map(u => (
                <div key={u.race_number} className="driver-card used">
                  <div className="driver-card-img" style={{ background: '#1a1a2e', position: 'relative' }}>
                    {u.driver?.headshot_url && <img src={u.driver.headshot_url} alt="" style={{ position: 'absolute', inset: 0, width: '100%', height: '100%', objectFit: 'cover', objectPosition: 'top center' }} onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />}
                    <div className="driver-card-number">
                      <img src={carBadgeUrl(u.car_number)} alt={`#${u.car_number}`}
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    {u.driver && (
                      <div className="driver-card-mfr">
                        <img src={MFR_LOGOS[u.driver.manufacturer] || ''} alt=""
                          onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                      </div>
                    )}
                    <div className="driver-card-overlay">
                      <div className="driver-card-name">{u.driver?.driver || `#${u.car_number}`}</div>
                      <div className="driver-card-team">{u.driver?.team}</div>
                    </div>
                  </div>
                  <div className="driver-card-info">
                    R{u.race_number} {u.track}{u.points != null ? ` | ${u.points} pts` : ' | pending'}
                    <div style={{ color: '#888' }}>
                      Avg {u.avg != null ? u.avg.toFixed(1) : '—'}
                      {u.value != null && (
                        <span style={{ color: u.value > 0 ? '#4ADE80' : u.value < 0 ? '#f87171' : '#888', fontWeight: 'bold' }}>
                          {' | '}{u.value > 0 ? '+' : ''}{u.value.toFixed(1)} value
                        </span>
                      )}
                    </div>
                  </div>
                </div>
              ))}
              {usedData.length === 0 && <p style={{ color: '#888' }}>No picks yet</p>}
            </div>
          </div>
        </div>
        <div className="card">
          <div className="card-header">{selected} — Still Available</div>
          <div className="card-body">
            <div style={{ marginBottom: 12, padding: '8px 12px', background: 'rgba(74,222,128,0.08)', border: '1px solid rgba(74,222,128,0.3)', borderRadius: 6 }}>
              <span style={{ fontWeight: 'bold', color: '#4ADE80' }}>ROS Potential: +{Math.round(rosPotential.total)} pts</span>
              <span style={{ fontSize: '0.75em', color: '#888', marginLeft: 8 }}>
                best {remaining} unused season averages ({remaining} races left{rosPotential.droppedCount > 0 ? `; ${rosPotential.droppedCount} lowest dropped for missed weeks` : ''})
              </span>
            </div>
            <div className="driver-grid">
              {available.map(d => (
                <div key={d.car_number} className="driver-card" style={d.counted ? undefined : { opacity: 0.4 }}>
                  <div className="driver-card-img" style={{ background: '#1a1a2e', position: 'relative' }}>
                    {d.headshot_url && <img src={d.headshot_url} alt="" style={{ position: 'absolute', inset: 0, width: '100%', height: '100%', objectFit: 'cover', objectPosition: 'top center' }} onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />}
                    <div className="driver-card-number">
                      <img src={carBadgeUrl(d.car_number)} alt={`#${d.car_number}`}
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    <div className="driver-card-mfr">
                      <img src={MFR_LOGOS[d.manufacturer] || ''} alt=""
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    <div className="driver-card-overlay">
                      <div className="driver-card-name">{d.driver}</div>
                      <div className="driver-card-team">{d.team}</div>
                    </div>
                  </div>
                  <div className="driver-card-info">
                    Avg {d.avg.toFixed(1)} pts{d.counted ? '' : ' | not counted'}
                  </div>
                </div>
              ))}
              {available.length === 0 && <p style={{ color: '#888' }}>All drivers used!</p>}
            </div>
          </div>
        </div>
      </div>
    </>
  )
}
