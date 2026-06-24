import { useMemo } from 'react'
import { PARTICIPANTS, COLORS, INITIALS, type Driver, type Result, carBadgeUrl, MFR_LOGOS } from '../data'
import type { picksToLong } from '../data'

interface Props {
  drivers: Driver[]; results: Result[]; picksLong: ReturnType<typeof picksToLong>;
}

export default function Roster({ drivers, results, picksLong }: Props) {
  const driverAvg = useMemo(() => {
    const map: Record<number, { total: number; count: number }> = {}
    for (const r of results) {
      if (!map[r.car_number]) map[r.car_number] = { total: 0, count: 0 }
      map[r.car_number].total += r.points
      map[r.car_number].count++
    }
    return map
  }, [results])

  const pickMap = useMemo(() => {
    const map: Record<number, string[]> = {}
    for (const p of picksLong) {
      if (!map[p.car_number]) map[p.car_number] = []
      if (!map[p.car_number].includes(p.participant)) map[p.car_number].push(p.participant)
    }
    return map
  }, [picksLong])

  const sorted = useMemo(() => {
    return [...drivers].sort((a, b) => {
      const avgA = driverAvg[a.car_number] ? driverAvg[a.car_number].total / driverAvg[a.car_number].count : 0
      const avgB = driverAvg[b.car_number] ? driverAvg[b.car_number].total / driverAvg[b.car_number].count : 0
      return avgB - avgA
    })
  }, [drivers, driverAvg])

  return (
    <div className="card">
      <div className="card-header">2026 NASCAR Cup Series Roster</div>
      <div className="card-body">
        <div className="driver-grid">
          {sorted.map(d => {
            const avg = driverAvg[d.car_number]
            const pickers = pickMap[d.car_number] || []
            const avgPts = avg ? (avg.total / avg.count).toFixed(1) : '0'
            const races = avg?.count || 0
            return (
              <div key={d.car_number} className="driver-card">
                <div className="driver-card-badges">
                  {PARTICIPANTS.map(p => (
                    pickers.includes(p)
                      ? <span key={p} className="pick-badge" style={{ background: COLORS[p] }}>{INITIALS[p]}</span>
                      : <span key={p} className="pick-badge empty" />
                  ))}
                </div>
                <div className="driver-card-img" style={{ background: '#1a1a2e', position: 'relative' }}>
                  {d.headshot_url && (
                    <img src={d.headshot_url} alt={d.driver}
                      style={{ position: 'absolute', inset: 0, width: '100%', height: '100%', objectFit: 'cover', objectPosition: 'top center' }}
                      onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                  )}
                  {!d.headshot_url && (
                    <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', height: '100%', fontFamily: 'Orbitron', fontSize: 48, fontWeight: 'bold', color: '#FFD700' }}>
                      #{d.car_number}
                    </div>
                  )}
                  <div className="driver-card-number">
                    <img src={carBadgeUrl(d.car_number)} alt={`#${d.car_number}`}
                      onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                  </div>
                  <div className="driver-card-mfr">
                    <img src={MFR_LOGOS[d.manufacturer] || ''} alt={d.manufacturer}
                      onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                  </div>
                  <div className="driver-card-overlay">
                    <div className="driver-card-name">{d.driver}</div>
                    <div className="driver-card-team">{d.team}</div>
                  </div>
                </div>
                <div className="driver-card-info">
                  {races > 0 ? `${avgPts} avg pts (${races} races)` : 'No race data'}
                </div>
              </div>
            )
          })}
        </div>
      </div>
    </div>
  )
}
