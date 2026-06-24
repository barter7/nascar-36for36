import { useMemo } from 'react'
import { PARTICIPANTS, COLORS, carBadgeUrl, type Score, type Schedule, type Result, type picksToLong } from '../data'

interface Props {
  scores: Score[]; schedule: Schedule[]; completedRaces: number[]; results: Result[];
  lastPicked: Record<string, number>; picksLong: ReturnType<typeof picksToLong>;
}

export default function PickHistory({ scores, schedule, completedRaces, results, lastPicked, picksLong }: Props) {
  const driverAvg = useMemo(() => {
    const map: Record<string, { total: number; count: number }> = {}
    for (const r of results) {
      const key = `${r.car_number}-${r.driver}`
      if (!map[key]) map[key] = { total: 0, count: 0 }
      map[key].total += r.points
      map[key].count++
    }
    return map
  }, [results])

  return (
    <>
      <div className="card">
        <div className="card-header">All Picks — Car # & Points</div>
        <div className="card-body picks-table" style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th style={{ position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>Player</th>
                {completedRaces.map(r => {
                  const label = schedule.find(s => s.race_num === r)?.track_short || `R${r}`
                  return <th key={r} style={{ minWidth: 55, fontSize: '0.65em' }}>{label}</th>
                })}
                <th>Total</th>
              </tr>
            </thead>
            <tbody>
              {PARTICIPANTS.map(p => {
                const pScores = scores.filter(s => s.participant === p)
                const total = pScores.reduce((a, s) => a + s.points, 0)
                return (
                  <tr key={p}>
                    <td style={{ color: COLORS[p], fontWeight: 'bold', position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>{p}</td>
                    {completedRaces.map(r => {
                      const sc = pScores.find(s => s.race_number === r)
                      if (!sc) {
                        const hasPick = picksLong.some(pl => pl.participant === p && pl.race_number === r)
                        const withinRange = r <= (lastPicked[p] || 0)
                        if (withinRange && !hasPick) {
                          return <td key={r} style={{ fontSize: '1.2em' }} title="Missed pick">😢</td>
                        }
                        return <td key={r} style={{ color: '#555' }}>-</td>
                      }
                      return (
                        <td key={r}>
                          <img src={carBadgeUrl(sc.car_number)} alt={`#${sc.car_number}`}
                            style={{ height: 28 }}
                            onError={e => {
                              const el = e.target as HTMLImageElement
                              el.outerHTML = `<span style="color:#FFD700;font-weight:bold">#${sc.car_number}</span>`
                            }} />
                          <div style={{ fontSize: '0.75em', color: '#ccc' }}>{sc.points}</div>
                        </td>
                      )
                    })}
                    <td style={{ fontWeight: 'bold', color: '#FFD700' }}>{total}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      </div>

      <div className="card">
        <div className="card-header">Value Over Average</div>
        <div className="card-body" style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th style={{ position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>Player</th>
                {completedRaces.map(r => {
                  const label = schedule.find(s => s.race_num === r)?.track_short || `R${r}`
                  return <th key={r} style={{ minWidth: 50, fontSize: '0.65em' }}>{label}</th>
                })}
                <th>Total</th>
              </tr>
            </thead>
            <tbody>
              {PARTICIPANTS.map(p => {
                const pScores = scores.filter(s => s.participant === p)
                let totalValue = 0
                return (
                  <tr key={p}>
                    <td style={{ color: COLORS[p], fontWeight: 'bold', position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>{p}</td>
                    {completedRaces.map(r => {
                      const sc = pScores.find(s => s.race_number === r)
                      if (!sc) {
                        const withinRange = r <= (lastPicked[p] || 0)
                        const hasPick = picksLong.some(pl => pl.participant === p && pl.race_number === r)
                        if (withinRange && !hasPick) return <td key={r} style={{ fontSize: '1.2em' }}>😢</td>
                        return <td key={r} style={{ color: '#555' }}>-</td>
                      }
                      const key = `${sc.car_number}-${sc.driver}`
                      const avg = driverAvg[key]
                      const val = avg ? sc.points - avg.total / avg.count : 0
                      totalValue += val
                      const color = val > 0 ? '#4ADE80' : val < 0 ? '#f87171' : '#888'
                      return <td key={r} style={{ color, fontSize: '0.85em' }}>{val.toFixed(1)}</td>
                    })}
                    <td style={{ fontWeight: 'bold', color: totalValue > 0 ? '#4ADE80' : '#f87171' }}>{totalValue.toFixed(1)}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      </div>
    </>
  )
}
