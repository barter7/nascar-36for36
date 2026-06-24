import { useMemo } from 'react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts'
import { PARTICIPANTS, COLORS, computeCumulative, type Score, type Schedule } from '../data'

interface Props {
  scores: Score[]; schedule: Schedule[]; completedRaces: number[];
}

export default function Rankings({ scores, schedule, completedRaces }: Props) {
  const rankData = useMemo(() => {
    const cum = computeCumulative(scores)
    return completedRaces.map(r => {
      const row: Record<string, number | string> = {
        race: r,
        label: schedule.find(s => s.race_num === r)?.track_short || `R${r}`,
      }
      const cumAtRace = PARTICIPANTS.map(p => {
        const entries = cum.filter(c => c.participant === p && c.race_number <= r)
        const latest = entries.sort((a, b) => b.race_number - a.race_number)[0]
        return { p, cum: latest?.cumulative || 0 }
      }).sort((a, b) => b.cum - a.cum)
      cumAtRace.forEach((c, i) => { row[c.p] = i + 1 })
      return row
    })
  }, [scores, schedule, completedRaces])

  const weeklyRanks = useMemo(() => {
    return completedRaces.map(r => {
      const raceScores = scores.filter(s => s.race_number === r).sort((a, b) => b.points - a.points)
      const row: Record<string, number | string> = {
        label: schedule.find(s => s.race_num === r)?.track_short || `R${r}`,
      }
      PARTICIPANTS.forEach(p => {
        const idx = raceScores.findIndex(s => s.participant === p)
        row[p] = idx >= 0 ? idx + 1 : '-'
      })
      return row
    })
  }, [scores, schedule, completedRaces])

  const avgRanks = useMemo(() => {
    const totals: Record<string, { sum: number; count: number }> = {}
    for (const row of weeklyRanks) {
      for (const p of PARTICIPANTS) {
        const v = row[p]
        if (typeof v === 'number') {
          if (!totals[p]) totals[p] = { sum: 0, count: 0 }
          totals[p].sum += v
          totals[p].count++
        }
      }
    }
    return totals
  }, [weeklyRanks])

  return (
    <>
      <div className="card">
        <div className="card-header">Position Over Time</div>
        <div className="card-body">
          <ResponsiveContainer width="100%" height={400}>
            <LineChart data={rankData}>
              <CartesianGrid stroke="#2a2a3a" />
              <XAxis dataKey="label" tick={{ fill: '#aaa', fontSize: 11 }} angle={-45} textAnchor="end" height={60} />
              <YAxis reversed tick={{ fill: '#aaa' }} domain={[1, 4]}
                ticks={[1, 2, 3, 4]} tickFormatter={v => ['1st', '2nd', '3rd', '4th'][v - 1] || ''} />
              <Tooltip contentStyle={{ background: '#1a1a2e', border: '1px solid #333', color: '#e0e0e0' }} />
              <Legend />
              {PARTICIPANTS.map(p => (
                <Line key={p} type="monotone" dataKey={p} stroke={COLORS[p]} strokeWidth={2.5} dot={{ r: 4 }} />
              ))}
            </LineChart>
          </ResponsiveContainer>
        </div>
      </div>
      <div className="card">
        <div className="card-header">Weekly Rank (1 = Best Pick That Week)</div>
        <div className="card-body" style={{ overflowX: 'auto' }}>
          <table>
            <thead>
              <tr>
                <th style={{ position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>Player</th>
                {weeklyRanks.map((r, i) => <th key={i} style={{ fontSize: '0.65em' }}>{r.label as string}</th>)}
                <th>Avg</th>
              </tr>
            </thead>
            <tbody>
              {PARTICIPANTS.map(p => (
                <tr key={p}>
                  <td style={{ color: COLORS[p], fontWeight: 'bold', position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>{p}</td>
                  {weeklyRanks.map((r, i) => <td key={i}>{r[p] ?? '-'}</td>)}
                  <td style={{ fontWeight: 'bold' }}>
                    {avgRanks[p] ? (avgRanks[p].sum / avgRanks[p].count).toFixed(2) : '-'}
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </>
  )
}
