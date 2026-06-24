import { useMemo } from 'react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts'
import { PARTICIPANTS, COLORS, STAGES, type Score, type Schedule, type Result, computeCumulative } from '../data'

interface Props {
  scores: Score[]; schedule: Schedule[]; completedRaces: number[]; results: Result[];
}

export default function Standings({ scores, schedule, completedRaces, results }: Props) {
  const standings = useMemo(() => {
    const totals = PARTICIPANTS.map(p => {
      const ps = scores.filter(s => s.participant === p)
      const pts = ps.reduce((a, s) => a + s.points, 0)
      const races = new Set(ps.map(s => s.race_number)).size
      const ptsArr = ps.map(s => s.points)
      return {
        participant: p, total: pts, races,
        avg: races > 0 ? Math.round(pts / races) : 0,
        best: ptsArr.length > 0 ? Math.max(...ptsArr) : 0,
        worst: ptsArr.length > 0 ? Math.min(...ptsArr) : 0,
      }
    }).sort((a, b) => b.total - a.total)
    const maxPts = totals[0]?.total || 0
    return totals.map((t, i) => ({ ...t, rank: i + 1, gap: maxPts - t.total }))
  }, [scores])

  const stagePts = useMemo(() => {
    return PARTICIPANTS.map(p => {
      const row: Record<string, number | string> = { participant: p }
      let total = 0
      for (const s of STAGES) {
        const pts = scores.filter(sc => sc.participant === p && s.races.includes(sc.race_number))
          .reduce((a, sc) => a + sc.points, 0)
        row[s.name] = pts
        total += pts
      }
      row.Total = total
      return row
    }).sort((a, b) => (b.Total as number) - (a.Total as number))
  }, [scores])

  const leader = standings[0]
  const bestWeek = useMemo(() => {
    if (scores.length === 0) return null
    const best = scores.reduce((a, b) => a.points > b.points ? a : b)
    const track = schedule.find(s => s.race_num === best.race_number)?.track_short
    return { ...best, track }
  }, [scores, schedule])

  const cumData = useMemo(() => {
    const cum = computeCumulative(scores)
    const allRaces = [...new Set(cum.map(c => c.race_number))].sort((a, b) => a - b)
    return allRaces.map(r => {
      const row: Record<string, number | string> = {
        race: r,
        label: schedule.find(s => s.race_num === r)?.track_short || `R${r}`,
      }
      for (const p of PARTICIPANTS) {
        const entry = cum.filter(c => c.participant === p && c.race_number <= r).sort((a, b) => b.race_number - a.race_number)[0]
        row[p] = entry?.cumulative || 0
      }
      return row
    })
  }, [scores, schedule])

  const stageCards = useMemo(() => {
    return STAGES.map((stage, i) => {
      const completed = stage.races.filter(r => completedRaces.includes(r))
      const isComplete = completed.length === stage.races.length
      if (completed.length === 0) {
        return { num: i + 1, status: 'future' as const, leader: 'TBD', pts: 0, label: `0/${stage.races.length} races` }
      }
      const stageSc = scores.filter(s => completed.includes(s.race_number))
      const byP = PARTICIPANTS.map(p => ({
        p, pts: stageSc.filter(s => s.participant === p).reduce((a, s) => a + s.points, 0),
      })).sort((a, b) => b.pts - a.pts)
      return {
        num: i + 1,
        status: isComplete ? 'complete' as const : 'in-progress' as const,
        leader: byP[0]?.p || 'TBD',
        pts: byP[0]?.pts || 0,
        label: isComplete ? `Winner (${completed.length}/${stage.races.length})` : `Leader (${completed.length}/${stage.races.length})`,
      }
    })
  }, [scores, completedRaces])

  return (
    <>
      <div className="card">
        <div className="card-header">Overall Standings</div>
        <div className="card-body">
          <table>
            <thead><tr><th>Rank</th><th>Player</th><th>Points</th><th>Gap</th><th>Races</th><th>Avg</th><th>Best</th><th>Worst</th></tr></thead>
            <tbody>
              {standings.map(s => (
                <tr key={s.participant}>
                  <td>{s.rank}</td>
                  <td style={{ color: COLORS[s.participant], fontWeight: 'bold' }}>{s.participant}</td>
                  <td style={{ fontWeight: 'bold' }}>{s.total}</td>
                  <td>{s.gap}</td><td>{s.races}</td><td>{s.avg}</td><td>{s.best}</td><td>{s.worst}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
      <div className="card">
        <div className="card-header">Points by Stage</div>
        <div className="card-body">
          <table>
            <thead><tr><th>Player</th>{STAGES.map(s => <th key={s.name}>{s.name}</th>)}<th>Total</th></tr></thead>
            <tbody>
              {stagePts.map(row => (
                <tr key={row.participant as string}>
                  <td style={{ color: COLORS[row.participant as string], fontWeight: 'bold' }}>{row.participant as string}</td>
                  {STAGES.map(s => <td key={s.name}>{row[s.name] as number}</td>)}
                  <td style={{ fontWeight: 'bold' }}>{row.Total as number}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
      <div className="stat-row">
        <div className="stat-card">
          <div className="stat-value" style={{ color: leader ? COLORS[leader.participant] : '#fff' }}>{leader?.participant || '—'}</div>
          <div className="stat-label">{leader ? `${leader.total} points` : 'Leader'}</div>
        </div>
        <div className="stat-card">
          <div className="stat-value">{bestWeek?.points || '—'}</div>
          <div className="stat-label">{bestWeek ? `${bestWeek.participant} @ ${bestWeek.track}` : 'Best Week'}</div>
        </div>
        <div className="stat-card">
          <div className="stat-value">{completedRaces.length}/36</div>
          <div className="stat-label">Races Run</div>
        </div>
      </div>
      <div className="card">
        <div className="card-header">Stage Winners</div>
        <div className="card-body">
          <div className="stage-row">
            {stageCards.slice(0, 3).map(s => (
              <div key={s.num} className={`stage-card ${s.status}`}>
                <div style={{ fontFamily: 'Orbitron', fontSize: '0.75em', color: '#888' }}>Stage {s.num}</div>
                <div style={{ fontFamily: 'Rajdhani', fontSize: '1em', fontWeight: 700, marginTop: 4, color: s.status === 'complete' ? '#4ADE80' : s.status === 'in-progress' ? '#FFD700' : '#666' }}>{s.leader}</div>
                {s.pts > 0 && <div style={{ fontSize: '0.8em', color: '#ccc' }}>{s.pts} pts</div>}
                <div style={{ fontSize: '0.65em', color: '#888', marginTop: 2 }}>{s.label}</div>
              </div>
            ))}
          </div>
          <div className="stage-row">
            {stageCards.slice(3).map(s => (
              <div key={s.num} className={`stage-card ${s.status}`}>
                <div style={{ fontFamily: 'Orbitron', fontSize: '0.75em', color: '#888' }}>Stage {s.num}</div>
                <div style={{ fontFamily: 'Rajdhani', fontSize: '1em', fontWeight: 700, marginTop: 4, color: s.status === 'complete' ? '#4ADE80' : s.status === 'in-progress' ? '#FFD700' : '#666' }}>{s.leader}</div>
                {s.pts > 0 && <div style={{ fontSize: '0.8em', color: '#ccc' }}>{s.pts} pts</div>}
                <div style={{ fontSize: '0.65em', color: '#888', marginTop: 2 }}>{s.label}</div>
              </div>
            ))}
          </div>
        </div>
      </div>
      <div className="card">
        <div className="card-header">Cumulative Points</div>
        <div className="card-body">
          <ResponsiveContainer width="100%" height={400}>
            <LineChart data={cumData}>
              <CartesianGrid stroke="#2a2a3a" />
              <XAxis dataKey="label" tick={{ fill: '#aaa', fontSize: 11 }} angle={-45} textAnchor="end" height={60} />
              <YAxis tick={{ fill: '#aaa' }} />
              <Tooltip contentStyle={{ background: '#1a1a2e', border: '1px solid #333', color: '#e0e0e0' }} />
              <Legend />
              {PARTICIPANTS.map(p => (
                <Line key={p} type="monotone" dataKey={p} stroke={COLORS[p]} strokeWidth={2.5} dot={{ r: 4 }} />
              ))}
            </LineChart>
          </ResponsiveContainer>
        </div>
      </div>
    </>
  )
}
