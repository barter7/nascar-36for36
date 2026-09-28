import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine } from 'recharts'
import { PARTICIPANTS, COLORS } from '../data'
import type { AppData } from '../App'
import { PlayerName } from '../components'

export default function Standings({ data }: { data: AppData }) {
  const { stats, schedule, raceLabel, year } = data
  const { players, stages, gapSeries, completed, nextRace, remaining } = stats
  const leader = players[0]
  const runnerUp = players[1]
  const best = players.map(p => p.best).filter(Boolean).sort((a, b) => b!.points - a!.points)[0]
  const next = nextRace ? schedule.find(s => s.race_num === nextRace) : undefined
  const anyDropped = players.some(p => p.dropped > 0)

  return (
    <>
      <div className="stat-row">
        <div className="stat-card">
          <div className="stat-value" style={{ color: COLORS[leader?.participant] }}>{leader?.participant ?? '—'}</div>
          <div className="stat-label">{leader ? `leads by ${leader.total - (runnerUp?.total ?? 0)}` : 'Leader'}</div>
        </div>
        <div className="stat-card">
          <div className="stat-value">{best?.points ?? '—'}</div>
          <div className="stat-label">{best ? `Best week · ${best.participant} R${best.race_number}` : 'Best week'}</div>
        </div>
        <div className="stat-card">
          <div className="stat-value">{remaining}</div>
          <div className="stat-label">
            {year === 2026 && next ? `left · next ${next.track_short} ${next.date.slice(5).replace('-', '/')}` : 'races left'}
          </div>
        </div>
      </div>

      <div className="card">
        <div className="card-header"><span>Standings</span><span className="card-sub">Thru R{completed[completed.length - 1] ?? 0}</span></div>
        <div className="table-scroll">
          <table className="standings">
            <thead>
              <tr><th>#</th><th className="left">Player</th><th>Pts</th><th>Gap</th><th title="Weekly top scores">Wins</th><th title="Points per race run">Avg</th><th title="Current points + best remaining unused drivers">Proj</th></tr>
            </thead>
            <tbody>
              {players.map(p => (
                <tr key={p.participant}>
                  <td className="rank">
                    {p.rank}
                    {p.movement !== 0 && <span className={p.movement > 0 ? 'pos' : 'neg'}>{p.movement > 0 ? '▲' : '▼'}</span>}
                  </td>
                  <td className="left">
                    <PlayerName name={p.participant} />
                    {p.misses > 0 && <span className="tag">{p.misses} missed</span>}
                  </td>
                  <td className="strong">{p.total}</td>
                  <td className="muted">{p.gap ? `-${p.gap}` : '—'}</td>
                  <td>{p.wins}</td>
                  <td>{p.avgPerRace.toFixed(1)}</td>
                  <td className="gold">{remaining ? p.projected : '—'}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        {remaining > 0 && (
          <div className="footnote">
            Proj = current points + season averages of each player's best {remaining} unused active drivers (one per remaining race).
            {anyDropped && ' Players with missed weeks have extra unused drivers, so their lowest averages drop off.'}
          </div>
        )}
      </div>

      <div className="card">
        <div className="card-header"><span>Stages</span><span className="card-sub">6-race segments · winner highlighted</span></div>
        <div className="table-scroll">
          <table className="stages">
            <thead>
              <tr>
                <th className="left">Player</th>
                {stages.map((s, i) => (
                  <th key={s.name} className={s.status}>
                    S{i + 1}
                    <div className="race-num">{s.status === 'live' ? `${s.done}/6` : `R${s.races[0]}–${s.races[5]}`}</div>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {PARTICIPANTS.map(p => (
                <tr key={p}>
                  <td className="left"><PlayerName name={p} /></td>
                  {stages.map(s => (
                    <td key={s.name} className={s.leaders.includes(p) ? `stage-lead ${s.status}` : s.status === 'future' ? 'muted' : ''}>
                      {s.status === 'future' ? '·' : s.points[p]}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {gapSeries.length > 1 && (
        <div className="card">
          <div className="card-header"><span>Points Behind Leader</span><span className="card-sub">after each race</span></div>
          <div className="card-body chart">
            <ResponsiveContainer width="100%" height={260}>
              <LineChart data={gapSeries} margin={{ top: 8, right: 12, left: -12, bottom: 0 }}>
                <CartesianGrid stroke="#23233a" vertical={false} />
                <XAxis dataKey="race" tick={{ fill: '#888', fontSize: 11 }} tickFormatter={r => `R${r}`} interval="preserveStartEnd" minTickGap={16} />
                <YAxis tick={{ fill: '#888', fontSize: 11 }} />
                <ReferenceLine y={0} stroke="#FFD700" strokeDasharray="4 4" />
                <Tooltip
                  contentStyle={{ background: '#161625', border: '1px solid #333', borderRadius: 8 }}
                  labelFormatter={r => raceLabel(Number(r))}
                  formatter={(v: number, name: string) => [v === 0 ? 'Leader' : v, name]}
                  itemSorter={item => -(item.value as number)} />
                {PARTICIPANTS.map(p => (
                  <Line key={p} type="linear" dataKey={p} stroke={COLORS[p]} strokeWidth={2} dot={false} isAnimationActive={false} />
                ))}
              </LineChart>
            </ResponsiveContainer>
            <div className="legend">
              {PARTICIPANTS.map(p => <span key={p}><i className="swatch" style={{ background: COLORS[p] }} />{p}</span>)}
            </div>
          </div>
        </div>
      )}

    </>
  )
}
