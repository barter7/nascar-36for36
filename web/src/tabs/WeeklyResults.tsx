import { useState, useMemo } from 'react'
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell, LabelList } from 'recharts'
import { COLORS, carBadgeUrl, MFR_LOGOS, type Score, type Schedule, type Driver } from '../data'

interface Props {
  scores: Score[]; schedule: Schedule[]; completedRaces: number[]; drivers: Driver[];
}

export default function WeeklyResults({ scores, schedule, completedRaces, drivers }: Props) {
  const [selectedRace, setSelectedRace] = useState<number | null>(null)
  const race = selectedRace ?? completedRaces[completedRaces.length - 1] ?? null

  const raceScores = useMemo(() => {
    if (!race) return []
    return scores.filter(s => s.race_number === race).sort((a, b) => b.points - a.points)
  }, [scores, race])

  const barData = useMemo(() =>
    raceScores.map(s => ({ name: s.participant, points: s.points, color: COLORS[s.participant] })),
  [raceScores])

  const driverMap = useMemo(() => {
    const m: Record<number, Driver> = {}
    for (const d of drivers) m[d.car_number] = d
    return m
  }, [drivers])

  return (
    <>
      <div className="race-buttons">
        {completedRaces.map(r => {
          const label = schedule.find(s => s.race_num === r)?.track_short || `R${r}`
          return (
            <button key={r} className={`race-btn ${race === r ? 'active' : ''}`}
              onClick={() => setSelectedRace(r)}>
              {r}. {label}
            </button>
          )
        })}
      </div>
      {race && (
        <div className="two-col">
          <div className="card">
            <div className="card-header">Picks & Points</div>
            <div className="card-body">
              <div className="driver-grid">
                {raceScores.map((s, i) => {
                  const d = driverMap[s.car_number]
                  return (
                    <div key={s.participant} className={`driver-card ${i === 0 ? 'highlight' : ''}`}>
                      <div className="driver-card-img" style={{
                        backgroundImage: d?.headshot_url ? `url(${d.headshot_url})` : undefined,
                        background: d?.headshot_url ? undefined : '#1a1a2e',
                      }}>
                        <div className="driver-card-number">
                          <img src={carBadgeUrl(s.car_number)} alt={`#${s.car_number}`}
                            onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                        </div>
                        {d && (
                          <div className="driver-card-mfr">
                            <img src={MFR_LOGOS[d.manufacturer] || ''} alt=""
                              onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                          </div>
                        )}
                        <div className="driver-card-overlay">
                          <div className="driver-card-name">{s.driver}</div>
                          <div className="driver-card-team">{d?.team}</div>
                        </div>
                      </div>
                      <div className="driver-card-info">
                        P{s.finish_pos} | {s.points} pts | {s.participant}
                      </div>
                    </div>
                  )
                })}
              </div>
            </div>
          </div>
          <div className="card">
            <div className="card-header">Weekly Points</div>
            <div className="card-body">
              <ResponsiveContainer width="100%" height={300}>
                <BarChart data={barData}>
                  <XAxis dataKey="name" tick={{ fill: '#aaa' }} />
                  <YAxis tick={{ fill: '#aaa' }} />
                  <Tooltip contentStyle={{ background: '#1a1a2e', border: '1px solid #333', color: '#e0e0e0' }} />
                  <Bar dataKey="points" radius={[4, 4, 0, 0]}>
                    {barData.map((d, i) => <Cell key={i} fill={d.color} />)}
                    <LabelList dataKey="points" position="top" fill="#e0e0e0" style={{ fontWeight: 'bold' }} />
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </div>
        </div>
      )}
    </>
  )
}
