import { useState, useEffect } from 'react'
import { loadData, picksToLong, computeScores, getLastPickedRace, type Driver, type Result, type Schedule, type Score, type Pick } from './data'
import Standings from './tabs/Standings'
import Roster from './tabs/Roster'
import WeeklyResults from './tabs/WeeklyResults'
import PickHistory from './tabs/PickHistory'
import Rankings from './tabs/Rankings'
import DriversUsed from './tabs/DriversUsed'

const TABS = ['Standings', 'Roster', 'Weekly', 'Picks', 'Rankings', 'Drivers']

export default function App() {
  const [year, setYear] = useState(2026)
  const [tab, setTab] = useState('Standings')
  const [drivers, setDrivers] = useState<Driver[]>([])
  const [results, setResults] = useState<Result[]>([])
  const [schedule, setSchedule] = useState<Schedule[]>([])
  const [scores, setScores] = useState<Score[]>([])
  const [picksLong, setPicksLong] = useState<ReturnType<typeof picksToLong>>([])
  const [lastPicked, setLastPicked] = useState<Record<string, number>>({})
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    setLoading(true)
    loadData(year).then(d => {
      setDrivers(d.drivers)
      setResults(d.results)
      setSchedule(d.schedule)
      const pl = picksToLong(d.picks)
      setPicksLong(pl)
      setScores(computeScores(pl, d.results))
      setLastPicked(getLastPickedRace(d.picks))
      setLoading(false)
    })
  }, [year])

  const completedRaces = [...new Set(results.map(r => r.race_number))].sort((a, b) => a - b)

  return (
    <>
      <nav className="navbar">
        <div className="navbar-title">
          NASCAR 36 for 36
          <div style={{ display: 'flex', gap: 4 }}>
            <button className={`year-btn ${year === 2026 ? 'active' : ''}`} onClick={() => setYear(2026)}>2026</button>
            <button className={`year-btn ${year === 2025 ? 'active' : ''}`} onClick={() => setYear(2025)}>2025</button>
          </div>
        </div>
      </nav>
      <div className="tabs">
        {TABS.map(t => (
          <button key={t} className={`tab ${tab === t ? 'active' : ''}`} onClick={() => setTab(t)}>{t}</button>
        ))}
      </div>
      <div className="content">
        {loading ? (
          <div className="loading">Loading data...</div>
        ) : (
          <>
            {tab === 'Standings' && <Standings scores={scores} schedule={schedule} completedRaces={completedRaces} results={results} />}
            {tab === 'Roster' && <Roster drivers={drivers} results={results} picksLong={picksLong} />}
            {tab === 'Weekly' && <WeeklyResults scores={scores} schedule={schedule} completedRaces={completedRaces} drivers={drivers} />}
            {tab === 'Picks' && <PickHistory scores={scores} schedule={schedule} completedRaces={completedRaces} results={results} lastPicked={lastPicked} picksLong={picksLong} drivers={drivers} onPickSaved={() => {
                      loadData(year).then(d => {
                        const pl = picksToLong(d.picks)
                        setPicksLong(pl)
                        setScores(computeScores(pl, d.results))
                        setLastPicked(getLastPickedRace(d.picks))
                      })
                    }} />}
            {tab === 'Rankings' && <Rankings scores={scores} schedule={schedule} completedRaces={completedRaces} />}
            {tab === 'Drivers' && <DriversUsed drivers={drivers} picksLong={picksLong} scores={scores} schedule={schedule} />}
          </>
        )}
      </div>
    </>
  )
}
