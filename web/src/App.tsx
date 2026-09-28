import { useState, useEffect, useCallback, useMemo } from 'react'
import { loadData, picksToLong, computeScores, getLastPickedRace, type Driver, type Result, type Schedule, type PickLong } from './data'
import { buildStats, type Stats } from './stats'
import Standings from './tabs/Standings'
import Weekly from './tabs/Weekly'
import Picks from './tabs/Picks'
import Drivers from './tabs/Drivers'

const TABS = ['Picks', 'Standings', 'Weekly', 'Drivers'] as const
type Tab = typeof TABS[number]
const ALIASES: Record<string, Tab> = { roster: 'Drivers', rankings: 'Standings' }

function getTabFromHash(): Tab {
  const hash = window.location.hash.replace('#', '').toLowerCase()
  return TABS.find(t => t.toLowerCase() === hash) ?? ALIASES[hash] ?? 'Picks'
}

export interface AppData {
  year: number; drivers: Driver[]; results: Result[]; schedule: Schedule[];
  picks: PickLong[]; scores: ReturnType<typeof computeScores>;
  lastPicked: Record<string, number>; stats: Stats;
  trackName: (race: number) => string;
  raceLabel: (race: number) => string;
  driverFor: (car: number) => Driver | undefined;
}

export default function App() {
  const [year, setYear] = useState(2026)
  const [tab, setTab] = useState<Tab>(getTabFromHash)
  const [drivers, setDrivers] = useState<Driver[]>([])
  const [results, setResults] = useState<Result[]>([])
  const [schedule, setSchedule] = useState<Schedule[]>([])
  const [picks, setPicks] = useState<PickLong[]>([])
  const [lastPicked, setLastPicked] = useState<Record<string, number>>({})
  const [loading, setLoading] = useState(true)

  const selectTab = useCallback((t: Tab) => {
    setTab(t)
    window.location.hash = t.toLowerCase()
  }, [])

  useEffect(() => {
    const onHashChange = () => setTab(getTabFromHash())
    window.addEventListener('hashchange', onHashChange)
    return () => window.removeEventListener('hashchange', onHashChange)
  }, [])

  useEffect(() => {
    setLoading(true)
    loadData(year).then(d => {
      setDrivers(d.drivers)
      setResults(d.results)
      setSchedule(d.schedule)
      setPicks(picksToLong(d.picks))
      setLastPicked(getLastPickedRace(d.picks))
      setLoading(false)
    })
  }, [year])

  const handlePickSaved = useCallback((participant: string, race: number, carNumber: number | null) => {
    setPicks(prev => {
      const others = prev.filter(p => !(p.participant === participant && p.race_number === race))
      return carNumber ? [...others, { participant, race_number: race, car_number: carNumber }] : others
    })
    if (carNumber) setLastPicked(prev => ({ ...prev, [participant]: Math.max(prev[participant] || 0, race) }))
  }, [])

  const data = useMemo<AppData>(() => {
    const scores = computeScores(picks, results)
    const trackNames = new Map(schedule.map(s => [s.race_num, s.track_short]))
    const driverMap = new Map(drivers.map(d => [d.car_number, d]))
    return {
      year, drivers, results, schedule, picks, scores, lastPicked,
      stats: buildStats(drivers, results, picks, scores),
      trackName: r => trackNames.get(r) || '',
      raceLabel: r => [`R${r}`, trackNames.get(r)].filter(Boolean).join(' · '),
      driverFor: c => driverMap.get(c),
    }
  }, [year, drivers, results, schedule, picks, lastPicked])

  const { completed, nextRace } = data.stats
  const lastRace = completed[completed.length - 1]

  return (
    <>
      <nav className="navbar">
        <div className="navbar-title">NASCAR 36 for 36</div>
        <div className="year-toggle">
          {[2026, 2025].map(y => (
            <button key={y} className={`year-btn ${year === y ? 'active' : ''}`} onClick={() => setYear(y)}>{y}</button>
          ))}
        </div>
      </nav>
      <div className="tabs">
        {TABS.map(t => (
          <button key={t} className={`tab ${tab === t ? 'active' : ''}`} onClick={() => selectTab(t)}>{t}</button>
        ))}
        {!loading && lastRace && (
          <span className="tabs-status">
            Thru R{lastRace}{nextRace && year === 2026 ? ` · Next: ${data.raceLabel(nextRace)}` : ''}
          </span>
        )}
      </div>
      <main className="content">
        {loading ? <div className="loading">Loading…</div> : (
          <>
            {tab === 'Picks' && <Picks data={data} onPickSaved={handlePickSaved} />}
            {tab === 'Standings' && <Standings data={data} />}
            {tab === 'Weekly' && <Weekly data={data} />}
            {tab === 'Drivers' && <Drivers data={data} />}
          </>
        )}
      </main>
    </>
  )
}
