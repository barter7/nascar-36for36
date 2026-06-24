import { useMemo, useRef, useState, useCallback } from 'react'
import { PARTICIPANTS, COLORS, carBadgeUrl, type Score, type Schedule, type Result, type Driver, type picksToLong } from '../data'

interface Props {
  scores: Score[]; schedule: Schedule[]; completedRaces: number[]; results: Result[];
  lastPicked: Record<string, number>; picksLong: ReturnType<typeof picksToLong>;
  drivers: Driver[]; onPickSaved?: () => void;
}

export default function PickHistory({ scores, schedule, completedRaces, results, lastPicked, picksLong, drivers, onPickSaved }: Props) {
  const picksScrollRef = useRef<HTMLDivElement>(null)
  const valueScrollRef = useRef<HTMLDivElement>(null)
  const [editing, setEditing] = useState<{ participant: string; race: number } | null>(null)
  const [saving, setSaving] = useState(false)

  const handleScroll = useCallback((source: 'picks' | 'value') => {
    const from = source === 'picks' ? picksScrollRef.current : valueScrollRef.current
    const to = source === 'picks' ? valueScrollRef.current : picksScrollRef.current
    if (from && to) to.scrollLeft = from.scrollLeft
  }, [])

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

  const allRaces = useMemo(() => {
    const maxCompleted = completedRaces.length > 0 ? Math.max(...completedRaces) : 0
    const nextRace = maxCompleted + 1
    const races = [...completedRaces]
    if (nextRace <= 36 && !races.includes(nextRace)) races.push(nextRace)
    return races
  }, [completedRaces])

  const getAvailableDrivers = useCallback((participant: string, race: number) => {
    const usedCars = picksLong
      .filter(p => p.participant === participant && p.race_number !== race)
      .map(p => p.car_number)
    return drivers.filter(d => !usedCars.includes(d.car_number)).sort((a, b) => a.car_number - b.car_number)
  }, [picksLong, drivers])

  const savePick = async (participant: string, race: number, carNumber: number) => {
    setSaving(true)
    try {
      const res = await fetch('/api/picks', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ participant, race, car_number: carNumber }),
      })
      if (res.ok) {
        onPickSaved?.()
        setEditing(null)
      } else {
        const err = await res.json()
        alert(`Failed to save: ${err.error}`)
      }
    } catch (e: any) {
      alert(`Error: ${e.message}`)
    }
    setSaving(false)
  }

  const renderPickCell = (p: string, r: number) => {
    const sc = scores.find(s => s.participant === p && s.race_number === r)
    const hasPick = picksLong.some(pl => pl.participant === p && pl.race_number === r)
    const withinRange = r <= (lastPicked[p] || 0)
    const isCompleted = completedRaces.includes(r)
    const isEditing = editing?.participant === p && editing?.race === r

    if (isEditing) {
      const available = getAvailableDrivers(p, r)
      return (
        <td key={r} style={{ minWidth: 120, padding: 4 }}>
          <select
            autoFocus
            style={{ background: '#1e1e2e', color: '#e0e0e0', border: '1px solid #FFD700', borderRadius: 4, padding: '4px', fontSize: '0.8em', width: '100%' }}
            onChange={e => { if (e.target.value) savePick(p, r, Number(e.target.value)) }}
            onBlur={() => setEditing(null)}
            disabled={saving}
          >
            <option value="">Select...</option>
            {available.map(d => (
              <option key={d.car_number} value={d.car_number}>#{d.car_number} {d.driver}</option>
            ))}
          </select>
        </td>
      )
    }

    if (sc) {
      return (
        <td key={r} style={{ cursor: isCompleted ? 'default' : 'pointer' }}
          onClick={() => !isCompleted && setEditing({ participant: p, race: r })}>
          <img src={carBadgeUrl(sc.car_number)} alt={`#${sc.car_number}`}
            style={{ height: 28 }}
            onError={e => {
              const el = e.target as HTMLImageElement
              el.outerHTML = `<span style="color:#FFD700;font-weight:bold">#${sc.car_number}</span>`
            }} />
          <div style={{ fontSize: '0.75em', color: '#ccc' }}>{sc.points}</div>
        </td>
      )
    }

    if (hasPick) {
      const pick = picksLong.find(pl => pl.participant === p && pl.race_number === r)
      return (
        <td key={r}>
          <img src={carBadgeUrl(pick!.car_number)} alt={`#${pick!.car_number}`}
            style={{ height: 28 }}
            onError={e => {
              const el = e.target as HTMLImageElement
              el.outerHTML = `<span style="color:#FFD700;font-weight:bold">#${pick!.car_number}</span>`
            }} />
          <div style={{ fontSize: '0.75em', color: '#555' }}>pending</div>
        </td>
      )
    }

    if (withinRange) {
      return <td key={r} style={{ fontSize: '1.2em' }} title="Missed pick">😢</td>
    }

    return (
      <td key={r}
        style={{ color: '#555', cursor: 'pointer' }}
        onClick={() => setEditing({ participant: p, race: r })}
        title="Click to add pick">
        <span style={{ fontSize: '1.2em' }}>+</span>
      </td>
    )
  }

  return (
    <>
      <div className="card">
        <div className="card-header">All Picks — Car # & Points</div>
        <div className="card-body picks-table"
          ref={picksScrollRef}
          style={{ overflowX: 'auto' }}
          onScroll={() => handleScroll('picks')}>
          <table>
            <thead>
              <tr>
                <th style={{ position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>Player</th>
                {allRaces.map(r => {
                  const label = schedule.find(s => s.race_num === r)?.track_short || `R${r}`
                  const isNext = !completedRaces.includes(r)
                  return <th key={r} style={{ minWidth: 55, fontSize: '0.65em', color: isNext ? '#FFD700' : undefined }}>{label}{isNext ? ' *' : ''}</th>
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
                    {allRaces.map(r => renderPickCell(p, r))}
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
        <div className="card-body"
          ref={valueScrollRef}
          style={{ overflowX: 'auto' }}
          onScroll={() => handleScroll('value')}>
          <table>
            <thead>
              <tr>
                <th style={{ position: 'sticky', left: 0, background: '#161625', zIndex: 1 }}>Player</th>
                {allRaces.filter(r => completedRaces.includes(r)).map(r => {
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
                    {allRaces.filter(r => completedRaces.includes(r)).map(r => {
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
