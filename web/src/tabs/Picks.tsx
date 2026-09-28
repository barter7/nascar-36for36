import { useEffect, useMemo, useRef, useState } from 'react'
import { PARTICIPANTS } from '../data'
import type { AppData } from '../App'
import { TOTAL_RACES, valueOverAvg } from '../stats'
import { CarBadge, PlayerName, signed, valueClass } from '../components'

interface Props {
  data: AppData
  onPickSaved: (participant: string, race: number, carNumber: number | null) => void
}

const RACES = Array.from({ length: TOTAL_RACES }, (_, i) => i + 1)

export default function Picks({ data, onPickSaved }: Props) {
  const { stats, scores, picks, drivers, lastPicked, trackName, year } = data
  const { completed, nextRace, cars, raceTop, players } = stats
  const editable = year === 2026
  const scrollRef = useRef<HTMLDivElement>(null)
  const nextRef = useRef<HTMLTableCellElement>(null)
  const [editing, setEditing] = useState<{ participant: string; race: number } | null>(null)
  const [saving, setSaving] = useState(false)

  useEffect(() => {
    const box = scrollRef.current, cell = nextRef.current
    if (box && cell) box.scrollLeft = Math.max(0, cell.offsetLeft - box.clientWidth + cell.offsetWidth * 3)
  }, [])

  const totals = useMemo(() => Object.fromEntries(players.map(p => [p.participant, p])), [players])

  const optionsFor = (participant: string, race: number) => {
    const current = picks.find(p => p.participant === participant && p.race_number === race)?.car_number
    const used = new Set(picks.filter(p => p.participant === participant && p.race_number !== race).map(p => p.car_number))
    return drivers
      .filter(d => d.car_number === current || (!used.has(d.car_number) && cars[d.car_number]?.active !== false))
      .sort((a, b) => a.car_number - b.car_number)
  }

  const savePick = async (participant: string, race: number, carNumber: number | null) => {
    setSaving(true)
    try {
      const res = await fetch('/api/picks', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ participant, race, car_number: carNumber || 0 }),
      })
      if (res.ok) {
        onPickSaved(participant, race, carNumber)
        setEditing(null)
      } else {
        const err = await res.json().catch(() => ({ error: res.statusText }))
        alert(`Failed to save: ${err.error}`)
      }
    } catch (e) {
      alert(`Error: ${(e as Error).message}`)
    }
    setSaving(false)
  }

  const cell = (p: string, r: number) => {
    const pick = picks.find(x => x.participant === p && x.race_number === r)
    const sc = scores.find(s => s.participant === p && s.race_number === r)
    const done = completed.includes(r)
    const open = () => editable && setEditing({ participant: p, race: r })

    if (editing?.participant === p && editing.race === r) {
      return (
        <td key={r} className="pick-cell editing">
          <select autoFocus disabled={saving} defaultValue=""
            onChange={e => {
              const v = e.target.value
              if (v === 'REMOVE') savePick(p, r, null)
              else if (v) savePick(p, r, Number(v))
            }}
            onBlur={() => { if (!saving) setTimeout(() => setEditing(null), 200) }}>
            <option value="">Pick R{r}…</option>
            {pick && <option value="REMOVE">✕ Remove pick</option>}
            {optionsFor(p, r).map(d => (
              <option key={d.car_number} value={d.car_number}>
                #{d.car_number} {d.driver}{cars[d.car_number] ? ` · ${cars[d.car_number].avg.toFixed(1)}` : ''}
              </option>
            ))}
          </select>
        </td>
      )
    }

    if (sc) {
      const v = valueOverAvg(sc, cars)
      const top = sc.points > 0 && sc.points === raceTop[r]
      return (
        <td key={r} className={`pick-cell${top ? ' top' : ''}`} onClick={open}
          title={`#${sc.car_number} ${sc.driver} — P${sc.finish_pos}, ${sc.points} pts`}>
          <CarBadge car={sc.car_number} height={22} />
          <div className="pts">{sc.points}</div>
          {v != null && <div className={`val ${valueClass(v)}`}>{signed(v)}</div>}
        </td>
      )
    }

    if (pick) {
      return (
        <td key={r} className={`pick-cell pending${done ? ' dns' : ''}`} onClick={open}
          title={done ? 'Car did not race' : 'Pending'}>
          <CarBadge car={pick.car_number} height={22} />
          <div className="pts">{done ? '0' : '—'}</div>
        </td>
      )
    }

    if (done && r <= (lastPicked[p] || 0)) {
      return (
        <td key={r} className="pick-cell missed" onClick={open} title="Missed pick">
          😢<div className="pts">0</div>
        </td>
      )
    }

    return <td key={r} className={`pick-cell empty${editable ? '' : ' locked'}`} onClick={open}>{editable ? '+' : ''}</td>
  }

  return (
    <div className="card">
      <div className="card-header">
        <span>Picks</span>
        <span className="card-sub">{editable ? 'Tap any cell to set or change a pick' : '2025 season (read-only)'}</span>
      </div>
      <div className="grid-scroll" ref={scrollRef}>
        <table className="picks-grid">
          <thead>
            <tr>
              <th className="sticky-col">Player</th>
              {RACES.map(r => (
                <th key={r} ref={r === nextRace ? nextRef : undefined}
                  className={r === nextRace ? 'next' : completed.includes(r) ? '' : 'future'}>
                  <div className="race-num">R{r}</div>
                  <div className="race-track">{trackName(r)}</div>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {PARTICIPANTS.map(p => (
              <tr key={p}>
                <td className="sticky-col">
                  <PlayerName name={p} />
                  <div className="row-total">{totals[p]?.total ?? 0}</div>
                </td>
                {RACES.map(r => cell(p, r))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="legend">
        <span><i className="swatch top" /> Top score that week</span>
        <span><i className="swatch pending" /> Pending</span>
        <span><i className="swatch missed" /> Missed</span>
        <span><b className="pos">+8</b> vs car's season avg</span>
      </div>
    </div>
  )
}
