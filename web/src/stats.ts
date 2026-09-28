import { PARTICIPANTS, STAGES, type Driver, type Result, type Score, type PickLong } from './data'

export const TOTAL_RACES = 36
const FORM_WINDOW = 5
const INACTIVE_AFTER = 3

export interface CarStat {
  avg: number; races: number; form: number | null; active: boolean; lastRace: number;
}

export interface PlayerStat {
  participant: string; rank: number; total: number; gap: number;
  avgPerRace: number; wins: number; misses: number; best: Score | null;
  potential: number; projected: number; poolSize: number; dropped: number;
  movement: number;
}

export interface StageStat {
  name: string; races: number[]; done: number;
  status: 'complete' | 'live' | 'future';
  points: Record<string, number>; leaders: string[];
}

export interface Stats {
  completed: number[]; nextRace: number | null; remaining: number;
  cars: Record<number, CarStat>;
  raceTop: Record<number, number>;
  players: PlayerStat[];
  stages: StageStat[];
  gapSeries: Record<string, number>[];
  usedBy: Record<string, Map<number, number>>;
}

export function valueOverAvg(s: Score, cars: Record<number, CarStat>): number | null {
  const c = cars[s.car_number]
  return c ? s.points - c.avg : null
}

export function buildStats(drivers: Driver[], results: Result[], picks: PickLong[], scores: Score[]): Stats {
  const completed = [...new Set(results.map(r => r.race_number))].sort((a, b) => a - b)
  const last = completed.length ? completed[completed.length - 1] : 0
  const nextRace = last < TOTAL_RACES ? last + 1 : null
  const remaining = TOTAL_RACES - completed.length

  const byCar: Record<number, Result[]> = {}
  for (const r of results) (byCar[r.car_number] ||= []).push(r)
  const recent = new Set(completed.slice(-INACTIVE_AFTER))
  const formRaces = new Set(completed.slice(-FORM_WINDOW))
  const cars: Record<number, CarStat> = {}
  for (const [car, rows] of Object.entries(byCar)) {
    const pts = rows.map(r => r.points)
    const form = rows.filter(r => formRaces.has(r.race_number)).map(r => r.points)
    cars[Number(car)] = {
      avg: pts.reduce((a, b) => a + b, 0) / pts.length,
      races: rows.length,
      form: form.length ? form.reduce((a, b) => a + b, 0) / form.length : null,
      active: completed.length < INACTIVE_AFTER || rows.some(r => recent.has(r.race_number)),
      lastRace: Math.max(...rows.map(r => r.race_number)),
    }
  }

  const raceTop: Record<number, number> = {}
  for (const s of scores) raceTop[s.race_number] = Math.max(raceTop[s.race_number] ?? 0, s.points)

  const usedBy: Record<string, Map<number, number>> = {}
  for (const p of PARTICIPANTS) usedBy[p] = new Map()
  for (const p of picks) usedBy[p.participant]?.set(p.car_number, p.race_number)

  const totalThrough = (p: string, race: number) =>
    scores.filter(s => s.participant === p && s.race_number <= race).reduce((a, s) => a + s.points, 0)
  const rankAt = (race: number) => {
    const order = [...PARTICIPANTS].sort((a, b) => totalThrough(b, race) - totalThrough(a, race))
    return (p: string) => order.indexOf(p as typeof PARTICIPANTS[number]) + 1
  }
  const prevRank = completed.length > 1 ? rankAt(completed[completed.length - 2]) : null

  const pool = drivers.filter(d => cars[d.car_number]?.active)
  const rows = PARTICIPANTS.map(p => {
    const mine = scores.filter(s => s.participant === p)
    const total = mine.reduce((a, s) => a + s.points, 0)
    const pickedRaces = new Set(picks.filter(pk => pk.participant === p).map(pk => pk.race_number))
    const usedDone = new Set(picks.filter(pk => pk.participant === p && completed.includes(pk.race_number)).map(pk => pk.car_number))
    const unused = pool.filter(d => !usedDone.has(d.car_number)).map(d => cars[d.car_number].avg).sort((a, b) => b - a)
    const potential = unused.slice(0, remaining).reduce((a, b) => a + b, 0)
    return {
      participant: p, total,
      avgPerRace: completed.length ? total / completed.length : 0,
      wins: completed.filter(r => {
        const s = mine.find(x => x.race_number === r)
        return s && s.points > 0 && s.points === raceTop[r]
      }).length,
      misses: completed.filter(r => !pickedRaces.has(r)).length,
      best: mine.reduce<Score | null>((a, s) => (!a || s.points > a.points ? s : a), null),
      potential: Math.round(potential),
      projected: Math.round(total + potential),
      poolSize: unused.length,
      dropped: Math.max(0, unused.length - remaining),
    }
  }).sort((a, b) => b.total - a.total)
  const leader = rows[0]?.total ?? 0
  const players: PlayerStat[] = rows.map((r, i) => ({
    ...r, rank: i + 1, gap: leader - r.total,
    movement: prevRank ? prevRank(r.participant) - (i + 1) : 0,
  }))

  const stages: StageStat[] = STAGES.map(st => {
    const doneRaces = st.races.filter(r => completed.includes(r))
    const points: Record<string, number> = {}
    for (const p of PARTICIPANTS) {
      points[p] = scores.filter(s => s.participant === p && doneRaces.includes(s.race_number)).reduce((a, s) => a + s.points, 0)
    }
    const top = Math.max(...Object.values(points))
    return {
      name: st.name, races: st.races, done: doneRaces.length,
      status: doneRaces.length === 0 ? 'future' : doneRaces.length === st.races.length ? 'complete' : 'live',
      points,
      leaders: doneRaces.length && top > 0 ? PARTICIPANTS.filter(p => points[p] === top) : [],
    }
  })

  const cum: Record<string, number> = Object.fromEntries(PARTICIPANTS.map(p => [p, 0]))
  const gapSeries = completed.map(r => {
    for (const p of PARTICIPANTS) cum[p] += scores.find(s => s.participant === p && s.race_number === r)?.points ?? 0
    const top = Math.max(...Object.values(cum))
    const row: Record<string, number> = { race: r }
    for (const p of PARTICIPANTS) row[p] = cum[p] - top
    return row
  })

  return { completed, nextRace, remaining, cars, raceTop, players, stages, gapSeries, usedBy }
}
