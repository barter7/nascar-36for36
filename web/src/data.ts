import Papa from 'papaparse'

export interface Driver {
  car_number: number; driver: string; team: string; manufacturer: string;
  headshot_url: string; number_url: string;
}
export interface Result {
  race_number: number; car_number: number; driver: string;
  finish_pos: number; points: number;
}
export interface Schedule {
  race_num: number; date: string; track: string; track_short: string;
}
export interface Pick {
  participant: string; [key: string]: string | number | undefined;
}
export interface Score {
  participant: string; race_number: number; car_number: number;
  driver: string; finish_pos: number; points: number;
}

export const PARTICIPANTS = ['Mike', 'Matt', 'Brian', 'Tom'] as const
export type Participant = typeof PARTICIPANTS[number]

export const COLORS: Record<string, string> = {
  Mike: '#E41A1C', Matt: '#377EB8', Brian: '#4DAF4A', Tom: '#FF7F00',
}
export const INITIALS: Record<string, string> = {
  Mike: 'MT', Matt: 'MD', Brian: 'BM', Tom: 'TM',
}

export const MFR_LOGOS: Record<string, string> = {
  Toyota: 'https://www.nascar.com/wp-content/uploads/sites/7/2020/04/06/Toyota-180x180.png',
  Chevrolet: 'https://www.nascar.com/wp-content/uploads/sites/7/2025/03/04/Chevrolet_2025-330x140.png',
  Ford: 'https://www.nascar.com/wp-content/uploads/sites/7/2026/02/18/Ford-Racing-Logo-300-100.png',
}

export function carBadgeUrl(car: number) {
  return `https://cf.nascar.com/data/images/carbadges/1/${car}.png`
}

async function loadCsv<T>(path: string): Promise<T[]> {
  const res = await fetch(path)
  const text = await res.text()
  const parsed = Papa.parse<T>(text, { header: true, skipEmptyLines: true, dynamicTyping: true })
  return parsed.data
}

const GITHUB_RAW = 'https://raw.githubusercontent.com/barter7/nascar-36for36/main/data'

async function loadPicksLive(suffix: string): Promise<Pick[]> {
  try {
    const res = await fetch(`${GITHUB_RAW}/picks${suffix}.csv?t=${Date.now()}`)
    if (res.ok) {
      const text = await res.text()
      const parsed = Papa.parse<Pick>(text, { header: true, skipEmptyLines: true, dynamicTyping: true })
      return parsed.data
    }
  } catch {}
  return loadCsv<Pick>(`/data/picks${suffix}.csv`)
}

export async function loadData(year: number) {
  const suffix = year === 2025 ? '_2025' : ''
  const [drivers, results, schedule, picks] = await Promise.all([
    loadCsv<Driver>('/data/drivers.csv'),
    loadCsv<Result>(`/data/results${suffix}.csv`),
    year === 2025
      ? Promise.resolve(Array.from({ length: 36 }, (_, i) => ({
          race_num: i + 1, date: '', track: `Race ${i + 1}`, track_short: `R${i + 1}`,
        })))
      : loadCsv<Schedule>('/data/schedule.csv'),
    loadPicksLive(suffix),
  ])
  return { drivers, results, schedule, picks }
}

export function picksToLong(picks: Pick[]): { participant: string; race_number: number; car_number: number }[] {
  const out: { participant: string; race_number: number; car_number: number }[] = []
  for (const p of picks) {
    for (let r = 1; r <= 36; r++) {
      const val = p[`race_${r}`]
      if (val != null && val !== '') {
        out.push({ participant: p.participant, race_number: r, car_number: Number(val) })
      }
    }
  }
  return out
}

export function getLastPickedRace(picks: Pick[]): Record<string, number> {
  const result: Record<string, number> = {}
  for (const p of picks) {
    let last = 0
    for (let r = 36; r >= 1; r--) {
      const val = p[`race_${r}`]
      if (val != null && val !== '') { last = r; break }
    }
    result[p.participant] = last
  }
  return result
}

export function computeScores(picksLong: ReturnType<typeof picksToLong>, results: Result[]): Score[] {
  const rMap = new Map<string, Result>()
  for (const r of results) rMap.set(`${r.race_number}-${r.car_number}`, r)
  const scores: Score[] = []
  for (const p of picksLong) {
    const r = rMap.get(`${p.race_number}-${p.car_number}`)
    if (r) scores.push({ participant: p.participant, ...r })
  }
  return scores
}

export function computeCumulative(scores: Score[]) {
  const byParticipant: Record<string, { race_number: number; points: number; car_number: number }[]> = {}
  for (const s of scores) {
    if (!byParticipant[s.participant]) byParticipant[s.participant] = []
    byParticipant[s.participant].push({ race_number: s.race_number, points: s.points, car_number: s.car_number })
  }
  const result: { participant: string; race_number: number; cumulative: number; car_number: number }[] = []
  for (const [p, races] of Object.entries(byParticipant)) {
    races.sort((a, b) => a.race_number - b.race_number)
    let cum = 0
    for (const r of races) {
      cum += r.points
      result.push({ participant: p, race_number: r.race_number, cumulative: cum, car_number: r.car_number })
    }
  }
  return result
}

export const STAGES = [
  { name: 'Stage 1', races: [1,2,3,4,5,6] },
  { name: 'Stage 2', races: [7,8,9,10,11,12] },
  { name: 'Stage 3', races: [13,14,15,16,17,18] },
  { name: 'Stage 4', races: [19,20,21,22,23,24] },
  { name: 'Stage 5', races: [25,26,27,28,29,30] },
  { name: 'Stage 6', races: [31,32,33,34,35,36] },
]
