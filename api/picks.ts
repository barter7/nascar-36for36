const REPO = 'barter7/nascar-36for36'
const FILE_PATH = 'data/picks.csv'
const BRANCH = 'main'
const PARTICIPANTS = ['Mike', 'Matt', 'Brian', 'Tom']
const GH_HEADERS = (token: string) => ({ Authorization: `Bearer ${token}`, Accept: 'application/vnd.github.v3+json' })

export default async function handler(req: any, res: any) {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type')

  if (req.method === 'OPTIONS') return res.status(204).end()

  const token = process.env.GH_TOKEN

  if (req.method === 'GET') {
    if (req.query && req.query.csv) {
      if (!token) return res.status(404).json({ error: 'GH_TOKEN not set' })
      const fileRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}?ref=${BRANCH}`, {
        headers: GH_HEADERS(token),
      })
      if (!fileRes.ok) return res.status(502).json({ error: 'Failed to read picks file' })
      const fileData = await fileRes.json()
      const content = Buffer.from(fileData.content, 'base64').toString('utf-8')
      res.setHeader('Content-Type', 'text/csv')
      res.setHeader('Cache-Control', 'no-store')
      return res.status(200).send(content)
    }
    return res.status(200).json({ status: 'picks API is running', hasToken: !!token })
  }

  if (req.method !== 'POST') return res.status(405).json({ error: 'POST only' })
  if (!token) return res.status(500).json({ error: 'GH_TOKEN not set' })

  const { participant, race, car_number } = req.body || {}
  const raceNum = Number(race)
  const car = Number(car_number) || 0
  if (!PARTICIPANTS.includes(participant) || !Number.isInteger(raceNum) || raceNum < 1 || raceNum > 36) {
    return res.status(400).json({ error: 'Invalid participant or race' })
  }
  if (!Number.isInteger(car) || car < 0 || car > 999) {
    return res.status(400).json({ error: 'Invalid car number' })
  }
  const newVal = car ? String(car) : ''

  try {
    // Two saves at once make the second PUT fail with a stale sha (409), so re-read and retry.
    for (let attempt = 0; attempt < 3; attempt++) {
      const fileRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}?ref=${BRANCH}`, {
        headers: GH_HEADERS(token),
      })
      if (!fileRes.ok) return res.status(500).json({ error: 'Failed to read picks file' })
      const fileData = await fileRes.json()
      const content = Buffer.from(fileData.content, 'base64').toString('utf-8')
      const lines = content.split('\n').filter((l: string) => l.trim())

      let changed = false
      const updatedLines = lines.map((line: string, i: number) => {
        if (i === 0) return line
        const cols = line.split(',')
        if (cols[0] !== participant) return line
        while (cols.length < 37) cols.push('')
        if ((cols[raceNum] || '') !== newVal) changed = true
        cols[raceNum] = newVal
        return cols.join(',')
      })

      if (!changed) return res.status(200).json({ ok: true, participant, race: raceNum, car_number: car || null, unchanged: true })

      const updateRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}`, {
        method: 'PUT',
        headers: { ...GH_HEADERS(token), 'Content-Type': 'application/json' },
        body: JSON.stringify({
          message: car ? `Pick: ${participant} race ${raceNum} = car #${car}` : `Remove pick: ${participant} race ${raceNum}`,
          content: Buffer.from(updatedLines.join('\n') + '\n').toString('base64'),
          sha: fileData.sha,
          branch: BRANCH,
        }),
      })
      if (updateRes.ok) return res.status(200).json({ ok: true, participant, race: raceNum, car_number: car || null })
      if (updateRes.status !== 409) {
        return res.status(500).json({ error: `GitHub update failed: ${await updateRes.text()}` })
      }
    }
    return res.status(409).json({ error: 'Someone else saved at the same time — please try again' })
  } catch (e: any) {
    return res.status(500).json({ error: e.message })
  }
}
