import type { VercelRequest, VercelResponse } from '@vercel/node'

const REPO = 'barter7/nascar-36for36'
const FILE_PATH = 'data/picks.csv'
const BRANCH = 'main'

export default async function handler(req: VercelRequest, res: VercelResponse) {
  res.setHeader('Access-Control-Allow-Origin', '*')
  res.setHeader('Access-Control-Allow-Methods', 'POST, OPTIONS')
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type')

  if (req.method === 'OPTIONS') return res.status(204).end()
  if (req.method !== 'POST') return res.status(405).json({ error: 'POST only' })

  const token = process.env.GITHUB_TOKEN
  if (!token) return res.status(500).json({ error: 'GITHUB_TOKEN not set' })

  try {
    const { participant, race, car_number } = req.body
    if (!participant || !race || !car_number) {
      return res.status(400).json({ error: 'Missing participant, race, or car_number' })
    }

    const fileRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}?ref=${BRANCH}`, {
      headers: { Authorization: `Bearer ${token}`, Accept: 'application/vnd.github.v3+json' },
    })
    if (!fileRes.ok) return res.status(500).json({ error: 'Failed to read picks file' })

    const fileData = await fileRes.json()
    const content = Buffer.from(fileData.content, 'base64').toString('utf-8')
    const lines = content.split('\n').filter((l: string) => l.trim())
    const raceIdx = race

    const updatedLines = lines.map((line: string, i: number) => {
      if (i === 0) return line
      const cols = line.split(',')
      if (cols[0] === participant) {
        while (cols.length < 37) cols.push('')
        cols[raceIdx] = String(car_number)
        return cols.join(',')
      }
      return line
    })

    const newContent = updatedLines.join('\n') + '\n'
    const encoded = Buffer.from(newContent).toString('base64')

    const updateRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}`, {
      method: 'PUT',
      headers: {
        Authorization: `Bearer ${token}`,
        Accept: 'application/vnd.github.v3+json',
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        message: `Pick: ${participant} race ${race} = car #${car_number}`,
        content: encoded,
        sha: fileData.sha,
        branch: BRANCH,
      }),
    })

    if (!updateRes.ok) {
      const err = await updateRes.text()
      return res.status(500).json({ error: `GitHub update failed: ${err}` })
    }

    return res.status(200).json({ ok: true, participant, race, car_number })
  } catch (e: any) {
    return res.status(500).json({ error: e.message })
  }
}
