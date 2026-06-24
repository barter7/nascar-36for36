export const config = { runtime: 'edge' }

const REPO = 'barter7/nascar-36for36'
const FILE_PATH = 'data/picks.csv'
const BRANCH = 'main'

export default async function handler(req: Request) {
  if (req.method === 'OPTIONS') {
    return new Response(null, { status: 204, headers: corsHeaders() })
  }

  if (req.method !== 'POST') {
    return json({ error: 'POST only' }, 405)
  }

  const token = process.env.GITHUB_TOKEN
  if (!token) return json({ error: 'Server not configured' }, 500)

  try {
    const { participant, race, car_number } = await req.json()
    if (!participant || !race || !car_number) {
      return json({ error: 'Missing participant, race, or car_number' }, 400)
    }

    const fileRes = await fetch(`https://api.github.com/repos/${REPO}/contents/${FILE_PATH}?ref=${BRANCH}`, {
      headers: { Authorization: `Bearer ${token}`, Accept: 'application/vnd.github.v3+json' },
    })
    if (!fileRes.ok) return json({ error: 'Failed to read picks file' }, 500)

    const fileData = await fileRes.json()
    const content = atob(fileData.content.replace(/\n/g, ''))
    const lines = content.split('\n').filter(l => l.trim())
    const raceIdx = race

    const updatedLines = lines.map((line, i) => {
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
    const encoded = btoa(unescape(encodeURIComponent(newContent)))

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
      return json({ error: `GitHub update failed: ${err}` }, 500)
    }

    return json({ ok: true, participant, race, car_number })
  } catch (e: any) {
    return json({ error: e.message }, 500)
  }
}

function json(data: any, status = 200) {
  return new Response(JSON.stringify(data), {
    status,
    headers: { 'Content-Type': 'application/json', ...corsHeaders() },
  })
}

function corsHeaders() {
  return {
    'Access-Control-Allow-Origin': '*',
    'Access-Control-Allow-Methods': 'POST, OPTIONS',
    'Access-Control-Allow-Headers': 'Content-Type',
  }
}
