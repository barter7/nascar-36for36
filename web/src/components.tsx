import { useState } from 'react'
import { COLORS, carBadgeUrl, type Driver } from './data'

export function CarBadge({ car, height = 24 }: { car: number; height?: number }) {
  const [failed, setFailed] = useState(false)
  if (failed) return <span className="car-num" style={{ fontSize: height * 0.6 }}>#{car}</span>
  return <img className="car-badge" src={carBadgeUrl(car)} alt={`#${car}`} style={{ height }} onError={() => setFailed(true)} />
}

export function Avatar({ driver, car, size = 36 }: { driver?: Driver; car: number; size?: number }) {
  const [failed, setFailed] = useState(false)
  return (
    <span className="avatar" style={{ width: size, height: size }}>
      {driver?.headshot_url && !failed
        ? <img src={driver.headshot_url} alt="" onError={() => setFailed(true)} />
        : <span className="avatar-fallback" style={{ fontSize: size * 0.34 }}>{car}</span>}
    </span>
  )
}

export function PlayerName({ name }: { name: string }) {
  return <span className="player-name" style={{ color: COLORS[name] }}>{name}</span>
}

export function signed(n: number, digits = 0) {
  const v = n.toFixed(digits)
  return n > 0 ? `+${v}` : v
}

export function valueClass(n: number | null | undefined) {
  if (n == null || Math.abs(n) < 0.5) return 'muted'
  return n > 0 ? 'pos' : 'neg'
}
