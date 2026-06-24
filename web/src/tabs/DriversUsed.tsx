import { useState, useMemo } from 'react'
import { PARTICIPANTS, COLORS, carBadgeUrl, MFR_LOGOS, type Driver, type Score, type Schedule, type picksToLong } from '../data'

interface Props {
  drivers: Driver[]; picksLong: ReturnType<typeof picksToLong>;
  scores: Score[]; schedule: Schedule[];
}

export default function DriversUsed({ drivers, picksLong, scores, schedule }: Props) {
  const [selected, setSelected] = useState('Mike')

  const usedCars = useMemo(() => {
    return picksLong.filter(p => p.participant === selected).map(p => p.car_number)
  }, [picksLong, selected])

  const usedData = useMemo(() => {
    return picksLong
      .filter(p => p.participant === selected)
      .map(p => {
        const sc = scores.find(s => s.participant === selected && s.race_number === p.race_number)
        const track = schedule.find(s => s.race_num === p.race_number)?.track_short || `R${p.race_number}`
        const d = drivers.find(d => d.car_number === p.car_number)
        return { ...p, points: sc?.points, track, driver: d }
      })
      .sort((a, b) => a.race_number - b.race_number)
  }, [picksLong, scores, schedule, drivers, selected])

  const available = useMemo(() => {
    return drivers.filter(d => !usedCars.includes(d.car_number)).sort((a, b) => a.car_number - b.car_number)
  }, [drivers, usedCars])

  return (
    <>
      <div className="participant-buttons">
        {PARTICIPANTS.map(p => (
          <button key={p} className="participant-btn"
            style={{ background: COLORS[p], opacity: selected === p ? 1 : 0.5 }}
            onClick={() => setSelected(p)}>
            {p}
          </button>
        ))}
      </div>
      <div className="two-col">
        <div className="card">
          <div className="card-header">{selected} — Drivers Used</div>
          <div className="card-body">
            <div className="driver-grid">
              {usedData.map(u => (
                <div key={u.race_number} className="driver-card used">
                  <div className="driver-card-img" style={{
                    backgroundImage: u.driver?.headshot_url ? `url(${u.driver.headshot_url})` : undefined,
                    background: u.driver?.headshot_url ? undefined : '#1a1a2e',
                  }}>
                    <div className="driver-card-number">
                      <img src={carBadgeUrl(u.car_number)} alt={`#${u.car_number}`}
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    {u.driver && (
                      <div className="driver-card-mfr">
                        <img src={MFR_LOGOS[u.driver.manufacturer] || ''} alt=""
                          onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                      </div>
                    )}
                    <div className="driver-card-overlay">
                      <div className="driver-card-name">{u.driver?.driver || `#${u.car_number}`}</div>
                      <div className="driver-card-team">{u.driver?.team}</div>
                    </div>
                  </div>
                  <div className="driver-card-info">
                    R{u.race_number} {u.track}{u.points != null ? ` | ${u.points} pts` : ''}
                  </div>
                </div>
              ))}
              {usedData.length === 0 && <p style={{ color: '#888' }}>No picks yet</p>}
            </div>
          </div>
        </div>
        <div className="card">
          <div className="card-header">{selected} — Still Available</div>
          <div className="card-body">
            <div className="driver-grid">
              {available.map(d => (
                <div key={d.car_number} className="driver-card">
                  <div className="driver-card-img" style={{
                    backgroundImage: d.headshot_url ? `url(${d.headshot_url})` : undefined,
                    background: d.headshot_url ? undefined : '#1a1a2e',
                  }}>
                    <div className="driver-card-number">
                      <img src={carBadgeUrl(d.car_number)} alt={`#${d.car_number}`}
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    <div className="driver-card-mfr">
                      <img src={MFR_LOGOS[d.manufacturer] || ''} alt=""
                        onError={e => { (e.target as HTMLImageElement).style.display = 'none' }} />
                    </div>
                    <div className="driver-card-overlay">
                      <div className="driver-card-name">{d.driver}</div>
                      <div className="driver-card-team">{d.team}</div>
                    </div>
                  </div>
                </div>
              ))}
              {available.length === 0 && <p style={{ color: '#888' }}>All drivers used!</p>}
            </div>
          </div>
        </div>
      </div>
    </>
  )
}
