import computeHaversine = require('haversine')
import { Calculator } from './'

export let haversine: Calculator = {
  csvHeader: 'haversine_distance',
  async distance([a, b]: [number, number], [c, d]: [number, number]) {
    return computeHaversine(
      { latitude: a, longitude: b },
      { latitude: c, longitude: d },
      { unit: 'mile' }
    )
  }
}
