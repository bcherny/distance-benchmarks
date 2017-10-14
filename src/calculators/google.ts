import request = require('request-promise')
import { metersToMiles } from '../util'
import { Calculator } from './'

const KEY = 'AIzaSyBrWxU1O3Lfdl9s42-Rd4bRri4kHwi8B90'

export let google: Calculator = {
  csvHeader: 'google_distance',
  async distance([a, b]: [number, number], [c, d]: [number, number]) {
    let res = await request({
      json: true,
      uri: `https://maps.googleapis.com/maps/api/directions/json?destination=${a},${b}&origin=${c},${d}&key=${KEY}`
    })
    return metersToMiles(res.routes[0].legs[0].distance.value)
  }
}
