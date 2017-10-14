import request = require('request-promise')
import { metersToMiles } from '../util'
import { Calculator } from './'

const KEY = 'pk.eyJ1IjoiYmNoZXJueSIsImEiOiJjaWd6cGdseWoweDNwd3ltMGhsenI1d2tvIn0.jzRreSEiv5JLGK2DcHyuug'

export let mapbox: Calculator = {
  csvHeader: 'mapbox_distance',
  async distance([a, b]: [number, number], [c, d]: [number, number]) {
    let res = await request({
      json: true,
      uri: `https://api.mapbox.com/directions/v5/mapbox/driving/${b},${a};${d},${c}?access_token=${KEY}`
    })
    return metersToMiles(res.routes[0].legs[0].distance)
  }
}
