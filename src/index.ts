import { zip } from 'lodash'
import { mapf } from 'mapf'
import { google } from './calculators/google'
import { haversine } from './calculators/haversine'
import { mapbox } from './calculators/mapbox'
import { generateCSV, parseCSV, readStdIn, spread2 } from './util'

const CALCULATORS = [
  haversine,
  google,
  mapbox
]

async function main() {

  // read CSV input
  let stdin = await readStdIn()
  let csv = await parseCSV<[number, number][]>(stdin)
  let pairs = zip(csv, csv.slice().reverse()).slice(0, 5)

  // compute distances
  let results = await Promise.all(
    CALCULATORS.map(_ => mapf(pairs, spread2(_.distance)))
  )

  // generate CSV output
  let output = await generateCSV(
    zip(...results).map((_, n) =>
      [pairs[n][0][0], pairs[n][0][1], pairs[n][1][0], pairs[n][1][1], ..._]
    ),
    ['from_lat', 'from_lng', 'to_lat', 'to_lng', ...CALCULATORS.map(_ => _.csvHeader)]
  )

  process.stdout.write(output)

}

main()
