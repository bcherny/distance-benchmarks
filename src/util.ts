import { parse, stringify } from 'csv'

export function readStdIn() {
  process.stdin.setEncoding('utf8')

  return new Promise<string>((resolve, reject) => {

    let data = ''

    process.stdin.on('readable', () => {
      let chunk = process.stdin.read()
      if (chunk !== null) {
        data += chunk
      }
    })

    process.stdin.on('end', () => resolve(data))
    process.stdin.on('error', () => reject(data))
  })
}

export function parseCSV<T>(csv: string) {
  return new Promise<T>((resolve, reject) =>
    parse(csv, { auto_parse: true, from: 2 }, (err: Error | null, res: T) =>
      err ? reject(err) : resolve(res)
    )
  )
}

export function generateCSV<T>(csv: T[], columnNames: string[]) {
  return new Promise<string>((resolve, reject) => {
    stringify(
      csv,
      {
        columns: columnNames,
        header: true
      },
      (err, data) => {
        if (!err && data) {
          resolve(data)
        } else {
          reject(data)
        }
      }
    )
  })
}

export function metersToMiles(n: number) {
  return n * 0.000621371
}

export function spread2<T, U>(f: (a: T, b: T) => U) {
  return ([a, b]: [T, T]) => f(a, b)
}
