export interface Calculator {
  csvHeader: string
  distance([a, b]: [number, number], [c, d]: [number, number]): Promise<number>
}
