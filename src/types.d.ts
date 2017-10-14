declare module 'csv' {
  import { Duplex } from 'stream'

  //////////// parse

  export interface ParseOptions {

  }

  export function parse<T>(
    data: string,
    options: ParseOptions,
    callback: (err: Error | null, output: T | null) => any
  ): Duplex

  export function parse<T>(
    data: string,
    callback: (err: Error | null, output: T | null) => any
  ): Duplex

  //////////// stringify

  export interface StringifyOptions {

  }
  export function stringify(
    data: any,
    options: StringifyOptions,
    callback: (err: Error | null, output: string | null) => any
  ): Duplex
  export function stringify(
    data: any,
    callback: (err: null, output: string) => any
  ): Duplex
}
