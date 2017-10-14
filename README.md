# distance-benchmarks

> Compares distances between two points using several methods

## Methods

- Haversine distance
- Google directions distance
- Mapbox directions distance

## Usage

Install NodeJS and [Yarn](https://yarnpkg.com/en/), then:

```sh
git clone git@github.com:bcherny/distance-benchmarks.git
cd distance-benchmarks
yarn
cat input.csv | ./node_modules/.bin/ts-node src/index.ts > output.csv
```

## Test dataset

500 rental units in San Francisco, from Realtor's websites.

## Results

See [analysis.md](analysis.md)
