# Data scripts

| Script | When it runs | What it does |
|---|---|---|
| `build_data.js` (`npm run build-data`) | By hand, only when a raw input or the cleaning logic changes | Cleans the raw case CSVs in `data/raw/` (`lib/clean-cases.js`) and writes `cases.json`, `school_profiles.json` and `summaries.json` to `public/data/`. Commit the outputs. |
| `clean_school_closures.js` | Every deploy, and locally when you have the spreadsheet | Reads `private-data/COVID School Closures_V2.xlsx` and writes `public/data/school_closures.json` (gitignored). Auto-corrections are logged to `private-data/closure_corrections_log.json`. |
| `check_export.js` | After every build (`postbuild`) | Fails the build if `out/` contains a spreadsheet, CSV, the corrections log, or env/key files. |
| `generate_sample_closures.js` | By hand, rarely | Writes the synthetic closures fixture used by CI and the browser tests. |

## Compact data format

`cases.json` stores each distinct school (name, board, coordinates, city, municipality) once
in `places`, each date once in `dates`, and one row per record:
`[date_index, place_index, student, staff, unspecified, total]`. Field names are stored in the
file (`placeFields`, `countFields`). `src/lib/data-loader.ts` turns it back into records.
Decoding gives back exactly the records the cleaning step produces.

`school_profiles.json` keeps only the fields the map popups show, as one array per school.

## Checks

`src/__tests__/data-files.test.ts` runs with `npm test` and checks the committed files:
record count, unique schools, boards and municipalities, the case total, valid dates, and that
every school has coordinates and a profile. If a change to the cleaning logic is meant to change
those numbers, update them in the test in the same PR.

## Raw inputs (`data/raw/`)

Not published with the site.

- `covid19_schools_active_with_demographics_19_20.csv`, `..._20_21.csv`: per-school case
  records joined with demographics, from the original R app's pipeline
- `school_demographics_lookup.json`: school information and demographics, keyed by school number
  (also used by `clean_school_closures.js` for closure locations)
- `schoolcovidsummary.csv`, `schoolcovidsummary2021_2022.csv`: Ontario's province-wide school
  case summaries
