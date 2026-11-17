// scripts/build_data.js
//
// Builds the case-side data files the dashboard fetches, from the raw inputs in data/raw/.
// The case data is frozen (Ontario stopped publishing it after 2021-12-22), so run this
// only when a raw input or the cleaning logic changes, and commit the outputs.
//
//   public/data/cases.json            per-school daily case records, compact (see below)
//   public/data/school_profiles.json  demographics shown in map popups: { profileFields,
//                                     demographicFields, schools: { [school_number]: [...values] } }
//   public/data/summaries.json        province-wide daily summaries (both school years)
//
// school_closures.json is built separately by clean_school_closures.js, from the private
// spreadsheet, at deploy time.
//
// cases.json format (decoded by src/lib/data-loader.ts):
//   placeFields: ['school_number', 'school_name', ...]   field names for each place
//   countFields: ['confirmed_student_cases', ...]        field names for each row's counts
//   places: [[school_number, school_name, school_board, latitude, longitude, city, municipality], ...]
//   dates:  ['2020-09-10', ...]  sorted
//   rows:   [[date_index, place_index, ...counts], ...]  in source order
// A "place" is each distinct combination of the school fields as they appear in the source,
// so decoding gives back exactly the records the cleaning step produced.
//
// Usage: node scripts/build_data.js

const fs = require('fs');
const path = require('path');
const { parse } = require('csv-parse/sync');
const { cleanCases, RAW_DIR } = require('./lib/clean-cases');
const {
  PLACE_FIELDS,
  CASE_COUNT_FIELDS,
  PROFILE_FIELDS,
  DEMOGRAPHIC_FIELDS,
} = require('./lib/data-format');

const OUT_DIR = path.join(__dirname, '../public/data');

function encodeCases(records) {
  const placeIndex = new Map();
  const places = [];
  const dates = Array.from(new Set(records.map((r) => r.collected_date))).sort();
  const dateIndex = new Map(dates.map((d, i) => [d, i]));

  const rows = records.map((r) => {
    const place = PLACE_FIELDS.map((f) => r[f] ?? null);
    const key = JSON.stringify(place);
    if (!placeIndex.has(key)) {
      placeIndex.set(key, places.length);
      places.push(place);
    }
    return [dateIndex.get(r.collected_date), placeIndex.get(key), ...CASE_COUNT_FIELDS.map((f) => r[f] ?? null)];
  });

  return { placeFields: PLACE_FIELDS, countFields: CASE_COUNT_FIELDS, places, dates, rows };
}

function buildProfiles() {
  const lookup = JSON.parse(fs.readFileSync(path.join(RAW_DIR, 'school_demographics_lookup.json'), 'utf8'));
  const schools = {};
  for (const [number, s] of Object.entries(lookup)) {
    schools[number] = [
      ...PROFILE_FIELDS.map((f) => s[f] ?? null),
      ...DEMOGRAPHIC_FIELDS.map((f) => (s.demographics ? s.demographics[f] ?? null : null)),
    ];
  }
  return { profileFields: PROFILE_FIELDS, demographicFields: DEMOGRAPHIC_FIELDS, schools };
}

function buildSummaries() {
  const files = ['schoolcovidsummary.csv', 'schoolcovidsummary2021_2022.csv'];
  const rows = files.flatMap((file) =>
    parse(fs.readFileSync(path.join(RAW_DIR, file), 'utf8'), { columns: true, skip_empty_lines: true })
  );
  return rows
    .map((row) => {
      const out = {};
      for (const [key, value] of Object.entries(row)) {
        if (key.endsWith('_date')) out[key] = value;
        else out[key] = value === '' ? null : Number(value);
      }
      return out;
    })
    .sort((a, b) => a.collected_date.localeCompare(b.collected_date));
}

function write(name, data) {
  const file = path.join(OUT_DIR, name);
  fs.writeFileSync(file, JSON.stringify(data));
  console.log(`Wrote ${path.relative(process.cwd(), file)} (${(fs.statSync(file).size / 1024).toFixed(0)} KB)`);
}

async function main() {
  fs.mkdirSync(OUT_DIR, { recursive: true });
  const records = await cleanCases();
  const cases = encodeCases(records);
  console.log(`${records.length} case records, ${cases.places.length} places, ${cases.dates.length} dates`);
  write('cases.json', cases);
  write('school_profiles.json', buildProfiles());
  write('summaries.json', buildSummaries());
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
