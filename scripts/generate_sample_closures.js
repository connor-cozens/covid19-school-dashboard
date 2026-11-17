// scripts/generate_sample_closures.js
// Generates tests/fixtures/school_closures.sample.json — a synthetic stand-in for
// public/data/school_closures.json, which is not checked in. CI copies the sample
// into public/data/ so the app can be built and browser-tested without the real data.
//
// School names, school numbers and closure dates are fictional. Board names, cities
// and coordinates are taken from the (already public) committed case data so the
// map filters behave realistically. Output is deterministic (seeded PRNG).
//
// Usage: node scripts/generate_sample_closures.js

const fs = require('fs');
const path = require('path');

const CASES_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');
const OUTPUT_PATH = path.join(__dirname, '../tests/fixtures/school_closures.sample.json');
const RECORD_COUNT = 60;

// Mulberry32 — small deterministic PRNG so the fixture is stable across runs
function prng(seed) {
  return () => {
    seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function addDays(isoDate, days) {
  const d = new Date(`${isoDate}T00:00:00Z`);
  d.setUTCDate(d.getUTCDate() + days);
  return d.toISOString().slice(0, 10);
}

function main() {
  const rand = prng(20201116);
  const pick = (arr) => arr[Math.floor(rand() * arr.length)];

  // One location per board/city pair from the public case data
  const cases = JSON.parse(fs.readFileSync(CASES_PATH, 'utf8'));
  const locations = new Map();
  for (const c of cases) {
    if (!c.school_board || !c.city || c.latitude == null) continue;
    const key = `${c.school_board}|${c.city}`;
    if (!locations.has(key)) locations.set(key, { board: c.school_board, city: c.city, lat: c.latitude, lng: c.longitude });
  }
  const pool = [...locations.values()].sort((a, b) => `${a.board}${a.city}`.localeCompare(`${b.board}${b.city}`));

  const reasons = ['Closed Per PHU Direction', 'Closed by School Board', 'Joint Decision', 'PHU Directed', ''];
  const closureWindows = [['2020-09-16', '2020-12-18'], ['2021-01-04', '2021-04-16'], ['2021-09-13', '2022-05-16']];

  const records = [];
  for (let i = 0; i < RECORD_COUNT; i++) {
    const loc = pick(pool);
    const [from, to] = pick(closureWindows);
    const span = (new Date(to) - new Date(from)) / 86400000;
    const closed = addDays(from, Math.floor(rand() * span));
    const record = {
      board_number: `B9${String(1000 + (i % 25)).padStart(4, '0')}`,
      board_name: loc.board,
      school_number: 990000 + i,
      school_name: `Sample ${['Public', 'Catholic', 'Secondary', 'Elementary'][i % 4]} School ${i + 1}`,
      date_of_closure: closed,
      date_of_reopening: addDays(closed, 7 + Math.floor(rand() * 14)),
      reason_for_closure: pick(reasons),
    };
    // Mirror the real data's edge cases: ~1 in 8 schools have no demographics/location
    if (i % 8 !== 7) {
      Object.assign(record, {
        latitude: +(loc.lat + (rand() - 0.5) * 0.02).toFixed(5),
        longitude: +(loc.lng + (rand() - 0.5) * 0.02).toFixed(5),
        city: loc.city,
        has_demographics: true,
      });
    }
    // ...and the occasional auto-corrected reopening date
    if (i % 20 === 5) {
      record.original_reopening_date = addDays(closed, -3);
      record.reopening_date_corrected = true;
    }
    records.push(record);
  }

  // Guarantee a closure active on the date the browser tests select
  Object.assign(records[0], { date_of_closure: '2020-11-10', date_of_reopening: '2020-11-24' });

  fs.mkdirSync(path.dirname(OUTPUT_PATH), { recursive: true });
  fs.writeFileSync(OUTPUT_PATH, JSON.stringify(records, null, 2) + '\n');
  console.log(`Wrote ${records.length} synthetic closures to ${OUTPUT_PATH}`);
}

main();
