// scripts/update_baseline.js
//
// Captures key metrics from the current combined dataset and writes them to
// public/data/.baseline.json.  Run this after intentionally changing the data
// pipeline so that future check_regression.js runs compare against the new
// expected values.
//
// Run with: node scripts/update_baseline.js

const fs = require('fs');
const path = require('path');

const DATA_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');
const BASELINE_PATH = path.join(__dirname, '../public/data/.baseline.json');

function computeMetrics(data) {
  const uniqueSchools = new Set(data.map(r => r.school_number).filter(Boolean)).size;
  const uniqueBoards = new Set(data.map(r => r.school_board).filter(Boolean)).size;
  const uniqueMunicipalities = new Set(data.map(r => r.municipality).filter(Boolean)).size;
  const totalCases = data.reduce((sum, r) => sum + (r.total_confirmed_cases || 0), 0);
  const recordsWithLatLng = data.filter(r => r.latitude != null && r.longitude != null).length;
  const recordsWithMunicipality = data.filter(r => r.municipality != null && r.municipality !== '').length;

  return {
    totalRecords: data.length,
    uniqueSchools,
    uniqueBoards,
    uniqueMunicipalities,
    totalCases,
    recordsWithLatLng,
    recordsWithMunicipality,
  };
}

function main() {
  let data;
  try {
    data = JSON.parse(fs.readFileSync(DATA_PATH, 'utf8'));
  } catch (err) {
    console.error(`FATAL: Could not read combined data: ${err.message}`);
    process.exit(1);
  }

  const metrics = computeMetrics(data);
  const baseline = {
    recordedAt: new Date().toISOString(),
    metrics,
  };

  fs.writeFileSync(BASELINE_PATH, JSON.stringify(baseline, null, 2) + '\n', 'utf8');

  console.log('Baseline updated:');
  console.log(`  File: ${BASELINE_PATH}`);
  console.log(`  Recorded at: ${baseline.recordedAt}`);
  console.log();
  for (const [key, val] of Object.entries(metrics)) {
    console.log(`  ${key}: ${val.toLocaleString()}`);
  }
}

main();
