// scripts/check_regression.js
//
// Regression detection script: compares key metrics from the current
// combined dataset against a saved baseline (public/data/.baseline.json).
//
// Run with: node scripts/check_regression.js
// Generate/update baseline: node scripts/update_baseline.js
//
// Exits 0 on PASS, 1 on FAIL (so it can gate CI/builds).

const fs = require('fs');
const path = require('path');

const DATA_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');
const BASELINE_PATH = path.join(__dirname, '../public/data/.baseline.json');

// Maximum allowed percentage regression for each metric before failing
const THRESHOLDS = {
  totalRecords:      0.001, // 0.1% — records should never shrink
  uniqueSchools:     0.01,  // 1%   — allow minor school_number variance
  uniqueBoards:      0.05,  // 5%   — board canonicalisation may vary
  uniqueMunicipalities: 0.05,
  totalCases:        0.001, // 0.1% — case totals must be stable
  recordsWithLatLng: 0.01,  // 1%   — geocoding coverage
  recordsWithMunicipality: 0.05, // 5% — municipality enrichment
};

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

function pctChange(current, baseline) {
  if (baseline === 0) return current === 0 ? 0 : Infinity;
  return (baseline - current) / baseline; // positive = regression (shrinkage)
}

function main() {
  // Load baseline
  if (!fs.existsSync(BASELINE_PATH)) {
    console.error(`FATAL: Baseline file not found at ${BASELINE_PATH}`);
    console.error('Run: node scripts/update_baseline.js  to create it.');
    process.exit(1);
  }

  let baseline;
  try {
    baseline = JSON.parse(fs.readFileSync(BASELINE_PATH, 'utf8'));
  } catch (err) {
    console.error(`FATAL: Could not read baseline: ${err.message}`);
    process.exit(1);
  }

  // Load current data
  let data;
  try {
    data = JSON.parse(fs.readFileSync(DATA_PATH, 'utf8'));
  } catch (err) {
    console.error(`FATAL: Could not read combined data: ${err.message}`);
    process.exit(1);
  }

  const current = computeMetrics(data);

  console.log('='.repeat(70));
  console.log('COVID-19 Ontario School Data — Regression Check');
  console.log('='.repeat(70));
  console.log(`Baseline recorded: ${baseline.recordedAt || 'unknown'}`);
  console.log();

  const failures = [];

  for (const [metric, threshold] of Object.entries(THRESHOLDS)) {
    const base = baseline.metrics[metric];
    const curr = current[metric];
    if (base == null) {
      console.log(`  ${metric}: baseline missing — skipping`);
      continue;
    }
    const regression = pctChange(curr, base);
    const pctStr = (regression * 100).toFixed(2) + '%';
    const maxStr = (threshold * 100).toFixed(1) + '%';

    if (regression > threshold) {
      const msg = `${metric}: ${curr.toLocaleString()} vs baseline ${base.toLocaleString()} (regression ${pctStr}, max ${maxStr})`;
      failures.push(msg);
      console.log(`  FAIL  ${metric}: ${curr.toLocaleString()} (baseline ${base.toLocaleString()}, regression ${pctStr})`);
    } else if (regression > 0) {
      console.log(`  WARN  ${metric}: ${curr.toLocaleString()} (baseline ${base.toLocaleString()}, regression ${pctStr} — within ${maxStr} threshold)`);
    } else {
      const improvedStr = regression < 0 ? ` (+${((-regression) * 100).toFixed(2)}%)` : '';
      console.log(`  PASS  ${metric}: ${curr.toLocaleString()}${improvedStr}`);
    }
  }

  console.log();
  console.log('='.repeat(70));
  if (failures.length === 0) {
    console.log('OVERALL: PASS — No regressions detected.');
    process.exit(0);
  } else {
    console.log(`OVERALL: FAIL — ${failures.length} regression(s) detected:`);
    failures.forEach((f, i) => console.log(`  ${i + 1}. ${f}`));
    console.log();
    console.log('If this regression is intentional, update the baseline:');
    console.log('  node scripts/update_baseline.js');
    process.exit(1);
  }
}

main();
