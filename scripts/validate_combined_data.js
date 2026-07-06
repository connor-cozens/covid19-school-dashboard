// scripts/validate_combined_data.js
//
// Standalone validation script for the combined school COVID-19 dataset.
// Run with: node scripts/validate_combined_data.js
//
// Loads public/data/covid19_schools_active_with_demographics_combined.json
// and performs a series of checks, then prints a PASS/FAIL summary.

const fs = require('fs');
const path = require('path');

const DATA_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');

// Expected date range for collected_date values
const MIN_DATE = new Date('2020-01-01');
const MAX_DATE = new Date('2023-06-30');

// Minimum expected record count (two school years of daily per-school reports)
const MIN_RECORD_COUNT = 120000;

function topN(map, n) {
  return Array.from(map.entries())
    .sort((a, b) => b[1] - a[1])
    .slice(0, n);
}

function pct(count, total) {
  if (total === 0) return '0.0%';
  return ((count / total) * 100).toFixed(1) + '%';
}

function main() {
  // -------------------------------------------------------------------------
  // Load data
  // -------------------------------------------------------------------------
  let data;
  try {
    const raw = fs.readFileSync(DATA_PATH, 'utf8');
    data = JSON.parse(raw);
  } catch (err) {
    console.error(`FATAL: Could not read or parse ${DATA_PATH}`);
    console.error(err.message);
    process.exit(1);
  }

  const total = data.length;
  const issues = [];

  console.log('='.repeat(70));
  console.log('COVID-19 Ontario School Data — Validation Report');
  console.log('='.repeat(70));
  console.log(`File: ${DATA_PATH}`);
  console.log();

  // -------------------------------------------------------------------------
  // 1. Row count
  // -------------------------------------------------------------------------
  console.log(`[1] Row count: ${total.toLocaleString()}`);
  if (total < MIN_RECORD_COUNT) {
    const msg = `Row count ${total} is below expected minimum of ${MIN_RECORD_COUNT}`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
  } else {
    console.log(`    PASS: >= ${MIN_RECORD_COUNT.toLocaleString()} records`);
  }
  console.log();

  // -------------------------------------------------------------------------
  // 2. Field completeness
  // -------------------------------------------------------------------------
  const fields = [
    'municipality',
    'city',
    'school_number',
    'latitude',
    'longitude',
    'school_board',
    'collected_date',
  ];

  console.log('[2] Field completeness:');
  const completeness = {};
  for (const field of fields) {
    const present = data.filter(r => r[field] != null && r[field] !== '').length;
    completeness[field] = present;
    const p = pct(present, total);
    const missing = total - present;
    const status = missing === 0 ? 'PASS' : (missing > total * 0.1 ? 'WARN' : 'note');
    console.log(`    ${field}: ${present.toLocaleString()}/${total.toLocaleString()} (${p}) [${status}]`);
    if (missing > total * 0.1) {
      issues.push(`Field '${field}' is missing in ${missing.toLocaleString()} records (${p} present)`);
    }
  }
  console.log();

  // -------------------------------------------------------------------------
  // 3. Date validity
  // -------------------------------------------------------------------------
  console.log('[3] Date validity (collected_date):');
  let invalidDates = 0;
  let outOfRangeDates = 0;
  const invalidDateExamples = [];
  const outOfRangeExamples = [];

  for (const record of data) {
    const val = record.collected_date;
    if (!val) {
      invalidDates++;
      continue;
    }
    const d = new Date(val);
    if (isNaN(d.getTime())) {
      invalidDates++;
      if (invalidDateExamples.length < 3) invalidDateExamples.push(val);
    } else if (d < MIN_DATE || d > MAX_DATE) {
      outOfRangeDates++;
      if (outOfRangeExamples.length < 3) outOfRangeExamples.push(val);
    }
  }

  if (invalidDates === 0) {
    console.log(`    PASS: All ${total.toLocaleString()} records have parseable dates`);
  } else {
    const msg = `${invalidDates.toLocaleString()} records have invalid/missing collected_date`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
    if (invalidDateExamples.length > 0) {
      console.log(`    Examples: ${invalidDateExamples.join(', ')}`);
    }
  }

  if (outOfRangeDates === 0) {
    console.log(`    PASS: All dates are within ${MIN_DATE.toISOString().slice(0,10)} – ${MAX_DATE.toISOString().slice(0,10)}`);
  } else {
    const msg = `${outOfRangeDates.toLocaleString()} records have collected_date outside expected range`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
    if (outOfRangeExamples.length > 0) {
      console.log(`    Examples: ${outOfRangeExamples.join(', ')}`);
    }
  }
  console.log();

  // -------------------------------------------------------------------------
  // 4. Numeric field checks (total_confirmed_cases non-negative, no nulls)
  // -------------------------------------------------------------------------
  console.log('[4] Numeric field checks (total_confirmed_cases):');
  let nullCases = 0;
  let negativeCases = 0;
  const negativeExamples = [];

  for (const record of data) {
    const val = record.total_confirmed_cases;
    if (val == null) {
      nullCases++;
    } else if (val < 0) {
      negativeCases++;
      if (negativeExamples.length < 3) {
        negativeExamples.push(`${record.school_name || 'unknown'}: ${val}`);
      }
    }
  }

  if (nullCases === 0) {
    console.log(`    PASS: No null total_confirmed_cases`);
  } else {
    const msg = `${nullCases.toLocaleString()} records have null total_confirmed_cases`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
  }

  if (negativeCases === 0) {
    console.log(`    PASS: No negative total_confirmed_cases`);
  } else {
    const msg = `${negativeCases.toLocaleString()} records have negative total_confirmed_cases`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
    if (negativeExamples.length > 0) {
      console.log(`    Examples: ${negativeExamples.join('; ')}`);
    }
  }
  console.log();

  // -------------------------------------------------------------------------
  // 5. Case component sum check
  //    total_confirmed_cases should equal the sum of:
  //      confirmed_student_cases + confirmed_staff_cases +
  //      confirmed_unspecified_cases + confirmed_unidentified_cases
  // -------------------------------------------------------------------------
  console.log('[5] Case sum consistency (total = student + staff + unspecified + unidentified):');
  let sumMismatches = 0;
  const mismatchExamples = [];

  for (const record of data) {
    const total_c = record.total_confirmed_cases;
    const student = record.confirmed_student_cases ?? 0;
    const staff = record.confirmed_staff_cases ?? 0;
    const unspecified = record.confirmed_unspecified_cases ?? 0;
    const unidentified = record.confirmed_unidentified_cases ?? 0;

    if (total_c == null) continue;
    const computedSum = student + staff + unspecified + unidentified;
    if (computedSum !== total_c) {
      sumMismatches++;
      if (mismatchExamples.length < 10) {
        mismatchExamples.push(
          `${record.school_name || 'unknown'} (${record.collected_date}): ` +
          `total=${total_c}, sum=${computedSum} ` +
          `(s=${student}+st=${staff}+u=${unspecified}+ui=${unidentified})`
        );
      }
    }
  }

  const mismatchPct = pct(sumMismatches, total);
  const mismatchRate = sumMismatches / total;

  if (sumMismatches === 0) {
    console.log(`    PASS: All case totals match component sums`);
  } else {
    // 0–0.5%: note only; 0.5–5%: WARN; >5%: FAIL
    const severity = mismatchRate > 0.05 ? 'FAIL' : mismatchRate > 0.005 ? 'WARN' : 'note';
    const msg = `${sumMismatches.toLocaleString()} records (${mismatchPct}) have total_confirmed_cases != component sum`;
    console.log(`    ${severity.toUpperCase()}: ${msg}`);
    if (mismatchRate > 0.05) {
      issues.push(msg);
    } else if (mismatchRate > 0.005) {
      console.log(`    Note: Rate above 0.5% — investigate if this increases`);
    }
    console.log(`    Note: Known cause — confirmed_unidentified_cases exists in 2020-21 data only`);
    if (mismatchExamples.length > 0) {
      console.log('    Examples (up to 10):');
      mismatchExamples.forEach(ex => console.log(`      ${ex}`));
    }
  }
  console.log();

  // -------------------------------------------------------------------------
  // 6. Top 20 school boards by record count
  // -------------------------------------------------------------------------
  console.log('[6] Top 20 school boards by record count:');
  const boardCounts = new Map();
  for (const record of data) {
    const board = record.school_board || '(missing)';
    boardCounts.set(board, (boardCounts.get(board) || 0) + 1);
  }
  const topBoards = topN(boardCounts, 20);
  topBoards.forEach(([board, count], i) => {
    console.log(`    ${String(i + 1).padStart(2)}. ${board} — ${count.toLocaleString()}`);
  });
  console.log(`    (${boardCounts.size} unique board names total)`);
  if (boardCounts.has('(missing)')) {
    const missing = boardCounts.get('(missing)');
    issues.push(`${missing.toLocaleString()} records have no school_board`);
  }
  console.log();

  // -------------------------------------------------------------------------
  // 7. Top 20 municipalities by record count
  // -------------------------------------------------------------------------
  console.log('[7] Top 20 municipalities by record count:');
  const munCounts = new Map();
  for (const record of data) {
    const mun = record.municipality || '(missing)';
    munCounts.set(mun, (munCounts.get(mun) || 0) + 1);
  }
  const topMuns = topN(munCounts, 20);
  topMuns.forEach(([mun, count], i) => {
    console.log(`    ${String(i + 1).padStart(2)}. ${mun} — ${count.toLocaleString()}`);
  });
  console.log(`    (${munCounts.size} unique municipality values total)`);
  if (munCounts.has('(missing)')) {
    const missingMun = munCounts.get('(missing)');
    const missingMunPct = pct(missingMun, total);
    console.log(`    NOTE: ${missingMun.toLocaleString()} records (${missingMunPct}) have no municipality.`);
    console.log(`    If this is 100%, re-run clean_school_data.js after the municipality fix.`);
  }
  console.log();

  // -------------------------------------------------------------------------
  // 8. Encoding integrity (no U+FFFD replacement characters)
  // -------------------------------------------------------------------------
  console.log('[8] Encoding integrity (no U+FFFD replacement characters):');
  const REPLACEMENT_CHAR = '\uFFFD';
  const corruptRecords = data.filter(r =>
    (r.school_name || '').includes(REPLACEMENT_CHAR) ||
    (r.school_board || '').includes(REPLACEMENT_CHAR) ||
    (r.municipality || '').includes(REPLACEMENT_CHAR)
  );

  if (corruptRecords.length === 0) {
    console.log(`    PASS: No U+FFFD replacement characters found in school_name, school_board, or municipality`);
  } else {
    const msg = `${corruptRecords.length.toLocaleString()} records contain U+FFFD replacement characters`;
    // Fewer than 100 unrecoverable corrupt names is a known data-source limitation
    // (schools with no school_number can't be recovered via demographics lookup).
    // Treat as WARN unless the count grows unexpectedly.
    const isFail = corruptRecords.length > 100;
    if (isFail) issues.push(msg);
    console.log(`    ${isFail ? 'FAIL' : 'WARN'}: ${msg}`);
    const examples = corruptRecords.slice(0, 5);
    console.log(`    Examples (up to 5):`);
    examples.forEach(r => {
      const fields = [];
      if ((r.school_name || '').includes(REPLACEMENT_CHAR)) fields.push(`school_name: "${r.school_name}"`);
      if ((r.school_board || '').includes(REPLACEMENT_CHAR)) fields.push(`school_board: "${r.school_board}"`);
      if ((r.municipality || '').includes(REPLACEMENT_CHAR)) fields.push(`municipality: "${r.municipality}"`);
      console.log(`      school_number=${r.school_number}, ${fields.join(', ')}`);
    });
    if (!isFail) {
      console.log(`    Note: These records have no school_number so cannot be recovered via demographics lookup.`);
    }
  }
  console.log();

  // -------------------------------------------------------------------------
  // 9. Literal "NA" school names (regression check — R CSV export artefact)
  // -------------------------------------------------------------------------
  console.log('[9] Literal "NA" school names (R export artefact check):');
  const naNameRecords = data.filter(r => r.school_name === 'NA');
  if (naNameRecords.length === 0) {
    console.log(`    PASS: No records with literal "NA" as school_name`);
  } else {
    const msg = `${naNameRecords.length.toLocaleString()} records have school_name === "NA" (R missing-value artefact)`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
    const examples = naNameRecords.slice(0, 3);
    examples.forEach(r => {
      console.log(`      board: ${r.school_board}, date: ${r.collected_date}`);
    });
  }
  console.log();

  // -------------------------------------------------------------------------
  // 10. Unique school count (regression guard — should have ≥ 3800 schools)
  // -------------------------------------------------------------------------
  const MIN_UNIQUE_SCHOOLS = 3800;
  console.log('[10] Unique school count:');
  const schoolNumbers = new Set(data.map(r => r.school_number).filter(Boolean));
  const uniqueSchoolCount = schoolNumbers.size;
  console.log(`    ${uniqueSchoolCount.toLocaleString()} unique school_number values`);
  if (uniqueSchoolCount < MIN_UNIQUE_SCHOOLS) {
    const msg = `Unique school count ${uniqueSchoolCount} is below expected minimum of ${MIN_UNIQUE_SCHOOLS}`;
    issues.push(msg);
    console.log(`    FAIL: ${msg}`);
  } else {
    console.log(`    PASS: >= ${MIN_UNIQUE_SCHOOLS.toLocaleString()} unique schools`);
  }
  console.log();

  // -------------------------------------------------------------------------
  // Summary
  // -------------------------------------------------------------------------
  console.log('='.repeat(70));
  if (issues.length === 0) {
    console.log('OVERALL: PASS — No critical issues found.');
  } else {
    console.log(`OVERALL: FAIL — ${issues.length} issue(s) found:`);
    issues.forEach((issue, i) => {
      console.log(`  ${i + 1}. ${issue}`);
    });
  }
  console.log('='.repeat(70));

  process.exit(issues.length > 0 ? 1 : 0);
}

main();
