// scripts/validate_school_closures.js
// Validates the accuracy of public/data/school_closures.json and provides a
// raw Excel date sample so dates can be manually verified against source records.
//
// Usage:
//   node scripts/validate_school_closures.js
//
// The script does NOT modify any files.

const fs = require('fs').promises;
const path = require('path');

// ------------------------------------------------------------------
// Excel serial-to-ISO conversion (mirrors the fixed version in
// clean_school_closures.js — kept here so the validator is self-contained
// and can be used to verify the formula independently).
// Verified: serial 44197 => 2021-01-01
// ------------------------------------------------------------------
function excelSerialToISO(serial) {
  if (!serial || isNaN(serial)) return '';
  const ms = (serial - 25569) * 86400 * 1000;
  return new Date(ms).toISOString().slice(0, 10);
}

// ------------------------------------------------------------------
// Section 1 — raw Excel date sample
// Reads up to 10 date cells from the source xlsx and prints both the
// raw value and the converted ISO string for manual verification.
// ------------------------------------------------------------------
async function printRawExcelDateSample() {
  let xlsx;
  try {
    xlsx = require('xlsx');
  } catch (e) {
    console.log('[SKIP] xlsx package not available — skipping raw Excel date sample.');
    return;
  }

  const xlsxPath = path.join(__dirname, '../public/data/COVID School Closures_V2.xlsx');
  try {
    await fs.access(xlsxPath);
  } catch {
    console.log('[SKIP] Excel source file not found — skipping raw Excel date sample.');
    return;
  }

  const workbook = xlsx.readFile(xlsxPath, { cellDates: false });

  console.log('=== Raw Excel Date Sample ===');
  console.log('(Verify these dates against the Ontario government source records.)');
  console.log('Formula used: (serial - 25569) * 86400 * 1000  =>  no +1 offset\n');

  // Known anchor for manual verification
  console.log('Known anchor: Excel serial 44197 => expected 2021-01-01');
  console.log(`  Computed:   ${excelSerialToISO(44197)}`);
  console.log('');

  // Sheet 1 — named columns
  const sheet1 = workbook.Sheets[workbook.SheetNames[0]];
  const records1 = xlsx.utils.sheet_to_json(sheet1, { defval: '' });
  console.log('Sheet 1 sample (up to 5 records):');
  records1.slice(0, 5).forEach((rec, i) => {
    const raw = rec['Date of Closure'];
    const iso = (raw instanceof Date)
      ? raw.toISOString().slice(0, 10)
      : excelSerialToISO(raw);
    console.log(`  [${i + 1}] School: "${rec['School Name']}"  raw="${raw}"  =>  converted="${iso}"`);
  });
  console.log('');

  // Sheet 2 — __EMPTY columns
  const sheet2 = workbook.Sheets[workbook.SheetNames[1]];
  const records2 = xlsx.utils.sheet_to_json(sheet2, { defval: '' });
  const validRecs2 = records2.filter(rec => {
    const sn = rec['__EMPTY_1'];
    return sn !== '' && !isNaN(Number(sn));
  });
  console.log('Sheet 2 sample (up to 3 records):');
  validRecs2.slice(0, 3).forEach((rec, i) => {
    const raw = rec['__EMPTY_3'];
    const iso = (raw instanceof Date)
      ? raw.toISOString().slice(0, 10)
      : excelSerialToISO(raw);
    console.log(`  [${i + 1}] School: "${rec['__EMPTY_2']}"  raw="${raw}"  =>  converted="${iso}"`);
  });
  console.log('');

  // Sheet 3 — __EMPTY columns
  const sheet3 = workbook.Sheets[workbook.SheetNames[2]];
  const records3 = xlsx.utils.sheet_to_json(sheet3, { defval: '' });
  const validRecs3 = records3.filter(rec => {
    const sn = rec['__EMPTY_1'];
    return sn !== '' && !isNaN(Number(sn));
  });
  console.log('Sheet 3 sample (up to 2 records):');
  validRecs3.slice(0, 2).forEach((rec, i) => {
    const raw = rec['__EMPTY_3'];
    const iso = (raw instanceof Date)
      ? raw.toISOString().slice(0, 10)
      : excelSerialToISO(raw);
    console.log(`  [${i + 1}] School: "${rec['__EMPTY_2']}"  raw="${raw}"  =>  converted="${iso}"`);
  });
  console.log('');
}

// ------------------------------------------------------------------
// Section 2 — validate school_closures.json
// ------------------------------------------------------------------
async function validateClosures() {
  const jsonPath = path.join(__dirname, '../public/data/school_closures.json');
  let data;
  try {
    data = JSON.parse(await fs.readFile(jsonPath, 'utf8'));
  } catch (e) {
    console.error(`ERROR: Could not read ${jsonPath}: ${e.message}`);
    process.exit(1);
  }

  const VALID_FROM = new Date('2020-09-01');
  const VALID_TO   = new Date('2022-07-01');
  const DEFAULT_WINDOW_DAYS = 14;

  let totalRecords = data.length;
  let pass = true;

  // Counters
  let invalidClosureDate = 0;
  let closureDateOutOfRange = 0;
  let invalidReopenDate = 0;
  let reopenBeforeClosure = [];
  let missingSchoolNumber = 0;
  let withLatLng = 0;
  let missingLatLng = 0;
  let default14DayApplied = 0;
  let default14DayViolations = [];

  // Date helper
  function parseDate(s) {
    if (!s) return null;
    const d = new Date(s);
    return isNaN(d.getTime()) ? null : d;
  }

  data.forEach((rec, idx) => {
    // Check school number
    if (!rec.school_number && rec.school_number !== 0) {
      missingSchoolNumber++;
    }

    // Check closure date
    const closure = parseDate(rec.date_of_closure);
    if (!closure) {
      invalidClosureDate++;
    } else if (closure < VALID_FROM || closure > VALID_TO) {
      closureDateOutOfRange++;
      console.warn(`  [WARN] Row ${idx}: closure date out of range: ${rec.date_of_closure} (school: ${rec.school_name})`);
    }

    // Check reopening date
    const reopen = parseDate(rec.date_of_reopening);
    if (!reopen) {
      invalidReopenDate++;
    } else {
      // Reopen must be after closure
      if (closure && reopen <= closure) {
        reopenBeforeClosure.push({ idx, school: rec.school_name, closure: rec.date_of_closure, reopen: rec.date_of_reopening });
      }

      // Check if default 14-day window was applied (reopen = closure + 14 days)
      if (closure) {
        const expected14 = new Date(closure);
        expected14.setDate(expected14.getDate() + DEFAULT_WINDOW_DAYS);
        const expected14Str = expected14.toISOString().slice(0, 10);
        if (rec.date_of_reopening === expected14Str) {
          default14DayApplied++;
        }
      }
    }

    // Check lat/lng
    const hasLat = typeof rec.latitude === 'number' && !isNaN(rec.latitude);
    const hasLng = typeof rec.longitude === 'number' && !isNaN(rec.longitude);
    if (hasLat && hasLng) {
      withLatLng++;
    } else {
      missingLatLng++;
    }
  });

  // ------------------------------------------------------------------
  // Print report
  // ------------------------------------------------------------------
  console.log('=== Closure JSON Validation Report ===\n');
  console.log(`Total records:                    ${totalRecords}`);
  console.log(`Records with lat/lng:             ${withLatLng}  (${((withLatLng / totalRecords) * 100).toFixed(1)}%)`);
  console.log(`Records missing lat/lng:          ${missingLatLng}  (${((missingLatLng / totalRecords) * 100).toFixed(1)}%)`);
  console.log(`Records missing school_number:    ${missingSchoolNumber}`);
  console.log(`Records with invalid closure date:  ${invalidClosureDate}`);
  console.log(`Closure dates out of range:         ${closureDateOutOfRange}  (expected 2020-09-01 to 2022-07-01)`);
  console.log(`Records with invalid reopen date:   ${invalidReopenDate}`);
  console.log(`Records with reopen <= closure:     ${reopenBeforeClosure.length}`);
  console.log(`Records using default +14d reopen:  ${default14DayApplied}`);
  console.log('');

  // Failures
  if (reopenBeforeClosure.length > 0) {
    pass = false;
    console.error('FAIL — reopening date on or before closure date:');
    reopenBeforeClosure.slice(0, 10).forEach(v => {
      console.error(`  Row ${v.idx}: ${v.school} | closure=${v.closure} reopen=${v.reopen}`);
    });
    console.log('');
  }

  if (invalidClosureDate > 0) {
    pass = false;
    console.error(`FAIL — ${invalidClosureDate} records have unparseable closure dates.`);
    console.log('');
  }

  if (invalidReopenDate > 0) {
    console.warn(`WARN — ${invalidReopenDate} records have unparseable or missing reopen dates.`);
    console.log('');
  }

  if (closureDateOutOfRange > 0) {
    pass = false;
    console.error(`FAIL — ${closureDateOutOfRange} closure dates fall outside the expected 2020-09-01..2022-07-01 window.`);
    console.log('');
  }

  if (missingSchoolNumber > 0) {
    // Informational only — the cleaning script keeps these for reference
    console.warn(`WARN — ${missingSchoolNumber} records have no school_number (kept as reference rows).`);
    console.log('');
  }

  // Summary
  if (pass) {
    console.log('RESULT: PASS — no critical errors found in school_closures.json.');
  } else {
    console.log('RESULT: FAIL — see errors above.');
    console.log('');
    console.log('NOTE: school_closures.json reflects the OLD (pre-fix) cleaning run.');
    console.log('Regenerate it by running: node scripts/clean_school_closures.js');
  }
}

// ------------------------------------------------------------------
// Section 3 — report auto-corrections from the audit log
// ------------------------------------------------------------------
async function reportCorrections() {
  const logPath = path.join(__dirname, '../public/data/closure_corrections_log.json');
  let corrections;
  try {
    corrections = JSON.parse(await fs.readFile(logPath, 'utf8'));
  } catch {
    console.log('[SKIP] closure_corrections_log.json not found — run clean_school_closures.js to generate it.\n');
    return;
  }

  console.log('=== Auto-Correction Audit Log ===\n');
  console.log(`Total auto-corrections applied: ${corrections.length}`);
  if (corrections.length > 0) {
    corrections.forEach((c, i) => {
      console.log(`  [${i + 1}] ${c.sheet} | ${c.school_name}`);
      console.log(`       closure=${c.closure_date}  original_reopen=${c.original_reopening_date}  corrected_reopen=${c.corrected_reopening_date}`);
      console.log(`       reason: ${c.reason}`);
    });
  }
  console.log('');
}

// ------------------------------------------------------------------
// Entry point
// ------------------------------------------------------------------
async function main() {
  await printRawExcelDateSample();
  await reportCorrections();
  await validateClosures();
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});
