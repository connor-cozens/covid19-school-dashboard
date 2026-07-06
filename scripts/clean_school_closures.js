// scripts/clean_school_closures.js
const fs = require('fs').promises;
const path = require('path');
const xlsx = require('xlsx');

// Mirrors the R app's coalesce logic (shiny-app/data_downloader.R):
//   coalesce(`Date of Reopening`, `Date of Closure` + 14)
// Some COVID closures lasted less or more than this; we use it only when the
// source Excel cell is blank or contains an invalid/earlier-than-closure date.
const DEFAULT_CLOSURE_DURATION_DAYS = 14;

// Utility to convert Excel serial date (or Date object or string) to ISO string.
// Excel serial 25569 = 1970-01-01 (Unix epoch). The formula (serial - 25569) * 86400 * 1000
// gives the correct Unix timestamp in ms for dates in the 2020-2022 range WITHOUT any
// additional offset. The old +1 adjustment was incorrect — Excel's leap-year bug only
// affects dates on or before 1900-02-28, which is irrelevant for our data.
// Verified: Excel serial 44197 => (44197 - 25569) * 86400 * 1000 = 2021-01-01 ✓
function excelDateToISO(val) {
  if (!val) return '';
  // If xlsx library already parsed it as a Date object
  if (val instanceof Date) return val.toISOString().slice(0, 10);
  // If a numeric Excel serial
  if (typeof val === 'number') {
    if (isNaN(val)) return '';
    const ms = (val - 25569) * 86400 * 1000;
    return new Date(ms).toISOString().slice(0, 10);
  }
  // If a string, attempt direct parse
  const d = new Date(val);
  return isNaN(d.getTime()) ? '' : d.toISOString().slice(0, 10);
}

async function main() {
  const inputXlsx = path.join(__dirname, '../public/data/COVID School Closures_V2.xlsx');
  const outputJson = path.join(__dirname, '../public/data/school_closures.json');
  const correctionsLog = path.join(__dirname, '../public/data/closure_corrections_log.json');
  const demographicsJson = path.join(__dirname, '../public/data/school_demographics_lookup.json');

  const workbook = xlsx.readFile(inputXlsx);
  const demographicsLookup = JSON.parse(await fs.readFile(demographicsJson, 'utf8'));

  console.log(`Loaded demographics lookup for ${Object.keys(demographicsLookup).length} unique schools`);

  let allClosures = [];
  // Audit trail: every auto-correction is recorded here and written to closure_corrections_log.json
  const corrections = [];

  /** Apply default +14d reopening and record any auto-correction.
   * @param {string} schoolName - for the corrections log
   * @param {string} closureDate - ISO date
   * @param {string} rawReopenDate - ISO date from source (may be '' or invalid)
   * @param {string} sheet - sheet label for the log
   * @returns {{ reopenDate: string, corrected: boolean, originalReopenDate: string|null }}
   */
  function resolveReopenDate(schoolName, closureDate, rawReopenDate, sheet) {
    if (!closureDate) return { reopenDate: '', corrected: false, originalReopenDate: null };

    const addDefault = () => {
      const d = new Date(closureDate);
      d.setDate(d.getDate() + DEFAULT_CLOSURE_DURATION_DAYS);
      return d.toISOString().slice(0, 10);
    };

    // Missing reopen date — apply default silently (not a correction, just a fill)
    if (!rawReopenDate) {
      return { reopenDate: addDefault(), corrected: false, originalReopenDate: null };
    }

    // Reopen on or before closure — source data error (year typo or invalid entry)
    if (rawReopenDate <= closureDate) {
      const corrected = addDefault();
      const reason = rawReopenDate < closureDate
        ? `Reopening date (${rawReopenDate}) precedes closure date (${closureDate}) — likely year typo in source Excel`
        : `Reopening date equals closure date (${closureDate}) — defaulted to +${DEFAULT_CLOSURE_DURATION_DAYS} days`;
      console.warn(`  [CORRECTION] ${sheet} | ${schoolName}: reopen ${rawReopenDate} -> ${corrected} (${reason})`);
      corrections.push({
        sheet,
        school_name: schoolName,
        closure_date: closureDate,
        original_reopening_date: rawReopenDate,
        corrected_reopening_date: corrected,
        reason,
      });
      return { reopenDate: corrected, corrected: true, originalReopenDate: rawReopenDate };
    }

    return { reopenDate: rawReopenDate, corrected: false, originalReopenDate: null };
  }

  // --- Sheet 1: Sept - Apr 20_21 ---
  const sheet1 = workbook.Sheets[workbook.SheetNames[0]];
  const records1 = xlsx.utils.sheet_to_json(sheet1, { defval: '' });
  console.log(`Processing sheet 1: ${records1.length} records`);

  records1.forEach((rec) => {
    const closureDate = excelDateToISO(rec['Date of Closure']);
    const rawReopenDate = excelDateToISO(rec['Date of Reopening']);
    const { reopenDate, corrected, originalReopenDate } = resolveReopenDate(
      rec['School Name'], closureDate, rawReopenDate, 'Sheet1'
    );
    const entry = {
      board_number: rec['Board Number'],
      board_name: rec['Board Name'],
      school_number: rec['School Number'],
      school_name: rec['School Name'],
      date_of_closure: closureDate,
      date_of_reopening: reopenDate,
      reason_for_closure: rec['Reason for Closure'] || '',
    };
    if (corrected) {
      entry.reopening_date_corrected = true;
      entry.original_reopening_date = originalReopenDate;
    }
    allClosures.push(entry);
  });

  // --- Sheet 2: Sept - Dec 21 ---
  const sheet2 = workbook.Sheets[workbook.SheetNames[1]];
  const records2 = xlsx.utils.sheet_to_json(sheet2, { defval: '' });
  console.log(`Processing sheet 2: ${records2.length} records`);

  // Filter out header/title rows: a valid data row must have a numeric school number in __EMPTY_1.
  // Using .slice(1) was wrong — it skipped the first actual data record. Instead we filter
  // by checking that __EMPTY_1 is present and parses as a number.
  records2.filter(rec => {
    const sn = rec['__EMPTY_1'];
    return sn !== '' && !isNaN(Number(sn));
  }).forEach((rec) => {
    const closureDate = excelDateToISO(rec['__EMPTY_3']);
    const rawReopenDate = excelDateToISO(rec['__EMPTY_4']);
    const { reopenDate, corrected, originalReopenDate } = resolveReopenDate(
      rec['__EMPTY_2'], closureDate, rawReopenDate, 'Sheet2'
    );
    const entry = {
      board_number: rec['COVID School Closures - September 2021 through December 2021'],
      board_name: rec['__EMPTY'],
      school_number: rec['__EMPTY_1'],
      school_name: rec['__EMPTY_2'],
      date_of_closure: closureDate,
      date_of_reopening: reopenDate,
      reason_for_closure: rec['__EMPTY_5'] || '',
    };
    if (corrected) {
      entry.reopening_date_corrected = true;
      entry.original_reopening_date = originalReopenDate;
    }
    allClosures.push(entry);
  });

  // --- Sheet 3: Jan - May 22 ---
  // Sheet 3 has no reopening date column at all — the default +14d fill is applied to every
  // record and is intentional (not an auto-correction), so no correction flag is set.
  const sheet3 = workbook.Sheets[workbook.SheetNames[2]];
  const records3 = xlsx.utils.sheet_to_json(sheet3, { defval: '' });
  console.log(`Processing sheet 3: ${records3.length} records`);

  // Same fix as Sheet 2: filter by numeric school number rather than .slice(1),
  // which would skip the first valid data record.
  records3.filter(rec => {
    const sn = rec['__EMPTY_1'];
    return sn !== '' && !isNaN(Number(sn));
  }).forEach((rec) => {
    const closureDate = excelDateToISO(rec['__EMPTY_3']);
    let reopenDate = '';
    if (closureDate) {
      const d = new Date(closureDate);
      d.setDate(d.getDate() + DEFAULT_CLOSURE_DURATION_DAYS);
      reopenDate = d.toISOString().slice(0, 10);
    }
    allClosures.push({
      board_number: rec['COVID School Closures - January 2022 through June 2022'],
      board_name: rec['__EMPTY'],
      school_number: rec['__EMPTY_1'],
      school_name: rec['__EMPTY_2'],
      date_of_closure: closureDate,
      date_of_reopening: reopenDate,
      reason_for_closure: '',
    });
  });

  console.log(`Total closures processed: ${allClosures.length}`);

  // Filter out records with missing school numbers
  const validClosures = allClosures.filter(rec => rec.school_number && rec.school_number !== '');
  const invalidClosures = allClosures.filter(rec => !rec.school_number || rec.school_number === '');
  
  console.log(`\nData Quality:`);
  console.log(`- Valid closures (with school numbers): ${validClosures.length}`);
  console.log(`- Invalid closures (missing school numbers): ${invalidClosures.length}`);

  // --- Enrich with lat/lng and demographics using school number matching ---
  let matchedCount = 0;
  let unmatchedCount = 0;
  
  const enrichedClosures = validClosures.map((rec) => {
    const schoolNumber = rec.school_number;
    const demographics = demographicsLookup[schoolNumber];
    
    if (demographics) {
      matchedCount++;
      return {
        ...rec,
        // Only include essential location data for map display
        latitude: demographics.latitude,
        longitude: demographics.longitude,
        city: demographics.city,
        // Reference to demographics lookup
        has_demographics: true,
      };
    } else {
      unmatchedCount++;
      return rec;
    }
  });

  // Only output valid closures (those with school numbers). The "invalid" rows are
  // completely blank Excel rows with no school, board, or date — there is no useful
  // information to preserve. Logging them is sufficient.
  allClosures = enrichedClosures;

  console.log(`\nMatching Results:`);
  console.log(`- Matched closures: ${matchedCount}`);
  console.log(`- Unmatched closures: ${unmatchedCount}`);
  console.log(`- Match rate (valid closures only): ${((matchedCount / validClosures.length) * 100).toFixed(1)}%`);
  console.log(`- Overall match rate (all closures): ${((matchedCount / allClosures.length) * 100).toFixed(1)}%`);
  console.log(`\nData Structure:`);
  console.log(`- Location data: latitude, longitude, city (for map display)`);
  console.log(`- Demographics: Referenced via school_demographics_lookup.json`);
  console.log(`- File size optimized: No duplicate demographic data`);

  // Show sample of unmatched records for review
  const unmatched = enrichedClosures.filter(r => !r.latitude || !r.longitude);
  if (unmatched.length > 0) {
    console.log(`\nSample of unmatched closure records (missing lat/lng):`);
    unmatched.slice(0, 5).forEach((r, i) => {
      console.log(`#${i+1}: School #${r.school_number} - ${r.school_name}, Board: ${r.board_name}`);
    });
  }

  // Show sample of invalid records
  if (invalidClosures.length > 0) {
    console.log(`\nSample of invalid closure records (missing school numbers):`);
    invalidClosures.slice(0, 3).forEach((r, i) => {
      console.log(`#${i+1}: School: ${r.school_name}, Board: ${r.board_name}`);
    });
  }

  // Write audit trail — commit this file alongside school_closures.json so corrections
  // are reviewable in code review. The validator reads it to report correction counts.
  await fs.writeFile(correctionsLog, JSON.stringify(corrections, null, 2));
  console.log(`\nWrote ${corrections.length} auto-correction(s) to ${correctionsLog}`);

  await fs.writeFile(outputJson, JSON.stringify(allClosures, null, 2));
  console.log(`Wrote cleaned school closures data to ${outputJson}`);
}

main().catch(console.error); 