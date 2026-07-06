// scripts/clean_school_data.js
const fs = require('fs').promises;
const path = require('path');
const { parse } = require('csv-parse/sync');
const process = require('process');

// Utility to normalize column names
function normalizeHeader(header) {
  return header
    .replace(/\s+/g, '_')
    .replace(/\./g, '_')
    .replace(/[^a-zA-Z0-9_]/g, '')
    .toLowerCase();
}

// Utility to fix encoding issues and normalize Unicode
function fixEncoding(val) {
  if (typeof val !== 'string') return val;
  // Normalize Unicode and replace common mis-encodings
  return val
    .normalize('NFC')
    .replace(/Ã‰/g, 'É')
    .replace(/Ã¨/g, 'è')
    .replace(/Ã©/g, 'é')
    .replace(/Ãª/g, 'ê')
    .replace(/Ã¢/g, 'â')
    .replace(/Ã´/g, 'ô')
    .replace(/Ã®/g, 'î')
    .replace(/Ã§/g, 'ç')
    .replace(/Ã¼/g, 'ü')
    .replace(/Ã¶/g, 'ö')
    .replace(/ÃŸ/g, 'ß')
    .replace(/â€™/g, "'")
    .replace(/â€“/g, '-')
    .replace(/â€œ/g, '"')
    .replace(/â€/g, '"')
    .replace(/â€/g, '"');
}

// Utility to canonicalize school board names.
//
// Strategy: The two source CSVs (19_20 and 20_21) use inconsistent values in
// the `school_board` column. The 19_20 CSV consistently uses full names. The
// 20_21 CSV sometimes uses abbreviated forms (e.g. "Peel DSB" instead of
// "Peel District School Board") or drops/adds hyphens (e.g. "Dufferin Peel
// Catholic District School Board" vs "Dufferin-Peel Catholic District School
// Board"). These mismatches cause duplicate entries in filter dropdowns.
//
// The canonical form is the full official name as used in the 19_20 CSV and
// the Ontario Ministry of Education school board registry.
//
// After alias lookup, whitespace is also normalized so that any accidental
// double spaces or leading/trailing spaces are cleaned up.
const boardAliases = {
  // Algoma
  'Algoma DSB': 'Algoma District School Board',
  // Dufferin-Peel Catholic — both hyphenated and unhyphenated forms appear
  'Dufferin Peel Catholic District School Board': 'Dufferin-Peel Catholic District School Board',
  'Dufferin-Peel CDSB': 'Dufferin-Peel Catholic District School Board',
  // Durham
  'Durham DSB': 'Durham District School Board',
  'DDSB': 'Durham District School Board',
  // Grand Erie
  'Grand Erie DSB': 'Grand Erie District School Board',
  // Halton
  'Halton DSB': 'Halton District School Board',
  'Halton CDSB': 'Halton Catholic District School Board',
  // Near North
  'Near North DSB': 'Near North District School Board',
  // Peel
  'Peel DSB': 'Peel District School Board',
  // Renfrew County
  'Renfrew County DSB': 'Renfrew County District School Board',
  // Simcoe County
  'Simcoe County DSB': 'Simcoe County District School Board',
  // Toronto
  'Toronto DSB': 'Toronto District School Board',
  // Waterloo
  'Waterloo CDSB': 'Waterloo Catholic District School Board',
  'Waterloo Region DSB': 'Waterloo Region District School Board',
  // Windsor-Essex Catholic
  'Windsor-Essex CDSB': 'Windsor-Essex Catholic District School Board',
  // York Region
  'York Region DSB': 'York Region District School Board',
  // Ottawa-Carleton — hyphenated and non-hyphenated forms both appear in source data
  'Ottawa Carleton District School Board': 'Ottawa-Carleton District School Board',
  'Ottawa Carleton DSB': 'Ottawa-Carleton District School Board',
  // Dufferin-Peel — non-hyphenated form appears in 19_20 CSV
  'Dufferin Peel Catholic District School Board': 'Dufferin-Peel Catholic District School Board',
  'Dufferin-Peel CDSB': 'Dufferin-Peel Catholic District School Board',
  // Hamilton-Wentworth
  'Hamilton Wentworth District School Board': 'Hamilton-Wentworth District School Board',
  'Hamilton-Wentworth DSB': 'Hamilton-Wentworth District School Board',
  'Hamilton Wentworth Catholic District School Board': 'Hamilton-Wentworth Catholic District School Board',
  // One source record has the school name in the board field with a trailing U+FFFD —
  // the correct board for Aurora "Catholic Education Centre" is York Catholic DSB.
  ['Catholic Education Centre\uFFFD']: 'York Catholic District School Board',
};
function canonicalizeBoardName(val) {
  if (!val) return val;
  // Fix encoding, trim, and normalize internal whitespace
  const fixed = fixEncoding(val).trim().replace(/\s+/g, ' ');
  return boardAliases[fixed] || fixed;
}

// Utility to trim and clean string values
function cleanString(val) {
  return typeof val === 'string' ? fixEncoding(val.trim().replace(/^"|"$/g, '')) : val;
}

// Utility to treat R's literal "NA" string as null
function nullIfNA(val) {
  if (val === 'NA' || val === null || val === undefined) return null;
  return val;
}

// Utility to convert to number if possible
function toNumber(val) {
  if (val == null) return null;
  const num = Number(String(val).replace(/[^0-9.\-]/g, ''));
  return isNaN(num) ? null : num;
}

// Utility to convert to ISO date string if possible
function toISODate(val) {
  if (!val) return null;
  const d = new Date(val);
  return isNaN(d.getTime()) ? null : d.toISOString().slice(0, 10);
}

// Add a utility to load and clean a CSV file
async function loadAndCleanCsv(inputCsv) {
  const csvRaw = await fs.readFile(inputCsv, 'utf8');
  const records = parse(csvRaw, {
    columns: true,
    skip_empty_lines: true,
  });
  return records.map((row) => {
    const obj = {};
    Object.entries(row).forEach(([key, value]) => {
      const normKey = normalizeHeader(key);
      let cleanedValue = typeof value === 'string' ? cleanString(value) : value;
      if (normKey.endsWith('date') && typeof value === 'string') {
        obj[normKey] = toISODate(cleanedValue);
      } else if (
        (typeof value === 'string') && (
          normKey.includes('cases') ||
          normKey.includes('enrolment') ||
          normKey.includes('latitude') ||
          normKey.includes('longitude') ||
          normKey.includes('percentage') ||
          normKey.includes('number')
        )
      ) {
        obj[normKey] = toNumber(cleanedValue);
      } else if (normKey === 'school_board' || normKey === 'board_name' || normKey === 'board name') {
        obj[normKey] = canonicalizeBoardName(cleanedValue);
      } else if (normKey === 'school' || normKey === 'school_name' || normKey === 'school name') {
        obj[normKey] = fixEncoding(cleanedValue);
      } else {
        obj[normKey] = cleanedValue;
      }
    });
    // Add generic fields for frontend compatibility
    if (obj['collected_date']) {
      obj['date'] = obj['collected_date'];
    }
    if (typeof obj['total_confirmed_cases'] !== 'undefined') {
      obj['cases'] = obj['total_confirmed_cases'];
    }
    return obj;
  });
}

async function main() {
  const inputCsvs = [
    path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_19_20.csv'),
    path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_20_21.csv'),
  ];
  const outputJson = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');

  const allCleaned = [];
  for (const csvPath of inputCsvs) {
    const cleaned = await loadAndCleanCsv(csvPath);
    allCleaned.push(...cleaned);
  }

  // Load demographics lookup for encoding recovery
  const demographicsLookupPath = path.join(__dirname, '../public/data/school_demographics_lookup.json');
  const demographicsLookup = JSON.parse(await fs.readFile(demographicsLookupPath, 'utf8'));

  // U+FFFD replacement character — used throughout school-name resolution below
  const REPLACEMENT_CHAR = '\uFFFD';

  // Create lean version with only essential data
  const leanData = allCleaned.map(record => {
    // School name resolution — the CSV has two name columns:
    //   `school`      — original Ontario gov name (always present in source data)
    //   `school_name` — from a demographics left-join (literal "NA" when join missed)
    //
    // Priority:
    //   1. Gov name (`school`), if it is present, non-"NA", and not corrupt (no U+FFFD)
    //   2. Demographics join name (`school_name`), if it is present and non-"NA"
    //      (the join name is recovered from a clean Excel export, usually free of U+FFFD)
    //   3. Raw gov name as last resort (still corrupt — the recovery pass below will attempt
    //      to fix it via the demographics-lookup JSON or the schoolNameAliases dict)
    const cleanGovName = (record.school && record.school !== 'NA' && !record.school.includes(REPLACEMENT_CHAR))
      ? record.school : null;
    const cleanDemoName = (record.school_name && record.school_name !== 'NA' && !record.school_name.includes(REPLACEMENT_CHAR))
      ? record.school_name : null;
    const resolvedSchoolName = cleanGovName || cleanDemoName || nullIfNA(record.school) || nullIfNA(record.school_name);

    return ({
    // Essential case data
    collected_date: record.collected_date,
    reported_date: record.reported_date,
    confirmed_student_cases: record.confirmed_student_cases,
    confirmed_staff_cases: record.confirmed_staff_cases,
    confirmed_unspecified_cases: record.confirmed_unspecified_cases,
    total_confirmed_cases: record.total_confirmed_cases,
    confirmed_unidentified_cases: record.confirmed_unidentified_cases,

    // School identification (for demographics lookup)
    // school_number: treat R's "NA" string as null (some records have no demographics match)
    school_number: nullIfNA(record.school_number),
    school_name: resolvedSchoolName,
    school_board: record.school_board,
    
    // Essential location data (for map display and municipality filtering)
    // Note: `municipality` is the Ontario government's administrative region (from the
    // covid19 case CSV). `city` is from the demographics join and used for closures.
    // Both are retained because they serve different purposes in the frontend.
    latitude: record.latitude,
    longitude: record.longitude,
    city: record.city,
    municipality: record.municipality,
    
    // Frontend compatibility fields
    date: record.collected_date,
    cases: record.total_confirmed_cases,
    
    // Flag for demographics availability (true only when a valid school_number exists)
    has_demographics: !!nullIfNA(record.school_number),
  });
  });

  // Recovery: fix corrupt school names and board names (U+FFFD) using demographics lookup
  let recoveredCount = 0;
  let stillCorruptCount = 0;
  let recoveredBoardCount = 0;

  const fixedLeanData = leanData.map(record => {
    const schoolName = record.school_name || '';
    const schoolBoard = record.school_board || '';
    const needsNameRecovery = schoolName.includes(REPLACEMENT_CHAR);
    const needsBoardRecovery = schoolBoard.includes(REPLACEMENT_CHAR);

    if (!needsNameRecovery && !needsBoardRecovery) return record;

    // Try to recover from demographics lookup by school_number
    const schoolNum = String(record.school_number || '');
    const demographicsEntry = schoolNum ? demographicsLookup[schoolNum] : null;

    let updated = { ...record };

    if (needsNameRecovery) {
      const correctName = demographicsEntry ? (demographicsEntry['school name'] || demographicsEntry['school_name']) : null;
      if (correctName && !correctName.includes(REPLACEMENT_CHAR)) {
        recoveredCount++;
        updated.school_name = correctName;
      } else {
        stillCorruptCount++;
      }
    }

    if (needsBoardRecovery) {
      const correctBoard = demographicsEntry ? demographicsEntry['board_name'] : null;
      if (correctBoard && !correctBoard.includes(REPLACEMENT_CHAR)) {
        recoveredBoardCount++;
        updated.school_board = correctBoard;
      }
    }

    return updated;
  });

  // Manual aliases for school names that couldn't be recovered via the demographics lookup
  // (records with no school_number, so the lookup-based recovery above cannot reach them).
  // Each alias maps the exact corrupt string (with U+FFFD) to the authoritative clean name.
  // Sources: confirmed via demographics lookup (same board, same school_number in other rows)
  // or via the raw CSV school column where a clean row was found for the same school.
  const schoolNameAliases = {
    // Toronto Catholic DSB — school #784435 in demographics lookup
    'Sts Cosmas Damian\uFFFD Catholic Elementary School': 'Sts Cosmas and Damian Catholic School',
    // Ottawa Catholic School Board — school #688193 in demographics lookup
    // "C" is an abbreviation for "Catholic"; corrupt char source unclear
    'Assumption C Elementary \uFFFD School': 'Assumption Catholic Elementary School',
    // Conseil des écoles s de l'Est de l'Ontario — "à" (U+00E0) was corrupted
    'Centre éducatif services \uFFFD la jeunesse': 'Centre éducatif services à la jeunesse',
    // Le Conseil MonAvenir — trailing corrupt char stripped; confirmed via raw CSV
    'École élémentaire du Sacré Cœur\uFFFD': 'École élémentaire du Sacré Cœur',
    // Renfrew County DSB — three trailing/embedded corrupt chars stripped
    'Eganville District\uFFFD Public School\uFFFD \uFFFD': 'Eganville District Public School',
    // York Region DSB, Richmond Hill — "Ross Doan PS" is the only Doan school in York Region;
    // "Rose" vs "Ross" difference and corrupt chars suggest same school, different name variant
    'Rose\uFFFD Doan\uFFFD Public School': 'Ross Doan Public School',
  };

  let aliasRecoveredCount = 0;
  const finalData = fixedLeanData.map(record => {
    const name = record.school_name || '';
    if (name.includes(REPLACEMENT_CHAR) && schoolNameAliases[name]) {
      aliasRecoveredCount++;
      return { ...record, school_name: schoolNameAliases[name] };
    }
    return record;
  });

  await fs.writeFile(outputJson, JSON.stringify(finalData, null, 2), 'utf8');

  // Log statistics
  const originalSize = JSON.stringify(allCleaned).length;
  const leanSize = JSON.stringify(finalData).length;
  const reduction = ((originalSize - leanSize) / originalSize * 100).toFixed(1);

  console.log(`Wrote lean data to ${outputJson}`);
  console.log(`Original size: ${(originalSize / 1024 / 1024).toFixed(1)} MB`);
  console.log(`Lean size: ${(leanSize / 1024 / 1024).toFixed(1)} MB`);
  console.log(`Size reduction: ${reduction}%`);

  console.log(`\nEncoding recovery:`);
  console.log(`- School names recovered via demographics lookup: ${recoveredCount}`);
  console.log(`- School names recovered via manual alias: ${aliasRecoveredCount}`);
  console.log(`- School names still corrupt after all recovery: ${stillCorruptCount - aliasRecoveredCount}`);
  if (stillCorruptCount - aliasRecoveredCount > 0) {
    console.warn(`  WARNING: ${stillCorruptCount - aliasRecoveredCount} school names could not be recovered`);
  }
  console.log(`- School board names recovered via lookup: ${recoveredBoardCount}`);

  const corruptBoards = finalData.filter(r => (r.school_board||'').includes(REPLACEMENT_CHAR)).length;
  const corruptMuns = finalData.filter(r => (r.municipality||'').includes(REPLACEMENT_CHAR)).length;
  if (corruptBoards > 0) console.warn(`WARNING: ${corruptBoards} records have corrupt school_board`);
  if (corruptMuns > 0) console.warn(`WARNING: ${corruptMuns} records have corrupt municipality`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
}); 