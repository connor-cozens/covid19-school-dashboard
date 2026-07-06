const fs = require('fs');
const path = require('path');

// Path to the combined JSON file
const DATA_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');

// Required fields and their types (updated to match actual data)
const REQUIRED_FIELDS = {
  collected_date: 'string', // date field
  total_confirmed_cases: 'number', // cases field
  school_name: 'string',
  city: 'string',
  province: 'string',
  postal_code: 'string',
  enrolment: 'number',
};

// Helper to check if a value is a valid date string
function isValidDate(dateStr) {
  const d = new Date(dateStr);
  return !isNaN(d.getTime());
}

// Helper to check for malformed French characters
const MALFORMED_FRENCH_PATTERNS = [
  /Ã©/, /Ã¨/, /Ãª/, /Ã /, /Ã¢/, /Ã«/, /Ã¹/, /Ã»/, /Ã¼/, /Ã´/, /Ã¶/, /Ã§/, /Ã‰/, /â€™/, /â€“/, /â€œ/, /â€/, /â€˜/, /â€¢/, /â€”,/ // common mis-encodings
];

function hasMalformedFrench(str) {
  return MALFORMED_FRENCH_PATTERNS.some((pat) => pat.test(str));
}

function checkRecord(record, index) {
  const errors = [];
  for (const field in REQUIRED_FIELDS) {
    const type = REQUIRED_FIELDS[field];
    if (!(field in record)) {
      errors.push(`Missing field: ${field}`);
      continue;
    }
    if (type === 'number' && typeof record[field] !== 'number') {
      errors.push(`Field ${field} is not a number (value: ${record[field]})`);
    } else if (type === 'string' && typeof record[field] !== 'string') {
      errors.push(`Field ${field} is not a string (value: ${record[field]})`);
    }
    // Check for malformed French characters in string fields
    if (type === 'string' && typeof record[field] === 'string' && hasMalformedFrench(record[field])) {
      errors.push(`Malformed French characters in field ${field}: ${record[field]}`);
    }
  }
  // Check all other string fields for malformed French characters
  for (const key in record) {
    if (typeof record[key] === 'string' && hasMalformedFrench(record[key])) {
      errors.push(`Malformed French characters in field ${key}: ${record[key]}`);
    }
  }
  // Date validity
  if ('collected_date' in record && typeof record.collected_date === 'string' && !isValidDate(record.collected_date)) {
    errors.push(`Invalid collected_date: ${record.collected_date}`);
  }
  return errors;
}

function main() {
  const raw = fs.readFileSync(DATA_PATH, 'utf-8');
  let data;
  try {
    data = JSON.parse(raw);
  } catch (e) {
    console.error('Failed to parse JSON:', e);
    process.exit(1);
  }

  let malformedCount = 0;
  const malformedSamples = [];

  data.forEach((record, i) => {
    const errors = checkRecord(record, i);
    if (errors.length > 0) {
      malformedCount++;
      if (malformedSamples.length < 10) {
        malformedSamples.push({ index: i, errors, record });
      }
    }
  });

  console.log(`Checked ${data.length} records.`);
  console.log(`Malformed records: ${malformedCount}`);
  if (malformedSamples.length > 0) {
    console.log('\nSample malformed records:');
    malformedSamples.forEach(sample => {
      console.log(`\nIndex: ${sample.index}`);
      console.log('Errors:', sample.errors);
      console.log('Record:', sample.record);
    });
  } else {
    console.log('No malformed records found.');
  }
}

main(); 