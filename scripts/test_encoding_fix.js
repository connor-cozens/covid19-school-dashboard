// scripts/test_encoding_fix.js
//
// Unit tests for the U+FFFD encoding recovery logic used in clean_school_data.js
// Run with: node scripts/test_encoding_fix.js

const assert = require('assert');

const REPLACEMENT_CHAR = '\uFFFD';

// ---------------------------------------------------------------------------
// Recovery logic (mirrors the implementation in clean_school_data.js)
// ---------------------------------------------------------------------------
function recoverSchoolName(record, demographicsLookup) {
  const schoolName = record.school_name || '';
  if (!schoolName.includes(REPLACEMENT_CHAR)) return record;

  const schoolNum = String(record.school_number || '');
  const demographicsEntry = schoolNum ? demographicsLookup[schoolNum] : null;
  const correctName = demographicsEntry ? (demographicsEntry['school name'] || demographicsEntry['school_name']) : null;

  if (correctName && !correctName.includes(REPLACEMENT_CHAR)) {
    return { ...record, school_name: correctName };
  }

  return record;
}

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------
let passed = 0;
let failed = 0;

function test(description, fn) {
  try {
    fn();
    console.log(`  PASS: ${description}`);
    passed++;
  } catch (err) {
    console.log(`  FAIL: ${description}`);
    console.log(`        ${err.message}`);
    failed++;
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------
console.log('test_encoding_fix.js — Encoding recovery unit tests');
console.log('='.repeat(60));

test('Corrupt name is recovered from lookup when entry exists', () => {
  const corruptRecord = {
    school_number: 488291,
    school_name: `\uFFFDcole \uFFFDl\uFFFDmentaire catholique Roger-Saint-Denis`,
  };
  const lookup = {
    '488291': { 'school name': 'École élémentaire catholique Roger-Saint-Denis' },
  };
  const result = recoverSchoolName(corruptRecord, lookup);
  assert.strictEqual(result.school_name, 'École élémentaire catholique Roger-Saint-Denis');
  assert(!result.school_name.includes(REPLACEMENT_CHAR), 'Result should not contain U+FFFD');
});

test('Clean name is left unchanged (no lookup call needed)', () => {
  const cleanRecord = {
    school_number: 488291,
    school_name: 'École élémentaire catholique Roger-Saint-Denis',
  };
  const lookup = {
    '488291': { 'school name': 'École élémentaire catholique Roger-Saint-Denis' },
  };
  const result = recoverSchoolName(cleanRecord, lookup);
  assert.strictEqual(result.school_name, cleanRecord.school_name);
});

test('Corrupt name is left unchanged when no lookup entry exists', () => {
  const corruptRecord = {
    school_number: 999999,
    school_name: `\uFFFDcole inconnue`,
  };
  const lookup = {};
  const result = recoverSchoolName(corruptRecord, lookup);
  assert.strictEqual(result.school_name, corruptRecord.school_name,
    'Name should be unchanged when lookup has no entry');
  assert(result.school_name.includes(REPLACEMENT_CHAR),
    'Name should still contain U+FFFD when unrecoverable');
});

test('Corrupt name is left unchanged when school_number is missing', () => {
  const corruptRecord = {
    school_name: `\uFFFDcole sans num\uFFFDro`,
  };
  const lookup = {
    '': { 'school name': 'Should not be used' },
  };
  const result = recoverSchoolName(corruptRecord, lookup);
  assert.strictEqual(result.school_name, corruptRecord.school_name);
});

test('school_number as integer is matched against string-keyed lookup', () => {
  // school_number in JSON is a number, lookup keys are strings
  const corruptRecord = {
    school_number: 123456,
    school_name: `\uFFFDcole test`,
  };
  const lookup = {
    '123456': { 'school name': 'École test' },
  };
  const result = recoverSchoolName(corruptRecord, lookup);
  assert.strictEqual(result.school_name, 'École test');
});

test('Recovery does not alter other record fields', () => {
  const corruptRecord = {
    school_number: 488291,
    school_name: `\uFFFDcole \uFFFDl\uFFFDmentaire`,
    school_board: 'Ottawa-Carleton District School Board',
    collected_date: '2021-01-15',
    total_confirmed_cases: 3,
  };
  const lookup = {
    '488291': { 'school name': 'École élémentaire' },
  };
  const result = recoverSchoolName(corruptRecord, lookup);
  assert.strictEqual(result.school_board, corruptRecord.school_board);
  assert.strictEqual(result.collected_date, corruptRecord.collected_date);
  assert.strictEqual(result.total_confirmed_cases, corruptRecord.total_confirmed_cases);
});

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------
console.log('='.repeat(60));
const total = passed + failed;
console.log(`Results: ${passed}/${total} tests passed`);
if (failed > 0) {
  console.error(`FAIL: ${failed} test(s) failed`);
  process.exit(1);
} else {
  console.log('PASS: All tests passed');
}
