/**
 * Checks the committed data files in public/data/ (built by scripts/build_data.js).
 * The case data is frozen, so these numbers should only change if the cleaning
 * logic changes on purpose. Then update them here, in the same PR.
 */
import fs from 'fs';
import path from 'path';
import { decodeCases, decodeProfiles, EncodedCases, EncodedProfiles } from '@/lib/data-loader';
import { SchoolSummary } from '@/types';

const DATA_DIR = path.join(__dirname, '../../public/data');
const read = <T>(name: string): T => JSON.parse(fs.readFileSync(path.join(DATA_DIR, name), 'utf8'));

const cases = decodeCases(read<EncodedCases>('cases.json'));
const profiles = decodeProfiles(read<EncodedProfiles>('school_profiles.json'));
const summaries = read<SchoolSummary[]>('summaries.json');

describe('cases.json', () => {
  test('has the expected records, schools and case total', () => {
    expect(cases).toHaveLength(120352);
    expect(new Set(cases.map((c) => c.school_number).filter(Boolean)).size).toBe(3815);
    expect(new Set(cases.map((c) => c.school_board).filter(Boolean)).size).toBe(118);
    expect(new Set(cases.map((c) => c.municipality).filter(Boolean)).size).toBe(315);
    expect(cases.reduce((sum, c) => sum + (c.total_confirmed_cases || 0), 0)).toBe(228058);
  });

  test('every record has a valid date in the school years covered', () => {
    const bad = cases.filter((c) => !/^\d{4}-\d{2}-\d{2}$/.test(c.collected_date) ||
      c.collected_date < '2020-09-01' || c.collected_date > '2022-06-30');
    expect(bad).toHaveLength(0);
  });

  test('every record has coordinates and a municipality', () => {
    expect(cases.filter((c) => c.latitude == null || c.longitude == null)).toHaveLength(0);
    expect(cases.filter((c) => !c.municipality)).toHaveLength(0);
  });

  test('case counts are non-negative numbers', () => {
    expect(cases.filter((c) => !(c.total_confirmed_cases >= 0))).toHaveLength(0);
  });

  test('no school or board name has a broken character (U+FFFD)', () => {
    expect(cases.filter((c) => `${c.school_name}${c.school_board}`.includes('�'))).toHaveLength(0);
  });
});

describe('school_profiles.json', () => {
  test('has a profile, with demographics, for every school number in the cases', () => {
    const numbers = new Set(cases.map((c) => c.school_number).filter((n): n is number => Boolean(n)));
    const missing = [...numbers].filter((n) => !profiles[n]);
    expect(missing).toHaveLength(0);
    const sample = profiles[[...numbers][0]];
    expect(sample.demographics).toHaveProperty('percentage_of_students_identified_as_gifted');
  });
});

describe('summaries.json', () => {
  test('covers both school years, sorted by date, ending 2021-12-22', () => {
    expect(summaries[0].collected_date).toBe('2020-09-10');
    expect(summaries.at(-1)!.collected_date).toBe('2021-12-22');
    const dates = summaries.map((s) => s.collected_date);
    expect([...dates].sort()).toEqual(dates);
  });
});
