import { aggregateCases, getCaseMarkerColor, getCaseMarkerRadius } from '@/lib/map-aggregation';
import { SchoolCase } from '@/types';
import { DemographicsData } from '@/hooks/useDemographicsLookup';
import {
  CASE_COLOR_LOW,
  CASE_COLOR_HIGH,
  MARKER_RADIUS_MIN,
  MARKER_RADIUS_MAX,
} from '@/lib/constants';

function makeCase(overrides: Partial<SchoolCase> = {}): SchoolCase {
  return {
    collected_date: '2020-09-15',
    reported_date: '2020-09-15',
    school_board: 'Toronto DSB',
    school_number: 123456,
    school_name: 'Test School',
    municipality: 'Toronto',
    confirmed_student_cases: 2,
    confirmed_staff_cases: 1,
    confirmed_unspecified_cases: 0,
    total_confirmed_cases: 3,
    latitude: 43.7,
    longitude: -79.4,
    ...overrides,
  };
}

const emptyLookup: Record<number, DemographicsData> = {};

// ---------------------------------------------------------------------------
// aggregateCases
// ---------------------------------------------------------------------------

describe('aggregateCases', () => {
  test('1. Two records with same school_number → one aggregated entry', () => {
    const cases = [
      makeCase({ school_number: 111, confirmed_student_cases: 2, total_confirmed_cases: 2 }),
      makeCase({ school_number: 111, confirmed_student_cases: 3, total_confirmed_cases: 3 }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(1);
  });

  test('2. Aggregated entry total_confirmed_cases = sum of both records', () => {
    const cases = [
      makeCase({ school_number: 111, total_confirmed_cases: 2 }),
      makeCase({ school_number: 111, total_confirmed_cases: 5 }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result[0].total_confirmed_cases).toBe(7);
  });

  test('3. Aggregated entry collected_date = the more recent of the two dates', () => {
    const cases = [
      makeCase({ school_number: 111, collected_date: '2020-09-10' }),
      makeCase({ school_number: 111, collected_date: '2020-09-20' }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result[0].collected_date).toBe('2020-09-20');
  });

  test('4. Aggregated entry _recordCount = 2', () => {
    const cases = [
      makeCase({ school_number: 111 }),
      makeCase({ school_number: 111 }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result[0]._recordCount).toBe(2);
  });

  test('5. Record missing latitude is excluded from output', () => {
    const cases = [
      makeCase({ school_number: 222, latitude: undefined }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(0);
  });

  test('6. Record missing longitude is excluded from output', () => {
    const cases = [
      makeCase({ school_number: 333, longitude: undefined }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(0);
  });

  test('7. Record with no school_number (0) falls back to school_name as dedup key', () => {
    const cases = [
      makeCase({ school_number: 0, school_name: 'Unnamed School', total_confirmed_cases: 1 }),
      makeCase({ school_number: 0, school_name: 'Unnamed School', total_confirmed_cases: 4 }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(1);
    expect(result[0].total_confirmed_cases).toBe(5);
  });

  test('8. Record with neither school_number (0) nor school_name is skipped', () => {
    const cases = [
      makeCase({ school_number: 0, school_name: '' }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(0);
  });

  test('9. Two records with same school_number but different school_names → ONE entry (school_number takes priority)', () => {
    const cases = [
      makeCase({ school_number: 999, school_name: 'School Alpha', total_confirmed_cases: 1 }),
      makeCase({ school_number: 999, school_name: 'School Beta', total_confirmed_cases: 2 }),
    ];
    const result = aggregateCases(cases, emptyLookup);
    expect(result).toHaveLength(1);
    expect(result[0].total_confirmed_cases).toBe(3);
  });

  test('10a. _demo is set to the lookup value for school_number when present in lookup', () => {
    const demo = {
      school_number: 123456,
      school_name: 'Test School',
      school_id: '',
      board_number: '',
      board_name: '',
      board_type: '',
      latitude: 43.7,
      longitude: -79.4,
      city: 'Toronto',
      province: 'ON',
      postal_code: '',
      street: '',
      municipality: 'Toronto',
      school_type: '',
      school_level: 'Elementary',
      school_language: 'English',
      grade_range: 'JK-8',
      school_special_condition_code: '',
      enrolment: 500,
      demographics: {
        percentage_of_students_whose_first_language_is_not_english: 10,
        percentage_of_students_whose_first_language_is_not_french: 90,
        percentage_of_students_who_are_new_to_canada_from_a_non_english_speaking_country: 5,
        percentage_of_students_who_are_new_to_canada_from_a_non_french_speaking_country: 5,
        percentage_of_students_receiving_special_education_services: 15,
        percentage_of_students_identified_as_gifted: 2,
        percentage_of_school_aged_children_who_live_in_low_income_households: 20,
        percentage_of_students_whose_parents_have_no_degree__diploma_or_certificate: 30,
      },
    } as DemographicsData;

    const lookup: Record<number, DemographicsData> = { 123456: demo };
    const cases = [makeCase({ school_number: 123456 })];
    const result = aggregateCases(cases, lookup);
    expect(result[0]._demo).toEqual(demo);
  });

  test('10b. _demo is null when school_number is not in lookup', () => {
    const cases = [makeCase({ school_number: 999999 })];
    const result = aggregateCases(cases, emptyLookup);
    expect(result[0]._demo).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// getCaseMarkerColor
// ---------------------------------------------------------------------------

describe('getCaseMarkerColor', () => {
  test('11. 1 case → CASE_COLOR_LOW (orange)', () => {
    expect(getCaseMarkerColor(1)).toBe(CASE_COLOR_LOW);
  });

  test('12. 5 cases → CASE_COLOR_LOW (boundary: <= 5 is low)', () => {
    expect(getCaseMarkerColor(5)).toBe(CASE_COLOR_LOW);
  });

  test('13. 6 cases → CASE_COLOR_HIGH (red)', () => {
    expect(getCaseMarkerColor(6)).toBe(CASE_COLOR_HIGH);
  });

  test('14. 0 cases → CASE_COLOR_LOW', () => {
    expect(getCaseMarkerColor(0)).toBe(CASE_COLOR_LOW);
  });
});

// ---------------------------------------------------------------------------
// getCaseMarkerRadius
// ---------------------------------------------------------------------------

describe('getCaseMarkerRadius', () => {
  test('15. 0 cases → MARKER_RADIUS_MIN (8)', () => {
    expect(getCaseMarkerRadius(0)).toBe(MARKER_RADIUS_MIN);
  });

  test('16. Very large case count → capped at MARKER_RADIUS_MAX (20)', () => {
    expect(getCaseMarkerRadius(100000)).toBe(MARKER_RADIUS_MAX);
  });

  test('17. 12 cases → 8 + 12 * 0.6 = 15.2', () => {
    expect(getCaseMarkerRadius(12)).toBeCloseTo(15.2);
  });
});
