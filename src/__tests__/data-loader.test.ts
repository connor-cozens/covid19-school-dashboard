import { decodeCases, decodeProfiles, loadDashboardData, EncodedCases, EncodedProfiles } from '@/lib/data-loader';

const encodedCases: EncodedCases = {
  placeFields: ['school_number', 'school_name', 'school_board', 'latitude', 'longitude', 'city', 'municipality'],
  countFields: ['confirmed_student_cases', 'confirmed_staff_cases', 'confirmed_unspecified_cases', 'total_confirmed_cases'],
  places: [
    [123, 'A School', 'A Board', 43.7, -79.4, 'Toronto', 'Toronto'],
    [null, 'B School', 'B Board', 45.4, -75.7, 'Ottawa', 'Ottawa'],
  ],
  dates: ['2020-09-10', '2020-09-11'],
  rows: [
    [1, 0, 2, 1, null, 3],
    [0, 1, 0, 1, 0, 1],
  ],
};

const encodedProfiles: EncodedProfiles = {
  profileFields: ['school_level', 'school_language', 'grade_range', 'enrolment'],
  demographicFields: ['percentage_of_students_identified_as_gifted'],
  schools: { '123': ['Elementary', 'English', 'JK-8', 400, 2] },
};

describe('decodeCases', () => {
  test('rebuilds one record per row, in order, with place and count fields', () => {
    expect(decodeCases(encodedCases)).toEqual([
      {
        collected_date: '2020-09-11', school_number: 123, school_name: 'A School', school_board: 'A Board',
        latitude: 43.7, longitude: -79.4, city: 'Toronto', municipality: 'Toronto',
        confirmed_student_cases: 2, confirmed_staff_cases: 1, confirmed_unspecified_cases: null, total_confirmed_cases: 3,
      },
      {
        collected_date: '2020-09-10', school_number: null, school_name: 'B School', school_board: 'B Board',
        latitude: 45.4, longitude: -75.7, city: 'Ottawa', municipality: 'Ottawa',
        confirmed_student_cases: 0, confirmed_staff_cases: 1, confirmed_unspecified_cases: 0, total_confirmed_cases: 1,
      },
    ]);
  });
});

describe('decodeProfiles', () => {
  test('keys profiles by school number and nests the demographics', () => {
    expect(decodeProfiles(encodedProfiles)).toEqual({
      123: {
        school_level: 'Elementary', school_language: 'English', grade_range: 'JK-8', enrolment: 400,
        demographics: { percentage_of_students_identified_as_gifted: 2 },
      },
    });
  });
});

describe('loadDashboardData', () => {
  afterEach(() => jest.restoreAllMocks());

  function mockFetch(responses: Record<string, { ok: boolean; body?: unknown }>) {
    jest.spyOn(global, 'fetch').mockImplementation((url: string | Request | URL) => {
      const name = url.toString().split('/').pop()!;
      const { ok, body } = responses[name] ?? { ok: false };
      return Promise.resolve({
        ok, status: ok ? 200 : 404, statusText: ok ? 'OK' : 'Not Found', json: () => Promise.resolve(body),
      } as Response);
    });
  }

  test('names the file and HTTP status when a fetch fails', async () => {
    mockFetch({ 'cases.json': { ok: true, body: encodedCases } });
    await expect(loadDashboardData()).rejects.toThrow(/HTTP 404/);
  });

  test('rejects a truncated cases file', async () => {
    mockFetch({
      'cases.json': { ok: true, body: encodedCases },
      'school_closures.json': { ok: true, body: [] },
      'summaries.json': { ok: true, body: [{ collected_date: '2020-09-10' }] },
      'school_profiles.json': { ok: true, body: encodedProfiles },
    });
    await expect(loadDashboardData()).rejects.toThrow(/only 2 records/);
  });
});
