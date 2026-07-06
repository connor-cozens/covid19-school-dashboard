import { loadCSVData, loadSchoolData, loadCombinedSchoolCases } from '@/lib/csv-loader';

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

type FetchResponse = {
  ok: boolean;
  status?: number;
  body: unknown;
};

function mockFetch(responses: Record<string, FetchResponse>) {
  jest.spyOn(global, 'fetch').mockImplementation((url: string | Request | URL) => {
    const urlStr = url.toString();
    const match = Object.entries(responses).find(([key]) => urlStr.includes(key));
    if (!match) return Promise.reject(new Error(`Unexpected fetch: ${urlStr}`));
    const [, { ok, status = 200, body }] = match;
    const text = typeof body === 'string' ? body : JSON.stringify(body);
    return Promise.resolve({
      ok,
      status,
      statusText: ok ? 'OK' : 'Not Found',
      text: () => Promise.resolve(text),
      json: () => Promise.resolve(typeof body === 'string' ? JSON.parse(text) : body),
    } as Response);
  });
}

afterEach(() => jest.restoreAllMocks());

// ---------------------------------------------------------------------------
// loadCSVData — type coercion
// ---------------------------------------------------------------------------

describe('loadCSVData — type coercion', () => {
  test('date string is preserved as string; numeric string becomes number', async () => {
    const csv = 'collected_date,count\n2020-09-10,42';
    mockFetch({ '/data/test.csv': { ok: true, body: csv } });

    const rows = await loadCSVData<{ collected_date: string | number; count: string | number }>(
      '/data/test.csv'
    );

    expect(rows).toHaveLength(1);
    expect(rows[0].collected_date).toBe('2020-09-10');
    expect(rows[0].count).toBe(42);
  });

  test('quoted field containing a comma is parsed as a single value', async () => {
    const csv = 'name,val\n"Smith, John",1';
    mockFetch({ '/data/test.csv': { ok: true, body: csv } });

    const rows = await loadCSVData<{ name: string | number; val: string | number }>(
      '/data/test.csv'
    );

    expect(rows).toHaveLength(1);
    expect(rows[0].name).toBe('Smith, John');
  });

  test('empty field between commas is stored as empty string', async () => {
    const csv = 'a,b,c\nfoo,,bar';
    mockFetch({ '/data/test.csv': { ok: true, body: csv } });

    const rows = await loadCSVData<{ a: string | number; b: string | number; c: string | number }>(
      '/data/test.csv'
    );

    expect(rows).toHaveLength(1);
    expect(rows[0].b).toBe('');
  });

  test('row with wrong column count is skipped; valid rows still returned', async () => {
    // Three headers, first data row has only 2 columns, second is valid
    const csv = 'a,b,c\nfoo,bar\nalpha,beta,gamma';
    mockFetch({ '/data/test.csv': { ok: true, body: csv } });

    const rows = await loadCSVData<{ a: string | number; b: string | number; c: string | number }>(
      '/data/test.csv'
    );

    expect(rows).toHaveLength(1);
    expect(rows[0].a).toBe('alpha');
  });

  test('HTTP 404 response throws an error containing the status code', async () => {
    mockFetch({ '/data/test.csv': { ok: false, status: 404, body: '' } });

    await expect(loadCSVData('/data/test.csv')).rejects.toThrow('404');
  });
});

// ---------------------------------------------------------------------------
// loadCombinedSchoolCases — validation
// ---------------------------------------------------------------------------

describe('loadCombinedSchoolCases', () => {
  test('rejects when response is not ok', async () => {
    mockFetch({
      'covid19_schools_active_with_demographics_combined.json': {
        ok: false,
        status: 500,
        body: '[]',
      },
    });

    await expect(loadCombinedSchoolCases()).rejects.toThrow('500');
  });

  test('rejects when array length is below 100,000', async () => {
    const smallArray = Array(10).fill({ school: 'test' });
    mockFetch({
      'covid19_schools_active_with_demographics_combined.json': {
        ok: true,
        body: smallArray,
      },
    });

    await expect(loadCombinedSchoolCases()).rejects.toThrow(/100.?000|100000/);
  });

  test('resolves when array length is >= 100,001', async () => {
    const largeArray = Array(100001).fill({ school: 'test' });
    mockFetch({
      'covid19_schools_active_with_demographics_combined.json': {
        ok: true,
        body: largeArray,
      },
    });

    const result = await loadCombinedSchoolCases();
    expect(result).toHaveLength(100001);
  });
});

// ---------------------------------------------------------------------------
// loadSchoolData — merge, sort, and validation
// ---------------------------------------------------------------------------

describe('loadSchoolData', () => {
  const minimalCasesCsv = 'school_name,cases\nTest School,1';
  const minimalClosures = [{ school_name: 'Test', date_of_closure: '2021-01-01' }];

  test('merges both summary CSVs and sorts ascending by collected_date', async () => {
    const summary2021Csv = 'collected_date,cumulative_school_related_cases\n2021-04-01,100';
    const summary2022Csv = 'collected_date,cumulative_school_related_cases\n2021-09-01,200';

    mockFetch({
      schoolsactivecovid: { ok: true, body: minimalCasesCsv },
      schoolcovidsummary2021_2022: { ok: true, body: summary2022Csv },
      schoolcovidsummary: { ok: true, body: summary2021Csv },
      school_closures: { ok: true, body: minimalClosures },
    });

    const { schoolSummaries } = await loadSchoolData();
    expect(schoolSummaries).toHaveLength(2);
    // Year 1 date (2021-04-01) comes before year 2 date (2021-09-01)
    expect(String(schoolSummaries[0].collected_date)).toBe('2021-04-01');
    expect(String(schoolSummaries[1].collected_date)).toBe('2021-09-01');
  });

  test('throws when both summary CSVs are empty', async () => {
    const emptyCsv = 'collected_date,cumulative_school_related_cases\n';

    mockFetch({
      schoolsactivecovid: { ok: true, body: minimalCasesCsv },
      schoolcovidsummary2021_2022: { ok: true, body: emptyCsv },
      schoolcovidsummary: { ok: true, body: emptyCsv },
      school_closures: { ok: true, body: minimalClosures },
    });

    await expect(loadSchoolData()).rejects.toThrow('KPI statistics');
  });
});
