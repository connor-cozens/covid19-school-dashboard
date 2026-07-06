import { SchoolCase, SchoolSummary } from '@/types';

export async function loadCSVData<T>(filePath: string): Promise<T[]> {
  const response = await fetch(filePath);
  if (!response.ok) {
    throw new Error(`Failed to load ${filePath}: HTTP ${response.status} ${response.statusText}`);
  }

  const csvText = await response.text();
  const lines = csvText.split('\n');
  const headers = lines[0].split(',').map(h => h.trim());

  const data: T[] = [];
  let skippedRows = 0;

  for (let i = 1; i < lines.length; i++) {
    const line = lines[i].trim();
    if (!line) continue;

    const values = parseCSVLine(line);

    if (values.length === headers.length) {
      const row: Record<string, string | number> = {};
      headers.forEach((header, index) => {
        const value = values[index];
        // Use Number() not parseFloat(): parseFloat('2020-09-10') = 2020 (wrong),
        // Number('2020-09-10') = NaN (correct — keeps date strings as strings).
        const numValue = value === '' ? NaN : Number(value);
        row[header] = isNaN(numValue) ? value : numValue;
      });
      data.push(row as T);
    } else {
      skippedRows++;
    }
  }

  if (skippedRows > 0) {
    console.warn(`loadCSVData(${filePath}): skipped ${skippedRows} rows with column count mismatch (expected ${headers.length} columns)`);
  }

  return data;
}

function parseCSVLine(line: string): string[] {
  const values: string[] = [];
  let current = '';
  let inQuotes = false;
  
  for (let i = 0; i < line.length; i++) {
    const char = line[i];
    
    if (char === '"') {
      inQuotes = !inQuotes;
    } else if (char === ',' && !inQuotes) {
      values.push(current.trim());
      current = '';
    } else {
      current += char;
    }
  }
  
  values.push(current.trim());
  return values;
}

export async function loadSchoolData() {
  const [schoolCases, summaries2021, summaries2022, schoolClosures] = await Promise.all([
    loadCSVData<SchoolCase>("/data/schoolsactivecovid.csv"),
    loadCSVData<SchoolSummary>("/data/schoolcovidsummary.csv"),
    loadCSVData<SchoolSummary>("/data/schoolcovidsummary2021_2022.csv"),
    (async () => {
      const response = await fetch('/data/school_closures.json');
      if (!response.ok) {
        throw new Error(`Failed to load school closures: HTTP ${response.status} ${response.statusText}`);
      }
      return await response.json();
    })(),
  ]);

  // Merge both summary CSVs and sort by collected_date ascending
  const schoolSummaries = [...summaries2021, ...summaries2022].sort((a, b) =>
    String(a.collected_date).localeCompare(String(b.collected_date))
  );

  if (schoolSummaries.length === 0) {
    throw new Error('School summary data loaded but contains no records. The KPI statistics cannot be calculated.');
  }

  return {
    schoolCases,
    schoolClosures,
    schoolSummaries,
  };
}

const MIN_COMBINED_RECORDS = 100_000; // Two full school years of daily per-school records

// Load the combined school cases with demographics JSON
export async function loadCombinedSchoolCases(): Promise<SchoolCase[]> {
  const response = await fetch('/data/covid19_schools_active_with_demographics_combined.json');
  if (!response.ok) {
    throw new Error(`Failed to load combined school cases: HTTP ${response.status} ${response.statusText}`);
  }
  const data: SchoolCase[] = await response.json();
  if (data.length < MIN_COMBINED_RECORDS) {
    throw new Error(
      `Combined school cases loaded only ${data.length} records (expected ≥ ${MIN_COMBINED_RECORDS}). ` +
      `The file may be truncated or corrupted.`
    );
  }
  return data;
}