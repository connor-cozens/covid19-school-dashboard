import { SchoolCase, SchoolClosure, SchoolProfiles, SchoolSummary, SchoolDemographics } from '@/types';

// Everything the dashboard shows comes from four static files in public/data/,
// fetched once per page load. The case and profile files use a compact format
// written by scripts/build_data.js; the decoders below turn them back into records.

export interface EncodedCases {
  placeFields: string[];
  countFields: string[];
  places: (string | number | null)[][];
  dates: string[];
  rows: (number | null)[][];
}

export interface EncodedProfiles {
  profileFields: string[];
  demographicFields: string[];
  schools: Record<string, (string | number | null)[]>;
}

export interface DashboardData {
  cases: SchoolCase[];
  closures: SchoolClosure[];
  summaries: SchoolSummary[];
  profiles: SchoolProfiles;
}

/** Two full school years of daily per-school records; fewer means a truncated file. */
export const MIN_CASE_RECORDS = 100_000;

export function decodeCases(encoded: EncodedCases): SchoolCase[] {
  const places = encoded.places.map((place) =>
    Object.fromEntries(encoded.placeFields.map((field, i) => [field, place[i]]))
  );
  return encoded.rows.map(([dateIndex, placeIndex, ...counts]) => {
    const record: Record<string, unknown> = {
      collected_date: encoded.dates[dateIndex as number],
      ...places[placeIndex as number],
    };
    encoded.countFields.forEach((field, i) => { record[field] = counts[i]; });
    return record as unknown as SchoolCase;
  });
}

export function decodeProfiles(encoded: EncodedProfiles): SchoolProfiles {
  const profiles: SchoolProfiles = {};
  for (const [number, values] of Object.entries(encoded.schools)) {
    const n = encoded.profileFields.length;
    const profile: Record<string, unknown> = Object.fromEntries(
      encoded.profileFields.map((field, i) => [field, values[i]])
    );
    profile.demographics = Object.fromEntries(
      encoded.demographicFields.map((field, i) => [field, values[n + i]])
    ) as unknown as SchoolDemographics;
    profiles[Number(number)] = profile as unknown as SchoolProfiles[number];
  }
  return profiles;
}

async function fetchJson<T>(name: string): Promise<T> {
  const response = await fetch(`/data/${name}`);
  if (!response.ok) {
    throw new Error(`Failed to load ${name}: HTTP ${response.status} ${response.statusText}`);
  }
  return response.json();
}

export async function loadDashboardData(): Promise<DashboardData> {
  const [encodedCases, closures, summaries, encodedProfiles] = await Promise.all([
    fetchJson<EncodedCases>('cases.json'),
    fetchJson<SchoolClosure[]>('school_closures.json'),
    fetchJson<SchoolSummary[]>('summaries.json'),
    fetchJson<EncodedProfiles>('school_profiles.json'),
  ]);

  const cases = decodeCases(encodedCases);
  if (cases.length < MIN_CASE_RECORDS) {
    throw new Error(
      `cases.json has only ${cases.length} records (expected at least ${MIN_CASE_RECORDS}). ` +
      'The file may be truncated or corrupted.'
    );
  }
  if (summaries.length === 0) {
    throw new Error('summaries.json contains no records, so the case totals cannot be calculated.');
  }

  return { cases, closures, summaries, profiles: decodeProfiles(encodedProfiles) };
}
