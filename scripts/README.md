# Data Pipeline Scripts

This directory contains Node.js scripts for building and validating the data
files consumed by the React dashboard.

---

## Script Overview

### `clean_school_data.js`

**Purpose:** Reads the two raw Ontario government COVID-19 school case CSVs,
joins them with the demographics data, cleans and normalises all fields, and
outputs a single "lean" JSON file used by the frontend.

**Input files:**
- `public/data/covid19_schools_active_with_demographics_19_20.csv`
- `public/data/covid19_schools_active_with_demographics_20_21.csv`

**Output:**
- `public/data/covid19_schools_active_with_demographics_combined.json`

**Key transformations:**
- Column names normalised (dots → underscores, lowercased)
- Encoding artefacts fixed (UTF-8 mojibake for French accents, smart quotes)
- Board names canonicalised via `boardAliases` (e.g. `Peel DSB` → `Peel District School Board`)
- Dates converted to ISO-8601 strings; numeric fields coerced to numbers
- Only essential fields are kept in the lean output to minimise bundle size

**Run with:**
```bash
node scripts/clean_school_data.js
```

---

### `generate_filter_options.js`

**Purpose:** Reads the combined JSON produced by `clean_school_data.js` and
the school closures JSON to produce the dropdown option files used by the
React filter UI.

**Input files:**
- `public/data/covid19_schools_active_with_demographics_combined.json`
- `public/data/school_closures.json`

**Output:** Various filter option JSON files in `public/data/`

**Run with:**
```bash
node scripts/generate_filter_options.js
```

---

### `validate_combined_data.js`

**Purpose:** Validates the combined JSON for data quality issues. Run this
after `clean_school_data.js` to confirm the output is correct before deploying.

**Input:**
- `public/data/covid19_schools_active_with_demographics_combined.json`

**Checks performed:**
1. Row count — must be >= 120,000 records (both school years combined)
2. Field completeness — reports what % of records have `municipality`, `city`,
   `school_number`, `latitude`, `longitude`, `school_board`, `collected_date`
3. Date validity — all `collected_date` values must be parseable and fall
   between 2020-01-01 and 2023-06-30
4. Numeric checks — `total_confirmed_cases` must be non-negative and non-null
5. Case sum consistency — checks that `total_confirmed_cases` equals the sum
   of student + staff + unspecified + unidentified component fields
6. Top 20 school boards by record count (helps spot canonicalization duplicates)
7. Top 20 municipalities by record count

Exits with code 0 on PASS, 1 on FAIL.

**Run with:**
```bash
node scripts/validate_combined_data.js
```

---

### `clean_school_closures.js`

**Purpose:** Cleans the raw school closures Excel/CSV data and outputs
`public/data/school_closures.json`.

---

### `validate_school_closures.js`

**Purpose:** Validates the `school_closures.json` file for data quality.

---

### `check_combined_school_data.js`

**Purpose:** Additional ad-hoc checks on the combined school data file.

---

## Recommended Run Order

When rebuilding the data pipeline from scratch:

```
1. node scripts/clean_school_data.js
2. node scripts/validate_combined_data.js    # verify the combined JSON
3. node scripts/clean_school_closures.js
4. node scripts/validate_school_closures.js  # verify closures JSON
5. node scripts/generate_filter_options.js
```

Steps 2 and 4 are validation-only and do not modify any files. If either
exits with code 1, fix the issues and re-run the corresponding cleaning script
before proceeding.

---

## Expected Output Sizes

| File | Approx. records |
|---|---|
| `covid19_schools_active_with_demographics_combined.json` | ~120,000+ |
| `school_closures.json` | varies |
