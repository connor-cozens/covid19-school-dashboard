// scripts/validate_case_totals.js
//
// Validates total case counts by comparing:
//   1. The authoritative cumulative totals from schoolcovidsummary CSVs
//   2. What the combined JSON's total_confirmed_cases field actually represents
//   3. The current dashboard KPI calculation (sum of all total_confirmed_cases)
//
// Run with: node scripts/validate_case_totals.js
//
// Background:
//   The per-school CSVs record "currently active cases" per school per day
//   (a daily snapshot), NOT cumulative confirmed cases. Summing total_confirmed_cases
//   across all rows overcounts dramatically. The authoritative cumulative totals
//   are in schoolcovidsummary.csv and schoolcovidsummary2021_2022.csv.

const fs = require('fs');
const path = require('path');

const DATA_DIR = path.join(__dirname, '../public/data');

// ── Helpers ────────────────────────────────────────────────────────────────────

function parseCSV(filePath) {
  const raw = fs.readFileSync(filePath, 'latin1'); // Ontario CSVs have latin1 encoding
  const lines = raw.split('\n').filter(l => l.trim());
  const headers = lines[0].split(',').map(h => h.replace(/"/g, '').trim());
  return lines.slice(1).map(line => {
    // Basic CSV parse — handles quoted fields
    const vals = [];
    let inQuote = false, curr = '';
    for (const ch of line) {
      if (ch === '"') { inQuote = !inQuote; }
      else if (ch === ',' && !inQuote) { vals.push(curr.trim()); curr = ''; }
      else { curr += ch; }
    }
    vals.push(curr.trim());
    const row = {};
    headers.forEach((h, i) => row[h] = vals[i] ?? '');
    return row;
  });
}

function num(val) {
  const n = parseFloat(String(val).replace(/[^0-9.-]/g, ''));
  return isNaN(n) ? 0 : n;
}

function fmt(n) { return n.toLocaleString(); }
function pct(a, b) { return b === 0 ? '—' : ((a / b) * 100).toFixed(2) + '%'; }

function section(title) {
  console.log('\n' + '═'.repeat(70));
  console.log(`  ${title}`);
  console.log('═'.repeat(70));
}

function pass(msg) { console.log(`  ✓  ${msg}`); }
function warn(msg) { console.log(`  !  ${msg}`); }
function fail(msg) { console.log(`  ✗  ${msg}`); }
function info(msg) { console.log(`     ${msg}`); }

// ── Load data ──────────────────────────────────────────────────────────────────

console.log('\nLoading data files...');

const summary2021 = parseCSV(path.join(DATA_DIR, 'schoolcovidsummary.csv'));
const summary2122 = parseCSV(path.join(DATA_DIR, 'schoolcovidsummary2021_2022.csv'));
const combined    = JSON.parse(fs.readFileSync(path.join(DATA_DIR, 'covid19_schools_active_with_demographics_combined.json'), 'utf8'));

console.log(`  schoolcovidsummary.csv              — ${fmt(summary2021.length)} rows`);
console.log(`  schoolcovidsummary2021_2022.csv     — ${fmt(summary2122.length)} rows`);
console.log(`  combined JSON                       — ${fmt(combined.length)} records`);

// ── Section 1: Authoritative cumulative totals (summary CSVs) ─────────────────

section('1. Authoritative cumulative totals (schoolcovidsummary CSVs)');
info('These CSVs report the province-wide cumulative total each day.');
info('The final row of each file gives the true cumulative total for that school year.');
console.log();

const last2021 = summary2021[summary2021.length - 1];
const last2122 = summary2122[summary2122.length - 1];

const cumulative2021 = num(last2021.cumulative_school_related_cases);
const cumulative2122 = num(last2122.cumulative_school_related_cases);
const cumulativeStudent2021 = num(last2021.cumulative_school_related_student_cases);
const cumulativeStaff2021   = num(last2021.cumulative_school_related_staff_cases);
const cumulativeStudent2122 = num(last2122.cumulative_school_related_student_cases);
const cumulativeStaff2122   = num(last2122.cumulative_school_related_staff_cases);

console.log('  2020-2021 school year (schoolcovidsummary.csv):');
info(`Date range:  ${summary2021[0].collected_date}  →  ${last2021.collected_date}`);
info(`Cumulative total cases:    ${fmt(cumulative2021)}`);
info(`  ↳ Student cases:         ${fmt(cumulativeStudent2021)}`);
info(`  ↳ Staff cases:           ${fmt(cumulativeStaff2021)}`);
console.log();

console.log('  2021-2022 school year (schoolcovidsummary2021_2022.csv):');
info(`Date range:  ${summary2122[0].collected_date}  →  ${last2122.collected_date}`);
info(`Cumulative total cases:    ${fmt(cumulative2122)}`);
info(`  ↳ Student cases:         ${fmt(cumulativeStudent2122)}`);
info(`  ↳ Staff cases:           ${fmt(cumulativeStaff2122)}`);
console.log();

const authoritativeTotal = cumulative2021 + cumulative2122;
console.log(`  ► AUTHORITATIVE GRAND TOTAL (both years): ${fmt(authoritativeTotal)}`);

// ── Section 2: What the combined JSON's total_confirmed_cases actually is ──────

section('2. What total_confirmed_cases means in the combined JSON');
info('Examining a single school across multiple dates to determine if the');
info('field is a daily active-case snapshot or a cumulative count.');
console.log();

// Find a real school with at least 10 records and changing values
const schoolGroups = {};
combined.forEach(r => {
  if (r.school_number > 0) {
    if (!schoolGroups[r.school_number]) schoolGroups[r.school_number] = [];
    schoolGroups[r.school_number].push(r);
  }
});

// Pick a school with varied values to make the point clear
const exampleSchool = Object.values(schoolGroups)
  .filter(rows => rows.length >= 20)
  .sort((a, b) => {
    const aVals = new Set(a.map(r => r.total_confirmed_cases)).size;
    const bVals = new Set(b.map(r => r.total_confirmed_cases)).size;
    return bVals - aVals;
  })[0];

if (exampleSchool) {
  const sorted = exampleSchool.sort((a, b) => a.collected_date.localeCompare(b.collected_date));
  const sample = sorted.slice(0, 12);
  console.log(`  Example: ${sample[0].school_name} (school_number: ${sample[0].school_number})`);
  info(`  ${sorted.length} records across ${new Set(sorted.map(r => r.collected_date)).size} dates`);
  console.log();
  console.log('  date           total_confirmed_cases   student   staff');
  console.log('  ' + '─'.repeat(56));
  sample.forEach(r => {
    const date  = r.collected_date.padEnd(14);
    const total = String(r.total_confirmed_cases).padStart(3);
    const stu   = String(r.confirmed_student_cases || 0).padStart(7);
    const sta   = String(r.confirmed_staff_cases || 0).padStart(7);
    console.log(`  ${date}  ${total}                  ${stu}  ${sta}`);
  });
  console.log();

  const maxVal = Math.max(...sorted.map(r => r.total_confirmed_cases));
  const sumVal = sorted.reduce((s, r) => s + r.total_confirmed_cases, 0);
  info(`  MAX across all dates for this school: ${maxVal}`);
  info(`  SUM across all dates for this school: ${sumVal}`);
  console.log();

  // Determine if values can decrease (only possible if these are active, not cumulative)
  let decreases = 0;
  for (let i = 1; i < sorted.length; i++) {
    if (sorted[i].total_confirmed_cases < sorted[i-1].total_confirmed_cases) decreases++;
  }
  if (decreases > 0) {
    warn(`Values DECREASE ${decreases} time(s) for this school — confirms these are`);
    warn('currently-ACTIVE cases (daily snapshot), NOT cumulative confirmed cases.');
  } else {
    info('Values do not decrease for this sample school (check another).');
  }
}

// Global decrease check across all schools
let globalDecreases = 0;
Object.values(schoolGroups).forEach(rows => {
  const sorted = rows.sort((a, b) => a.collected_date.localeCompare(b.collected_date));
  for (let i = 1; i < sorted.length; i++) {
    if (sorted[i].total_confirmed_cases < sorted[i-1].total_confirmed_cases) globalDecreases++;
  }
});
console.log();
info(`Across ALL schools: ${fmt(globalDecreases)} instances where total_confirmed_cases`);
info('DECREASES from one date to the next — conclusive proof these are active');
info('case snapshots, not cumulative counts.');

// ── Section 3: Current dashboard KPI vs authoritative total ───────────────────

section('3. Current dashboard KPI vs authoritative total');
info('The dashboard currently sums total_confirmed_cases across all filteredCases rows.');
info('This sums active-case snapshots over time, not unique confirmed cases.');
console.log();

const dashboardKpiTotal = combined.reduce((s, r) => s + (r.total_confirmed_cases || 0), 0);
const combinedDates = combined.map(r => r.collected_date).filter(Boolean).sort();
const combinedStart = combinedDates[0];
const combinedEnd   = combinedDates[combinedDates.length - 1];

console.log(`  Dashboard KPI "Total Cases" (current):   ${fmt(dashboardKpiTotal)}`);
console.log(`  Authoritative cumulative total:          ${fmt(authoritativeTotal)}`);
console.log(`  Overcount factor:                        ${(dashboardKpiTotal / authoritativeTotal).toFixed(1)}×`);
console.log(`  Overcount amount:                        ${fmt(dashboardKpiTotal - authoritativeTotal)}`);
console.log();
fail(`The dashboard is OVERCOUNTING by ${(dashboardKpiTotal / authoritativeTotal).toFixed(1)}×.`);
info(`Summing active-case snapshots (${fmt(combined.length)} rows × avg active cases/day)`);
info('is not the same as summing unique confirmed cases.');

// ── Section 4: Per-school breakdown ───────────────────────────────────────────

section('4. How total_confirmed_cases should be used');
info('For MAP RENDERING: use total_confirmed_cases as-is — it correctly shows how');
info('many active cases exist at a school on a given day. ✓');
console.log();
info('For the "Total Cases" KPI: use cumulative_school_related_cases from the');
info('summary CSV for the selected date range. The final row of each summary');
info('CSV gives the authoritative end-of-year total. ✗ (currently wrong)');
console.log();

// Show what the summary CSV looks like for date-range filtering
info('For a date range ending on D, read cumulative_school_related_cases from');
info('the summary row where collected_date = D (or the nearest available date).');
info('This is exactly how the R app computed the KPI.');

// ── Section 5: Date coverage ───────────────────────────────────────────────────

section('5. Dataset coverage summary');

const records2021 = combined.filter(r => r.collected_date >= '2020-09-01' && r.collected_date < '2021-09-01');
const records2122 = combined.filter(r => r.collected_date >= '2021-09-01');
const schools2021 = new Set(records2021.map(r => r.school_number).filter(Boolean)).size;
const schools2122 = new Set(records2122.map(r => r.school_number).filter(Boolean)).size;

console.log('  Source                                Records    Schools    Dates');
console.log('  ' + '─'.repeat(64));
console.log(`  20-21 (combined JSON subset)          ${fmt(records2021.length).padStart(8)}   ${fmt(schools2021).padStart(7)}    ${records2021.map(r=>r.collected_date).sort()[0]} → ${records2021.map(r=>r.collected_date).sort().at(-1)}`);
console.log(`  21-22 (combined JSON subset)          ${fmt(records2122.length).padStart(8)}   ${fmt(schools2122).padStart(7)}    ${records2122.map(r=>r.collected_date).sort()[0]} → ${records2122.map(r=>r.collected_date).sort().at(-1)}`);
console.log(`  TOTAL combined JSON                   ${fmt(combined.length).padStart(8)}`);
console.log();
info('Note: the file named _19_20.csv contains 2021-22 data;');
info('      the file named _20_21.csv contains 2020-21 data. Names are swapped.');
info('      There is no 2019-20 data in the dataset.');

// ── Summary ────────────────────────────────────────────────────────────────────

section('Summary & recommended fix');

console.log(`  Authoritative cumulative total (both years): ${fmt(authoritativeTotal)}`);
console.log(`    2020-21: ${fmt(cumulative2021)}  (${cumulativeStudent2021.toLocaleString()} students, ${cumulativeStaff2021.toLocaleString()} staff)`);
console.log(`    2021-22: ${fmt(cumulative2122)}  (${cumulativeStudent2122.toLocaleString()} students, ${cumulativeStaff2122.toLocaleString()} staff)`);
console.log();
console.log(`  Current dashboard KPI "Total Cases": ${fmt(dashboardKpiTotal)}  ← WRONG`);
console.log();
console.log('  Fix: replace the dashboard total-cases KPI calculation with a lookup');
console.log('  into schoolSummaries (already loaded) using cumulative_school_related_cases');
console.log('  for the row matching the selected end date.');
console.log('  This is tracked as a data accuracy issue to address in the store.');
console.log();
