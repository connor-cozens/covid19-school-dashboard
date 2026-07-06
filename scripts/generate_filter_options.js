const fs = require('fs');
const path = require('path');

const CASES_PATH = path.join(__dirname, '../public/data/covid19_schools_active_with_demographics_combined.json');
const CLOSURES_PATH = path.join(__dirname, '../public/data/school_closures.json');
const OUTPUT_BOARDS = path.join(__dirname, '../public/data/school_boards.json');
const OUTPUT_MUNICIPALITIES = path.join(__dirname, '../public/data/municipalities.json');

function extractUnique(values) {
  return Array.from(new Set(values.filter(Boolean))).sort((a, b) => a.localeCompare(b));
}

function getField(obj, ...fields) {
  for (const field of fields) {
    if (obj[field]) return obj[field];
  }
  return undefined;
}

function main() {
  // Read and parse data files
  const cases = JSON.parse(fs.readFileSync(CASES_PATH, 'utf8'));
  const closures = JSON.parse(fs.readFileSync(CLOSURES_PATH, 'utf8'));

  // Extract school boards
  const boardsFromCases = cases.map(c => getField(c, 'school_board', 'board_name'));
  const boardsFromClosures = closures.map(c => getField(c, 'school_board', 'board_name'));
  const allBoards = extractUnique([...boardsFromCases, ...boardsFromClosures]);

  // Extract municipalities/cities.
  // Cases: prefer `municipality` (the Ontario gov administrative field) since that's
  // what the store filter checks (case_.municipality === selectedMunicipality).
  // Closures: use `city` (from demographics lookup — closures use closure.city).
  const citiesFromCases = cases.map(c => getField(c, 'municipality', 'city'));
  const citiesFromClosures = closures.map(c => getField(c, 'city', 'municipality'));
  const allCities = extractUnique([...citiesFromCases, ...citiesFromClosures]);

  // Write output files
  fs.writeFileSync(OUTPUT_BOARDS, JSON.stringify(allBoards, null, 2));
  fs.writeFileSync(OUTPUT_MUNICIPALITIES, JSON.stringify(allCities, null, 2));

  console.log(`Generated ${OUTPUT_BOARDS} (${allBoards.length} boards)`);
  console.log(`Generated ${OUTPUT_MUNICIPALITIES} (${allCities.length} municipalities)`);
}

main();