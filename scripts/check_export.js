// scripts/check_export.js
// Runs after `next build` (postbuild). Fails the build if the static export in out/
// contains anything that must never be published: the source closures spreadsheet,
// the corrections log, raw data formats, or env/secret files. Everything in out/
// is served publicly by GitHub Pages.
//
// Usage: node scripts/check_export.js [dir]   (default: out)

const fs = require('fs');
const path = require('path');

const OUT_DIR = path.resolve(__dirname, '..', process.argv[2] || 'out');

const FORBIDDEN = [
  { test: (f) => /\.(xlsx|xls|xlsm|numbers|rds|rdata)$/i.test(f), why: 'source spreadsheet / raw data file' },
  { test: (f) => path.basename(f) === 'closure_corrections_log.json', why: 'closures corrections log' },
  { test: (f) => /^\.env/.test(path.basename(f)), why: 'environment file' },
  { test: (f) => /\.(pem|key)$/i.test(f), why: 'key file' },
];

function walk(dir) {
  return fs.readdirSync(dir, { withFileTypes: true }).flatMap((e) => {
    const p = path.join(dir, e.name);
    return e.isDirectory() ? walk(p) : [p];
  });
}

function main() {
  if (!fs.existsSync(OUT_DIR)) {
    console.error(`check_export: ${OUT_DIR} does not exist — run next build first`);
    process.exit(1);
  }
  const files = walk(OUT_DIR);
  const problems = [];
  for (const file of files) {
    const rule = FORBIDDEN.find((r) => r.test(file));
    if (rule) problems.push(`${path.relative(OUT_DIR, file)} (${rule.why})`);
  }
  if (problems.length) {
    console.error('check_export: FAIL — these files must not be published:');
    for (const p of problems) console.error(`  - ${p}`);
    console.error('Private data belongs in private-data/, not public/.');
    process.exit(1);
  }
  console.log(`check_export: PASS — ${files.length} files in ${path.relative(process.cwd(), OUT_DIR) || OUT_DIR}, nothing private`);
}

main();
