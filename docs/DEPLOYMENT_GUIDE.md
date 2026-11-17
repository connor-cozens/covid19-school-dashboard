# Deployment Guide

The dashboard is a **fully static site**: `next build` exports plain HTML, JavaScript and data
files to `out/`, and GitHub Actions publishes them to **GitHub Pages**. There is no server,
container or cloud project to maintain.

## How a deploy works

[`.github/workflows/deploy.yml`](../.github/workflows/deploy.yml) runs on every push to `master`
(i.e. every merged PR) and can be run manually from **Actions → Deploy → Run workflow**.

1. Checks out the private data repo
   [`connor-cozens/covid19-school-dashboard-data`](https://github.com/connor-cozens/covid19-school-dashboard-data)
   using the `DATA_REPO_TOKEN` secret.
2. Copies the closures spreadsheet into `private-data/` and generates
   `public/data/school_closures.json` with `scripts/clean_school_closures.js`.
3. `npm run build`:
   - **prebuild**: regenerates filter options and runs the data regression check
   - **build**: static export to `out/`
   - **postbuild**: `scripts/check_export.js` fails the build if `out/` contains the spreadsheet,
     the corrections log, raw data files or env/key files
4. Runs the Playwright browser tests against `out/` with the real data.
5. Publishes `out/` to GitHub Pages. If any earlier step fails, the live site is left unchanged.

## Data privacy

| Data | Where it lives | Published? |
|---|---|---|
| Closures source spreadsheet | Private data repo; locally in `private-data/` (gitignored) | **Never** |
| `closure_corrections_log.json` | `private-data/` (gitignored) | **Never** |
| `school_closures.json` | Generated at build time into `public/data/` (gitignored) | Yes, served by the site |
| All other data | Committed in `public/data/` | Yes |

Everything in `public/` is published, so private files must never be placed there.
`check_export.js` enforces this on every build.

## Updating the closures data

1. Replace `closures/COVID School Closures_V2.xlsx` in the private data repo (keep the filename).
2. Run **Actions → Deploy → Run workflow** in this repo.

## One-time setup

### 1. Data repo token

The deploy needs read access to the private data repo:

1. GitHub → **Settings → Developer settings → Personal access tokens → Fine-grained tokens →
   Generate new token**
   - **Repository access**: Only select repositories → `covid19-school-dashboard-data`
   - **Permissions**: Repository permissions → **Contents: Read-only** (nothing else)
   - **Expiration**: up to 1 year. Set a reminder to rotate it.
2. In this repo: **Settings → Secrets and variables → Actions → New repository secret**,
   name `DATA_REPO_TOKEN`, value = the token.

When the token expires, deploys fail at "Check data repo token is configured" or
"Fetch private closures data". Generate a new token and update the secret.

### 2. GitHub Pages

**Settings → Pages → Build and deployment → Source: GitHub Actions.**

### 3. Custom domain

1. **Settings → Pages → Custom domain**: `covid19schooldashboard.com`, then save.
2. At the DNS provider, remove the old Google (Cloud Run) records and add:

   | Type | Name | Value |
   |---|---|---|
   | A | `@` | `185.199.108.153` |
   | A | `@` | `185.199.109.153` |
   | A | `@` | `185.199.110.153` |
   | A | `@` | `185.199.111.153` |
   | AAAA | `@` | `2606:50c0:8000::153` |
   | AAAA | `@` | `2606:50c0:8001::153` |
   | AAAA | `@` | `2606:50c0:8002::153` |
   | AAAA | `@` | `2606:50c0:8003::153` |
   | CNAME | `www` | `connor-cozens.github.io` |

3. Once the DNS check passes on the Pages settings page, tick **Enforce HTTPS**.
4. Recommended: verify the domain under **GitHub profile Settings → Pages → Verified domains**
   to prevent anyone else's repo from claiming it.

## Testing a production build locally

```bash
node scripts/clean_school_closures.js   # needs the spreadsheet in private-data/
npm run build                           # static export to out/ (+ data and privacy checks)
npm start                               # serves out/ at http://localhost:3000
npm run test:e2e                        # browser tests against out/
```

## Rolling back

Revert the offending commit on `master` (via a PR). The Deploy workflow then publishes the
previous version. To redeploy without code changes, re-run the latest successful Deploy run.

## Troubleshooting

| Problem | Likely cause | Fix |
|---|---|---|
| Deploy fails at "Check data repo token" | `DATA_REPO_TOKEN` secret missing | See *Data repo token* above |
| Deploy fails at "Fetch private closures data" | Token expired or lacks access to the data repo | Generate a new token, update the secret |
| `check_export: FAIL` | A private file ended up in `public/` | Move it to `private-data/` |
| Data regression check fails | Committed case data changed unexpectedly | Run `npm run check-regression` locally; if the change is intended, `npm run update-baseline` |
| Site shows "Failed to load dashboard data" | A data file is missing from `public/data/` | Check the build log; run the browser tests locally |
| Custom domain shows a 404 or certificate error | DNS not yet propagated, or HTTPS not yet issued | Wait (up to 24h) and re-check the Pages settings page |
