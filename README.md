# COVID-19 Ontario School Dashboard

An interactive dashboard for visualizing COVID-19 cases and school closures across Ontario schools during the 2020–2022 pandemic. This is a React/Next.js port of the original R Shiny application.

## Features

- **Interactive Map**: Per-school case markers and closure markers with full demographic overlays
- **Time-based Filtering**: Explore the data by date range, school board, and municipality
- **Summary Analytics**: Key statistics and charts showing case trends over time
- **Data Tables**: Sortable and searchable tables for cases and closures
- **Content Pages**: About, Team, Media, and Data Sources

## Tech Stack

- **Framework**: Next.js 16 with App Router, exported as a static site
- **Hosting**: GitHub Pages
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **State Management**: Zustand
- **Maps**: React-Leaflet
- **Charts**: Recharts

## Project Structure

```
src/
├── app/                 # Next.js app router pages
├── components/          # React components
│   ├── layout/         # Header and Layout wrapper
│   ├── map/            # Map components
│   ├── summary/        # Analytics and summary
│   ├── about/          # About page
│   ├── team/           # Team page
│   ├── media/          # Media page
│   ├── ui/             # Reusable UI components
│   ├── charts/         # Chart components
│   └── tables/         # Table components
├── lib/                # Utility functions and data loading
├── store/              # Zustand state management
├── types/              # TypeScript type definitions
└── hooks/              # Custom React hooks
```

## Getting Started

1. **Install dependencies**:
   ```bash
   npm install
   ```

2. **Add school closures data** (not included in the repository — see note below). Copy the source spreadsheet from the private [data repo](https://github.com/connor-cozens/covid19-school-dashboard-data) to:
   ```
   private-data/COVID School Closures_V2.xlsx
   ```
   Then regenerate the processed closure files:
   ```bash
   node scripts/clean_school_closures.js
   ```

3. **Run the development server**:
   ```bash
   npm run dev
   ```

4. **Open your browser** and navigate to `http://localhost:3000`

> **Note on school closures data**: the source spreadsheet is private and must never be committed or placed in `public/` (everything in `public/` is published). It lives in the private data repo and, locally, in the gitignored `private-data/` folder. `school_closures.json` is generated from it and is gitignored. All other data files are included. To work without it, use the sample data (see Testing).

## Testing

- **Unit tests**: `npm test`
- **Browser tests** (Playwright): `npm run build`, then `npm run test:e2e` — serves the static export in `out/` and checks every view renders with data. Run `npx playwright install chromium` once first. To test an already-running site instead, set `BASE_URL`.
- **Without the real closures data**: `npm run ci:sample-data` copies synthetic data from `tests/fixtures/school_closures.sample.json` into `public/data/` (this overwrites a real `school_closures.json` — regenerate it afterwards with `node scripts/clean_school_closures.js`). Regenerate the sample with `node scripts/generate_sample_closures.js`.

### CI

Every PR to `master` runs [`.github/workflows/ci.yml`](.github/workflows/ci.yml):

| Check | What it does |
|---|---|
| Unit tests | `npm ci`, Jest (including checks on the committed data files), and lint |
| Security audit | `npm audit` on production dependencies and GitHub dependency review — fails on high/critical |
| Build & browser tests | Builds the static site with sample closures data (running the private-file check), then runs the Playwright tests against it |

Dependabot ([`.github/dependabot.yml`](.github/dependabot.yml)) opens weekly grouped PRs for npm and GitHub Actions; security fixes arrive in their own group.

## Deployment

Merging to `master` deploys automatically: [`.github/workflows/deploy.yml`](.github/workflows/deploy.yml) builds the static site with the real closures data, runs the browser tests, and publishes `out/` to GitHub Pages.

- **Build locally**: `npm run build` (static export to `out/`), then `npm start` to serve it
- **Update closures data**: replace the spreadsheet in the private data repo, then run **Actions → Deploy → Run workflow**

See [docs/DEPLOYMENT_GUIDE.md](docs/DEPLOYMENT_GUIDE.md) for setup (data repo token, Pages, custom domain) and troubleshooting.

## Data

The app fetches four JSON files from `public/data/` (everything there is published):

| File | Contents | Built by |
|---|---|---|
| `cases.json` | Per-school daily case records, in a compact format | `npm run build-data` (committed) |
| `school_profiles.json` | School details and demographics for map popups | `npm run build-data` (committed) |
| `summaries.json` | Province-wide daily case summaries | `npm run build-data` (committed) |
| `school_closures.json` | School closures | `scripts/clean_school_closures.js` from the private spreadsheet (not committed) |

The case data is frozen (it ends 2021-12-22), so `npm run build-data` only needs to run if a raw input in `data/raw/` or the cleaning logic in `scripts/lib/clean-cases.js` changes. `src/__tests__/data-files.test.ts` checks the committed files' record counts and totals. See [scripts/README.md](scripts/README.md).

## Data Sources

- School COVID-19 case records (Ontario government, 2020–2022)
- School closure data (Ontario Ministry of Education)
- School demographic data (Ontario Ministry of Education)
- Geographic coordinates for Ontario schools

## License

This project maintains the same license as the original R Shiny application.
