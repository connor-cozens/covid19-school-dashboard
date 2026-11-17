# COVID-19 Ontario School Dashboard

An interactive dashboard for visualizing COVID-19 cases and school closures across Ontario schools during the 2020–2022 pandemic. This is a React/Next.js port of the original R Shiny application.

## Features

- **Interactive Map**: Per-school case markers and closure markers with full demographic overlays
- **Time-based Filtering**: Explore the data by date range, school board, and municipality
- **Summary Analytics**: Key statistics and charts showing case trends over time
- **Data Tables**: Sortable and searchable tables for cases and closures
- **Content Pages**: About, Team, Media, and Data Sources

## Tech Stack

- **Framework**: Next.js 15 with App Router
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

2. **Add school closures data** (not included in the repository — see note below):
   ```
   public/data/COVID School Closures_V2.xlsx
   ```
   Then regenerate the processed closure files:
   ```bash
   node scripts/clean_school_closures.js
   ```

3. **Generate filter options**:
   ```bash
   npm run setup
   ```

4. **Run the development server**:
   ```bash
   npm run dev
   ```

5. **Open your browser** and navigate to `http://localhost:3000`

> **Note on school closures data**: `school_closures.json` and the source Excel file are excluded from this repository. They must be added manually before running the app. All other data files are included.

## Testing

- **Unit tests**: `npm test`
- **Browser tests** (Playwright): `npm run build`, then `npm run test:e2e` — starts the built app and checks every view renders with data. Run `npx playwright install chromium` once first. To test a running server or container instead, set `BASE_URL` (e.g. `BASE_URL=http://localhost:3000 npm run test:e2e`).
- **Without the real closures data**: `npm run ci:sample-data` copies synthetic data from `tests/fixtures/school_closures.sample.json` into `public/data/` (this overwrites a real `school_closures.json` — regenerate it afterwards with `node scripts/clean_school_closures.js`). Regenerate the sample with `node scripts/generate_sample_closures.js`.

### CI

Every PR to `master` runs [`.github/workflows/ci.yml`](.github/workflows/ci.yml):

| Check | What it does |
|---|---|
| Unit tests | `npm ci`, Jest, and lint (report only) |
| Security audit | `npm audit` on production dependencies and GitHub dependency review — fails on high/critical |
| Docker build & browser tests | Builds the production Docker image with sample closures data (running the data regression checks), smoke-tests the page and data files, then runs the Playwright tests against the container |

Dependabot ([`.github/dependabot.yml`](.github/dependabot.yml)) opens weekly grouped PRs for npm, the Docker base image and GitHub Actions; security fixes arrive in their own group.

## Deployment

Deploy to Google Cloud Run:
```bash
PROJECT_ID=your-project-id ./scripts/setup/deploy.sh
```

See [docs/DEPLOYMENT_GUIDE.md](docs/DEPLOYMENT_GUIDE.md) for full deployment instructions.

## Data Sources

- School COVID-19 case records (Ontario government, 2020–2022)
- School closure data (Ontario Ministry of Education)
- School demographic data (Ontario Ministry of Education)
- Geographic coordinates for Ontario schools

## License

This project maintains the same license as the original R Shiny application.
