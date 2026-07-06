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
