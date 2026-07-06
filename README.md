# COVID-19 School Dashboard

This is a modern React-based version of the COVID-19 School Dashboard, providing interactive visualization of COVID-19 cases and school closures across Ontario. This application has been migrated from the original R Shiny version to provide better performance, maintainability, and user experience.

## Features

- **Interactive Map**: View COVID-19 cases and school closures on an interactive map
- **Summary Analytics**: Key statistics and trends for COVID-19 in Ontario schools
- **Time-based Filtering**: Filter data by date to see historical trends
- **Demographic Data**: School demographic information and analysis
- **Responsive Design**: Works on desktop and mobile devices

## Tech Stack

- **Framework**: Next.js 14 with App Router
- **Language**: TypeScript
- **Styling**: Tailwind CSS
- **State Management**: Zustand
- **Maps**: React-Leaflet (planned)
- **Charts**: Recharts (planned)
- **Icons**: Heroicons

## Project Structure

```
src/
├── app/                 # Next.js app router pages
├── components/          # React components
│   ├── layout/         # Layout components (Header, Layout)
│   ├── map/            # Map-related components
│   ├── summary/        # Analytics and summary components
│   ├── about/          # About page components
│   ├── team/           # Team page components
│   ├── media/          # Media page components
│   ├── ui/             # Reusable UI components
│   ├── charts/         # Chart components (planned)
│   └── tables/         # Table components (planned)
├── lib/                # Utility functions and data loading
├── store/              # Zustand state management
├── types/              # TypeScript type definitions
└── hooks/              # Custom React hooks (planned)
```

## Getting Started

1. **Install dependencies**:
   ```bash
   npm install
   ```

2. **Generate filter options**:
   ```bash
   npm run setup
   ```

3. **Run the development server**:
   ```bash
   npm run dev
   ```

4. **Open your browser** and navigate to `http://localhost:3000`

## Deployment

### Quick Setup for Google Cloud

1. **Install Google Cloud CLI** (if not already installed):
   ```bash
   ./setup-gcloud.sh
   ```

2. **Deploy to Google Cloud Run**:
   ```bash
   PROJECT_ID=your-project-id ./scripts/setup/deploy.sh
   ```

### Manual Deployment

See the full deployment guide:
- [Deployment Guide](docs/DEPLOYMENT_GUIDE.md)

## Current Status

### ✅ **Fully Implemented**
- **Interactive Map**: Complete with case markers, closure markers, and demographics overlay
- **Summary Analytics**: Dynamic statistics and charts showing case trends
- **Data Tables**: Sortable and searchable tables for cases and closures
- **Filtering System**: Date range, school board, municipality, and data type filters
- **Content Pages**: About, Team, Media, and Data Sources pages fully implemented
- **Responsive Design**: Works on desktop and mobile devices

### 🚀 **Ready for Production**
The application has achieved feature parity with the original R Shiny version and is ready for deployment.

## Development TODOs & Feature Parity Plan

See [docs/DEVELOPMENT_PLAN.md](docs/DEVELOPMENT_PLAN.md) for the full development plan, feature parity checklist, and implementation notes.

- **P0 Priority:** All map functionality and user experience improvements are the current top focus.
- The development plan and checklist have been moved to a separate file for clarity and easier tracking.

---

## Data Sources

The application uses real data from Ontario's COVID-19 school reporting:
- School COVID-19 cases data (2020-2022)
- School closure information
- Demographic data from Ontario Ministry of Education
- Geographic coordinates for all schools

All data is processed and integrated into the application for interactive visualization.

## Contributing

This is a port of an existing R Shiny application. The goal is to create a more maintainable and modern web application while preserving all the original functionality.

## License

This project maintains the same license as the original R Shiny application.
