import { defineConfig, devices } from '@playwright/test';

// Browser tests run against a built copy of the site.
// - With no BASE_URL, Playwright serves the static export in out/ (`npm start`).
//   Run `npm run build` first; real or sample closures data must be in public/data.
// - Set BASE_URL to test an already-running site instead.
const baseURL = process.env.BASE_URL ?? 'http://localhost:3000';

export default defineConfig({
  testDir: './e2e',
  timeout: 60_000,
  expect: { timeout: 20_000 },
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  reporter: process.env.CI ? [['github'], ['html', { open: 'never' }]] : 'list',
  use: {
    baseURL,
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
  },
  projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
  webServer: process.env.BASE_URL
    ? undefined
    : { command: 'npm start', url: baseURL, reuseExistingServer: true, timeout: 60_000 },
});
