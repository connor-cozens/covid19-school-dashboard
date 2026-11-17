import { defineConfig, devices } from '@playwright/test';

// Browser tests run against a running build of the site.
// - CI sets BASE_URL to the Docker container started from the production image.
// - Locally, with no BASE_URL, Playwright starts `next start` itself
//   (run `npm run build` first; real or sample closures data must be in public/data).
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
    : { command: 'npx next start -p 3000', url: baseURL, reuseExistingServer: true, timeout: 120_000 },
});
