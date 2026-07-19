import { defineConfig, devices } from "@playwright/test";

// The backend serves both the frontend bundle and devices.json, so the tests
// run against the real thing. It retries the serial connection in the
// background, so it comes up fine without the LoRa gateway plugged in.
export default defineConfig({
  testDir: "./tests",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  reporter: "list",
  use: {
    baseURL: "http://localhost:8006",
    trace: "on-first-retry",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
  webServer: {
    command: "nix develop .#gleam-backend --command gleam run -- ttyUSB0",
    cwd: "..",
    url: "http://localhost:8006/devices.json",
    reuseExistingServer: true,
    // First run compiles the backend from scratch.
    timeout: 300_000,
  },
});
