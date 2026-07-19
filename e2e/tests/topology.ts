import { APIRequestContext, Page } from "@playwright/test";

export type Topology = {
  devices: {
    node: number;
    name: string;
    solenoids: { solenoid: number; zone: string }[];
  }[];
};

export type FlatZone = {
  node: number;
  solenoid: number;
  zone: string;
};

/// Read the topology the app itself is served, so these tests follow
/// devices.json rather than duplicating it.
export async function fetchZones(request: APIRequestContext): Promise<FlatZone[]> {
  const response = await request.get("/devices.json");
  const topology: Topology = await response.json();
  return topology.devices.flatMap((device) =>
    device.solenoids.map((s) => ({
      node: device.node,
      solenoid: s.solenoid,
      zone: s.zone,
    })),
  );
}

/// Replace the browser WebSocket with a stub that records what the frontend
/// sends and lets a test push messages back, so we can assert on the wire
/// protocol without the backend or the LoRa gateway.
export async function stubWebSocket(page: Page): Promise<void> {
  await page.addInitScript(() => {
    (window as any).__sent = [];
    (window as any).WebSocket = class {
      url: string;
      onopen: ((e: unknown) => void) | null = null;
      onmessage: ((e: { data: string }) => void) | null = null;
      onclose: ((e: { code: number }) => void) | null = null;

      constructor(url: string) {
        this.url = url;
        (window as any).__ws = this;
        // lustre_websocket assigns the handlers after construction.
        setTimeout(() => this.onopen && this.onopen({}), 0);
      }

      send(message: string) {
        (window as any).__sent.push(message);
      }

      close() {}
    };
  });
}

export function sentMessages(page: Page): Promise<string[]> {
  return page.evaluate(() => (window as any).__sent as string[]);
}

/// Simulate a message arriving from a node, via the gateway and backend.
export function receiveMessage(page: Page, message: string): Promise<void> {
  return page.evaluate(
    (m) => (window as any).__ws.onmessage({ data: m }),
    message,
  );
}

export const zoneRow = (page: Page, zone: string) =>
  page.locator(`[data-zone="${zone}"]`);
