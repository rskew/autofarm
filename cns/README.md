# Steps to set up farm server
- Install NixOS on server machine and configure SSH access
- Give machine a fixed address (e.g. modify router config to give server a static IP on the local network via DHCP reservation).
  Add this address to the global config so that nodes can find the MQTT broker running on the server.
  TODO: currently hard-coded
- Deploy machine config in `farm-cns.nix` to the machine using nixops
  - `nixops create farm-cns.nix -d farm-cns; nixops deploy -d farm-cns`

## Stuff you'll want to update in the config
- dynamic dns configuration in `farm-cns.nix`
  - uses `freedns.afraid.org` with credentials in git-ignored `cns/dynamic-dns-url.txt`
