# Steps to set up farm server
- Install NixOS on server machine and configure SSH access
- Give machine a fixed address (e.g. modify router config to give server a static IP on the local network via DHCP reservation).
  Add this address to the global config so that nodes can find the MQTT broker running on the server.
  TODO: currently hard-coded
- SSH into the farm server and configure as the farm server adding cns/irrigation-control-config.nix to /etc/nixos/configuration.nix

## Commands to run manually on farm server
Paste these into the terminal of the farm server to make things happen

``` sh
farm-control start --farmVerb "Pump BorePump" --duration 60
farm-control start --farmVerb "Irrigate TopRow" --duration 1800
farm-control start --farmVerb "Irrigate PolyTunnel" --duration 1800
farm-control start --farmVerb "Irrigate Pumpkins" --duration 1800
```

## Viewing MQTT logs

``` sh
mosquitto_sub --host <farm-cns-host> --port 1883 -v -t '#'
```
