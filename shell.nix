{ pkgs ? import <nixos> {} }:
pkgs.mkShell {
  name = "farm-shell";

  buildInputs = [
    (import farm-control/default.nix { inherit pkgs; }).farm-control
    (import farm-logger/default.nix { inherit pkgs; })
    (import farm-monitor/default.nix { inherit pkgs; })
    pkgs.mosquitto
  ];
}
