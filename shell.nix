{ pkgs ? import <nixos> {} }:
pkgs.mkShell {
  name = "farm-control";

  buildInputs = [
    (import farm-control/default.nix { inherit pkgs; }).farm-control
  ];
}
