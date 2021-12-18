{ pkgs ? import <nixos> {} }:
pkgs.stdenv.mkDerivation {
  name = "farm-control";

  buildInputs = [
    (import farm-control/default.nix { inherit pkgs; }).farm-control
  ];
}
