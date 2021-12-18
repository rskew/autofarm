let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};

  nixops-src = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixops";
    rev = "1239ff7fb94bd647ea54d18e7c7da4b81e63f422";
    sha256 = "1gfl9jasnds7frb9lkx4k0dkynkrlxriirqshwp79kbcjw3g6lnp";
  };
  nixops = (import "${nixops-src}/default.nix").default;

in pkgs.stdenv.mkDerivation {
  name = "farm-cns-deployment-environment";

  buildInputs = [
    nixops
    (import ../farm-control/default.nix { inherit pkgs; }).farm-control
  ];
}
