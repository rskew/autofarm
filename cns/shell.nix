let
  # TODO centrally pin nixpkgs repo-wide
  # Latest master on 2021-03-02
  #pkgs-src = fetchTarball "https://github.com/NixOS/nixpkgs/archive/80308388cd77ee58823c9b6f24b46892cd359145.tar.gz";
  #pkgs = import pkgs-src {};
  # Use local version of nixpkgs pending PR: https://github.com/NixOS/nixpkgs/pull/115381
  pkgs = import ../../nixpkgs {};

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
