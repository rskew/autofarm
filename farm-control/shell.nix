{ }:
let
  # TODO reference repo-wide nixpkgs version
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};
  project = (import ./default.nix).farm-control;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs;
}
