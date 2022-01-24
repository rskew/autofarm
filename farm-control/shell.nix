{ }:
let
  # TODO reference repo-wide nixpkgs version
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};
  project = import ./default.nix {};
in
pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs;
}
