# nixpkgs from 2022-07-20
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d57f20b903c801b9325aea6279a4d61d19368fb0.tar.gz") {}
}:

let
  spago2nix = import (builtins.fetchGit {
    url = "https://github.com/justinwoo/spago2nix.git";
    rev = "1c834738a8216a4c89d9defac9bf1c331d302a6a";
  }) { inherit pkgs; };
in

pkgs.mkShell {
  name = "autofarm-frontend";
  buildInputs = [
    pkgs.spago
    pkgs.purescript
    pkgs.nodejs
    pkgs.nodePackages.node2nix
    spago2nix
  ];
}
