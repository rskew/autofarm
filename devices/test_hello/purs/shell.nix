# nixpkgs from 2022-07-20
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/d57f20b903c801b9325aea6279a4d61d19368fb0.tar.gz") {}
}:

pkgs.mkShell {
  name = "autofarm-test-device";
  buildInputs = [
    pkgs.spago
    pkgs.purescript
    pkgs.nodejs
    pkgs.esptool
  ];
}
