let
  # nixpkgs version with mpfshell
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f1079ad898e6defc6cb354194ce2fd90aa166fdf.tar.gz") {};
in pkgs.stdenv.mkDerivation {
  name = "irrigation-controller-build-env";

  buildInputs = [
    pkgs.mpfshell
    pkgs.esptool
  ];

  shellHook = ''
  '';
}
