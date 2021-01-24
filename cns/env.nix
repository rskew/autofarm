let
  # same is irrigation_controller
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/f1079ad898e6defc6cb354194ce2fd90aa166fdf.tar.gz") {};
in pkgs.stdenv.mkDerivation {
  name = "Management environment for the farm central-nervous-system";

  buildInputs = [
    pkgs.nixops
  ];

  shellHook = ''
  '';
}
