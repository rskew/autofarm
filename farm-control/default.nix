let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};
in
{
  farm-control = pkgs.haskellPackages.callPackage ./farm-control.nix {  };
}
