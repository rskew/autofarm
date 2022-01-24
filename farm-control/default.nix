{ pkgs ? import ../../nixpkgs {},
}:
pkgs.haskellPackages.callPackage ./farm-control.nix {  }
