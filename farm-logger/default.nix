{ pkgs ? import ../../nixpkgs {},
}:
pkgs.haskellPackages.callPackage ./farm-logger.nix {  }
