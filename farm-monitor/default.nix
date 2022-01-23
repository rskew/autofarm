{ pkgs ? import ../../nixpkgs {},
}:
pkgs.haskellPackages.callPackage ./farm-monitor.nix {  }
