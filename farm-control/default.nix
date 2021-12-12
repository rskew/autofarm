{ pkgs ? import ../../nixpkgs {},
}:

{
  farm-control = pkgs.haskellPackages.callPackage ./farm-control.nix {  };
}
