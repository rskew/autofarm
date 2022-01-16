{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    (pkgs.python38Packages.withPackages (p: with p; [
        mpfshell
      ]))
  ];
}
