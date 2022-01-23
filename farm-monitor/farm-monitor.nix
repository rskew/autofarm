{ mkDerivation, stdenv, pkgs,
  base, net-mqtt, aeson, optparse-generic, utf8-string }:
mkDerivation {
  pname = "farm-monitor";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    net-mqtt
    aeson
    optparse-generic
    utf8-string
  ];
  license = pkgs.lib.licenses.gpl3;
}
