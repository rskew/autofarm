{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz") {}
}:

pkgs.mkShell {
  name = "erlang-autofarm";
  buildInputs = [
    pkgs.erlang
    pkgs.rebar3
  ];
  ERL_FLAGS=" -args_file config/vm.args -config config/sys.config";
}
