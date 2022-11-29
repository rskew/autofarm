{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.05";
    nix-rebar3.url = "github:axelf4/nix-rebar3";
    nix-rebar3.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nix-rebar3 }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in rec {

      devShells.x86_64-linux.mcu = pkgs.mkShell {
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          pkgs.nodejs
          pkgs.esptool
        ];
        shellHook = ''
          cd devices/irrigation_controller_js

          npm run

          cat << EOF

          Install dependencies:
              npm ci

          Download firmware (firmware version will need to be updated, check what's there at https://www.espruino.com/binaries/travis/master):
              npm run download-firmware

          Flash firmware (assumes board is connected at /dev/ttyUSB0, if not then update command in package.json)
              npm run flash-firmware
          EOF
        '';
      };

      devShells.x86_64-linux.autofarm = pkgs.mkShell {
        buildInputs = [
          pkgs.erlang
          pkgs.rebar3
        ];
        ERL_FLAGS=" -args_file config/vm.args -config config/sys.config";
        shellHook = ''
          cd erlang

          cat << EOF

          Run in development mode:
              rebar3 shell
          EOF
        '';
      };
      packages.x86_64-linux.autofarm =
        (pkgs.callPackage nix-rebar3 {}).buildRebar3 {
          root = ./erlang;
          pname = "autofarm";
          version = "0.1.0";
          releaseType = "release";
        };
    };
}
