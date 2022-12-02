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

      devShells.x86_64-linux.mcu-test-purs = pkgs.mkShell rec {
        buildPrinty = pkgs.writeShellScriptBin "buildPrinty" ''
          spago build
          mkdir -p dist
          ./node_modules/rollup/dist/bin/rollup --format=cjs --file=dist/runPrintyBundle.cjs -- runPrinty.js
          sed -i 's/for (var \\([a-z]\\) in \\([a-z]*\\)) {/for (var \\1 in (\\2 || [])) {/g' dist/runPrintyBundle.cjs
          ./node_modules/uglify-js/bin/uglifyjs dist/runPrintyBundle.cjs --output dist/runPrintyBundle.min.cjs --mangle --rename --no-annotations --toplevel --validate
        '';
        runPrinty = pkgs.writeShellScriptBin "runPrinty" ''
          node ./dist/runPrintyBundle.min.cjs
        '';
        buildSimNode = pkgs.writeShellScriptBin "buildSimNode" ''
          spago build
          mkdir -p dist
          ./node_modules/rollup/dist/bin/rollup --format=cjs --file=dist/runSimNodeBundle.cjs -- runSimNode.js
          sed -i 's/for (var \\([a-z]\\) in \\([a-z]*\\)) {/for (var \\1 in (\\2 || [])) {/g' dist/runSimNodeBundle.cjs
          ./node_modules/uglify-js/bin/uglifyjs dist/runSimNodeBundle.cjs --output dist/runSimNodeBundle.min.cjs --mangle --rename --no-annotations --toplevel --validate
        '';
        runSimNode = pkgs.writeShellScriptBin "runSimNode" ''
          node ./dist/runSimNodeBundle.min.cjs
        '';
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          pkgs.nodejs
          pkgs.esptool
          buildPrinty
          runPrinty
          buildSimNode
          runSimNode
        ];
        shellHook = ''
          cd devices/test_purs
          npm run
        '';
      };

      devShells.x86_64-linux.mcu-test-ota = pkgs.mkShell {
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          pkgs.nodejs
          pkgs.esptool
        ];
        shellHook = ''
          cd devices/test_ota_update
          npm run
        '';
      };
    };
}
