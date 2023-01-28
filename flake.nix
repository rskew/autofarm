{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    nix-rebar3.url = "github:axelf4/nix-rebar3";
    nix-rebar3.inputs.nixpkgs.follows = "nixpkgs";
    spago2nix.url = "github:justinwoo/spago2nix";
    spago2nix.inputs.nixpkgs.follows = "nixpkgs";
    # TODO
    purerl.url = "github:rskew/nixpkgs-purerl";
    purerl.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nix-rebar3, spago2nix, purerl }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
        python-ftdi = pkgs.python3.withPackages (p: [ p.pyftdi ]);
        # Script to set the three gpio values (active low)
        # Values for relays 1, 2, 3 and 4 are arguments 1, 2, 3 and 4
        # Relay 1 is ftdi gpio 0, relay 2 is ftdi gpio 3, relay 3 is ftdi gpio 1, relay 4 is ftdi gpio 4
        # Call from erlang via:
        #   os:cmd(io_lib:format("bang ~B ~B ~B ~B", [0, 0, 0, 0])).
        bang = pkgs.writeShellScriptBin "bang" ''
          let "bits = 255 - $1 - $2 * 8 - $3 * 2 - $4 * 16"
          ${python-ftdi}/bin/python ${./devices/irrigation_controller_ftdi_gpio/set-gpio.py} $bits
        '';
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

          cat << EOF

          Install dependencies:
              npm ci

          Download firmware (firmware version will need to be updated, check what's there at https://www.espruino.com/binaries/travis/master):
              npm run download-firmware

          Flash firmware (assumes board is connected at /dev/ttyUSB0, if not then update command in package.json)
              npm run flash-firmware

          To flash app, first create config.js with the structure:
              netConfig = {
                  "wifiSSID": ...,
                  "wifiPassword": ...,
                  "serverIP": ...,
                  "serverPort": ...,
              };
              deviceConfig = {
                  "type": ...,
                  "id": ...,
              };
          then flash mcu-base.js to .boot2, mcu-app.js to .boot1, and config.js to .boot0
          EOF
        '';
      };

      devShells.x86_64-linux.autofarm = pkgs.mkShell {
        buildInputs = [
          pkgs.erlang
          pkgs.rebar3
        ];
        bang = "${bang}/bin/bang";
        AUTOFARM_DEVICE_MONITOR_DEVICE_LISTENER_PORT = 9222;
        AUTOFARM_ECRON_SERVER_ECRONTAB = "ecrontab";
        AUTOFARM_FRONTEND_SERVER_PORT = 8082;
        AUTOFARM_FRONTEND_SERVER_BASIC_AUTH_CREDENTIALS_FILE = "frontend-server-basic-auth-credentials";
        shellHook = ''
          cd erlang

          cat << EOF

          Run in development mode:
              rebar3 shell
          EOF
        '';
      };
      devShells.x86_64-linux.autofarm-purerl = pkgs.mkShell {
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          purerl.packages.x86_64-linux.purerl-0-0-18
          spago2nix.packages.x86_64-linux.spago2nix
        ];
        shellHook = ''
          cd erlang/apps/event_scheduler

          cat << EOF
          Development build:
              spago build

          Then test as part of erlang system:
              cd ../../../; nix develop .#autofarm; rebar3 shell

          On update to purescript dependencies, regenerate nix-ified version for prod build:
              spago2nix generate
          EOF
        '';
      };
      packages.x86_64-linux.autofarm-purerl =
        let
          spagoPackages = import ./erlang/apps/event_scheduler/spago-packages.nix { inherit pkgs; };
          # Customised version of spagoPackages.mkBuildProjectOutput that uses purs to compile .purs to corefn, then runs purerl
          mkBuildProjectOutput = { src, purs }:
            pkgs.stdenv.mkDerivation {
              name = "build-project-output";
              src = src;
              buildInputs = [ purs purerl.packages.x86_64-linux.purerl-0-0-18 ];
              installPhase = ''
                mkdir -p $out
                purs compile "$src/**/*.purs" ${builtins.toString
                  (builtins.map
                    (x: ''"${x.outPath}/src/**/*.purs"'')
                    (builtins.attrValues spagoPackages.inputs))} --codegen corefn
                purerl
                mv output $out
              '';
            };
          builtPursSources = mkBuildProjectOutput {
            src = ./erlang/apps/event_scheduler;
            purs = pkgs.purescript;
          };
        in builtPursSources;
      packages.x86_64-linux.autofarm =
        ((pkgs.callPackage nix-rebar3 {}).buildRebar3 {
          root = ./erlang;
          pname = "autofarm";
          version = "0.1.0";
          releaseType = "release";
        }).overrideAttrs (finalAttrs: previousAttrs: {
          preBuildPhases = [ "linkBangScript" "symlinkPurerl" "symlinkPrivToFrontend" ];
          linkBangScript = ''
            substituteInPlace apps/device_monitor/src/irrigation_controller.erl --replace "\$bang" "${bang}/bin/bang"
          '';
          symlinkPurerl = ''
            rm apps/event_scheduler/src/output
            ln -s ${packages.x86_64-linux.autofarm-purerl}/output apps/event_scheduler/src/output
          '';
          symlinkPrivToFrontend = ''
            rm apps/frontend_server/priv
            ln -s ${packages.x86_64-linux.frontend} apps/frontend_server/priv
          '';
        });
      devShells.x86_64-linux.influxdb = pkgs.mkShell {
        buildInputs = [
          pkgs.influxdb
        ];
        shellHook = ''
          cat << EOF

          Start influxdb via
            $ influxd run

          Connect to influx shell via
            $ influx -username <user> -password '<password>'

          If influxd has just started for the first time, set the user up via
            $ influx
            > CREATE USER <user> WITH PASSWORD '<password>' WITH ALL PRIVILEGES

          Query users:
            > SHOW USERS

          Database management:
            > SHOW DATABASES
            > CREATE DATABASE hello
            > DROP DATABASE hello
            > DELETE FROM hello

          Query data:
            > USE hello
            > SELECT * FROM hello

          Docs:
            - https://docs.influxdata.com/influxdb/v1.8/query_language/explore-data/
            - https://docs.influxdata.com/influxdb/v1.8/query_language/spec/

          EOF
          '';
      };

      nixosModule =
        { lib, config, ... }:
        let
          cfg = config.services.autofarm;
        in {
          options.services.autofarm = {
            deviceMonitorDeviceListenerPort = lib.mkOption {
              type = lib.types.int;
              default = 9222;
              description = "Port for devices to connect on";
            };
            deviceMonitorInfluxdbPort = lib.mkOption {
              type = lib.types.int;
              default = 8086;
            };
            ecronServerEcrontab = lib.mkOption {
              type = lib.types.path;
              description = "Path to persist device action schedules";
            };
            frontendServerPort = lib.mkOption {
              type = lib.types.int;
              default = 8082;
              description = "Frontend server port";
            };
            frontendServerBasicAuthCredentialsFile = lib.mkOption {
              type = lib.types.path;
              description = "File containing 'user:password' for frontend authorization";
            };
          };
          config = {
            services.influxdb = {
              enable = true;
              extraConfig = {
                http = { bind-address = ":${toString(cfg.deviceMonitorInfluxdbPort)}"; };
              };
            };
            services.grafana = {
              enable = true;
              settings = {};
            };
            networking.firewall.allowedTCPPorts = [ cfg.deviceMonitorDeviceListenerPort cfg.frontendServerPort ];
            systemd.services.autofarm = {
              description = "";
              after = [ "network-pre.target" ];
              wants = [ "network-pre.target" ];
              wantedBy = [ "multi-user.target" ];
              path = [pkgs.gawk];
              environment = {
                AUTOFARM_DEVICE_MONITOR_DEVICE_LISTENER_PORT = toString(cfg.deviceMonitorDeviceListenerPort);
                AUTOFARM_DEVICE_MONITOR_INFLUXDB_PORT = toString(cfg.deviceMonitorInfluxdbPort);
                AUTOFARM_ECRON_SERVER_ECRONTAB = cfg.ecronServerEcrontab;
                AUTOFARM_FRONTEND_SERVER_PORT = toString(cfg.frontendServerPort);
                AUTOFARM_FRONTEND_SERVER_BASIC_AUTH_CREDENTIALS_FILE = cfg.frontendServerBasicAuthCredentialsFile;
              };
              serviceConfig = {
                Restart = "always";
                RestartSec = 10;
                StartLimitBurst = 8640;
                StartLimitIntervalSec = 86400;
                StartLimitInterval = 86400;
                ExecStart = "${packages.x86_64-linux.autofarm}/bin/autofarm foreground";
              };
            };
          };
        };
      apps.x86_64-linux.autofarm-remsh =
        let program = pkgs.writeShellScriptBin "autofarm-remsh" ''
              ${packages.x86_64-linux.autofarm}/bin/autofarm remote_console
            '';
        in { type = "app"; program = "${program}/bin/autofarm-remsh"; };

      devShells.x86_64-linux.frontend = pkgs.mkShell {
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          pkgs.nodejs
          pkgs.nodePackages.node2nix
          spago2nix
        ];
        shellHook = ''
          cd frontend

          cat << EOF
          Development build:
              spago build

          Serve development version from development backend:
              cd ..
              nix develop .#autofarm
              rebar3 shell
          development version of frontend now served at 'localhost:8082/assets/index.html'

          On update to purescript dependencies, regenerate nix-ified version for prod build:
              spago2nix generate
          EOF
        '';
      };
      packages.x86_64-linux.frontend =
        let
          spagoPackages = import ./frontend/spago-packages.nix { inherit pkgs; };
          builtPursSources = spagoPackages.mkBuildProjectOutput {
            src = ./frontend/src;
            purs = pkgs.purescript;
          };
        in pkgs.stdenv.mkDerivation {
          name = "autofarm-frontend";
          src = ./frontend/src;
          assets = ./frontend/assets;
          buildInputs = [
            pkgs.esbuild
            pkgs.purescript
            pkgs.nodejs
            builtPursSources
          ];
          buildPhase = ''
            cd ..
            mkdir -p dist
            cp -r $assets/* dist/
            rm dist/index.js # remove index.js used for development
            echo 'import { main } from "${builtPursSources}/output/Main/index.js"; main();' > index.js
            esbuild --bundle index.js --outfile=dist/index.js
          '';
          installPhase = ''
            mkdir -p $out
            cp -r dist/* $out/
          '';
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
