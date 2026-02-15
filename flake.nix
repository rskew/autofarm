{
  inputs = {
    #nixpkgs.url = "nixpkgs/nixos-23.05";
    #nix-rebar3.url = "github:axelf4/nix-rebar3";
    #nix-rebar3.inputs.nixpkgs.follows = "nixpkgs";
    #spago2nixSource.url = "github:justinwoo/spago2nix";
    #spago2nixSource.inputs.nixpkgs.follows = "nixpkgs";
    #purerl.url = "github:purerl/nixpkgs-purerl";
    #purerl.inputs.nixpkgs.follows = "nixpkgs";
    gleam-nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    #nix-rebar3,
    #spago2nixSource,
    #purerl,
    gleam-nixpkgs,
  }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
        #spago2nix = import spago2nixSource { inherit pkgs; inherit (pkgs) nodejs; };
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
        gpkgs = gleam-nixpkgs.legacyPackages.x86_64-linux;
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
          #purerl.packages.x86_64-linux.purerl-0-0-18
          #spago2nix
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
              #buildInputs = [ purs purerl.packages.x86_64-linux.purerl-0-0-18 ];
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
      #packages.x86_64-linux.autofarm =
      #  ((pkgs.callPackage nix-rebar3 {}).buildRebar3 {
      #    root = ./erlang;
      #    pname = "autofarm";
      #    version = "0.1.0";
      #    releaseType = "release";
      #  }).overrideAttrs (finalAttrs: previousAttrs: {
      #    preBuildPhases = [ "linkBangScript" "symlinkPurerl" "symlinkPrivToFrontend" ];
      #    linkBangScript = ''
      #      substituteInPlace apps/device_monitor/src/irrigation_controller.erl --replace "\$bang" "${bang}/bin/bang"
      #    '';
      #    symlinkPurerl = ''
      #      rm apps/event_scheduler/src/output
      #      ln -s ${packages.x86_64-linux.autofarm-purerl}/output apps/event_scheduler/src/output
      #    '';
      #    symlinkPrivToFrontend = ''
      #      rm apps/frontend_server/priv
      #      ln -s ${packages.x86_64-linux.frontend} apps/frontend_server/priv
      #    '';
      #  });
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
              path = [ pkgs.gawk ];
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
                ExecStart = "${packages.x86_64-linux.autofarm}/bin/autofarm foreground";
              };
            };
          };
        };
      packages.x86_64-linux.autofarm-remsh = pkgs.writeShellScriptBin "autofarm-remsh"
        "${packages.x86_64-linux.autofarm}/bin/autofarm remote_console";

      devShells.x86_64-linux.frontend = pkgs.mkShell {
        buildInputs = [
          pkgs.spago
          pkgs.purescript
          pkgs.nodejs
          pkgs.nodePackages.node2nix
          #spago2nix
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
          npm run # show all scripts in package.json

          cat << EOF
          Build and run the program with library implementation that just logs to console:
              buildPrinty
              runPrinty

          Build and run the program with library implementation runs locally via node:
              buildSimNode
              runSimNode
          EOF
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
          npm run # show all scripts in package.json
        '';
      };

      packages.x86_64-linux.postgresHbaFileDev = pkgs.writeText "pg_hba_dev.conf" ''
        local all all              trust
        host  all all 127.0.0.1/32 trust
        host  all all ::1/128      trust
      '';
      packages.x86_64-linux.postgresWrapperDev = pkgs.writeShellApplication {
        name = "postgres-wrapper";
        runtimeInputs = [ (pkgs.postgresql.withPackages (p: [ p.postgis ])) ];
        text = ''
          # For some reason zellij sends SIGHUP to commands on exit,
          # which isn't a strong-enough signal to stop postgres
          stop_postgres() {
            echo Received exit signal, stopping postgres
            pg_ctl stop -w
          }
          trap stop_postgres SIGHUP SIGINT SIGTERM
          # Run postgres listening for TCP connections
          pg_ctl start -w -o "-c unix_socket_directories= -c hba_file=${packages.x86_64-linux.postgresHbaFileDev}"
          sleep infinity
          wait
        '';
      };

      devShells.x86_64-linux.db-dev = pkgs.mkShell rec {
        buildInputs = [
          pkgs.pgcli
          pkgs.postgresql
          pkgs.postgresqlPackages.postgis
          packages.x86_64-linux.postgresWrapperDev
          (pkgs.writeShellApplication {
            name = "initialize-db-empty";
            runtimeInputs = [ (pkgs.postgresql.withPackages (p: [ p.postgis ])) ];
            text = ''
              mkdir .db-data
              echo "$PGPASSWORD" > pwfile
              initdb --user "$PGUSER" --pwfile=./pwfile
              rm pwfile
              pg_ctl start -w -o "-c unix_socket_directories= -c hba_file=${packages.x86_64-linux.postgresHbaFileDev}"
              psql -c "CREATE EXTENSION IF NOT EXISTS postgis; CREATE EXTENSION IF NOT EXISTS postgis_raster;"
              worked=$?
              pg_ctl stop -w
              if [ "$worked" -eq 0 ];
              then echo Successfully initialized database
              else echo Failed to initialize database
              fi
            '';
          })
          # TODO move this out of repo, maybe into machine-configuration/applications
          # PGBACKREST_CONFIG=$PWD/pgbackrest.conf PGBACKREST_STANZA=farmdb nix develop .#db-dev --command initialize-db-from-prod
          (pkgs.writeShellApplication {
            name = "initialize-db-from-prod";
            runtimeInputs = [ pkgs.pgbackrest packages.x86_64-linux.postgresWrapperDev ];
            text = ''
              # Dereference variables to raise an error if unset
              echo "PGDATA: $PGDATA"
              echo "PGBACKREST_CONFIG: $PGBACKREST_CONFIG"
              echo "PGBACKREST_STANZA: $PGBACKREST_STANZA"
              echo "PGBACKREST_REPO1_S3_KEY: $(if [ -n "$PGBACKREST_REPO1_S3_KEY" ]; then echo set; else echo unset; fi)"
              echo "PGBACKREST_REPO1_S3_KEY_SECRET: $(if [ -n "$PGBACKREST_REPO1_S3_KEY_SECRET" ]; then echo set; else echo unset; fi)"
              echo "PGBACKREST_REPO1_CIPHER_PASS: $(if [ -n "$PGBACKREST_REPO1_CIPHER_PASS" ]; then echo set; else echo unset; fi)"

              test ! -d "$PGDATA" && echo "Data directory $PGDATA must exist. Create via initialize-db-empty" && exit 1

              echo Backing up pg_ident.conf postgresql.conf pg_hba.conf postmaster.opts
              now=$(date "+%s")
              mkdir /tmp/restoring-db-"$now"
              cp "$PGDATA"/pg_ident.conf "$PGDATA"/postgresql.conf "$PGDATA"/pg_hba.conf "$PGDATA"/postmaster.opts /tmp/restoring-db-"$now"

              # run pgbackrest restore
              echo Restoring prod backup to local DB
              pgbackrest --delta restore

              echo Restoring pg_ident.conf postgresql.conf pg_hba.conf postmaster.opts
              cp /tmp/restoring-db-"$now"/pg_ident.conf /tmp/restoring-db-"$now"/postgresql.conf /tmp/restoring-db-"$now"/pg_hba.conf /tmp/restoring-db-"$now"/postmaster.opts "$PGDATA"
              rm -rf /tmp/restoring-db-"$now"

              echo Checking connection to DB
              pg_ctl start -w -o "-c unix_socket_directories= -c hba_file=${packages.x86_64-linux.postgresHbaFileDev}"
              ${pkgs.inotify-tools}/bin/inotifywait -e delete_self "$PGDATA"/recovery.signal
              psql -c "SELECT * FROM pg_catalog.pg_tables WHERE schemaname = 'public';"
              worked=$?
              pg_ctl stop -w
              if [ "$worked" -eq 0 ];
              then echo Successfully initialized database
              else echo Failed to initialize database
              fi
            '';
          })
        ];
        PGDATA = ".db-data";
        PGPORT = 5452;
        PGHOST = "localhost";
        PGDATABASE = "postgres";
        PGUSER = "postgres";
        PGPASSWORD = "fdsa";
        shellHook = ''
          cd db

          cat << EOF
          Run dev DB:
            postgres-wrapper

          DB console:
            pgcli

          Ingest raster into postgis:
            raster2pgsql -t 50x50 -d -C -I -M -l 2,4,8,16,32,64,128,256 farm_dem.tif public.vicmap_dem | psql

          Initialize dev DB:
            initialize-db
          EOF
        '';
      };

      devShells.x86_64-linux.db-prod = pkgs.mkShell {
        buildInputs = [
          pkgs.pgcli
          pkgs.postgresql
          pkgs.postgresqlPackages.postgis
        ];
        PGPORT = 5432;
        PGHOST = "45.124.54.206";
        PGDATABASE = "postgres";
        PGUSER = "postgres";
        shellHook = ''
          cd db

          cat << EOF
          Prod DB console:
            PGPASSWORD=****** pgcli
          EOF
        '';
      };

      devShells.x86_64-linux.haskell-get-bin-from-postgres = pkgs.mkShell {
         packages = [
           (pkgs.haskellPackages.ghcWithPackages (p: [
             p.postgresql-simple
             p.bytestring
           ]))
         ];
      };

      nixosModules.gleam-backend =
        { lib, config, pkgs, ... }:
        let
          src = gpkgs.runCommand "gleam-backend-src" {} ''
            mkdir -p $out
            cp -r ${./gleam-backend}/* $out/
            chmod -R u+w $out
            # Fix the local package path to point to the nix store copy
            substituteInPlace $out/gleam.toml --replace-fail '../gleam-shared' '${./gleam-shared}'
            substituteInPlace $out/manifest.toml --replace-fail '../gleam-shared' '${./gleam-shared}'
          '';
          workDir = "/var/lib/gleam-backend";
        in {
          networking.firewall.allowedTCPPorts = [ 8006 ];
          systemd.services.gleam-backend = {
            description = "Irrigation control backend (gleam)";
            after = [ "network.target" ];
            wantedBy = [ "multi-user.target" ];
            path = [
              gpkgs.gleam
              gpkgs.beam28Packages.erlang
              gpkgs.beam28Packages.elixir
              gpkgs.rebar3
              gpkgs.sqlite
              gpkgs.gnumake
              gpkgs.gcc
            ];
            environment.HOME = workDir;
            serviceConfig = {
              ExecStartPre = let script = pkgs.writeShellScript "gleam-backend-setup" ''
                cd /tmp
                #rm -rf ${workDir}
                mkdir -p ${workDir}
                cp -r ${src}/* ${workDir}/
                mkdir -p ${workDir}/priv
                touch ${workDir}/priv/db.sqlite
                ln -s /home/rowan/autofarm/gleam-backend/priv/public ${workDir}/priv/public
                chmod -R u+w ${workDir}
                export HOME=${workDir}
                cd ${workDir}
                ${gpkgs.beam28Packages.elixir}/bin/mix local.hex --force
              ''; in "+${script}";
              ExecStart = "${gpkgs.gleam}/bin/gleam run -- ttyUSB0";
              WorkingDirectory = workDir;
              Restart = "on-failure";
              RestartSec = 5;
              SupplementaryGroups = [ "dialout" ];
              StateDirectory = "gleam-backend";
            };
          };
        };

      devShells.x86_64-linux.gleam-backend = gpkgs.mkShell {
        packages = [
          gpkgs.gleam
          gpkgs.beam28Packages.erlang
          gpkgs.beam28Packages.elixir
          gpkgs.rebar3
          gpkgs.sqlite
        ];
        shellHook = ''
          cd gleam-backend
          cat <<EOF
            gleam run -- ttyUSB0
          EOF
        '';
      };
      devShells.x86_64-linux.gleam-frontend = gpkgs.mkShell {
        packages = [
          gpkgs.gleam
          gpkgs.beam28Packages.erlang
          gpkgs.rebar3
          gpkgs.tailwindcss_4
          gpkgs.bun
        ];
        shellHook = ''
          cd gleam-frontend
          cat <<EOF
            Run the dev beckend available in the 'gleam-backend' shell
              cd ..; nix develop .#gleam-backend

            Build the frontend via:
              gleam run -m lustre/dev build --outdir=../gleam-backend/priv/public
          EOF
        '';
      };
      devShells.x86_64-linux.lora-node =
      let
        #arduinoEsp32 = gpkgs.fetchFromGitHub {
        #  owner = "espressif";
        #  repo = "arduino-esp32";
        #  tag = "3.3.4";
        #  hash = "sha256-efqYlgZ/T2QSAmffwVJvWNjcdEtGDW3BMa1ndVN9qRs=";
        #};
        #esptool = gpkgs.fetchFromGitHub {
        #  owner = "espressif";
        #  repo = "esptool";
        #  tag = "v5.1.0";
        #  hash = "sha256-pdkL/QfrrTs/NdXlsr+2Yo+r8UTFLkxw4E6XGDAt1yE=";
        #};
        #arduinoDataDir = gpkgs.runCommand "arduinoDataDir" {} ''
        #  mkdir -p $out/packages/esp32/hardware/esp32
        #  ln -s ${arduinoEsp32} $out/packages/esp32/hardware/esp32/3.3.4
        #  mkdir -p $out/packages/esp32/tools/esptool_py
        #  ln -s ${esptool} $out/packages/esp32/tools/esptool_py/5.1.0
        #  ln -s /tmp/arduino-inventory.yaml $out/inventory.yaml
        #  ln -s /tmp/arduino-staging $out/staging
        #'';
        lora = gpkgs.fetchFromGitHub {
          owner = "sandeepmistry";
          repo = "arduino-LoRa";
          rev = "f7b08b06373880a2a17e55577c0ddb759c608e62"; # 2024-06-13
          hash = "sha256-d/vgYw9fgVOESTuuH/11P0Y5mUEloS00ldWx+o2wFaI=";
        };
        arduinoUserDir = gpkgs.runCommand "arduinoUserDir" {} ''
          mkdir -p $out/libraries
          ln -s ${lora} $out/libraries/lora
        '';
        arduinoCliConfig = gpkgs.writeText "arduinoCliConfig" ''
          board_manager:
              additional_urls: []
          directories:
              #data: $${arduinoDataDir}
              user: ${arduinoUserDir}
        '';
        arduinoCliWithConfig = gpkgs.writeShellScriptBin "arduino-cli-hello" ''
          mkdir -p /tmp/arduino-staging
          ${gpkgs.lib.getExe gpkgs.arduino-cli} --config-file ${arduinoCliConfig} "$@"
        '';
      in
      gpkgs.mkShell {
        packages = [
          arduinoCliWithConfig
        ];
        shellHook = ''
          cd lora-node
          cat <<EOF
            Compile and upload Gateway sketch:
                arduino-cli-hello compile -b esp32:esp32:esp32da -p /dev/ttyUSB0 -u Gateway

            Monitor node MCU:
                arduino-cli-hello monitor -p /dev/ttyUSB1 --config baudrate=115200

            Compile and upload Node sketch:
                arduino-cli-hello compile -b esp32:esp32:esp32da -p /dev/ttyUSB1 -u Node
          EOF
        '';
      };
    };
}
