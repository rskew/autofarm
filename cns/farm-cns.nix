let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};
  farm-haskell = (pkgs.haskellPackages.callPackage ../farm-haskell/farm-haskell.nix { });
in
{
  farm_server =
    { config, lib, ... }:
    {
      deployment.targetHost = "192.168.1.13";

      imports =
        [ # Include the results of the hardware scan.
          ../machines/z230-hardware-configuration.nix
          ../machines/z230-configuration.nix
        ];

      environment.systemPackages = with pkgs; [
        farm-haskell
        mosquitto
      ];

      services.mosquitto = {
        enable = true;
        host = "0.0.0.0";
        port = 1883;
        allowAnonymous = true;
        aclExtraConf = ''
          topic readwrite #
        '';
        users = {};
      };
      networking.firewall.allowedTCPPorts = [ 1883 ];

      # Run the bore pump in the afternoon when the sun is shining on the solar panels
      systemd.services.run_bore_pump = {
        serviceConfig.Type = "oneshot";
        script = "${farm-haskell}/bin/farm-haskell start --farmVerb \"Pump BorePump\" --duration 1500 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.run_bore_pump = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_top_row.service" ];
        timerConfig.OnCalendar = "*-*-* 14:30:00";
      };

      # Run irrigation in the morning when it's coolest
      systemd.services.irrigate_top_row = {
        serviceConfig.Type = "oneshot";
        script = "${farm-haskell}/bin/farm-haskell start --farmVerb \"Irrigate TopRow\" --duration 3600 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_top_row = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_top_row.service" ];
        timerConfig.OnCalendar = "*-*-* 3:00:00";
      };

      systemd.services.irrigate_poly_tunnel = {
        serviceConfig.Type = "oneshot";
        script = "${farm-haskell}/bin/farm-haskell start --farmVerb \"Irrigate PolyTunnel\" --duration 3600 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_poly_tunnel = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_poly_tunnel.service" ];
        timerConfig.OnCalendar = "*-*-* 4:00:00";
      };

      systemd.services.irrigate_pumpkins = {
        serviceConfig.Type = "oneshot";
        script = "${farm-haskell}/bin/farm-haskell start --farmVerb \"Irrigate Pumpkins\" --duration 3600 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_pumpkins = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_pumpkins.service" ];
        timerConfig.OnCalendar = "*-*-* 5:00:00";
      };
    };
}
