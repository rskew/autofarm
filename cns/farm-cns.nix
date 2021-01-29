let
  #pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-20.09.tar.gz") {};
  pkgs = import <nixos-unstable> {};
  farm-control = (pkgs.haskellPackages.callPackage ../farm-control/farm-control.nix { });
in
{
  network.description = "Farm server";

  farm_server =
    { config, lib, ... }:
    {
      # "farmserver" should refer to an entry in your ssh config file
      # with key authorized on host
      deployment.targetHost = "farm_server";
      deployment.targetUser = "rowan";
      deployment.provisionSSHKey = false;
      # 'nixops deploy' requires target user to have passwordless sudo
      # see https://github.com/NixOS/nixops/pull/1270
      #security.pam.services.sudo.sshAgentAuth = true;
      #security.pam.enableSSHAgentAuth = true;
      #deployment.sshOptions = [ "-A" ];
      # Can't get the ssh-agent sudo auth working, so just update /etc/sudoers
      security.sudo.extraRules = [
        {
          #"rowan ALL=(ALL) NOPASSWD: ALL"
          users = ["rowan"];
          commands = [{command = "ALL"; options = ["NOPASSWD"]; } ];
        }
      ];
      deployment.keys.dynamic-dns-url = {
          text = builtins.readFile ./dynamic-dns-url.txt;
          user = "rowan";
          group = "wheel";
          permissions = "0640";
      };
      systemd.services.dynamic-dns = {
        serviceConfig.Type = "oneshot";
        script = ''
            echo "Updating dynamic DNS IP"
            LOGFILE=/var/log/dynamic-dns.log
            IP=$(${pkgs.curl}/bin/curl ifconfig.me)
            echo $(date) >> ''${LOGFILE}
            ${pkgs.curl}/bin/curl --silent $(cat /run/keys/dynamic-dns-url)''${IP} >> ''${LOGFILE}
        '';
      };
      systemd.timers.dynamic-dns = {
        wantedBy = [ "timers.target" ];
        partOf = [ "dynamic-dns.service" ];
        timerConfig.OnCalendar = "*-*-* *:*:00";
      };

      imports =
        [ # Include the results of the hardware scan.
          ../machines/z230-hardware-configuration.nix
          ../machines/z230-configuration.nix
        ];

      environment.systemPackages = with pkgs; [
        farm-control
        mosquitto
        vim
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
        script = "${farm-control}/bin/farm-control start --farmVerb \"Pump BorePump\" --duration 1500 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.run_bore_pump = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_top_row.service" ];
        timerConfig.OnCalendar = "*-*-* 12:00:00";
      };

      # Run irrigation in the morning when it's coolest
      systemd.services.irrigate_top_row = {
        serviceConfig.Type = "oneshot";
        script = "${farm-control}/bin/farm-control start --farmVerb \"Irrigate TopRow\" --duration 1800 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_top_row = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_top_row.service" ];
        timerConfig.OnCalendar = "*-*-* 4:00:00";
      };

      systemd.services.irrigate_poly_tunnel = {
        serviceConfig.Type = "oneshot";
        script = "${farm-control}/bin/farm-control start --farmVerb \"Irrigate PolyTunnel\" --duration 1800 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_poly_tunnel = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_poly_tunnel.service" ];
        timerConfig.OnCalendar = "*-*-* 4:30:00";
      };

      systemd.services.irrigate_pumpkins = {
        serviceConfig.Type = "oneshot";
        script = "${farm-control}/bin/farm-control start --farmVerb \"Irrigate Pumpkins\" --duration 1800 >> /var/log/farm_control/log.txt";
      };
      systemd.timers.irrigate_pumpkins = {
        wantedBy = [ "timers.target" ];
        partOf = [ "irrigate_pumpkins.service" ];
        timerConfig.OnCalendar = "*-*-* 5:00:00";
      };
    };
}
