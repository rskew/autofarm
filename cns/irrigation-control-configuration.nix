{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    farm-control
  ];

  # Setup MQTT broker
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
  systemd.services.log_mqtt = {
    description = "Record all mqtt traffic";
    wantedBy = [ "multi-user.target" ];
    requires = [ "mosquitto.service" ];
    after = [ "mosquitto.service" ];
    serviceConfig = {
      ExecStart = "${pkgs.bash}/bin/bash -c \"${pkgs.mosquitto}/bin/mosquitto_sub --host localhost --port 1883 -v -t '#' >> /var/log/mqtt.out\"";
      Restart = "on-failure";
      User = "root";
    };
  };

  # Run the bore pump in the afternoon when the sun is shining on the solar panels
  # Run every third day
  systemd.services.run_bore_pump = {
    serviceConfig.Type = "oneshot";
    script = "${pkgs.bash}/bin/bash -c '(( $(date +\%s)/ 86400 \% 3 == 0 )) && ${farm-control}/bin/farm-control start --farmVerb \"Pump BorePump\" --duration 900 >> /var/log/farm_control_log.out || echo only run every third day'";
  };
  systemd.timers.run_bore_pump = {
    wantedBy = [ "timers.target" ];
    partOf = [ "irrigate_top_row.service" ];
    timerConfig.OnCalendar = "*-*-* 12:00:00";
  };

  # Run irrigation in the morning
  # Don't irrigate top row at the moment
  #systemd.services.irrigate_top_row = {
  #  serviceConfig.Type = "oneshot";
  #  script = "${farm-control}/bin/farm-control start --farmVerb \"Irrigate TopRow\" --duration 1800 >> /var/log/farm_control_log.out";
  #};
  #systemd.timers.irrigate_top_row = {
  #  wantedBy = [ "timers.target" ];
  #  partOf = [ "irrigate_top_row.service" ];
  #  timerConfig.OnCalendar = "*-*-* 4:00:00";
  #};

  # Run every third day
  systemd.services.irrigate_poly_tunnel = {
    serviceConfig.Type = "oneshot";
    script = "${pkgs.bash}/bin/bash -c '(( $(date +\%s)/ 86400 \% 3 == 0 )) && ${farm-control}/bin/farm-control start --farmVerb \"Irrigate PolyTunnel\" --duration 1800 >> /var/log/farm_control_log.out || echo only run every third day'";
  };
  systemd.timers.irrigate_poly_tunnel = {
    wantedBy = [ "timers.target" ];
    partOf = [ "irrigate_poly_tunnel.service" ];
    timerConfig.OnCalendar = "*-*-* 6:30:00";
  };

  # Don't irrigate middle zone at the moment
  #systemd.services.irrigate_pumpkins = {
  #  serviceConfig.Type = "oneshot";
  #  script = "${farm-control}/bin/farm-control start --farmVerb \"Irrigate Pumpkins\" --duration 1800 >> /var/log/farm_control_log.out";
  #};
  #systemd.timers.irrigate_pumpkins = {
  #  wantedBy = [ "timers.target" ];
  #  partOf = [ "irrigate_pumpkins.service" ];
  #  timerConfig.OnCalendar = "*-*-* 5:00:00";
  #};
}
