{ config, lib, pkgs, ... }:

{

  imports =
    [ # Include the results of the hardware scan.
      ./machines/farm-server-hardware-configuration.nix
      ./machines/terminal-environment.nix
      ./machines/irrigation-control-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "farm-server";
  networking.wireless.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.UTF-8";
  console = {
    font = "source-code-pro";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "Australia/Melbourne";

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
    forwardX11 = false;
  };

  # Setup dynamic DNS
  # The dynamic dns url with username and password embedded is copied to
  # /run/keys/dynamic-dns-url on the target machine by nixops. This means if the
  # machine is rebooted, it will lose this key and have to be redeployed.
  # The dynamic-dns url needs to match the Host in your ssh config for deployment.targetHost
  # defined in the nixops deployment.
  deployment.keys.dynamic-dns-url = {
      text = builtins.readFile ./dynamic-dns-url.txt;
      user = "rowan";
      group = "wheel";
      permissions = "0640";
  };
  systemd.services.dynamic-dns = {
    serviceConfig.Type = "oneshot";
    script = ''
        RESPONSE="$(${pkgs.curl}/bin/curl --silent $(cat /run/keys/dynamic-dns-url))"
        echo "$(date -Iseconds) ''${RESPONSE}" >> /var/log/dynamic-dns.log
    '';
  };
  systemd.timers.dynamic-dns = {
    wantedBy = [ "timers.target" ];
    partOf = [ "dynamic-dns.service" ];
    timerConfig.OnCalendar = "*-*-* *:*:00";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.rowan = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC3oUx8oe0xQDKP9sw602ku4wOhP9AKLXNsGDARyLdw+MbBzGJTNFUvh6fj77fWYTqHlDnrfgoBlc5mS0uY9KUP/28PjfyqdIkGdhbfE403+vp4a1JMAnVv7xV6n3PYtiUYIF5hwCSzeiibIhQsCTsJGtMoiECdRpOvqCD11m6kTA1j5xlajEnvnNg7k7W+MaZWaqeuvEn0Vi7tu+Ia6xvnfkKwph9VpVuMsTrAy0y36pSpglax2yKEV53lt8ZGnasJiOu2fv2yT6np9qGizU2I8ccC5G9nNCkYHJsE2q1ogjdltva6oexCOJzLwMVZCC6UVTHej0494ipY35JSJmh3TW6oG8ddhdUdurPQNaw/w5tiUZwEG3640Ts3TbIJ0sagi1+l5TBRpW7wsgU8VbTyBvsMszXj46xri4jleESPVjr820CRnt27l2Dt/DGpdZHvhbB3endb0NkEqfMb/44SP6mXceT10GIBiCl110/7n7qehXyr1qt88VZ6QVbige9ts9NVkoNYkBrxaHq4ooa8IV9leO52m0X7BLDYSEUYBMFWx3lc7vbyvCV382gsfTQA/CtBHmEUTlljSQS7ZDOXwiSZYXeDKtafDTRNBrWr3HikgjnqMK2OjLU/y3nyoVtE9FzLANWuxwhJIld9S44QWZA82LsnrW/hQfXp7Y4VyQ== rowan@rowanX220"
    ];
    shell = "/run/current-system/sw/bin/fish";
  };

  nix.trustedUsers = [ "root" "rowan" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
