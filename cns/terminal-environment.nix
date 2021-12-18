{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    #utilities
    htop
    git

    # For nicer terminal experience over ssh
    rxvt_unicode
    vim
    byobu # use the byobu-screen command

    # For querying BIOS settings from inside linux
    dmidecode
  ];

  # Use fish shell
  programs.fish.enable = true;

  # vi mode in terminal
  programs.bash.interactiveShellInit = ''
    set -o vi
    alias byobu=byobu-screen
  '';
}
