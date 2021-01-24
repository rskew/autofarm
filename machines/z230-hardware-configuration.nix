{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/DC01-35B9";
      fsType = "vfat";
    };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/e5c736ed-d30e-45c7-a889-e55d4103cc83";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/91bbb70a-4e0c-4e07-a1d0-ac8de73372b6"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
