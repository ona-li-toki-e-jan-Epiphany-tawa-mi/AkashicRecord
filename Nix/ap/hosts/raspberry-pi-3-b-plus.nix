# This file is part of ap.
#
# Copyright (c) 2024 ona-li-toki-e-jan-Epiphany-tawa-mi
#
# ap is free software: you can redistribute it and/or modify it under the terms
# of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# ap is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# ap. If not, see <https://www.gnu.org/licenses/>.

# Hardware configuration for a Raspberry Pi 3B+.

{ modulesPath
, swapSpace
, lib
, swapFile
, ...
}:

let inherit (lib) mkIf;
in
{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix")
            ];



  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
  };

  swapDevices = mkIf (null != swapSpace) [{
    device = swapFile;
    size   = swapSpace*1024;
  }];



  # The Pi has low entropy so we need haveged to help top it up.
  services.haveged.enable = true;



  boot.initrd.availableKernelModules = [ "usbhid" ];

  boot.loader = {
    # NixOS wants to enable GRUB by default.
    grub.enable = false;
    # Enables the generation of /boot/extlinux/extlinux.conf.
    generic-extlinux-compatible.enable = true;
    # Steps up the CPU frequency.
    raspberryPi.firmwareConfig = [ "force_turbo=1" ];
  };
}
