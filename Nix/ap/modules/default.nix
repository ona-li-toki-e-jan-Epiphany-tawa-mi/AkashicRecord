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

# The default nix module that includes all parts of the access point.

{ inputs
, pkgs
, lib
, system
, timeZone
, extraAPModule
, ...
}:

let inherit (lib) mkForce;
in
{
  imports = [ ./ssh.nix
              ./admin.nix
              ./networking.nix
              extraAPModule                       # From configuration.
            ];



  nixpkgs.hostPlatform = system;

  # Enables flakes for truly reproduceable builds.
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Adds the nixpkgs channel from the flake to the NIX_PATH, so one doesn't need
  # to be added manually.
  nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  # Automatically cleans up old snapshots and unneeded stuff in the nix store.
  nix = {
    optimise = {
      automatic = true;
      dates     = [ "weekly" ];
    };

    gc = {
      automatic  = true;
      dates      = "weekly";
      options    = "--delete-older-than 15d";
      persistent = true;
    };
  };



  time.timeZone = timeZone;

  # Selects internationalisation properties.
  i18n = {
    defaultLocale       = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS        = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT    = "en_US.UTF-8";
      LC_MONETARY       = "en_US.UTF-8";
      LC_NAME           = "en_US.UTF-8";
      LC_NUMERIC        = "en_US.UTF-8";
      LC_PAPER          = "en_US.UTF-8";
      LC_TELEPHONE      = "en_US.UTF-8";
      LC_TIME           = "en_US.UTF-8";
    };
  };



  security.sudo.execWheelOnly = true;

  # Baller CPU scheduler.
  services.system76-scheduler.enable = true;

  # System management.
  environment = {
    defaultPackages = mkForce [];
    systemPackages  = [ pkgs.htop ];
  };



  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment? Yes, yes I did.
}
