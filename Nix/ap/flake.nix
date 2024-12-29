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

{
  description = "NixOS configuration flake for badass reproducable access points";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";

  outputs = { nixpkgs, ... } @ inputs:
    let # Modules to include in every configuration.
        extraModules = [ ./modules ];

        # Arguments to include in every configuration.
        extraSpecialArguments = { inherit inputs; } // (import ./config.nix);
    in {
      nixosConfigurations = {
        "raspberryPi3BPlus" = nixpkgs.lib.nixosSystem rec {
          specialArgs = extraSpecialArguments // { inherit system; };

          system  = "aarch64-linux";
          modules = [ ./hosts/raspberry-pi-3-b-plus.nix ] ++ extraModules;
        };
      };
    };
}
