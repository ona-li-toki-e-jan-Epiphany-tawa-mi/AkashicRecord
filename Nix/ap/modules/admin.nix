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

# Creates an admin user account.

{ adminUserName
, adminSSHKeys
, adminInitialHashedPassword
, ...
}:

{
  users.users."${adminUserName}" = {
    isNormalUser          = true;
    description           = "Admin User";
    initialHashedPassword = adminInitialHashedPassword;
    extraGroups           = [ "wheel" "networkmanager" ];

    openssh.authorizedKeys.keys = adminSSHKeys;
  };

  services.openssh.settings."AllowUsers" = [ adminUserName ];

  # Makes my user a trusted user for remote rebuilding.
  nix.settings.trusted-users = [ adminUserName ];
}
