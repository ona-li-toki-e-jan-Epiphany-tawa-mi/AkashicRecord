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

# Installs and configures an SSH server.

{ ... }:

{
  # Enables OpenSSH and forces key-based authentication.
  services.openssh = {
    enable       = true;
    openFirewall = true;

    settings = {
      "PermitRootLogin"              = "no";
      "PasswordAuthentication"       = false;
      "X11Forwarding"                = false;
      "KbdInteractiveAuthentication" = false;
      "AuthenticationMethods"        = "publickey";
      "StreamLocalBindUnlink"        = "no";
    };
  };

  # Fail2ban to block out brute-force bots. Shouldn't be a problem for an access
  # point, but you never know.
  services.fail2ban = {
    enable                   = true;
    bantime-increment.enable = true;
  };
}
