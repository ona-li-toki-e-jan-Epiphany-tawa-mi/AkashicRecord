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

# Networking code.

{ hostName
, ssid
, password
, internetInterface
, wifiInterface
, macAddressMode
, gatewayAddress
, ...
}:

{
  networking.hostName = hostName;

  # Network Manager is used to set the network to be an access point for.
  networking.networkmanager = {
    enable = true;

    ethernet.macAddress = macAddressMode;
    wifi.macAddress     = macAddressMode;
  };

  # Core functionality.
  services.create_ap = {
    enable   = true;
    settings = {
      "SSID"           = ssid;
      "PASSPHRASE"     = password;
      "INTERNET_IFACE" = internetInterface;
      "WIFI_IFACE"     = wifiInterface;
      "GATEWAY"        = gatewayAddress;
    };
  };
}
