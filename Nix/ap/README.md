# ap

A simple NixOS configuration for making wireless access points.

ap contains an SSH server for administration, and uses Network Manager and
create_ap to run and manage the access point.

# How to setup/deploy.

First, you will need to configure ap with a user account, network settings,
etc.. To do this, copy the file `config.nix.template` to `config.nix` and change
the values to your preferences.

Then, you will need to transfer the configuration onto the device and rebuild,
or some other means to get it there (you're on your own.) There are different
derivations made availibe in `flake.nix` depending on the device you wish to use
ap with. If a derviation does not exist for your device, feel free to send a
pull request with the required modifications to `flake.nix` and a host file in
`hosts`.

When rebuilding, if Nix complains about missing files, that's because it's
evaluating the git repository. Either move/delete `.git` or prepend `path:` to
the flake path, i.e.:

```
nixos-rebuild build --flake path:.#<device>
```

If you connected it to ethernet, it should start working immediately. If
connecting it to Wi-Fi (as a Wi-Fi repeater,) you will need to connect a
computer to the access point, SSH into the device, and use `nmtui` to connect it
to a wireless network.
