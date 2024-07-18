![logo](logo.png)

# Glow Chickens

A data pack for Minecraft 1.19 (but will probably work with 1.15+) that allows
you to feed glowstone dust to chickens (by dropping the items onto them) to make
them glow for 3 minutes.

## Installation

Download the data pack folder from the git repository and place it inside your
world's datapacks folder. See for more information:
https://minecraft.fandom.com/wiki/Tutorials/Installing_a_data_pack

The pack will automatically finish installation on world load, but you can force
it using (recommended if updating pack):

```text
/function glwchckns:install/initialize
```

## Uninstallation

To uninstall, you first need to run this command:

```text
/function glwchckns:install/uninstall
```

After that, remove the datapack from your world's datapacks folder before
reloading.

## Screenshots

![feeding a chicken glowstone](screenshots/feeding_glowstone.png)
![a glowing chicken](screenshots/glowing_chicken.png)
