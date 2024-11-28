# Changelog

- `reselectPlayer(playerList: string[] = players): string` now will no longer change the current player unless it manages to locate an avalible one from the given array.
- Added ability to play from URLs.
- Added specific options to mpv to reduce latency, prevent video output, disable user-configuration, and prevent any attempts to output text.
- Added specific options to mplayer to prevent video outputs and GUI usage, disable user-configuration, and reduce the number of attempts to output text.
- Added specific options to ffplay to prevent text output.
- Added specific options to cvlc to reduce text output and prevent video output.
- Added sepcific options to play and mpg321/mpg123 to prevent text output.

Note that text isn't going to appear in the console even if these options are
disabled; they just don't need to do it in the first place, so I took lengths to
disable it.

## 1.0.0

Initial release.
