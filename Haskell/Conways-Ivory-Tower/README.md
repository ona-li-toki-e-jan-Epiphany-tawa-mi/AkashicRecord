**!!NOTICE!!:** This project is no longer developed or maintained.

# Conway's Ivory Tower

An infinitely-sized interactive implementation of Conway's Game of Life.

## Controls

 - Press W or the up-arrow to move up.
 - Press A or the left-arrow to move left.
 - Press D or the right-arrow to move right.
 - Press S or the down-arrow to move down.
 - Prsss 1 or page up to zoom in.
 - Press 2 or page down to zoom out.
 - Press G to toggle the grid.
 - Press R to reset the board.
 - Press Space to pause/unpause the game.
 - Click on any cell to toggle it.

## How to build

You will need ghc 9.0.2, cabal, and stack installed on your system. I recommend
using GHCup (https://www.haskell.org/ghcup) to install them if not present.

Run the following command(s) in the project directory:

```console
stack build
```

The outputted executable will be somewhere in the `.stack-work/` directory; the
exact location will be shown in the output of the build command.

## How to run

You can run it directly from stack, for which you will need the programs listed
in the "How to build" section, by running the following command(s) in the
project directory:

```console
stack run
```
