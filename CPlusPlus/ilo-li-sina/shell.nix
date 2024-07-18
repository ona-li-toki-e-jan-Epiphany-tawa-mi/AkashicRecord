{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    cmake
    python312
    # ilo Library.
    ncurses
  ];
}
