{
  description = "Poltergeist server development environment";

  # We use nixpkgs-unstable since the NUR does as well.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;

      forAllSystems = f: lib.genAttrs lib.systems.flakeExposed (system: f {
        pkgs = import nixpkgs { inherit system; };
      });

    in
      {
        devShells = forAllSystems ({ pkgs }: {
          default = with pkgs; mkShell {
            nativeBuildInputs = [
              erlang
              xdotool
            ];
          };
        });
      };
  }
