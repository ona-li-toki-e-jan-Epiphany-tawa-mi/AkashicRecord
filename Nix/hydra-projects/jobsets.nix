# MIT License
#
# Copyright (c) 2024 ona-li-toki-e-jan-Epiphany-tawa-mi
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Takes the input from a project JSON file and outputs the JSON describing the
# jobsets for Hydra to build.

{ nixpkgs   # nixpkgs to build project against.
, src       # Project source code git repostory URI. Must contain a release.nix jobset.
, ...
}:

let pkgs = import nixpkgs {};
in
{
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON {
    master = {
      enabled = 1;
      hidden  = false;

      # Points to nix file containing jobs to run in the project repository.
      nixexprinput = "src";
      nixexprpath  = "release.nix";

      checkinterval    = 86400;                   # Runs once every day.
      schedulingshares = 1;                       # Scheduling priorty.
      keepnr           = 5;                       # Number of build iterations to keep.

      # Disables email.
      enableemail   = false;
      emailoverride = "";

      inputs = {
        nixpkgs = {
          type  = "path";
          value = nixpkgs;
        };
        src = {
          type  = "git";
          value = src;
        };
      };
    };
  });
}
