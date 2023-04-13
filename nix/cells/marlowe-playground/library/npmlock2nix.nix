{ inputs, cell }:

import inputs.npmlock2nix { inherit (cell.library) pkgs; }
