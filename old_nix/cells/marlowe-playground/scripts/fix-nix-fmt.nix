{ inputs, cell }:

let
  inherit (cell.packages) nixpkgs-fmt;
  inherit (cell.library) write-formatter;

in

write-formatter "nixpkgsfmt" "${nixpkgs-fmt}/bin/nixpkgs-fmt" [ "nix" ]
