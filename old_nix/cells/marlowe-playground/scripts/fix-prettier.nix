{ inputs, cell }:

let
  inherit (cell.library) pkgs write-formatter;
  inherit (pkgs.nodePackages) prettier;

in

write-formatter "prettier" "${prettier}/bin/prettier -w" [ "js" "ts" "css" "html" ]
