{ inputs, cell }:

let
  inherit (cell.library) pkgs purescript;
  inherit (cell.packages) marlowe-playground-server;
  inherit (pkgs.nodePackages) prettier;
  inherit (purescript) purs-tidy;

in

# Output containing the purescript bridge code
pkgs.runCommand "generated-purescript" { } ''
  mkdir $out
  ${marlowe-playground-server}/bin/marlowe-playground-server psgenerator $out
  cp ${builtins.path { name = "tidyrc.json"; path = inputs.self + "/.tidyrc.json"; } } $out/.tidyrc.json
  cp ${builtins.path { name = "tidyoperators"; path = inputs.self + "/.tidyoperators"; } } $out/.tidyoperators
  cd $out
  ${purs-tidy}/bin/purs-tidy format-in-place *
  ${prettier}/bin/prettier -w .
  rm -f $out/.tidyrc.json
  rm -f $out/.tidyoperators
''

