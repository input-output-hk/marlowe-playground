{ repoRoot, inputs, pkgs, system, lib }:

# Output containing the purescript bridge code
pkgs.runCommand "generated-purescript" { } ''
  mkdir $out
  ${inputs.self.packages.marlowe-playground-server}/bin/marlowe-playground-server psgenerator $out
  cp ${builtins.path { name = "tidyrc.json"; path = inputs.self + "/.tidyrc.json"; } } $out/.tidyrc.json
  cp ${builtins.path { name = "tidyoperators"; path = inputs.self + "/.tidyoperators"; } } $out/.tidyoperators
  cd $out
  ${repoRoot.nix.easy-purescript-nix.purs-tidy}/bin/purs-tidy format-in-place *
  ${pkgs.nodePackages.prettier}/bin/prettier -w .
  rm -f $out/.tidyrc.json
  rm -f $out/.tidyoperators
''

