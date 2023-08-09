{ inputs, cell }:

let
  inherit (cell.library) writeShellScriptInRepoRoot pkgs;
  inherit (pkgs) fd optipng;

in
writeShellScriptInRepoRoot "fix-png-optimization" ''
  ${fd}/bin/fd \
    --extension png \
    --exec "${optipng}/bin/optipng" {}
''
