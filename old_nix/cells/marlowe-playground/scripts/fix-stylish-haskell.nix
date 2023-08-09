{ inputs, cell }:

let
  inherit (cell.library) writeShellScriptInRepoRoot pkgs stylish-haskell;
  inherit (pkgs) fd;

in

writeShellScriptInRepoRoot "fix-stylish-haskell" ''
  ${fd}/bin/fd \
    --extension hs \
    --exclude 'dist-newstyle/*' \
    --exclude 'dist/*' \
    --exclude '.stack-work/*' \
    --exec bash -c "${stylish-haskell}/bin/stylish-haskell -i {}"
''
