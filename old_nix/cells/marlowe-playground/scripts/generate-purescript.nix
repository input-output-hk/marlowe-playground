{ inputs, cell }:

let
  inherit (cell.library) writeShellScriptInRepoRoot;

in

writeShellScriptInRepoRoot "generate-purs" ''
  generated=./marlowe-playground-client/generated

  # Clean old build
  rm -rf $generated

  # Re-generate the files
  cp -r $(nix build .#generated-purescript --no-link --print-out-paths) $generated
  chmod -R +w $generated
''
