{ inputs, cell }:

let
  inherit (cell.library) writeShellScriptInRepoRoot purescript;
  inherit (purescript) spago2nix;

in

writeShellScriptInRepoRoot "update-client-deps" ''
  cd marlowe-playground-client
  ${spago2nix}/bin/spago2nix generate
''
