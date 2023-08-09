{ inputs, cell }:

let
  inherit (cell.library) writeShellScriptInRepoRoot ghc-with-marlowe;

in

writeShellScriptInRepoRoot "marlowe-playground-server" ''
  echo "marlowe-playground-server: for development use only"
  export PATH="$(nix build .#ghc-with-marlowe --no-link --print-out-paths)/bin:$PATH"
  export FRONTEND_URL=https://localhost:8009
  nix run .#marlowe-playground-server -- webserver
''
