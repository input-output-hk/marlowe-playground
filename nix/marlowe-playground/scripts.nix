{ repoRoot, ... }:

{

  generate-purescript = ''
    cd "$(git rev-parse --show-toplevel)"

    generated=./marlowe-playground-client/generated

    # Clean old build
    rm -rf $generated

    # Re-generate the files
    cp -r $(nix build .#generated-purescript --no-link --print-out-paths) $generated
    chmod -R +w $generated
  '';


  start-backend = ''
    cd "$(git rev-parse --show-toplevel)"

    echo "marlowe-playground-server: for development use only"
    export PATH="$(nix build .#ghc-with-marlowe --no-link --print-out-paths)/bin:$PATH"
    export FRONTEND_URL=https://localhost:8009
    nix run .#marlowe-playground-server -- webserver
  '';


  update-client-deps = ''
    cd "$(git rev-parse --show-toplevel)"

    cd marlowe-playground-client

    ${repoRoot.nix.marlowe-playground.easy-purescript-nix.spago2nix}/bin/spago2nix generate
  '';

}
