{ repoRoot, inputs, pkgs, system, lib }:

{

  generate-purescript = ''
    repo_root="$(git rev-parse --show-toplevel)"

    generated="$repo_root/marlowe-playground-client/generated"

    # Clean old build
    rm -rf "$generated"
    mkdir -p "$generated"

    # Re-generate the files
    cp -r "$(nix build .#generated-purescript --no-link --print-out-paths "$@")/." "$generated"
    chmod -R +w "$generated"
  '';


  start-backend = ''
    cd "$(git rev-parse --show-toplevel)"
    echo "marlowe-playground-server: for development use only"
    export PATH="$(nix build .#ghc-with-marlowe --no-link --print-out-paths "$@")/bin:$PATH"
    export FRONTEND_URL=https://localhost:8009
    nix run .#marlowe-playground-server "$@" -- webserver
  '';


  gen-nix-lockfiles = ''
    repo_root="$(git rev-parse --show-toplevel)"

    nix_dir="$repo_root/nix"
    mpc_dir="$repo_root/marlowe-playground-client"

    (cd "$mpc_dir" && spago2nix generate) 
    mv "$mpc_dir/spago-packages.nix" "$nix_dir"
    
    echo "# This file was generated by prefetch-npm-deps" > "$nix_dir/npm-deps-hash.nix"
    echo "\"$(prefetch-npm-deps "$mpc_dir/package-lock.json")\"" >> $nix_dir/npm-deps-hash.nix

    nixpkgs-fmt "$nix_dir"
  '';

}
