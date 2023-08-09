{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (cell.packages) nixpkgs-fmt;
in
pkgs.runCommand "nixpkgs-fmt" { buildInputs = [ nixpkgs-fmt ]; } ''
  set +e
  nixpkgs-fmt --check ${inputs.self} 2>&1 >nixpkgs-fmt.log
  if [ $? -ne 0 ]; then
    mkdir -p $out/nix-support
    cat nixpkgs-fmt.log > $out/nix-support/hydra-build-products
    echo "*** nixpkgs-fmt found files that haven't been formatted"
    echo "*** Please run \`nix run .#fix-nix-fmt\` and commit the changes"
    exit 1
  else
    echo 0 > $out
  fi
''
