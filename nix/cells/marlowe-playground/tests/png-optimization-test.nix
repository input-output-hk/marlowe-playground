{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (cell.scripts) fix-png-optimization;

  # just haskell sources and the stylish-haskell config file
  src = pkgs.lib.sourceFilesBySuffices inputs.self [ ".png" ];
in
pkgs.runCommand "png-optimization-check" { buildInputs = [ fix-png-optimization pkgs.diffutils pkgs.glibcLocales ]; } ''
  set +e
  cp -a ${src} orig
  cp -a ${src} png
  chmod -R +w png
  cd png
  fix-png-optimization
  cd ..
  diff --brief --recursive orig png > /dev/null
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    mkdir -p $out/nix-support
    diff -ur orig png > $out/png.diff
    echo "file none $out/png.diff" > $out/nix-support/hydra-build-products
    echo "*** optipng found changes that need addressed first"
    echo "*** Please run \`nix run .#fixPngOptimization\` and commit changes."
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi
''
