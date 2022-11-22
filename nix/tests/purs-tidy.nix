{ runCommand, purs-tidy, src, lib }:
let
  # just purescript sources
  src' = lib.cleanSourceWith {
    inherit src;
    filter = with lib;
      name: type:
        let baseName = baseNameOf (toString name); in
        (
          baseName == ".tidyoperators" ||
          baseName == ".tidyrc.json" ||
          (type == "regular" && hasSuffix ".purs" baseName) ||
          type == "directory"
        );
  };
in
runCommand "purs-tidy-check"
{
  buildInputs = [ purs-tidy ];
} ''
  set +e
  echo ${toString src'}
  cp -a ${src'} orig
  cp -a ${src'} purs-tidy
  chmod -R +w purs-tidy
  cd purs-tidy
  ${purs-tidy}/bin/purs-tidy format-in-place *
  cd ..
  diff --brief --recursive orig purs-tidy
  EXIT_CODE=$?
  if [[ $EXIT_CODE != 0 ]]
  then
    echo "*** purs-tidy found changes that need addressed first"
    echo "*** Please run \`nix run .#fix-purs-tidy\` and commit changes"
    exit $EXIT_CODE
  else
    echo $EXIT_CODE > $out
  fi

''
