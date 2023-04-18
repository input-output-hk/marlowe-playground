{ inputs, cell }:

let
  inherit (cell.library) pkgs writeShellScriptInRepoRoot;
  inherit (builtins) concatStringsSep;
  xargs = "${pkgs.findutils}/bin/xargs";
  extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";
  writeFormatter = name: cmd: extensions: writeShellScriptInRepoRoot "fix-${name}" ''
    set -e
    echo formatting with ${name}
    ${pkgs.git}/bin/git ls-files ':!:bitte/node/config/*'\
      | grep -E '${extensionsToRegex extensions}' \
      | ${xargs} -d $'\\n' ${cmd}
    echo done.
  '';

in

writeFormatter
