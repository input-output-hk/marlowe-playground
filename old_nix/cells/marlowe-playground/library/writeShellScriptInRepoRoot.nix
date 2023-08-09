{ inputs, cell }:

let
  inherit (cell.library) pkgs;
in
name: script: pkgs.writeShellScriptBin name ''
  cd `${pkgs.git}/bin/git rev-parse --show-toplevel`
  ${script}
''

