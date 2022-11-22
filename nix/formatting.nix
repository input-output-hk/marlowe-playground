{ writeShellScriptBinInRepoRoot
, pkgs
, easyPS
}:
let
  inherit (builtins) concatStringsSep;
  inherit (pkgs.nodePackages) prettier;
  inherit (easyPS) purs-tidy;
  xargs = "${pkgs.findutils}/bin/xargs";
  extensionsToRegex = extensions: "\\.(${concatStringsSep "|" extensions})";
  writeFormatter = name: cmd: extensions: writeShellScriptBinInRepoRoot "fix-${name}" ''
    set -e
    echo formatting with ${name}
    ${pkgs.git}/bin/git ls-files ':!:bitte/node/config/*'\
      | grep -E '${extensionsToRegex extensions}' \
      | ${xargs} -d $'\\n' ${cmd}
    echo done.
  '';
in
{
  fix-prettier = writeFormatter
    "prettier"
    "${prettier}/bin/prettier -w"
    [ "js" "ts" "css" "html" ];
  fix-purs-tidy = writeFormatter
    "purs-tidy"
    "${purs-tidy}/bin/purs-tidy format-in-place"
    [ "purs" ];
  fix-nix-fmt = writeFormatter
    "nixfmt"
    "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt"
    [ "nix" ];
  purs-tidy-hook = {
    enable = true;
    name = "purs-tidy";
    entry = "${purs-tidy}/bin/purs-tidy format-in-place";
    files = "\\.purs$";
    language = "system";
  };
}
