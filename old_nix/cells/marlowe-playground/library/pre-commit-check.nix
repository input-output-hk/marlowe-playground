{ inputs, cell }:

let
  inherit (cell.library) pkgs stylish-haskell purescript;
  inherit (cell.packages) nixpkgs-fmt;

in

inputs.pre-commit-hooks.lib.run {

  src = pkgs.lib.cleanSource inputs.self;

  tools = {
    inherit (pkgs) shellcheck;
    inherit (pkgs.nodePackages) prettier;
    inherit stylish-haskell nixpkgs-fmt;
  };

  hooks = {
    shellcheck.enable = true;
    stylish-haskell.enable = true;
    nixpkgs-fmt.enable = true;

    prettier = {
      enable = true;
      types_or = [ "javascript" "css" "html" ];
    };

    png-optimization = {
      enable = true;
      name = "png-optimization";
      description = "Ensure that PNG files are optimized";
      entry = "${pkgs.optipng}/bin/optipng";
      files = "\\.png$";
    };

    purs-tidy-hook = {
      enable = true;
      name = "purs-tidy";
      description = "Ensure PureScript files are formatted";
      entry = "${purescript.purs-tidy}/bin/purs-tidy format-in-place";
      files = "\\.purs$";
      language = "system";
    };
  };
}
