{
  shellcheck.enable = true;
  stylish-haskell.enable = true;
  nixpkgs-fmt.enable = true;
  prettier.enable = true; 
  #types_or = [ "javascript" "css" "html" ]; TODO 
  purs-tidy.enable = true;
  #  = { TODO 
  #   enable = true;
  #   name = "purs-tidy";
  #   description = "Ensure PureScript files are formatted";
  #   entry = "${purescript.purs-tidy}/bin/purs-tidy format-in-place";
  #   files = "\\.purs$";
  #   language = "system";
  # };
}
