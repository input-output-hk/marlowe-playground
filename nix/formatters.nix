# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#38-nixformattersnix

{
  shellcheck.enable = true;
  stylish-haskell.enable = false;
  nixpkgs-fmt.enable = true;
  prettier.enable = true;
  purs-tidy.enable = true;
}
