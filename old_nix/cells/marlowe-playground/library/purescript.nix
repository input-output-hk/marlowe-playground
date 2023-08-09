{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (inputs) easy-purescript-nix;
  p = pkgs.callPackage (easy-purescript-nix) { };
in
p // { purs = p.purs-0_15_2; }
