{ inputs, system, ... }:
let 
  easy-purescript-nix = import inputs.iogx.inputs.easy-purescript-nix {};
in 
  easy-purescript-nix // { purs = easy-purescript-nix.purs-0_15_2; }