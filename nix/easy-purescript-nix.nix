{ repoRoot, inputs, pkgs, system, lib }:
let
  easy-purescript-nix = pkgs.callPackage inputs.iogx.inputs.easy-purescript-nix { };
in

easy-purescript-nix // { purs = easy-purescript-nix.purs-0_15_2; }
