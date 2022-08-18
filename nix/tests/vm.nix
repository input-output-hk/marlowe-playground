{ pkgs
, marlowe-playground
, web-ghc
, docs
, vmCompileTests
, sources
}:
let
  inherit (pkgs.stdenv) isDarwin;
  testing = import (pkgs.path + "/nixos/lib/testing-python.nix") { system = builtins.currentSystem; };
  makeTest = testing.makeTest;
  tests = pkgs.recurseIntoAttrs {
    marlowe-playground-server = pkgs.callPackage ./vm-tests/marlowe-playground.nix { inherit makeTest marlowe-playground; };
    web-ghc = pkgs.callPackage ./vm-tests/web-ghc.nix { inherit makeTest web-ghc sources; };
    all = pkgs.callPackage ./vm-tests/all.nix { inherit makeTest marlowe-playground web-ghc docs vmCompileTests sources; };
  };
in
if isDarwin then { } else tests
