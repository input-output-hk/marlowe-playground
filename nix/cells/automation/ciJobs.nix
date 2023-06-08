{ inputs, cell }:

let


  inherit (inputs.cells.marlowe-playground) library;
  inherit (library) pkgs;
  inherit (pkgs.stdenv) system;
  inherit (pkgs) lib;

  make-haskell-jobs = project:
    let
      packages = library.haskell-nix.haskellLib.selectProjectPackages project.hsPkgs;
    in
    {
      exes = library.haskell-nix.haskellLib.collectComponents' "exes" packages;
      tests = library.haskell-nix.haskellLib.collectComponents' "tests" packages;
      benchmarks = library.haskell-nix.haskellLib.collectComponents' "benchmarks" packages;
      libraries = library.haskell-nix.haskellLib.collectComponents' "library" packages;
      checks = library.haskell-nix.haskellLib.collectChecks' packages;
      roots = project.roots;
      plan-nix = project.plan-nix;
    };

  jobs = make-haskell-jobs library.cabal-project
    // inputs.cells.marlowe-playground.packages
    // inputs.cells.marlowe-playground.scripts
    // inputs.cells.marlowe-playground.tests
    // { devShells = inputs.cells.marlowe-playground.devshells; }
    // { operables = inputs.cells.marlowe-playground.operables; }
    // { oci-images = inputs.cells.marlowe-playground.oci-images; };

  # Hydra doesn't like these attributes hanging around in "jobsets": it thinks they're jobs!
  filtered-jobs = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations") jobs;

  required-job = pkgs.releaseTools.aggregate {
    name = "required-marlowe-playground";
    meta.description = "All jobs required to pass CI";
    constituents = lib.collect lib.isDerivation filtered-jobs;
  };

  final-jobset =
    # TODO: Fix issue with darwin builds described here: https://ci.zw3rk.com/eval/37438#tabs-errors
    # if system == "x86_64-linux" || system == "x86_64-darwin" then
    if system == "x86_64-linux" then
      filtered-jobs // { required = required-job; }
    else { };

in

final-jobset
