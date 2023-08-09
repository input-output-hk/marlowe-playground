{ inputs, cell }:

let
  inherit (cell.library) cabal-project haskell-nix ghc-compiler-nix-name pkgs;
  inherit (pkgs) lib;

  project = haskell-nix.hackage-project {
    name = "cabal-install";
    version = "3.6.2.0";
    compiler-nix-name = ghc-compiler-nix-name;
    index-state =
      let
        parseIndexState = rawCabalProject:
          let
            indexState = lib.lists.concatLists (
              lib.lists.filter (l: l != null)
                (map (l: builtins.match "^index-state: *(.*)" l)
                  (lib.splitString "\n" rawCabalProject)));
          in
          lib.lists.head (indexState ++ [ null ]);
      in
      parseIndexState (builtins.readFile (inputs.self + "/cabal.project"));

    configureArgs = "--disable-tests";
  };

in

project.hsPkgs.cabal-install.components.exes.cabal
