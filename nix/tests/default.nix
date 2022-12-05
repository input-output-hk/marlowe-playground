{ pkgs
, fixStylishHaskell
, purs-tidy
, fix-prettier
, fixPngOptimization
, src
, play-generated
}:
{
  shellcheck = pkgs.callPackage ./shellcheck.nix { inherit src; };
  stylishHaskell = pkgs.callPackage ./stylish-haskell.nix {
    inherit src fixStylishHaskell;
  };

  generated = pkgs.callPackage ./generated.nix {
    inherit src play-generated;
  };

  pursTidy = pkgs.callPackage ./purs-tidy.nix {
    inherit src purs-tidy;
  };

  prettier = pkgs.callPackage ./prettier.nix {
    inherit src fix-prettier;
  };

  nixpkgsFmt = pkgs.callPackage ./nixpkgs-fmt.nix {
    inherit src;
    inherit (pkgs) nixpkgs-fmt;
  };

  pngOptimization = pkgs.callPackage ./png-optimization.nix {
    inherit src fixPngOptimization;
  };

}
