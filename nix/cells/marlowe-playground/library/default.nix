{ inputs, cell }@block:
{
  cabal-install = import ./cabal-install.nix block;

  cabal-project = import ./cabal-project.nix block;

  ghc-compiler-nix-name = import ./ghc-compiler-nix-name.nix block;

  ghc = import ./ghc.nix block;

  haskell-language-server-project = import ./haskell-language-server-project.nix block;

  haskell-language-server = import ./haskell-language-server.nix block;

  haskell-nix = import ./haskell-nix.nix block;

  haskell-packages = import ./haskell-packages.nix block;

  hie-bios = import ./hie-bios.nix block;

  hlint = import ./hlint.nix block;

  nodejs-pkgs = import ./nodejs-pkgs.nix block;

  nodejs = import ./nodejs.nix block;

  npmlock2nix = import ./npmlock2nix.nix block;

  pkgs = import ./pkgs.nix block;

  pre-commit-check = import ./pre-commit-check.nix block;

  purescript = import ./purescript.nix block;

  stylish-haskell = import ./stylish-haskell.nix block;

  write-formatter = import ./write-formatter.nix block;

  writeShellScriptInRepoRoot = import ./writeShellScriptInRepoRoot.nix block;
}
