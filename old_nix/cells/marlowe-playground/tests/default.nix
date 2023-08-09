{ inputs, cell }@block:
{
  generated-test = import ./generated-test.nix block;

  nixpkgs-fmt-test = import ./nixpkgs-fmt-test.nix block;

  png-optimization-test = import ./png-optimization-test.nix block;

  prettier-test = import ./prettier-test.nix block;

  purs-tidy-test = import ./purs-tidy-test.nix block;

  shellcheck-test = import ./shellcheck-test.nix block;

  stylish-haskell-test = import ./stylish-haskell-test.nix block;
}
