{ inputs, cell }@block:
{
  assemble-changelog = import ./assemble-changelog.nix block;

  fix-nix-fmt = import ./fix-nix-fmt.nix block;

  fix-png-optimization = import ./fix-png-optimization.nix block;

  fix-prettier = import ./fix-prettier.nix block;

  fix-purs-tidy = import ./fix-purs-tidy.nix block;

  fix-stylish-haskell = import ./fix-stylish-haskell.nix block;

  generate-purescript = import ./generate-purescript.nix block;

  start-backend = import ./start-backend.nix block;

  update-client-deps = import ./update-client-deps.nix block;
}
