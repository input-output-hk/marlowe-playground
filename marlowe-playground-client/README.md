# Marlowe Playground Client

## Getting started

Make sure you have a local backend server running first:
```bash
[nix-shell] $ marlowe-playground-server
```

Check the [backend documentation](../marlowe-playground-server/README.md) for more information on how to setup the Github OAuth application.

Now we will build and run the front end:
```bash
[nix-shell] $ cd marlowe-playground-client
# Generate the purescript bridge files
[nix-shell] $ marlowe-playground-generate-purs
# Download javascript dependencies (we use ci to use the package-lock.json)
[nix-shell] $ npm ci
# Install purescript depdendencies
[nix-shell] $ npm run build:spago
# Precompile js dependencies bundle
[nix-shell] $ npm run build:webpack:dev:vendor
# Run aun auto-reloading dev build on http://localhost:8009
[nix-shell] $ npm run build:webpack:dev
```

## Adding dependencies

* Javascript dependencies are managed with npm, so add them to [package.json](./package.json)
* purescript uses package sets managed by spago so if the package set doesn't contain a dependency you can add it to [../packages.dhall](../packages.dhall)

Whenever you change `packages.dhall` you need to make sure that all dependencies can still properly be resolved and built.
You can do so using the `update-client-deps` script:

- Inside the nix-shell environment: `update-client-deps`
- Outside of the nix-shell environment (from the client directory): `$(nix-build -A plutus.updateClientDeps ../)/bin/update-client-deps`

The `update-client-deps` script will generate/update `.nix` files which have to be committed and are required for a successful CI run.

### NodeJS GitHub dependencies

All npm dependencies are handled by npmlock2nix automatically and transparently. The only exception to this rule are GitHub dependencies.
In order for these to work in restricted evaluation mode (which is what hydra uses) you have to specify the sha256 of the dependency you
want to use in your `buildNodeModules`. For example:

```
buildNodeModules {
    projectDir = ./.;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = {
      shmish111.nearley-webpack-loader."939360f9d1bafa9019b6ff8739495c6c9101c4a1" = "1brx669dgsryakf7my00m25xdv7a02snbwzhzgc9ylmys4p8c10x";
    };
}
```

You can add new dependencies with the sha256 set to `"0000000000000000000000000000000000000000000000000000"`. This will yield an error
message during the build with the actual hash value.

## Code formatting

The code is formatted using [purs-tidy](https://github.com/natefaubion/purescript-tidy), and there is a CI task that will fail if the code is not properly formatted. You can apply purs-tidy to the project by calling:

```bash
nix-shell shell.nix --run fix-purs-tidy
```

The code is formatted using [purs-tidy](https://github.com/natefaubion/purescript-tidy), and there is a CI task that will fail if the code is not properly formatted. You can apply purs-tidy to the project by calling:

## VSCode notes

In order to have the PureScript IDE working properly with this project you need to open this folder as the root folder.

### Custom Prelude

A custom prelude module called `Prologue` is available in web-common. It
exports everything from purescript-prelude, plus type and data constructors for
`Maybe`, `Either`, and `Tuple`, in addition to the `fst` and `snd` functions.
You can import this module instead of Prelude in your source files.
