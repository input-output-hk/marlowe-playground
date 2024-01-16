# Marlowe Playground Client

## Getting started

Make sure you have a local backend server running first:

```bash
[nix-shell] $ start-backend
```

Check the [backend documentation](../marlowe-playground-server/README.md) for more information on how to setup the Github OAuth application.

Now we will build and run the front end:

```bash
[nix-shell] $ cd marlowe-playground-client
# Generate the purescript bridge files
[nix-shell] $ generate-purescript
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

- Javascript dependencies are managed with npm, so add them to [package.json](./package.json)
- Purescript uses package sets managed by spago so if the package set doesn't contain a dependency you can add it to [../packages.dhall](../packages.dhall)

Whenever you change `packages.dhall` or `packages.json` you must also run the following commands inside the nix shell:
```bash
gen-nix-lockfiles
generate-purescript
``` 

## Code formatting

The code is formatted using [purs-tidy](https://github.com/natefaubion/purescript-tidy), and there is a CI task that will fail if the code is not properly formatted. You can apply purs-tidy to the project by calling:

```bash
pre-commit run purs-tidy
```

## VSCode notes

In order to have the PureScript IDE working properly with this project you need to open this folder as the root folder.

### Custom Prelude

A custom prelude module called `Prologue` is available in web-common. It
exports everything from purescript-prelude, plus type and data constructors for
`Maybe`, `Either`, and `Tuple`, in addition to the `fst` and `snd` functions.
You can import this module instead of Prelude in your source files.
