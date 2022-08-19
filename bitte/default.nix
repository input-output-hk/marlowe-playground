{ marlowe-playground, web-ghc, docs, pkgs, sources }:
let
  staticSite = pkgs.callPackage (sources.plutus-apps + "/bitte/static-site.nix") { };
  playgroundStatic = pkgs.callPackage (sources.plutus-apps + "/bitte/playground-static.nix") { inherit staticSite; docs = docs.site; };
in
{
  web-ghc-server-entrypoint = pkgs.callPackage (sources.plutus-apps + "/bitte/web-ghc-server.nix") {
    web-ghc-server = web-ghc;
  };

  marlowe-playground-server-entrypoint = pkgs.callPackage (sources.plutus-apps + "/bitte/plutus-playground-server.nix") {
    variant = "marlowe";
    pkg = marlowe-playground.server;
  };
  marlowe-playground-client-entrypoint = playgroundStatic {
    client = marlowe-playground.client;
    variant = "marlowe";
  };
}
