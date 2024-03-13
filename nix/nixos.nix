self: { lib, config, pkgs, ... }:
let
  inherit (lib) mkOption types mapAttrs';

  playgroundOptions = { name, ... }: {
    options = {
      domain = mkOption {
        type = types.str;
        default = name;
        description = "The domain to host the playground";
      };

      jwt-signature-file = mkOption {
        type = types.path;
        description = "A file containing a JWT signing token";
      };

      github-client-id = mkOption {
        type = types.str;
        description = "The GitHub OAuth application client ID";
      };

      github-client-secret-file = mkOption {
        type = types.path;
        description = "A file containing the GitHub OAuth application client secret";
      };

      flake = mkOption {
        type = types.attrs;
        default = self;
        description = "A Nix Flake for the playground application";
      };
    };
  };

  cfg = config.marlowe.playgrounds;
in
{
  options = {
    marlowe.playgrounds = mkOption {
      type = types.attrsOf (types.submodule playgroundOptions);
      default = { };
      description = "Marlowe Playground instances to run";
    };
  };
  config = {
    http-services.static-sites = mapAttrs'
      (name: playground: {
        name = "${name}-frontend";
        value = {
          inherit (playground) domain;
          root = "${playground.flake.packages.x86_64-linux.marlowe-playground-client}/share/marlowe-playground-client/static";
          index-fallback = true;
        };
      })
      cfg;

    http-services.proxied-services = mapAttrs'
      (name: playground:
        let
          flakePkgs = playground.flake.packages.x86_64-linux;
        in
        {
          name = "${name}-backend";
          value = {
            inherit (playground) domain;
            prefix = "/api";
            systemdConfig = port: {
              description = "Marlowe Playground (${name})";
              path = builtins.attrValues {
                inherit (flakePkgs) ghc-with-marlowe;
                inherit (pkgs) jq z3;
              };
              preStart = ''
                jq -n \
                  --rawfile ghClientSecret $CREDENTIALS_DIRECTORY/ghcs \
                  --rawfile jwt $CREDENTIALS_DIRECTORY/jwt \
                  --arg ghClientId ${lib.escapeShellArg playground.github-client-id} \
                  --arg frontendUrl ${lib.escapeShellArg playground.domain} \
                  '{ "github-client-id": $ghClientId
                   , "github-client-secret": $ghClientSecret
                   , "jwt-signature": $jwt
                   , "frontend-url": $frontendUrl
                   , "github-cb-path": "/#/gh-oauth-cb"
                   }' > /tmp/config.json
              '';
              environment.WEBGHC_URL = "localhost:${toString port}";
              serviceConfig = {
                ExecSearchPath = "${flakePkgs.marlowe-playground-server}/bin";
                DynamicUser = true;
                LoadCredential = [
                  "ghcs:${playground.github-client-secret-file}"
                  "jwt:${playground.jwt-signature-file}"
                ];
                ExecStart = "marlowe-playground-server --config /tmp/config.json webserver --port ${toString port}";
              };
            };
          };
        })
      cfg;
  };
}
