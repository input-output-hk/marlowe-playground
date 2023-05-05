{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (cell.packages) marlowe-playground-client marlowe-playground-server ghc-with-marlowe;
  inherit (pkgs) darkhttpd lib mailcap coreutils cacert;
  inherit (inputs.std.lib.ops) mkOperable;

in
{
  marlowe-playground-client = mkOperable {
    package = marlowe-playground-client;
    runtimeInputs = [ darkhttpd ];
    runtimeScript = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${marlowe-playground-client}}" --port 8080 --mimetypes ${mailcap}/etc/mime.types
    '';
  };
  marlowe-playground-server = mkOperable {
    package = marlowe-playground-server;
    runtimeInputs = [ ghc-with-marlowe coreutils cacert ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # FRONTEND_URL: The URL of the frontend server, for oauth redirection.

      #################
      # OPTIONAL VARS #
      #################
      # GITHUB_CLIENT_ID: The Client ID for the GitHub OAuth App
      # GITHUB_CLIENT_SECRET: The Client secret for the GitHub OAuth App
      # JWT_SIGNATURE: A secret signature for signing JWTs
      # GITHUB_CALLBACK_PATH: A callback path for GitHub OAuth
      # WEBGHC_URL: The URL of the WebGHC server.

      [ -z "''${FRONTEND_URL:-}" ] && echo "FRONTEND_URL env var must be set -- aborting" && exit 1

      # shellcheck source=/dev/null
      source ${cacert}/nix-support/setup-hook

      mkdir -p /tmp

      ${marlowe-playground-server}/bin/marlowe-playground-server webserver
    '';
  };
}
