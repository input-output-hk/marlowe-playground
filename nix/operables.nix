{ repoRoot, inputs, pkgs, system, lib }:

let

  inherit (pkgs) darkhttpd lib mailcap coreutils cacert z3;
  inherit (inputs.std.lib.ops) mkOperable;

in
{
  marlowe-playground-client = mkOperable {
    package = inputs.self.packages.marlowe-playground-client;
    runtimeInputs = [ darkhttpd ];
    runtimeScript = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${inputs.self.packages.marlowe-playground-client}}" --port 8080 --mimetypes ${mailcap}/etc/mime.types
    '';
  };
  marlowe-playground-server = mkOperable {
    package = inputs.self.packages.marlowe-playground-server;
    runtimeInputs = [ inputs.self.packages.ghc-with-marlowe coreutils cacert z3 ];
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
      mkdir -p /etc/ssl/certs/
      ln -s ${cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-bundle.crt
      ln -s ${cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt

      mkdir -p /tmp

      ${inputs.self.packages.marlowe-playground-server}/bin/marlowe-playground-server webserver
    '';
  };
}
