{ inputs }:
let
  inherit (inputs) self std nixpkgs bitte-cells;
  inherit (self) packages;
  inherit (nixpkgs) lib;
  inherit (nixpkgs.legacyPackages)
    jq
    sqitchPg
    postgresql
    coreutils
    writeShellScriptBin
    socat
    netcat
    curl
    darkhttpd
    mailcap
    ;
  inherit (inputs.bitte-cells._utils.packages) srvaddr;


  inherit (std.lib.ops) mkOperable;
  # TODO: monitoring
  probes = {
    livenessProbe = std.lib.ops.writeScript {
      name = "liveness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$HTTP_PORT/live"
      '';
    };
    readinessProbe = std.lib.ops.writeScript {
      name = "readiness-probe";
      runtimeInputs = [ curl ];
      text = ''
        [ -z "''${HTTP_PORT:-}" ] && echo "HTTP_PORT env var must be set -- aborting" && exit 1

        curl -f "http://localhost:$HTTP_PORT/ready"
      '';
    };
  };

  mkOperableWithProbes = args: mkOperable (args // probes);

in
{
  playground-client = mkOperable {
    package = packages."production-client";
    runtimeInputs = [ darkhttpd ];
    runtimeScript = ''
      exec darkhttpd "''${CONFIG_HTML_ROOT:-${packages.production-client}}" --port 8080 --mimetypes ${mailcap}/etc/mime.types
    '';
  };
  playground-server = mkOperable {
    package = packages."marlowe-playground-server:exe:marlowe-playground-server";
    runtimeInputs = [ packages.ghc-with-marlowe ];
    runtimeScript = ''
      #################
      # REQUIRED VARS #
      #################
      # FRONTEND_URL: TODO
      # GITHUB_CLIENT_ID TODO
      # GITHUB_CLIENT_SECRET TODO
      # JWT_SIGNATURE TODO
      # GITHUB_CALLBACK_PATH TODO
      # WEBGHC_URL TODO document and also create ticket to separate the container
      [ -z "''${FRONTEND_URL:-}" ] && echo "FRONTEND_URL env var must be set -- aborting" && exit 1


      ${packages."marlowe-playground-server:exe:marlowe-playground-server"}/bin/marlowe-playground-server \
        webserver
    '';
    # livenessProbe = std.lib.ops.writeScript {
    #   name = "liveness-probe";
    #   runtimeInputs = [ curl ];
    #   text = ''
    #     [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1

    #     curl -f "http://localhost:$PORT/healthcheck"
    #   '';
    # };
    # readinessProbe = std.lib.ops.writeScript {
    #   name = "readiness-probe";
    #   runtimeInputs = [ curl ];
    #   text = ''
    #     [ -z "''${PORT:-}" ] && echo "PORT env var must be set -- aborting" && exit 1

    #     curl -f "http://localhost:$PORT/healthcheck"
    #   '';
    # };
  };
}
