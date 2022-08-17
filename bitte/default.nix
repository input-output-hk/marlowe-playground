{ marlowe-playground, web-ghc, docs, pkgs, sources }:
let
  staticSite = pkgs.callPackage (sources.plutus-apps + "/bitte/static-site.nix") { };
  playgroundStatic = pkgs.callPackage (sources.plutus-apps + "/bitte/playground-static.nix") { inherit staticSite; docs = docs.site; };
  wait-for-socket = pkgs.writeShellScriptBin "wait-for-socket" ''
    set -eEuo pipefail

    export PATH="${pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.socat ]}"

    sock_path="$1"
    delay_iterations="''${2:-8}"

    for ((i=0;i<delay_iterations;i++))
    do
      if socat -u OPEN:/dev/null "UNIX-CONNECT:''${sock_path}"
      then
        exit 0
      fi
      let delay=2**i
      echo "Connecting to ''${sock_path} failed, sleeping for ''${delay} seconds" >&2
      sleep "''${delay}"
    done

    socat -u OPEN:/dev/null "UNIX-CONNECT:''${sock_path}"
  '';

  sleep-until-restart-slot = pkgs.writeShellScriptBin "sleep-until-restart-slot" ''
    set -eEuo pipefail

    export PATH="${pkgs.coreutils}/bin"

    nowHour=$(date -u +%-H)
    hoursLeft=$((3 - (nowHour % 3)))
    wakeHour=$(((nowHour + hoursLeft) % 24))
    exec sleep $((($(date -u -f - +%s- <<< "$wakeHour"$':00 tomorrow\nnow')0)%86400))
  '';

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
  node-socat = pkgs.callPackage ./node-socat.nix {
    inherit wait-for-socket;
  };
}
