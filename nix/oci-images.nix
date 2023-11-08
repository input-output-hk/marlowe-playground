{ repoRoot, inputs, pkgs, system, lib }:
let
  inherit (inputs.std.lib.ops) mkStandardOCI;

  mkImage = { name, description }:
    mkStandardOCI {
      inherit name;
      # meta.tags = [ "latest" ];
      operable = inputs.self.operables.${name};
      uid = "0";
      gid = "0";
      labels = {
        inherit description;
        source = "https://github.com/input-output-hk/marlowe-playground";
        license = "Apache-2.0";
      };
    };

  images = {
    marlowe-playground-server = mkImage {
      name = "marlowe-playground-server";
      description = "The backend of the Marlowe playground.";
    };
    marlowe-playground-client = mkImage {
      name = "marlowe-playground-client";
      description = "An HTTP server that serves the client for the Marlowe Playground.";
    };
  };

  forAllImages = f: lib.concatMapStrings (s: s + "\n") (lib.mapAttrsToList f images);

in
images // {
  all = {
    copyToDockerDaemon = inputs.std.lib.ops.writeScript {
      name = "copy-to-docker-daemon";
      text = forAllImages (name: img:
        "${inputs.n2c.packages.skopeo-nix2container}/bin/skopeo --insecure-policy copy nix:${img} docker-daemon:${name}:latest"
      );
    };
  };
}
