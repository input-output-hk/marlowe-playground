{ inputs }:
let
  inherit (inputs) std self nixpkgs;
  inherit (nixpkgs.lib) mapAttrsToList;
  inherit (nixpkgs.lib.strings) concatMapStrings;
  inherit (self) operables;

  mkImage = { name, description }:
    std.lib.ops.mkStandardOCI {
      inherit name;
      tag = "latest";
      operable = operables.${name};
      uid = "0";
      gid = "0";
      labels = {
        inherit description;
        source = "https://github.com/input-output-hk/marlowe-playground";
        license = "Apache-2.0";
      };
    };

  images = {
    playground-server = mkImage {
      name = "playground-server";
      description = "The backend of the Marlowe playground.";
    };
    playground-client = mkImage {
      name = "playground-client";
      description = "An HTTP server that serves the static client.";
    };
  };

  forAllImages = f: concatMapStrings (s: s + "\n") (mapAttrsToList (_: f) images);
in
images // {
  all = {
    copyToDockerDaemon = std.lib.ops.writeScript {
      name = "copy-to-docker-daemon";
      # text = forAllImages (img: std.blockTypes.containers.load);
      text = forAllImages (img: "${img.copyToDockerDaemon}/bin/copy-to-docker-daemon");
    };
  };
}
