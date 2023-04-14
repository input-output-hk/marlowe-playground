{ inputs, cell }:
let
  inherit (cell.library) pkgs;
  inherit (pkgs) lib stdenv;
  inherit (cell) operables;
  inherit (inputs.std.lib.ops) mkStandardOCI;

  mkImage = { name, description }:
    mkStandardOCI {
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

in
lib.attrsets.optionalAttrs stdenv.isLinux {
  marlowe-playground-server = mkImage {
    name = "marlowe-playground-server";
    description = "The backend of the Marlowe playground.";
  };
  marlowe-playground-client = mkImage {
    name = "marlowe-playground-client";
    description = "An HTTP server that serves the client for the Marlowe Playground.";
  };
}
