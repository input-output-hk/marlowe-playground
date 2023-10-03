# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixcabal-projectnix

{
  modules = [{
    packages.marlowe-symbolic.ghcOptions = [ "-Werror" ];
    packages.marlowe-playground-server.ghcOptions = [ "-Werror" ];
    packages.plutus-script-utils.ghcOptions = [ "-Wwarn" "-Wno-unused-packages" ];
  }];
}

