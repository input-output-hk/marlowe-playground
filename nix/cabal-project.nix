{
  modules = [{
    packages.marlowe-symbolic.ghcOptions = [ "-Werror" ];
    packages.marlowe-playground-server.ghcOptions = [ "-Werror" ];
    packages.plutus-script-utils.ghcOptions = [ "-Wwarn" "-Wno-unused-packages" ];
  }];
}

