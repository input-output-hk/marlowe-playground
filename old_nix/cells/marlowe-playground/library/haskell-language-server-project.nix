{ inputs, cell }:

let
  inherit (cell.library) haskell-nix ghc-compiler-nix-name;
  inherit (inputs) haskell-language-server;

in
haskell-nix.cabalProject' {
  cabalProjectLocal = ''
    constraints: stylish-haskell==0.12.2.0, hlint==3.2.7
  '';

  sha256map = {
    "https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" = "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
    "https://github.com/haskell/lsp.git"."ef59c28b41ed4c5775f0ab0c1e985839359cec96" = "1whcgw4hhn2aplrpy9w8q6rafwy7znnp0rczgr6py15fqyw2fwb5";
  };

  src = haskell-language-server;

  compiler-nix-name = ghc-compiler-nix-name;

  modules = [{
    packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
  }];
}
