# haskell.nix:pkgs
  pkgs = import inputs.nixpkgs.path {

    config = inputs.haskell-nix.config // {
      # This is required by SASS, which we should move away from!
      permittedInsecurePackages = [ "python-2.7.18.6" ];
    };

# purs-tidy
write-formatter "purs-tidy" "${purs-tidy}/bin/purs-tidy format-in-place" [ "purs" ]
