{ packages-file }:
let
  # TODO: Where should the Scarf package set come from?
  pkgs = import <nixpkgs> { };

  packages = builtins.fromJSON (builtins.readFile packages-file);

  env = pkgs.buildEnv {
    name = "my-env";
    paths = map (package: pkgs.${package}) packages;
  };
in
env
