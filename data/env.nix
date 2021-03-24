{ config-file }:
let
  # TODO: Where should the Scarf package set come from?
  pkgs = import <nixpkgs> { };

  config = builtins.fromJSON (builtins.readFile config-file);

  env = pkgs.buildEnv {
    name = "my-env";
    paths = map (package: pkgs.${package}) config.packages;
  };
in
env
