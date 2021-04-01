{ packages-file }:
let
  # TODO: Where should the Scarf package set come from?
  pkgs = import <nixpkgs> { };

  # TODO: Initial fetchgit is super slow with no outputs :(
  make-nixpkgs = rev: if rev == null then pkgs else
  import
    (builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      ref = rev;
    })
    { };

  packages = builtins.fromJSON (builtins.readFile packages-file);

  env = pkgs.buildEnv {
    name = "my-env";
    paths = map
      (package:
        let
          pkgs' = make-nixpkgs (package.rev or null);
          name = package.name or package;
        in
        pkgs'.${name})
      packages;
  };
in
env
