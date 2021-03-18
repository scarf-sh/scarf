{ pkgs ? import <nixpkgs> {}
, haskellPackages ? pkgs.haskellPackages
}: haskellPackages.callCabal2nix "scarf" ./. {}
