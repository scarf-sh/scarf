{ pkgs ? import <nixpkgs> { }
, haskellPackages ? pkgs.haskellPackages
}: let
  haskellPackagesNewAeson = haskellPackages.override {
    overrides = self: super: {
      aeson = self.callHackage "aeson" "1.5.2.0" {};
    };
  };
in haskellPackagesNewAeson.callCabal2nix "scarf" ./. {}
