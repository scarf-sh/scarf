{ system ? builtins.currentSystem }:
let
  sources = import ./nix/sources.nix { };

  pkgs = import sources.nixpkgs { inherit system; };

  # Don't use the upstream nix code to speed-up evaluation. Trade updating
  # cargoSha256 to avoid evaluating nixpkgs once again.
  treefmt = pkgs.rustPlatform.buildRustPackage {
    pname = "treefmt";
    version = "unstable";
    src = sources.treefmt;
    cargoSha256 = "0cpkw2jny3m654x6jg04ajfyhsf2mprxy5fy9s1bb0wid6y741b7";
  };

  haskellPackages = pkgs.haskellPackages;

in
rec {
  scarf = haskellPackages.callCabal2nix "scarf" ./. { };
  dev-env = pkgs.lib.overrideDerivation scarf.env (orig: {
    nativeBuildInputs = (orig.nativeBuildInputs or [ ]) ++ [ pkgs.nixpkgs-fmt pkgs.ormolu treefmt haskellPackages.cabal-fmt ];
  });
}

