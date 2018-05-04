{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};
in
  {
    servant-cookbook = haskellPackages.callPackage ./servant-cookbook.nix {};
  }
