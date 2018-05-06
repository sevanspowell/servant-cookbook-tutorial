{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  pkgsTmp = if compiler == "default"
         then pkgs.haskellPackages
         else pkgs.haskell.packages.${compiler};
  haskellPackages = pkgsTmp.extend (self: super: {
    #jose = self.callHackage "jose" "0.5.0.0" {};
    servant-auth-server = self.callHackage "servant-auth-server" "0.3.1.0" {};
  });

in
  {
    servant-cookbook = haskellPackages.callPackage ./servant-cookbook.nix {};
  }
