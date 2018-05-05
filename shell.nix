{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
(import ./default.nix { inherit nixpkgs compiler; }).servant-cookbook.env
