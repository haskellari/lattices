# this configuration is contrib, if it doesn't work anymore, please fix it and submit a PR
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./lattices.nix { }
