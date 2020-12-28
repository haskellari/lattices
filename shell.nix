# this configuration is contrib, if it doesn't work anymore, please fix it and submit a PR
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
