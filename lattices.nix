# this configuration is contrib, if it doesn't work anymore, please fix it and submit a PR
{ haskellPackages, lib }:
haskellPackages.callCabal2nix "lattices" (lib.cleanSource ./.)  {}
