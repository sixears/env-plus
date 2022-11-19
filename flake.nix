{
  description = "Utilities for working with the system environment";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    base1t.url       = github:sixears/base1t/r0.0.5.30;
    tasty-plus.url   = github:sixears/tasty-plus/r1.5.2.20;
  };

  outputs = { self, nixpkgs, build-utils
            , base1t, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "env-plus" {
      deps = {
        inherit base1t tasty-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
