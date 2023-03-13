{
  description = "haerospike";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    aerospike-client-c = {
      url = "github:Diamondy4/aerospike-client-c-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', config, inputs', pkgs, lib, system, ... }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc926;
          overrides = hfinal: hprev:
            {
              haerospike = pkgs.haskell.lib.dontCheck (hprev.haerospike.override {
                zlib = pkgs.zlib;
                aerospike = inputs'.aerospike-client-c.packages.default;
              });
            };
        };
        packages.default = config.packages.haerospike;
      };
    };
}
