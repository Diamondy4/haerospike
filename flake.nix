{
  description = "Haerospike - Haskell Aerospike client";

  outputs =
    inputs@{
      flake-parts,
      nixpkgs,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.git-hooks-nix.flakeModule
      ];
      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.aerospike-client-c.overlays.default
            ];
          };
          pre-commit.settings.hooks = {
            nixfmt-rfc-style.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            commitizen.enable = true;
          };
          haskellProjects.default = {
            devShell = {
              tools = hp: { inherit (hp) cabal-fmt; };
              mkShellArgs = {
                shellHook = ''
                  ${config.pre-commit.installationScript}
                '';
              };
            };
            settings = {
              haerospike = {
                check = false;
                extraLibraries = with pkgs; [
                  zlib.dev
                  aerospike-client-c
                ];
                custom = pkg: pkg.override { aerospike = pkgs.aerospike-client-c; };
              };
            };
          };
          packages.default = config.packages.haerospike;
        };
    };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";

    aerospike-client-c = {
      url = "github:Diamondy4/aerospike-client-c-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

}
