{
  description = "Working though 'Effective Haskell' by Rebecca Skinner";

  inputs = {
    emacs-overlay = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-23.05";
    pre-commit-hooks = {
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/pre-commit-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        inputs.treefmt-nix.flakeModule
        ./Chapter08/hcat/flake-module.nix
        ./Chapter13/har/flake-module.nix
      ];

      systems = [
        "x86_64-linux"
      ];

      perSystem = { config, pkgs, self', system, ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
          ];
          inherit system;
        };

        devShells = {
          default = pkgs.mkShell {
            inherit (self'.devShells.haskell) FONTCONFIG_FILE;

            inputsFrom = [
              self'.devShells.haskell
            ];
          };

          haskell = with pkgs; mkShell {
            FONTCONFIG_FILE = makeFontsConf {
              fontDirectories = [
                (nerdfonts.override { fonts = [ "Iosevka" ]; })
              ];
            };

            inputsFrom = [
              config.pre-commit.devShell
            ];

            nativeBuildInputs = [
              (
                emacsWithPackagesFromUsePackage {
                  alwaysEnsure = true;
                  config = ./emacs.el;
                }
              )
              cabal-install
              ghc
              ghcid
              haskell-language-server
              rnix-lsp
            ] ++ (with haskellPackages; [
              hpack
              hlint
              ormolu
              pointfree
            ]);
          };
        };

        pre-commit.settings.hooks = {
          treefmt.enable = true;
        };

        treefmt = {
          projectRootFile = ./flake.nix;
          programs = {
            deadnix.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu = {
              enable = true;
              ghcOpts = [
                "OverloadedStrings"
                "TemplateHaskell"
              ];
            };
          };
        };
      };
    };
}
