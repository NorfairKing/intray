{
  description = "intray";
  nixConfig = {
    extra-substituters = "https://intray.cachix.org";
    extra-trusted-public-keys = "intray.cachix.org-1:qD7I/NQLia2iy6cbzZvFuvn09iuL4AkTmHvjxrQlccQ=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    mergeless.url = "github:NorfairKing/mergeless";
    mergeless.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , weeder-nix
    , validity
    , safe-coloured-text
    , sydtest
    , opt-env-conf
    , fast-myers-diff
    , autodocodec
    , mergeless
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (mergeless + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
      pkgsMusl = pkgs.pkgsMusl;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.intrayRelease;
        static = pkgsMusl.intrayRelease;
      };
      apps.${system}.default = { type = "app"; program = "${pkgs.intrayReleasePackages.intray-cli}/bin/intray"; };
      lib.${system}.intrayNotification = pkgs.intrayNotification;
      checks.${system} = {
        release = self.packages.${system}.default;
        static = self.packages.${system}.static;
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "intray-cli"
            "intray-api"
            "intray-cli-data"
            "intray-client"
          ];
          coverage = [
            "intray-api-gen"
          ];
        };
        dependency-graph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph {
          packages = builtins.attrNames pkgs.haskellPackages.intrayPackages;
          inherit (pkgs) haskellPackages;
        };
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = builtins.attrNames pkgs.haskellPackages.intrayPackages;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            cabal2nix.enable = true;
            deadnix.enable = true;
            deadnix.excludes = [ ".*/default.nix" ];
            hlint.enable = true;
            hpack.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            ormolu.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "intray-shell";
        packages = p: builtins.attrValues p.intrayPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          zlib
          cabal-install
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system} = {
        serviceNotifications = import ./nix/service-notifications.nix { inherit (pkgsMusl.intrayRelease) notification; };
      };
      homeManagerModules.${system} = {
        default =
          builtins.trace "Note: use 'static' to get statically linked intray" (
            import ./nix/home-manager-module.nix {
              inherit (pkgs.intrayReleasePackages) intray-cli;
              inherit (pkgs.haskellPackages) opt-env-conf;
            }
          );
        static = import ./nix/home-manager-module.nix {
          inherit (pkgsMusl.intrayReleasePackages) intray-cli;
          inherit (pkgsMusl.haskellPackages) opt-env-conf;
        };
      };
      nix-ci = {
        auto-update = {
          enable = true;
          base = "development";
        };
        cachix = {
          name = "intray";
          public-key = "intray.cachix.org-1:qD7I/NQLia2iy6cbzZvFuvn09iuL4AkTmHvjxrQlccQ=";
        };
      };
    };
}
