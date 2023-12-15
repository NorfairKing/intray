{
  description = "intray";
  nixConfig = {
    extra-substituters = "https://intray.cachix.org";
    extra-trusted-public-keys = "intray.cachix.org-1:qD7I/NQLia2iy6cbzZvFuvn09iuL4AkTmHvjxrQlccQ=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    home-manager.url = "github:nix-community/home-manager?ref=release-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    haskell-dependency-graph-nix.url = "github:NorfairKing/haskell-dependency-graph-nix";
    haskell-dependency-graph-nix.inputs.nixpkgs.follows = "nixpkgs";
    haskell-dependency-graph-nix.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
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
    mergeless.url = "github:NorfairKing/mergeless";
    mergeless.flake = false;
    yesod-autoreload.url = "github:NorfairKing/yesod-autoreload";
    yesod-autoreload.flake = false;
    yesod-static-remote.url = "github:NorfairKing/yesod-static-remote";
    yesod-static-remote.flake = false;
    openapi-code-generator.url = "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator";
    openapi-code-generator.flake = false;
    linkcheck.url = "github:NorfairKing/linkcheck";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck";
    seocheck.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , haskell-dependency-graph-nix
    , validity
    , safe-coloured-text
    , sydtest
    , fast-myers-diff
    , autodocodec
    , mergeless
    , yesod-autoreload
    , yesod-static-remote
    , openapi-code-generator
    , linkcheck
    , seocheck
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (mergeless + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (yesod-autoreload + "/nix/overlay.nix"))
          (import (yesod-static-remote + "/nix/overlay.nix"))
          (import (openapi-code-generator + "/nix/overlay.nix"))
          (import (linkcheck + "/nix/overlay.nix"))
          (import (seocheck + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix { inherit (pkgs.intrayReleasePackages) intray-server intray-web-server; };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.intrayRelease;
        generatedIntrayStripeCode = pkgs.generatedIntrayStripeCode;
      };
      apps.${system}.default = { type = "app"; program = "${pkgs.intrayReleasePackages.intray-cli}/bin/intray"; };
      checks.${system} = {
        coverage-report = pkgs.dekking.makeCoverageReport {
          name = "test-coverage-report";
          packages = [
            "intray-cli"
            "intray-web-server"
            "intray-api"
            "intray-cli-data"
            "intray-client"
            "intray-data"
          ];
          coverables = [
            "intray-server"
          ];
          coverage = [
            "intray-api-gen"
            "intray-data-gen"
            "intray-server-gen"
          ];
        };
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit pkgs;
          home-manager = home-manager.nixosModules.home-manager;
          intray-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          intray-home-manager-module = self.homeManagerModules.${system}.default;
        };
        dependency-graph = haskell-dependency-graph-nix.lib.${system}.makeDependencyGraph {
          packages = builtins.attrNames pkgs.haskellPackages.intrayPackages;
          inherit (pkgs) haskellPackages;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "intray-shell";
        packages = p: builtins.attrValues p.intrayPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system} = {
        default = mkNixosModule { envname = "production"; };
        serviceNotifications = import ./nix/service-notifications.nix { inherit (pkgs) intrayRelease; };
      };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { intrayReleasePackages = pkgs.intrayReleasePackages; };
      nix-ci.cachix = {
        name = "intray";
        public-key = "intray.cachix.org-1:qD7I/NQLia2iy6cbzZvFuvn09iuL4AkTmHvjxrQlccQ=";
      };
    };
}
