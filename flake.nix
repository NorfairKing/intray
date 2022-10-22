{
  description = "intray";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    mergeless.url = "github:NorfairKing/mergeless?ref=flake";
    mergeless.flake = false;
    linkcheck.url = "github:NorfairKing/linkcheck";
    linkcheck.flake = false;
    seocheck.url = "github:NorfairKing/seocheck";
    seocheck.flake = false;
    openapi-code-generator.url = "github:Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator?ref=flake";
    openapi-code-generator.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , flake-utils
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , mergeless
    , openapi-code-generator
    , linkcheck
    , seocheck
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            self.overlays.${system}
            (import (autodocodec + "/nix/overlay.nix"))
            (import (safe-coloured-text + "/nix/overlay.nix"))
            (import (sydtest + "/nix/overlay.nix"))
            (import (mergeless + "/nix/overlay.nix"))
            (import (validity + "/nix/overlay.nix"))
            (import (openapi-code-generator + "/nix/overlay.nix"))
            (import (linkcheck + "/nix/overlay.nix"))
            (import (seocheck + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;

      in
      {
        overlays = import ./nix/overlay.nix;
        packages.release = pkgs.intrayRelease;
        packages.default = self.packages.${system}.release;
        checks = {
          # nixos-module-test = import ./nix/nixos-module-test.nix {
          #   inherit pkgs;
          #   home-manager = home-manager.nixosModules.home-manager;
          #   intray-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          #   intray-home-manager-module = self.homeManagerModules.${system}.default;
          # };
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
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "intray-shell";
          packages = (p:
            (builtins.attrValues p.intrayPackages)
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
        nixosModuleFactories.default = import ./nix/nixos-module.nix;
        homeManagerModules.default = import ./nix/home-manager-module.nix { intrayReleasePackages = pkgs.intrayReleasePackages; };
      });
}
