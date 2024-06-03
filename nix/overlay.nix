final: prev:
with final.lib;
with final.haskell.lib;

{

  intrayNotification = final.callPackage ./notification.nix { };

  intrayRelease = final.symlinkJoin {
    name = "intray-release";
    paths = attrValues final.intrayReleasePackages;
    passthru = {
      notification = final.intrayNotification;
    } // final.intrayReleasePackages;
  };

  intrayReleasePackages =
    let
      enableStatic = pkg:
        if final.stdenv.hostPlatform.isMusl
        then
          overrideCabal pkg
            (old:
              let
                # Until https://github.com/NixOS/nixpkgs/pull/311411
                terminfoDirs = final.lib.concatStringsSep ":" [
                  "/etc/terminfo" # Debian, Fedora, Gentoo
                  "/lib/terminfo" # Debian
                  "/usr/share/terminfo" # upstream default, probably all FHS-based distros
                  "/run/current-system/sw/share/terminfo" # NixOS
                ];
                staticNcurses = (
                  (final.ncurses.override {
                    enableStatic = true;
                  })
                ).overrideAttrs
                  (old: {
                    configureFlags = (old.configureFlags or [ ]) ++ [
                      "--with-terminfo-dirs=${terminfoDirs}"
                    ];
                  });
              in
              {
                configureFlags = (old.configureFlags or [ ]) ++ [
                  "--ghc-option=-optl=-static"
                  # Static
                  "--extra-lib-dirs=${final.gmp6.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${final.libffi.overrideAttrs (_: { dontDisableStatic = true; })}/lib"
                  "--extra-lib-dirs=${final.zlib.static}/lib"
                  "--extra-lib-dirs=${staticNcurses}}/lib" # for -ltinfo
                ];
                enableSharedExecutables = false;
                enableSharedLibraries = false;

                postInstall = (old.postInstall or "") + ''
                  for b in $out/bin/*
                  do
                    if ldd "$b"
                    then
                      echo "ldd succeeded on $b, which may mean that it is not statically linked"
                      exit 1
                    fi
                  done
                '';
              })
        else pkg;
    in
    builtins.mapAttrs
      (_: pkg: justStaticExecutables (enableStatic pkg))
      final.haskellPackages.intrayPackages;

  sqlite =
    if final.stdenv.hostPlatform.isMusl
    then prev.sqlite.overrideAttrs (_: { dontDisableStatic = true; })
    else prev.sqlite;

  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides = composeExtensions (old.overrides or (_: _: { })) (
        self: super:
          let
            intrayPkg = name: overrideCabal (self.callPackage (../${name}/default.nix) { }) (old: {
              doBenchmark = true;
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
              # Turn off tests by default, only turn them on for the coverage report.
              doCheck = false;
              hyperlinkSource = false;
              enableLibraryProfiling = false;
              enableExecutableProfiling = false;

              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            });
            intrayPkgWithComp =
              exeName: name:
              self.generateOptparseApplicativeCompletions [ exeName ] (intrayPkg name);
            intrayPackages = {
              "intray-api" = intrayPkg "intray-api";
              "intray-api-gen" = intrayPkg "intray-api-gen";
              "intray-cli" = intrayPkgWithComp "intray" "intray-cli";
              "intray-cli-data" = intrayPkg "intray-cli-data";
              "intray-client" = intrayPkg "intray-client";
            };
          in
          {
            # Not actually broken
            servant-auth-server = unmarkBroken super.servant-auth-server;

            inherit intrayPackages;
          } // intrayPackages
      );
    });
}
