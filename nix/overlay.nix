final: prev:
with final.lib;
with final.haskell.lib;

let
  stripe-spec = builtins.fetchGit {
    url = "https://github.com/stripe/openapi";
    rev = "c48cf54aab65f4966ba285bdfaf86ed52f5fb70c";
  };
  generatedStripe = final.generateOpenAPIClient {
    name = "intray-stripe-client";
    configFile = ../stripe-client-gen.yaml;
    src = stripe-spec + "/openapi/spec3.yaml";
  };

in
{


  intrayReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.haskellPackages.intrayPackages;

  intrayRelease = final.symlinkJoin {
    name = "intray-release";
    paths = attrValues final.intrayReleasePackages;
    passthru = {
      notification = final.intrayNotification;
    };
  };

  intrayNotification = final.callPackage ./notification.nix { };

  generatedIntrayStripeCode = generatedStripe;

  haskellPackages =
    prev.haskellPackages.override (old: {
      overrides =
        composeExtensions (old.overrides or (_: _: { })) (
          self: super:
            let
              generatedStripePackage = self.callPackage (generatedStripe + "/default.nix") { };
              intrayPkg = name:
                overrideCabal
                  (self.callPackage
                    (../${name}/default.nix)
                    { }
                  )
                  (old: {
                    doBenchmark = true;
                    doHaddock = false;
                    doCoverage = false;
                    doHoogle = false;
                    doCheck = false; # Only check the release version.
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
                    buildDepends = (old.buildDepends or [ ]) ++ [
                      final.haskellPackages.autoexporter
                    ];
                    # Ugly hack because we can't just add flags to the 'test' invocation.
                    # Show test output as we go, instead of all at once afterwards.
                    testTarget = (old.testTarget or "") + " --show-details=direct";
                  });
              intrayPkgWithComp =
                exeName: name:
                generateOptparseApplicativeCompletion exeName (intrayPkg name);
              intrayPkgWithOwnComp = name: intrayPkgWithComp name name;
              intrayPackages =
                {

                  "intray-api" = intrayPkg "intray-api";
                  "intray-api-gen" = intrayPkg "intray-api-gen";
                  "intray-cli" = intrayPkgWithComp "intray" "intray-cli";
                  "intray-cli-data" = intrayPkg "intray-cli-data";
                  "intray-client" = intrayPkg "intray-client";
                  "intray-data" = intrayPkg "intray-data";
                  "intray-data-gen" = intrayPkg "intray-data-gen";
                  "intray-stripe-client" = generatedStripePackage;
                  "intray-server" = intrayPkgWithOwnComp "intray-server";
                  "intray-server-gen" = intrayPkg "intray-server-gen";
                  "intray-web-server" =
                    let
                      bulma-css =
                        builtins.fetchurl {
                          url = https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css;
                          sha256 = "sha256:0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
                        };
                      bulma-tooltip-css =
                        builtins.fetchurl {
                          url = https://cdn.jsdelivr.net/npm/bulma-tooltip@3.0.2/dist/css/bulma-tooltip.min.css;
                          sha256 = "sha256:0xih9z80znhb3svn2xs6jbhh1mfkbywa1yjrq6p2llxk80md2yaw";
                        };
                      jquery-js =
                        builtins.fetchurl {
                          url = https://code.jquery.com/jquery-3.1.1.min.js;
                          sha256 =
                            "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
                        };
                      icons-ttf =
                        builtins.fetchurl {
                          url =
                            https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
                          sha256 =
                            "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
                        };
                      icons-woff =
                        builtins.fetchurl {
                          url =
                            https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
                          sha256 =
                            "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
                        };
                      icons-woff2 =
                        builtins.fetchurl {
                          url =
                            https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
                          sha256 =
                            "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
                        };
                    in
                    overrideCabal (intrayPkgWithOwnComp "intray-web-server") (
                      old:
                      {
                        preConfigure =
                          ''
                            ${old.preConfigure or ""}

                            mkdir -p static/
                            ln -s ${jquery-js} static/jquery.min.js
                            mkdir -p static/bulma/
                            ln -s ${bulma-css} static/bulma/bulma.min.css
                            ln -s ${bulma-tooltip-css} static/bulma/bulma-tooltip.min.css
                            mkdir -p static/semantic/themes/default/assets/fonts
                            ln -s ${icons-ttf} static/semantic/themes/default/assets/fonts/icons.ttf
                            ln -s ${icons-woff} static/semantic/themes/default/assets/fonts/icons.woff
                            ln -s ${icons-woff2} static/semantic/themes/default/assets/fonts/icons.woff2
                          '';
                        postInstall = ''
                          ${old.postInstall or ""}

                          export INTRAY_WEB_SERVER_API_URL=http://localhost:8000 # dummy

                          ${self.intrayPackages.intray-server}/bin/intray-server --port 8000 &
                          $out/bin/intray-web-server --port 8080 &

                          sleep 0.5

                          ${final.linkcheck}/bin/linkcheck http://localhost:8080
                          ${final.seocheck}/bin/seocheck http://localhost:8080

                          ${final.killall}/bin/killall intray-web-server
                          ${final.killall}/bin/killall intray-server
                        '';
                      }
                    );
                };
            in
            {
              inherit intrayPackages;
            } // intrayPackages
        );
    }
    );
}
