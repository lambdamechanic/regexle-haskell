{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.kleene-src = {
    url = "github:lambdamechanic/kleene/22e780cd594f7a4b95dbb4ba6b46e1a3cea4e9f7";
    flake = false;
  };
  inputs.z3-src = {
    url = "github:lambdamechanic/z3-415.3/6f0f68671e682f7e1b085e662f3415895990bd0d";
    flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, kleene-src, z3-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: {
            cabal2nix-unwrapped = prev.cabal2nix-unwrapped.overrideAttrs (old: {
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ prev.nix ];
            });
          })
          (final: prev:
            let
              lib = prev.haskell.lib;
              rangeSetListSrc = prev.fetchFromGitHub {
                owner = "phadej";
                repo = "range-set-list";
                rev = "v0.1.4";
                hash = "sha256-oOtpX7/xOu1hhcxkf5A/6Q344pcLEJuuS/G115pF1Us=";
              };
            in
            {
              haskell = prev.haskell // {
                packages = prev.haskell.packages // {
                  ghc912 = prev.haskell.packages.ghc912.override {
                    overrides = self: super: {
                      "range-set-list" = lib.dontCheck (
                        lib.overrideCabal (self.callCabal2nix "range-set-list" rangeSetListSrc { }) (_: {
                          postPatch = ''
                            substituteInPlace range-set-list.cabal \
                              --replace "base        >=4.12.0.0 && <4.21" "base        >=4.12.0.0 && <5"
                            substituteInPlace range-set-list.cabal \
                              --replace "hashable    >=1.4.7.0  && <1.5" "hashable    >=1.4.7.0  && <1.6"
                          '';
                        })
                      );
                    };
                  };
                };
              };
            })
        ];
        pkgs = import nixpkgs {
          inherit system;
          overlays = overlays;
        };
        lib = pkgs.lib;
        haskellLib = pkgs.haskell.lib;
        basePackages = pkgs.haskell.packages.ghc912;
        hspkgs = basePackages.override (old: {
          overrides = lib.composeExtensions (old.overrides or (_: _: { })) (self: super:
            {
              lattices = haskellLib.doJailbreak super.lattices;
              kleene =
                let
                kleeneDrv = basePackages.callCabal2nix "kleene" kleene-src { };
                kleenePatched = haskellLib.overrideCabal kleeneDrv (_: {
                  revision = "8";
                  editedCabalFile = "sha256-vp6Us36FYPZytvl4V/X7U+H8qAq9+sma/Nuz9ksJczM=";
                });
              in
              haskellLib.doJailbreak (haskellLib.overrideCabal kleenePatched (oldDrv: {
                preConfigure = (oldDrv.preConfigure or "") + ''
                  substituteInPlace kleene.cabal \
                    --replace "base >= 4.5 && <5" "base >= 4.5 && <5.1"
                '';
              }));
            z3 =
              let
                z3Drv = basePackages.callCabal2nix "z3" z3-src { };
              in
              haskellLib.overrideCabal z3Drv (oldDrv: {
                configureFlags =
                  (oldDrv.configureFlags or [ ])
                  ++ [
                    "--extra-lib-dirs=${pkgs.z3}/lib"
                    "--extra-include-dirs=${pkgs.z3.dev}/include"
                  ];
                librarySystemDepends = (oldDrv.librarySystemDepends or [ ]) ++ [ pkgs.z3 pkgs.z3.dev ];
                doCheck = false;
              });
            });
        });
        drv = hspkgs.callCabal2nix "regexle-haskell" ./. { };
        testDrv = haskellLib.overrideCabal drv (_: {
          doCheck = true;
        });
      in {
        packages.default = drv;
        checks.tests = testDrv;
        devShells.default = pkgs.mkShell {
          buildInputs = [
            hspkgs.ghc
            hspkgs.cabal-install
            pkgs.zstd
          ];
        };
      });
}
