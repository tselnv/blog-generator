{ withHoogle ? false, forceShell ? false }:
# A tutorial on Nix, and how you can use 'developPackage' to override
# dependencies:
#   https://www.srid.ca/1948201.html
let
  # You can get a newer ref by looking under "nixpkgs-unstable" in https://status.nixos.org/
  nixpkgsRev = "19d4f7dc485f74109bd66ef74231285ff797a823";
  nixpkgsSha = "sha256:1middmy754sc5ni43hqpyppsb7cgnk30g94nzjxmq5jyqkgdl0s8";
  # We are using the default compiler (8.8 as of this writing) in nixpkgs.
  # To override, set it to for example: pkgs.haskell.packages.ghc865
  compiler = pkgs.haskellPackages;
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = nixpkgsSha;
  }) {} ;


in
  pkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs =  [
      compiler.cabal-install
      compiler.ghcid
      compiler.haskell-language-server
      # (happy9 compiler)
      # Used by bin/generated
      compiler.ghcide
      pkgs.ormolu
      pkgs.hpack
      compiler.ghc
      pkgs.haskellPackages.record-dot-preprocessor
    ];

}