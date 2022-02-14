{ pkgs ? import <nixpkgs> {}
, ghc ? "9_2_1"
, cabal ? "3_6_2_0"
}:

let
  ghcs = import (pkgs.fetchgit {
    url = "https://gitlab.haskell.org/bgamari/ghcs-nix";
    rev = "2fc3efb994fbd5bfb7d4cf1a5e71690166dbfee3";
    sha256 = "1w7xwza9djrr7l4w04ka8b7ykmpng70gpjn5ab27a4r8ik1zl72i";
  });
in pkgs.mkShell {
  packages = [
    ghcs."ghc-${ghc}"
    ghcs."cabal-install-${cabal}"
  ];
}
