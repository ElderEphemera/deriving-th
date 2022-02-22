let
  pinnedNixPkgs =
    import (builtins.fetchTarball {
      name = "nixpkgs-21.11";
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
      sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
    }) {};
in

{ pkgs ? pinnedNixPkgs
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
