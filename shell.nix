{ pkgs ? import <nixpkgs> {} }:

with pkgs;

pkgs.mkShell {
  buildInputs = [
    ghc
    cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.stylish-haskell
  ];
}
