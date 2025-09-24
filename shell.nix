{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.mkShell {
  inputsFrom  = [
    (pkgs.haskell.packages.ghc90.callCabal2nix "haskelldb-2.2.4.1" ./. { }).env 
  ];
  buildInputs = [
    pkgs.cabal-install
  ];
}
