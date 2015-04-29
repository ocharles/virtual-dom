with (import <nixpkgs> {}).pkgs;
let pkg = haskell-ng.packages.ghcjs.callPackage ./. {};
in
  pkg.env
