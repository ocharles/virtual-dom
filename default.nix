{ mkDerivation, base, ghcjs-base, ghcjs-dom, lens, stdenv }:
mkDerivation {
  pname = "virtual-dom";
  version = "0.1";
  src = ./.;
  buildDepends = [ base ghcjs-base ghcjs-dom lens ];
  license = stdenv.lib.licenses.bsd3;
}
