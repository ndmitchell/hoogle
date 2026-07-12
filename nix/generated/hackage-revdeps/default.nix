{ mkDerivation, alfred-margaret, ansi-terminal, base, bytestring
, Cabal, cabal-install, Cabal-syntax, callPackage, containers
, filepath, lib, optparse-applicative, tar, text, time, zlib
}:
mkDerivation {
  pname = "hackage-revdeps";
  version = "0.3";
  src = callPackage ./src.nix {};
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    alfred-margaret base bytestring Cabal-syntax containers filepath
    tar text time zlib
  ];
  executableHaskellDepends = [
    ansi-terminal base Cabal cabal-install Cabal-syntax containers
    filepath optparse-applicative time
  ];
  description = "List Hackage reverse dependencies";
  license = lib.licenses.bsd3;
}
