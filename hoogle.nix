{ mkDerivation, aeson, ansi-terminal, base, binary, blaze-html
, blaze-markup, bytestring, Cabal-syntax, cmdargs, conduit
, conduit-extra, containers, crypton-connection, data-default-class
, deepseq, directory, extra, filepath, ghc-lib-parser
, hackage-revdeps, hashable, haskell-src-exts, http-conduit
, http-types, js-flot, js-jquery, lib, mmap, old-locale
, process-extras, QuickCheck, resourcet, safe, storable-tuple, tar
, template-haskell, temporary, text, time, transformers, uniplate
, utf8-string, vector, wai, wai-logger, warp, warp-tls, zlib
}:
mkDerivation {
  pname = "hoogle";
  version = "5.0.20.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-terminal base binary blaze-html blaze-markup bytestring
    Cabal-syntax cmdargs conduit conduit-extra containers
    crypton-connection data-default-class deepseq directory extra
    filepath ghc-lib-parser hackage-revdeps hashable haskell-src-exts
    http-conduit http-types js-flot js-jquery mmap old-locale
    process-extras QuickCheck resourcet safe storable-tuple tar
    template-haskell temporary text time transformers uniplate
    utf8-string vector wai wai-logger warp warp-tls zlib
  ];
  executableHaskellDepends = [ base ];
  testFlags = [ "--no-net" ];
  homepage = "https://hoogle.haskell.org/";
  description = "Haskell API Search";
  license = lib.licenses.bsd3;
  mainProgram = "hoogle";
}
