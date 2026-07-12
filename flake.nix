{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    hdeps = {
      url = "github:LightAndLight/hdeps";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils, hdeps }:
    {
      overlays.default = final: prev: {
        haskellPackages =
          (prev.haskellPackages.extend (import ./nix/generated/overlay.nix)).extend (hfinal: hprev: {
            hoogle = prev.haskell.lib.dontHaddock (hprev.callPackage ./hoogle.nix {});
          });
      };
    } //
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # C
            clang-tools clang gdb

            # Haskell
            ghc cabal-install haskell-language-server

            # Project
            just haskellPackages.fourmolu cabal2nix hdeps.packages.${system}.default haskellPackages.implicit-hie

            zlib
          ];
        };
      }
    );
}
