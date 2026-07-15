{
  description = "haskell.org hoogle deployment";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      {
        packages = rec {
          hoogle =
            let
              hsPkgs = pkgs.haskellPackages.override {
                overrides = self: super: {
                  hackage-revdeps = super.hackage-revdeps_0_3;
                };
              };
            in hsPkgs.callCabal2nix "hoogle" ./. { };
          default = hoogle;
        };
        apps = rec {
          hoogle = flake-utils.lib.mkApp { drv = self.packages.${system}.hoogle; };
          default = hoogle;
        };
      }) // {
        nixosConfigurations."hoogle-test" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            { boot.isContainer = true; }
            (import ./deploy.nix {
              hoogle = self.packages.x86_64-linux.hoogle;
              cores = 4;
            })
            {
              services.nginx.virtualHosts."hoogle.haskell.org" = {
                locations."/" = {
                  proxyPass = "http://hoogle";
                };
                addSSL = true;
              };
            }
          ];
        };

        nixosModules.hoogle-haskell-org = { pkgs, ... }: {
          imports = [
            (import ./deploy.nix {
              hoogle = self.packages.${pkgs.stdenv.hostPlatform.system}.hoogle;
              cores = 4;
            })
          ];
        };
      };
}
