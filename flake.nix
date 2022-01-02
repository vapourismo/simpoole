{
  description = "Simple pool implementation";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }: {
    overlay = final: prev: {
      haskell = prev.haskell.override {
        packageOverrides = final: prev: {
          simpoole = prev.callCabal2nix "simpoole" self { };
        };
      };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    with (import nixpkgs {
      inherit system;
      config = { overlays = [ self.overlay ]; };
    });

    {
      packages = {
        simpoole = haskellPackages.simpoole;
      };

      defaultPackage = self.packages.${system}.simpoole;

      checks = {
        simpoole = self.packages.${system}.simpoole.overrideAttrs (_: { doCheck = true; });
      };

      devShell = mkShell {
        buildInputs = [
          (haskellPackages.ghcWithPackages (hspkgs:
            hspkgs.simpoole.propagatedBuildInputs ++ hspkgs.simpoole.buildInputs
          ))
          cabal-install
          haskell-language-server
          hlint
          stylish-haskell
          nixpkgs-fmt
        ];
      };
    }
  );
}
