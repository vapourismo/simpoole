{
  description = "Simple pool implementation";

  inputs = {
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }: {
    overlay = final: prev: {
      haskell = prev.haskell // {
        packageOverrides = hfinal: hprev: {
          simpoole =
            hfinal.callCabal2nix "simpoole"
              (
                final.nix-gitignore.gitignoreSourcePure
                  [
                    ./.gitignore
                    "*.nix"
                    "flake.lock"
                    "*.yaml"
                    "cabal.project*"
                    ".vscode"
                    ".github"
                    "*.md"
                  ]
                  self
              )
              { };
        };
      };
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    with (import nixpkgs {
      inherit system;
      overlays = [ self.overlay ];
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
          ghc
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
