{
  description = "reshala";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { localSystem = { inherit system; }; };
      hpkgs = pkgs.haskell.packages."ghc912";

      reshala = pkgs.haskell.lib.overrideCabal (hpkgs.callCabal2nix "reshala" ./. { }) (old: {
        doCheck = true;
        doHaddock = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
      });
    in
    {
      packages.default = reshala;

      devShells.default = pkgs.mkShell {
        buildInputs = [
          hpkgs.cabal-install
          hpkgs.haskell-language-server
          hpkgs.fourmolu
        ];
      };
    });
}
