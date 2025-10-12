{
  description = "hibet";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {inherit system;};

        haskellPackages = pkgs.haskellPackages;
      in
      {
        packages.hibet = haskellPackages.callCabal2nix "hibet" ./. {};

        defaultPackage = self.packages.${system}.hibet;

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
