{
  description = "hibet and hewts";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            hibet = hself.callCabal2nix "hibet" ./hibet {};
            hewts = hself.callCabal2nix "hewts" ./hewts {};
          };
        };
      in
      {
        packages = {
          hibet = haskellPackages.hibet;
          hewts = haskellPackages.hewts;
          default = haskellPackages.hibet;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}