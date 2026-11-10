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

        # The pinned nixpkgs predates these Hackage releases, so fetch them directly.
        hackageSrc = name: version: hash:
          pkgs.runCommand "${name}-${version}-src" {
            src = pkgs.fetchurl {
              url = "https://hackage.haskell.org/package/${name}-${version}/${name}-${version}.tar.gz";
              inherit hash;
            };
          } ''
            mkdir -p $out
            tar -xzf $src -C $out --strip-components=1
          '';

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            # hewts needs tasty-falsify, which needs falsify >= 0.4.
            falsify =
              hself.callCabal2nix "falsify"
                (hackageSrc "falsify" "0.4.0" "sha256-VqeMmuRiYh9M6A/Dv0ZWR60AYvtU5CcVSYYDFWxNUi8=")
                { };
            tasty-falsify =
              hself.callCabal2nix "tasty-falsify"
                (hackageSrc "tasty-falsify" "0.1.0" "sha256-9CFa7ko/KaIHrDiOHzv9u5X93SmlQO50fny5qExaGdA=")
                { };
            hibet = hself.callCabal2nix "hibet" ./hibet { };
            # The dictionaries are not data-files any more; wrap the binary so
            # `nix run` still finds the dataset in the repository.
            hibet-cli = pkgs.symlinkJoin {
              name = "hibet-cli";
              paths = [
                (pkgs.haskell.lib.overrideCabal
                  (hself.callCabal2nix "hibet-cli" ./hibet-cli { })
                  (_: { mainProgram = "hibet"; }))
              ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/hibet --set-default HIBET_DATA_DIR ${./hibet-data}
              '';
              meta.mainProgram = "hibet";
            };
            hewts = hself.callCabal2nix "hewts" ./hewts { };
          };
        };
      in
      {
        packages = {
          hibet = haskellPackages.hibet;
          hibet-cli = haskellPackages.hibet-cli;
          hewts = haskellPackages.hewts;
          default = haskellPackages.hibet-cli;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages; [
            haskell-language-server
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
          # The dictionaries are not part of the packages any more.
          shellHook = ''
            export HIBET_DATA_DIR=$PWD/hibet-data
          '';
        };
      });
}
