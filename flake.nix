{
  description = "A development environment flake for beancount-lima.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    steel = {
      # TODO switch back to mainline Steel once into-string PR merged
      # url = "github:mattwparas/steel/master";
      url = "github:tesujimath/steel/into-string";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    autobean-format = {
      url = "github:tesujimath/autobean-format.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ (import inputs.rust-overlay) ];
          pkgs = import inputs.nixpkgs {
            inherit system overlays;
          };
          flakePkgs = {
            steel = inputs.steel.packages.${system}.default;
            autobean-format = inputs.autobean-format.packages.${system}.default;
          };
          # cargo-nightly based on https://github.com/oxalica/rust-overlay/issues/82
          nightly = pkgs.rust-bin.selectLatestNightlyWith (t: t.default);
          cargo-nightly = pkgs.writeShellScriptBin "cargo-nightly" ''
            export RUSTC="${nightly}/bin/rustc";
            exec "${nightly}/bin/cargo" "$@"
          '';


          ci-packages = with pkgs; [
            bashInteractive
            coreutils
            diffutils
            elvish
            just
            rust-bin.stable.latest.default
            gcc
            flakePkgs.steel
          ];

          beancount-lima =
            let cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
            in pkgs.rustPlatform.buildRustPackage
              {
                pname = "beancount-lima";
                version = cargo.package.version;

                src = ./.;

                cargoDeps = pkgs.rustPlatform.importCargoLock {
                  lockFile = ./Cargo.lock;
                  outputHashes = {
                    "steel-core-0.6.0" = "sha256-ngPaqPLn3KC1r1A7t3gkYNIRzvd99ZmMKVcpXnuDOnU=";
                  };
                };

                # skip tests which require environment
                checkFlags = [
                  "--skip=tests::beancount_tests"
                  "--skip=tests::cog_tests"
                ];

                meta = with pkgs.lib; {
                  description = "Beancount frontend using Steel Scheme and Lima parser";
                  homepage = "https://github.com/tesujimath/beancount-lima";
                  license = with licenses; [ asl20 mit ];
                  # maintainers = [ maintainers.tesujimath ];
                };
              };
        in
        with pkgs;
        {
          devShells.default = mkShell {
            nativeBuildInputs = [
              cargo-modules
              cargo-nightly
              cargo-udeps
              cargo-outdated
              cargo-edit
              gdb

              # useful tools:
              beancount
              beanquery
              flakePkgs.autobean-format
            ] ++ ci-packages;

            shellHook = ''
              # LSP config
              # https://github.com/mattwparas/steel/tree/master/crates/steel-language-server#configuration
              export STEEL_LSP_HOME=$(pwd)/steel-lsp

              export BEANCOUNT_LIMA_COGPATH="''${BEANCOUNT_LIMA_COGPATH}''${BEANCOUNT_LIMA_COGPATH:+:}$(pwd)/cogs:${flakePkgs.steel}/lib/steel/cogs"

              PATH=$PATH:$(pwd)/target/debug
            '';
          };

          packages.default = beancount-lima;

          apps = {
            tests = {
              type = "app";
              program = "${writeShellScript "beancount-lima-tests" ''
                export PATH=${pkgs.lib.makeBinPath (ci-packages ++ [beancount-lima])}
                export BEANCOUNT_LIMA_COGPATH="$(pwd)/cogs:${flakePkgs.steel}/lib/steel/cogs"
                just test
              ''}";
            };
          };
        }
      );
}
