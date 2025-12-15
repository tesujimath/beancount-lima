{
  description = "A development environment flake for beancount-lima.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    autobean-format = {
      url = "github:SEIAROTg/autobean-format";
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
            just
            rust-bin.stable.latest.default
            gcc
          ];

          beancount-lima-pp =
            let cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
            in pkgs.rustPlatform.buildRustPackage
              {
                pname = "beancount-lima-pp";
                version = cargo.workspace.package.version;

                src = ./.;

                cargoDeps = pkgs.rustPlatform.importCargoLock {
                  lockFile = ./Cargo.lock;
                };

                meta = with pkgs.lib; {
                  description = "Beancount frontend using Lima parser";
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
              PATH=$PATH:$(pwd)/target/debug
            '';
          };

          packages.default = beancount-lima-pp;

          apps = {
            tests = {
              type = "app";
              program = "${writeShellScript "beancount-lima-tests" ''
                export PATH=${pkgs.lib.makeBinPath (ci-packages ++ [beancount-lima])}
                just test
              ''}";
            };
          };
        }
      );
}
