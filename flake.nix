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
          };
          # cargo-nightly based on https://github.com/oxalica/rust-overlay/issues/82
          nightly = pkgs.rust-bin.selectLatestNightlyWith (t: t.default);
          cargo-nightly = pkgs.writeShellScriptBin "cargo-nightly" ''
            export RUSTC="${nightly}/bin/rustc";
            exec "${nightly}/bin/cargo" "$@"
          '';

          ci-packages = with pkgs; [
            bashInteractive
            just
            rust-bin.stable.latest.default
            gcc
            flakePkgs.steel
          ];
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

              # useful for reference:
              beancount
              beanquery
            ] ++ ci-packages;

            shellHook = ''
              # LSP config
              # https://github.com/mattwparas/steel/tree/master/crates/steel-language-server#configuration
              export STEEL_LSP_HOME=$(pwd)/steel-lsp

              export BEANCOUNT_LIMA_COGPATH="$BEANCOUNT_LIMA_COGPATH:$(pwd)/examples/cogs:$(pwd)/cogs:${flakePkgs.steel}/lib/steel/cogs"

              PATH=$PATH:$(pwd)/target/debug
            '';
          };

          apps = {
            tests = {
              type = "app";
              program = "${writeShellScript "beancount-lima-tests" ''
                export PATH=${pkgs.lib.makeBinPath ci-packages}
                export BEANCOUNT_LIMA_COGPATH="$(pwd)/cogs:${flakePkgs.steel}/lib/steel/cogs"
                just test
              ''}";
            };

          };
        }
      );
}
