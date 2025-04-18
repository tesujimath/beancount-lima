{
  description = "A development environment flake for beancount-lima.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    steel = {
      url = "github:mattwparas/steel/master";
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
        in
        with pkgs;
        {
          devShells.default = mkShell {
            nativeBuildInputs = [
              # build dependencies
              cargo-modules
              cargo-nightly
              cargo-udeps
              cargo-outdated
              cargo-edit
              gcc
              gdb
              rust-bin.stable.latest.default

              flakePkgs.steel

              # useful for reference:
              beancount
              beanquery
            ];

            shellHook = ''
              export STEEL_HOME=$(pwd)/steel
              mkdir -p $STEEL_HOME

              # There's no cog path AFAIK, so for now we symlink them in
              mkdir -p $STEEL_HOME/cogs
              for coglib in srfi steel; do
                ln -sf "${flakePkgs.steel}/lib/steel/cogs/$coglib" $STEEL_HOME/cogs
              done

              # LSP config
              # https://github.com/mattwparas/steel/tree/master/crates/steel-language-server#configuration
              export STEEL_LSP_HOME=$(pwd)/steel-lsp

              PATH=$PATH:$(pwd)/target/debug
            '';
          };
        }
      );
}
