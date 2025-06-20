build:
    cargo build

test: build
    cargo test
    ./import-tests.elv
