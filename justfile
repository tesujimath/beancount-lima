build:
    cargo build

test: build
    cargo test
    ./run-import-tests
