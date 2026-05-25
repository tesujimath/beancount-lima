build: build-rust build-clj

build-release: build-rust-release build-clj

test: test-rust test-clj

[working-directory: 'rust']
build-rust:
    cargo build

[working-directory: 'rust']
build-rust-release:
    cargo build --release --all-targets

[working-directory: 'rust']
test-rust: build-rust
    cargo test

[working-directory: 'clj']
build-clj: build-clj-test
    clojure -T:build uber

[working-directory: 'clj-test']
build-clj-test:
    clojure -T:build jar

[working-directory: 'clj']
test-clj:
    clojure -X:test

test-clj-offline: build-clj build-rust
    #!/usr/bin/env bash
    VERSION=$(bash ./scripts.dev/get-version.sh)
    export LIMABEAN_UBERJAR="./clj/target/limabean-${VERSION}-standalone.jar"
    unset LIMABEAN_CLJ_LOCAL_ROOT
    export LIMABEAN_CLJ_DEPS="limabean/test-plugins {:local/root \"$(pwd)/clj/test-plugins\"}"
    export PATH=./rust/target/debug:$PATH
    for golden in test-cases/*.golden/{inventory,journal,rollup}; do
        query=${golden##*/}
        beanfile=${golden%.golden/$query}.beancount
        # rollup is no longer a stand-alone query, must be applied to an inventory
        if test "$query" == rollup; then
          query="rollup (inventory)"
        fi
        echo "Validating $golden"
        limabean -v --beanfile "$beanfile" --eval "(show ($query))" | diff - $golden
    done

[working-directory: 'clj']
refresh-golden-test-output:
    clojure -X:gen-golden
