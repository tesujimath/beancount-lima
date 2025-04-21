build:
    cargo build

test: build
    cargo test
    beancount-lima tests/auto-posting.beancount tests/auto-posting-tests.scm
