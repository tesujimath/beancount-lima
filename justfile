build:
    cargo build

test: build
    cargo test
    beancount-lima --batch tests/auto-posting.beancount tests/auto-posting-tests
