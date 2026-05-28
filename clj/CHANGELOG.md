# Change Log

All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

### Added

- inventory/build-with-history to enable tracking inventory over time

[commit log]: https://github.com/tesujimath/limabean/compare/0.6.1...HEAD

## [0.6.1] - 2026-05-25

### Fixed

- no errors printed if booking failure without plugins #118
- don't leave temp files behind after running tests #122

### Changed

- add support for running plugins from arbitrary sources in standalone mode #120

[commit log]: https://github.com/tesujimath/limabean/compare/0.6.0...0.6.1

## [0.6.0] - 2026-05-20

### Fixed

- sort Open directives before transactions - beancount-parser-lima#66
- {} lot reduction - limabean-booking#10
- correct error returned when prior posting depletes inventory limabean-booking#12
- tolerance for balance directives #103
- metavalue numbers or amounts in the presence of plugins #106 #108
- honour `LIMABEAN_CLJ_DEPS` when running from Clojars #111

### Added

- Containerfile for podman/docker #94
- tests for spec validation with plugins
- warn if `LIMABEAN_CLJ_DEPS` defined when running from uberjar
- spans and provenance from raw directives propagated into booked directives
- provenance tag :booked for booked plugins
- create synthetic spans for new directives created by booked plugins in addition to raw

### Changed

- removed native Windows as a supported platform in favour of WSL #114
- appending :directives to filters to pass custom directives is no longer supported
- refactoring of namespaces so limabean.user is the REPL and limabean the API
- printing of errors nor encapsulated in error/print-errors
- full.beancount test now uses identity test plugin, so skipped in uberjar tests
- limabean.test/loader-tests renamed as api-tests, now supports tests returning errors
- raw-idx field on booked directives renamed to idx
- deprecate plugin/error! in favour of plugin/dct-error! for plugin errors
- JSON-RPC method parser.resolve-span replaced by parser.resolve-spans

[commit log]: https://github.com/tesujimath/limabean/compare/0.5.1...0.6.0

## [0.5.1] - 2026-05-05

### Fixed

- metadata date: key failing to interpolate #92
- bump limabean-booking to fix bad rounding of interpolated posting #93
- create-golden ignores .fyi.beancount files

[commit log]: https://github.com/tesujimath/limabean/compare/0.5.0...0.5.1

## [0.5.0] - 2026-04-30

### Fixed

- balance directives now correctly apply to total of account and its sub-accounts #78

### Added

- `income-statement` query for profit and loss
- diagnostics for failed balance directives show all the sub-accounts
- f/none-f combinator filter, the opposite of f/some-f
- validation of directives against Clojure spec
- support for `#regex` tagged literals in plugin config

### Changed

- `rollup` must now be applied to an inventory, to facilitate use across different standard queries
- journal now includes posting flags, if any
- only `*beans*` is used by the top-level queries, with `*directives*` et al being ignored
- exception stack traces are no longer printed, instead they are saved in `*exception*`
- testing support library broken out into `limabean-test` in `clj-test`, for external use
- errors in resolving plugins inhibit further processing

[commit log]: https://github.com/tesujimath/limabean/compare/0.4.1...0.5.0

## [0.4.1] - 2026-04-15

Fumbled the packaging on 0.4.0, bah!

[commit log]: https://github.com/tesujimath/limabean/compare/0.4.0...0.4.1

## [0.4.0] - 2026-04-15

This is a major release focused on greatly improved support for plugins.

Both raw and booked plugins are now supported, with support for notifying the user of errors found by plugins.

See the [plugins guide](doc/40-plugins.md) for details.

### Fixed

- fix count of raw directives printed on startup
- exit 1 if eval fails

### Added

- support for running raw plugins, with auto_accounts as an example
- pass custom meta values through to Clojure
- if `LIMABEAN_DEBUG_DIR` is defined, dump intermediate beanfiles into that directory
- print method for directives
- pass all options through to Clojure
- create synthetic spans for plugin outputs so errors reported in context

### Changed

- limabean-pod provides JSON-RPC server instead of one-shot book command
- at least Clojure 1.12 required
- remove dynamic variables *plugins* and *booked-directives* in favour of *beans* aggregate
- when errors occur, fall back into the REPL with context saved in *beans*

[commit log]: https://github.com/tesujimath/limabean/compare/0.3.2...0.4.0

## [0.3.2] - 2026-03-06

### Added

- print-method for java.time.LocalDate so that EDN is emitted in the same form as read

[commit log]: https://github.com/tesujimath/limabean/compare/0.3.1...0.3.2

## [0.3.1] - 2026-03-05

### Fixed

- bump limabean-booking crate version number, which broke Rust release 0.3.0

[commit log]: https://github.com/tesujimath/limabean/compare/0.3.0...0.3.1

## [0.3.0] - 2026-03-05

### Added

- external plugins are loaded and run automatically #50
- plugin "beancount.plugins.implicit_prices" is now supported #38
- duplicate includes are allowed if the context is unchanged #49
- include totals in costs and prices, to preserve original precision #48

### Changed

- simplify how tolerance is handled in booking crate

### Fixed

- inference of cost-per-unit from posting weight is now supported #42
- inference of price-per-unit from total must be positive #47

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.7...0.3.0

## [0.2.7] - 2026-02-23

### Added

-- support for globs in include pragmas #35

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.6...0.2.7

## [0.2.6] - 2026-02-20

### Added

- plugin "beancount.plugins.auto_accounts" is now supported
- warn about unknown plugins

### Fixed

- fix intolerance of zero #32
- fix writing of POM file broken by existing pom.xml

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.5...0.2.6

## [0.2.5] - 2026-02-17

### Added

- Build uberjar again for standalone use #28

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.4...0.2.5

## [0.2.4] - 2026-02-13

### Fixed

- fix Parsing Beancount files with CR-LF is failing on Windows #24

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.3...0.2.4

## [0.2.3] - 2026-02-10

### Added

- implement show for set #10
- implement show for seq

### Fixed

- fix Unable to detect a system Terminal on Windows #5
- fix Weird terminal behaviour and Control-C handling on Windows #11

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.1...0.2.3

## [0.2.2] - 2026-02-10

Broken release, do not use

## [0.2.1] - 2026-02-07

### Fixed

- fix Hitting Control-C in the REPL can cause infinite loop of exception handling #9

[commit log]: https://github.com/tesujimath/limabean/compare/0.2.0...0.2.1

## [0.2.0] - 2026-02-05

First public release
