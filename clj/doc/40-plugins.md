# Plugins and user-supplied code

Plugins in `limabean` are [Clojure transducers](https://clojure.org/reference/transducers), which may run early on raw directives before the booking algorithm, or later on fully booked directives.

In addition, arbitrary user-provided code may be loaded into the REPL (below).

With the newly added support for raw plugins, the previous internal plugins have been removed.

Plugins are referenced in the Beanfile by their namespace, e.g.

```
plugin "limabean.contrib.plugins.examples.magic-money"
```

A single argument may be supplied, which is a single Clojure value in a Beancount string, i.e. with escaped quotes, e.g.

```
plugin "limabean.contrib.plugins.examples.magic-money" "{:units 1000.00M :cur \"USD\" :acc \"Equity:Rich-American-Uncle\"}"
```

In fact, any Clojure value may be supplied, not necessarily a map.  But it must match what the particular plugin is expecting.

Note that EDN does not support the [`#` dispatch macro](https://clojure.org/reference/reader#_dispatch), so in particular regular expressions in plugin config in Beancount files must be written as tagged literals, e.g. `#regex "i-am-a-regex"`.

The Clojure namespace must define one or both of the functions `raw-xf` and `booked-xf`, each of which is a function returning a Clojure transducer on raw or booked directives respectively.

## Running plugins

Plugins are run automatically when loading a beanfile.  Any errors resolving a particular plugin inhibit any further processing of the beanfile.

The original directives loaded from the file are available in the REPL as `(:raw-directives *beans*)`, with the post-plugin raw directives available as `(:raw-xf-directives *beans*)`, booked directives as `(:booked-directives *beans*)`, and post-plugin booked directives as `(:booked-xf-directives *beans*)`.

Any errors in actually running any plugin cause the loader to abort with whatever partial state was reached, for further investigation by the user using the REPL.

### Core plugins

The core plugins previously available with OG Beancount are intended to be bundled with `limabean`.  Currently this list comprises only `auto-accounts`.  Adding to these is a work-in-progress.  These plugins live in the [`beancount.plugins`](../src/beancount/plugins) namespace.

### Configuration required to resolve other plugins

Plugins are Clojure code, so they must be on the Java class-path in order to be resolvable at runtime.  The `limabean` launcher supports running the clojure command line with the [`-Sdeps` option to pass the required dependencies](https://clojure.org/guides/deps_and_cli#command_line_deps).

In order to run anything beyond the bundled plugins, the Clojure package containing the desired plugin should be passed in the environment variable `LIMABEAN_CLJ_DEPS`, as e.g. `io.github.tesujimath/limabean-contrib {:mvn/version "0.1.0"}`.  This environment variable comprises a space-separated list of package name, co-ordinate pairs, that is without the `{:deps {...} }` wrapper.

See the [Clojure deps reference](https://clojure.org/reference/deps_edn#deps) for what is possible, which includes local directories and git repos.

The following examples make use of `limabean-contrib` as a source for plugins, but you are free to create your own.  But do please consider contributing your plugins to [limabean-contrib](https://github.com/tesujimath/limabean-contrib).

To use a specified version from Clojars: `io.github.tesujimath/limabean-contrib {:mvn/version "0.1.0"}`

To use a library directly from GitHub: `io.github.tesujimath/limabean-contrib {:git/sha "bc55aa4105ca1b050fffe12301e1829c908a4689"}` - in this case the GitHub organization and repo are inferred from the library name, [unless overridden](https://clojure.org/reference/deps_edn#deps_git) using `:git/url`.

To use a library from a local path: `io.github.tesujimath/limabean-contrib {:local/root "/path/to/limabean-contrib"}`

As always, run with `limabean -v` to see what is going on with the Clojure invocation.

## Writing plugins

### Plugin namespaces

A limabean plugin namespace is simply a [Clojure namespace](https://guide.clojure.style/#naming-ns-naming-schemas).  Please avoid defining your own plugins in the `limabean` namespace, although [limabean.contrib.plugins](https://github.com/tesujimath/limabean-contrib) is a good choice if you want to contribute your plugin there (please do!).  Otherwise, use your own domain.

The intention is that `limabean.contrib.plugins` is a place for development and refinement of plugins, which upon gaining stability may be promoted into the `limabean.plugins` itself.

Legacy plugins appear with their original names, e.g. `beancount.plugins.auto-accounts`.  Because Clojure prefers hyphen to underscore in namespace names, any plugin name from a Beancount file containing underscores gets changed to hyphens before resolving as a Clojure namespace.  Therefore, such plugins may continue to be referenced by their original names from Beancount files.

### Errors

Any errors detected by plugins may be reported using the `limabean.plugin/dct-error!` macro, as shown for example in the [`limabean.test.plugins.fail`](../test-plugins/src/limabean/test/plugins/fail.clj) plugin, which annotates the directive emitted with the error indication.

Errors reported to the user do not include a full stack trace, but this may be found in `*exception*`.

### Testing

Plugins are tested using the `limabean-test` library, which compares actual output with pre-generated golden test output.

Each test comprises a Beancount file with a sibling golden output directory, e.g. `test-my-plugin.beancount` and `test-my-plugin.golden`.  The golden directory contains either or both of `raw-xf-directives.edn` and `directives.edn`, the former being the raw plugin output prior to booking.

These files may be generated by `clojure -X:gen-golden`, which rewrites all existing golden output files.  So the process is to first create whichever output files are required (with any content), before running `clojure -X:gen-golden`.

In addition to each EDN file created, a corresponding `.fyi.beancount` is written, which contains the human-readable equivalent.  This file is ignored during testing;  it simply serves as documentation of plugin behaviour.

(The `inventory`, `journal`, and `rollup` files which may also appear in the golden directory are more of an application test than a plugin test.)

### Examples

#### Set narration

The test plugin [set-narration](../test-plugins/src/limabean/test/plugins/set_narration.clj) is the simplest possible plugin example, which overrides the narration field of each transaction according to its configuration, as in [this example beancount file](../../test-cases/set-narration-plugin-with-config.beancount).

```
kiri> limabean --beanfile ./test-cases/set-narration-plugin-with-config.beancount
[Rebel readline] Type :repl/help for online help info
[limabean] 4 directives loaded from ./examples/beancount/set-narration-plugin.beancount
[limabean] 4 directives resulting from running plugins

user=> (show (journal))
2023-05-29  Expenses:Groceries   New World  Plugins rule ok!   10.00  NZD  10.00 NZD
2023-05-29  Assets:Bank:Current  New World  Plugins rule ok!  -10.00  NZD
2023-05-30  Expenses:Groceries   Countdown  Plugins rule ok!   17.50  NZD  17.50 NZD
2023-05-30  Assets:Bank:Current  Countdown  Plugins rule ok!  -17.50  NZD
:ok
```

#### Auto accounts

The original Beancount plugin `auto_accounts` has been implemented as a [raw plugin](../src/beancount/plugins/auto_accounts.clj).

#### Magic Money

The [magic-money example](https://github.com/tesujimath/limabean-contrib/blob/main/src/limabean/contrib/plugins/examples/magic_money.clj) is a more sophisticated plugin which inserts additional directives, namely a transaction after every `open` directive to add some money to the account, from a specified equity account.  It works as a [stateful transducer](https://clojure.org/reference/transducers#_transducers_with_reduction_state).

```
kiri> limabean --beanfile ./examples/beancount/magic-money-plugin.beancount
[Rebel readline] Type :repl/help for online help info
[limabean] 4 directives loaded from ./examples/beancount/magic-money-plugin.beancount
[limabean] 7 directives resulting from running plugins

user=> (show (journal))
2016-03-01  Equity:Rich-American-Uncle                        -1000.00  USD  -1000.00 USD
2016-03-01  Assets:Bank:Current         magical benefactor     1000.00  USD
2016-03-01  Equity:Rich-American-Uncle                        -1000.00  USD  -1000.00 USD
2016-03-01  Expenses:Groceries          magical benefactor     1000.00  USD
2023-05-29  Expenses:Groceries          New World                10.00  NZD     10.00 NZD
2023-05-29  Assets:Bank:Current         New World               -10.00  NZD
2023-05-30  Expenses:Groceries          Countdown                17.50  NZD     17.50 NZD
2023-05-30  Assets:Bank:Current         Countdown               -17.50  NZD
:ok
```

## Running plugins manually

The resolved plugins are readily available in `(:plugins *beans*)`, so may be applied manually.

```
user=> (:plugins *beans*)
[{:name "limabean.contrib.plugins.examples.set-narration",
  :config "{:narration \"Plugins rule ok!\"}",
  :booked-xf #object[limabean.contrib.plugins.examples.set_narration$booked_xf$fn__16968 0x1ecc1a99
                    "limabean.contrib.plugins.examples.set_narration$booked_xf$fn__16968@1ecc1a99"]}]

user=> (def set-narration-xf (get-in (:plugins *beans*) [0 :raw-xf]))

user=> (into [] set-narration-xf (:raw-directives *beans*))
[2016-03-01 open Assets:Bank:Current
 2016-03-01 open Expenses:Groceries
 2023-05-29 * "New World" "Plugins rule ok!"
  Expenses:Groceries 10.00 NZD
  Assets:Bank:Current
 2023-05-30 * "Countdown" "Plugins rule ok!"
  Expenses:Groceries 17.50 NZD
  Assets:Bank:Current
]
```

With the newly added output formatting for directives, it is necessary to use `pprint` to see the underlying Clojure data structures.

```
user=> (pprint (into [] set-narration-xf (:raw-directives *beans*)))
[{:span [0 82 119],
  :date #time/date "2016-03-01",
  :dct :open,
  :acc "Assets:Bank:Current"}
 {:span [0 119 155],
  :date #time/date "2016-03-01",
  :dct :open,
  :acc "Expenses:Groceries"}
 {:span [0 155 238],
  :date #time/date "2023-05-29",
  :dct :txn,
  :flag "*",
  :payee "New World",
  :postings
  [{:span [0 185 214],
    :acc "Expenses:Groceries",
    :units 10.00M,
    :cur "NZD"}
   {:span [0 217 236], :acc "Assets:Bank:Current"}],
  :narration "Plugins rule ok!"}
 {:span [0 238 320],
  :date #time/date "2023-05-30",
  :dct :txn,
  :flag "*",
  :payee "Countdown",
  :postings
  [{:span [0 268 297],
    :acc "Expenses:Groceries",
    :units 17.50M,
    :cur "NZD"}
   {:span [0 300 319], :acc "Assets:Bank:Current"}],
  :narration "Plugins rule ok!"}]
```

## User-provided code

The user may provide their own Clojure code.  The environment variable `LIMABEAN_USER_CLJ` is a colon-separated list of Clojure source files, which are loaded in order, and made available in the REPL.
This facility is not suitable for plugins, because the functions are loaded too late.  But it is a useful place for defining custom filters.

For a very simple example, see the [user-supplied `fy` function](../../examples/clj/user.clj) for a customized financial year filter.
