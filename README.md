# beancount-lima

This is new implementation of Beancount using [Steel Scheme](https://github.com/mattwparas/steel) and the [Lima parser](https://github.com/tesujimath/beancount-parser-lima).

The observation is that Rust is at the same time both wonderful and uncomfortable for end-users.  The idea is to use Steel Scheme for interactive Beancounting.
Instead of [Beancount Query Language](https://beancount.github.io/docs/beancount_query_language.html), Steel Scheme is used.
(There is currently no intention to reimplement the existing query language.)

A number of useful queries are provided out-of-the-box.

## Example

```
aya> lima

     _____ __            __
    / ___// /____  ___  / /          Version 0.7.0
    \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
   ___/ / /_/  __/  __/ /            :? for help
  /____/\__/\___/\___/_/

λ > (display-balance-sheet (cumulate-postings *directives*))
                                           GBP     NZD
Assets:Bank:Current                              -100.78
Assets:Bank:UK                            -5.00
Expenses:Donations                                 10.00
Expenses:Donations:Other                           20.00
Expenses:Entertainment:Drinks-and-snacks           48.00
Expenses:Groceries                         5.00    27.50
Income:Unknown                                     -4.72


λ > (display-rollup (cumulate-postings *directives*))
Assets                                    -100.78
Assets:Bank                                        -100.78
Assets:Bank:Current                                         -100.78
Expenses                                   105.50
Expenses:Donations                                   30.00    10.00
Expenses:Donations:Other                                      20.00
Expenses:Entertainment                               48.00
Expenses:Entertainment:Drinks-and-snacks                      48.00
Expenses:Groceries                                            27.50
Income                                      -4.72
Income:Unknown                                                -4.72
```

Note: the rollup ignores all but the primary currency (as determined by frequency of use).

## Usage Examples

```
$ lima --help

$ lima

$ lima -o rollup report assets

$ lima -o standalone import kiwibank/*250807.ofx >imported.beancount

$ lima --ledger imported.beancount

$ lima import kiwibank/*250807.ofx >>my-existing.beancount
```

## Import

[Import](doc/import.md) is particularly convenient and addresses pain points I encountered with import using classic Beancount tools.

## Balance assertions

A point of difference from classic Beancount is that balance assertions may be configured to assert the total for an account an all its subaccounts, using
the internal plugin `lima.balance_rollup`.  For example, if a bank account holds multiple logical amounts, they may be tracked as subaccounts, without violating
balance assertions.

Padding is only ever performed on the actual account asserted in the balance directive, never on its subaccounts.

Unless the plugin is enabled, the default behaviour is not to do this.

## Plugins

Lima does not support externally provided plugins.  The intention is that all desired behaviour may be implemented by the end user in Steel Scheme.

That said, there are a handful of internal plugins, as follows.

### Implicit Prices

The existing plugin `beancount.plugins.implicit_prices` is built in.

### Auto Accounts

The existing plugin `beancount.plugins.auto_accounts` is not yet supported, but will be implemented as a built-in plugin.

### Balance Rollup

As described above, the plugin `lima.balance_rollup` modifies the behaviour of the `balance` directive.

## Contributions

While issues are welcome and I am particularly interested in making this generally useful to others, given the current pace of development I am unlikely to be able to accept PRs for now.

## License

Licensed under either of

 * Apache License, Version 2.0
   [LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   [LICENSE-MIT](http://opensource.org/licenses/MIT)

at your option.
