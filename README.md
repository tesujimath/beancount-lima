# beancount-lima

This is an early stage work-in-progress Beancount frontend using Steel Scheme and the [Lima parser](https://github.com/tesujimath/beancount-parser-lima).

The observation is that Rust is at the same time both wonderful and uncomfortable for end-users.  The idea is to use Steel Scheme for interactive Beancounting.

Instead of [Beancount Query Language](https://beancount.github.io/docs/beancount_query_language.html), Steel Scheme is used.


A number of useful queries are provided out-of-the-box.

## Example

```
aya> lima

     _____ __            __
    / ___// /____  ___  / /          Version 0.7.0
    \__ \/ __/ _ \/ _ \/ /           https://github.com/mattwparas/steel
   ___/ / /_/  __/  __/ /            :? for help
  /____/\__/\___/\___/_/

λ > (display-balances *ledger*)
                                           GBP     NZD
Assets:Bank:Current                              -100.78
Assets:Bank:UK                            -5.00
Expenses:Donations                                 10.00
Expenses:Donations:Other                           20.00
Expenses:Entertainment:Drinks-and-snacks           48.00
Expenses:Groceries                         5.00    27.50
Income:Unknown                                     -4.72


λ > (display-rollup *ledger*)
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

## Import

[Import](doc/import.md) is particularly convenient and addresses pain points I encountered with import using classic Beancount tools.

## Contributions

While issues are welcome, and I am particularly interested in more example files for import, given the current pace of development I am unlikely to be able to accept PRs for now.

## License

Licensed under either of

 * Apache License, Version 2.0
   [LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   [LICENSE-MIT](http://opensource.org/licenses/MIT)

at your option.
