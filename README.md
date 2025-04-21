# beancount-lima

This is an early stage work-in-progress Beancount frontend using Steel Scheme and the [Lima parser](https://github.com/tesujimath/beancount-parser-lima).

The observation is that Rust is at the same time both wonderful and uncomfortable for end-users.  The idea is to use Steel Scheme for interactive Beancounting, and later, plugins and scripting.

Instead of [Beancount Query Language](https://beancount.github.io/docs/beancount_query_language.html), Steel Scheme is used.


A number of useful queries are provided out-of-the-box.

## Example

```
aya> beancount-lima --batch ./examples/beancount/full.beancount balances
                                              CAD     NZD
         Assets:AccountsReceivable:Taxes
                     Assets:Bank:Current          -319.65
                 Assets:Bank:Unspecified
Assets:CA:RBC-Investing:Taxable-CAD:Cash -1395.43
                   Assets:US:TD:Checking
                       Expenses:Car:Fuel           255.00
Expenses:Entertainment:Drinks-and-snacks            25.00
                      Expenses:Groceries            39.65
```
