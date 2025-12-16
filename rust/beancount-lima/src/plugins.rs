use beancount_parser_lima as parser;

#[derive(Clone, Default, Debug)]
pub(crate) struct InternalPlugins {
    // OG Beancount
    pub(crate) auto_accounts: bool,
    pub(crate) implicit_prices: bool,

    // Lima specific
    pub(crate) balance_rollup: bool, // whether balance directives apply to the rollup of all subaccounts
}

impl<'a> FromIterator<&'a parser::Plugin<'a>> for InternalPlugins {
    fn from_iter<T: IntoIterator<Item = &'a parser::Plugin<'a>>>(iter: T) -> Self {
        let mut internal_plugins = Self::default();
        for plugin in iter {
            match *plugin.module_name().item() {
                "beancount.plugins.auto_accounts" => {
                    internal_plugins.auto_accounts = true;
                }

                "beancount.plugins.implicit_prices" => {
                    internal_plugins.implicit_prices = true;
                }

                "lima.balance_rollup" => {
                    internal_plugins.balance_rollup = true;
                }
                _ => (),
            }
        }
        internal_plugins
    }
}
