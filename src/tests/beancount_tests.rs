#![cfg(test)]

use std::path::Path;
use test_generator::test_resources;

use crate::{
    bridge::load_from, create_engine, register_args, report_test_failures, tests::init_tracing,
    LoaderConfig,
};

use super::load_cog_path;

#[test_resources("tests/beancount/**/*.beancount")]
fn beancount_tests(beancount_relpath: &str) {
    init_tracing();
    tracing::debug!("hello beancount_tests");

    let cog_relpath = format!(
        "{}.scm",
        beancount_relpath.strip_suffix(".beancount").unwrap(),
    );

    match load_from(
        Path::new(beancount_relpath),
        LoaderConfig::default(),
        &std::io::stderr(),
    ) {
        Err(report) => {
            panic!("Failed loading {}: {}", beancount_relpath, &report);
        }

        Ok(prism) => {
            let mut steel_engine = create_engine();

            prism.register(&mut steel_engine);
            register_args(&mut steel_engine, Vec::default());

            load_cog_path(&mut steel_engine, &cog_relpath).unwrap();
            report_test_failures(&mut steel_engine, &cog_relpath).unwrap();
        }
    }
}
