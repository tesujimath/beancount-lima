#![cfg(test)]

use rstest::rstest;
use std::path::Path;
use std::path::PathBuf;

use crate::{
    bridge::load_from, create_engine, register_args, report_test_failures, tests::init_tracing,
    LoaderConfig,
};

use super::load_cog_path;

#[rstest]
fn beancount_tests(#[files("tests/beancount/**/*.beancount")] beancount_relpath: PathBuf) {
    init_tracing();
    tracing::debug!("hello beancount_tests");

    let beancount_reldir = beancount_relpath.parent().unwrap();
    let cog_relpath = beancount_reldir
        .join(Path::new(beancount_relpath.file_stem().unwrap()).with_extension("scm"));

    match load_from(
        &beancount_relpath,
        LoaderConfig::default(),
        &std::io::stderr(),
    ) {
        Err(report) => {
            panic!("Failed loading {:?}: {}", &beancount_relpath, &report);
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
