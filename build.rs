// Copyright (C) 2019  Frank Rehberger
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0>

fn main() {
    build_deps::rerun_if_changed_paths("tests").unwrap();
    build_deps::rerun_if_changed_paths("tests/**").unwrap();
    build_deps::rerun_if_changed_paths("tests/**/*").unwrap();
    build_deps::rerun_if_changed_paths("cogs").unwrap();
    build_deps::rerun_if_changed_paths("cogs/**").unwrap();
    build_deps::rerun_if_changed_paths("cogs/**/*").unwrap();
}
