use steel::steel_vm::engine::Engine;
use steel::steel_vm::register_fn::RegisterFn;
use steel_repl::run_repl;

fn dir_file_name_iter(path: String) -> impl Iterator<Item = String> {
    let read_dir =
        std::fs::read_dir(&path).unwrap_or_else(|e| panic!("failed to read {}: {}", &path, e));

    read_dir.map(|entry| {
        let entry = entry.unwrap_or_else(|e| panic!("failed on entry : {}", e));
        entry.file_name().to_string_lossy().into_owned()
    })
}

fn dir_list(path: String) -> Vec<String> {
    // Rust's Vec is returned as Steel List, probably because Steel Vector is immutable
    dir_file_name_iter(path).collect::<Vec<String>>()
}

fn main() -> std::io::Result<()> {
    let mut steel_engine = Engine::new();

    steel_engine.register_fn("dir-list", dir_list);

    run_repl(steel_engine)
}
