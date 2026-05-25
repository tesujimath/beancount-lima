use std::{ffi::OsStr, process::Command};

const LIMABEAN_CLJ_LOCAL_ROOT: &str = "LIMABEAN_CLJ_LOCAL_ROOT";
const LIMABEAN_CLJ_DEPS: &str = "LIMABEAN_CLJ_DEPS";
const LIMABEAN_UBERJAR: &str = "LIMABEAN_UBERJAR";
const LIMABEAN_UBERJAR_BUILDTIME: Option<&str> = option_env!("LIMABEAN_UBERJAR");
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Additional Java options
const JVM_OPTIONS: &[&str] = &[
    "--enable-native-access=ALL-UNNAMED", // inhibit warning triggered by JLine
];

fn deps_from_env() -> String {
    let limabean_coord = if let Ok(local_root) = std::env::var(LIMABEAN_CLJ_LOCAL_ROOT) {
        format!(r###"{{:local/root "{}"}}"###, &local_root,)
    } else if let Ok(uberjar) = std::env::var(LIMABEAN_UBERJAR) {
        format!(r###"{{:local/root "{}"}}"###, &uberjar,)
    } else if let Some(uberjar) = LIMABEAN_UBERJAR_BUILDTIME {
        format!(r###"{{:local/root "{}"}}"###, &uberjar,)
    } else {
        format!(r###"{{:mvn/version "{}"}}"###, VERSION,)
    };

    let extra_deps = std::env::var(LIMABEAN_CLJ_DEPS).map(|s| format!(" {}", s));

    format!(
        r###"{{:deps {{io.github.tesujimath/limabean {}{}}}}}"###,
        limabean_coord,
        extra_deps.unwrap_or("".to_string())
    )
}

fn command<S>(deps: &str, args: &[S]) -> Command
where
    S: AsRef<str>,
{
    let mut cmd = Command::new("clojure"); // use clojure not clj to avoid rlwrap
    cmd.args(JVM_OPTIONS.iter().map(|opt| format!("-J{}", opt)))
        .arg("-Sdeps")
        .arg(deps)
        .arg("-M")
        .arg("-m")
        .arg("limabean.main");

    cmd.args(
        args.iter()
            .map(|s| OsStr::new(s.as_ref()))
            .collect::<Vec<_>>(),
    );

    cmd
}

#[cfg(unix)]
fn run_or_fail(mut cmd: Command) {
    use std::os::unix::process::CommandExt;

    let e = cmd.exec(); // on success does not return

    eprintln!(
        "limabean can't run {}: {}",
        cmd.get_program().to_string_lossy(),
        &e
    );
    std::process::exit(1);
}

pub(crate) fn run(args: &[String]) {
    let verbose = args.iter().any(|arg| arg == "-v" || arg == "--verbose");
    let version = args.iter().any(|arg| arg == "--version");

    if version {
        println!("limabean.rs  {VERSION}");
    }

    let deps = deps_from_env();
    let cmd = command(&deps, args);

    if verbose {
        eprintln!("{:?}", &cmd);
    }

    run_or_fail(cmd)
}
