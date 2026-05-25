use std::process::Command;

#[derive(Clone, Debug)]
enum Health {
    Good(String),
    Bad(String),
}

pub(crate) fn check(verbose: bool) {
    check_clojure(verbose);
}

fn check_clojure(verbose: bool) {
    match clojure_health() {
        Health::Good(description) => {
            if verbose {
                eprintln!("{}", description);
            }
        }
        Health::Bad(reason) => {
            eprintln!("limabean {reason}");
            std::process::exit(1);
        }
    }
}

fn clojure_health() -> Health {
    match Command::new("clojure")
        .arg("--version")
        .output()
        .map(|op| String::from_utf8_lossy(op.stdout.as_slice()).replace("\n", "; "))
    {
        Ok(description) => Health::Good(format!("clojure: {}", description)),
        Err(e) => Health::Bad(format!("can't find clojure: {}", &e)),
    }
}
