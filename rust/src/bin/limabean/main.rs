use tracing_subscriber::EnvFilter;

fn main() {
    let subscriber = tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .finish();
    tracing::subscriber::set_global_default(subscriber).unwrap();

    let args = std::env::args().collect::<Vec<_>>();

    if let Some("health") = args.get(1).map(String::as_str) {
        health::check(true);
    } else {
        run::run(&args[1..]);
    }
}

mod health;
mod run;
