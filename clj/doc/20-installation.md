# Installation

## Packages

### Nix

`limabean` is available as a Nix flake at `url = "github:tesujimath/limabean"`, and this flake pulls in the Clojure CLI tools automatically.  It currently runs from Clojars, but is likely in future to run standalone.

### Arch Linux

`limabean` is available in the [Arch Linux user repository](https://aur.archlinux.org/packages/limabean), so may be installed using `paru -S limabean`.

This runs in standalone mode.

### Other packaging

If you have packaged or are interested in packaging `limabean` for your distro of choice, please reach out for help or at least a mention here.  Thanks!

## Installation modes

There are two ways to run `limabean`, either standalone or from Clojars.  Running from Clojars means that the required Clojure packages are downloaded by the Clojure runtime on first use.

Selection of runtime is determined by the following:

1. If the environment variable `LIMABEAN_CLJ_LOCAL_ROOT` is defined at runtime, that is the path to local Clojure source, and is used to run the [development version](50-development.md)
2. If the environment variable `LIMABEAN_UBERJAR` is defined at runtime, that is the path to the standalone application jarfile
3. If the environment variable `LIMABEAN_UBERJAR` was defined at buildtime, that is the path to the standalone application jarfile
4. Otherwise, the application whose version matches `limabean` is run from Clojars

Even when running from a jarfile, `clojure` is used rather than `java`, in order to support plugins from arbitrary sources.

## Manual Installation

Running from Clojars is recommended for anyone using the [GitHub release](https://github.com/tesujimath/limabean/releases), that is, not setting any of the environment variables listed above.

## Running from Podman

If you have Podman (or Docker) installed, you can build a local container to run limabean in standalone mode:

```
VERSION=$(bash ./scripts.dev/get-version.sh)
podman build -f Containerfile --build-arg VERSION=$VERSION -t limabean:$VERSION
```

Then run it, mounting the directory where the beancount files are as `/data`:

```
podman run -it --rm --name limabean -v .:/data limabean:$VERSION --beanfile accounting.beancount
```

## Running from Clojars

Requirements:

1. The [Clojure CLI](https://clojure.org/reference/clojure_cli) is required to be installed separately, and `clojure` must be on the user's path.

2. The two Rust binaries `limabean` and `limabean-pod` must be installed and on the path.

The corresponding `limabean` Clojure code is downloaded automatically on first run from [Clojars](https://clojars.org/io.github.tesujimath/limabean/).

Options for installing the Rust binaries:

1. Tarballs and zipfiles are provided for each [GitHub release](https://github.com/tesujimath/limabean/releases) for Linux and macOS.

2. If you have a Rust toolchain installed, `cargo install limabean` will install the two binaries `limabean` and `limabean-pod` into `~/.cargo/bin`.  Add this directory to your path before running `limabean`

### macOS

On macOS it is necessary to remove the quarantine attributes after unpacking the tarball, e.g.

```
xattr -rd com.apple.quarantine ./limabean/bin
```

### Windows

Native Windows is no longer supported.  Ubuntu on WSL 2 is recommended as an alternative on that platform.

## Standalone

Requirements:

1. The [Clojure CLI](https://clojure.org/reference/clojure_cli) is required to be installed separately, and `clojure` must be on the user's path.

2. The two Rust binaries `limabean` and `limabean-pod` must be installed and on the path.

3. The limabean standalone jarfile must be available at a location given by the environment variable `LIMABEAN_UBERJAR`

If this environment variable is defined when building the Rust binaries, it is not required at runtime, which is recommended when packaging `limabean`.

## Building from source

The [`justfile`](../../justfile) has recipes for building from source.

The two Rust binaries are built in `rust/target/{release,debug}`, and the jarfiles in `clj/target`.
