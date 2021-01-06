# p4-to-gcl
(name is a work in progress)

This tool aims to validate various safety properties of [P4] programs.
It does this by converting P4 code to [GCL] and generating logical predicates
which, when satisfiable, prove various properties about the code.

## Build
This project is written in Rust, so first install Rust:
https://www.rust-lang.org/tools/install

There is also a dependency on the [Z3] theorem prover, so install the library
(including header files) either from source or your distribution's package
manager (`z3-devel` for Fedora, `libz3-dev` for Debian systems).

Now that Rust and Z3 are installed, use `cargo` to build the project
(with optimizations):
```
cargo build --release
```

The compiled binary is located at `target/release/p4-to-gcl`.

[P4]: https://en.wikipedia.org/wiki/P4_(programming_language)
[GCL]: https://en.wikipedia.org/wiki/Guarded_Command_Language
[Z3]: https://github.com/Z3Prover/z3