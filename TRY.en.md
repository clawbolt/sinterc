# Trying Sinter (0.1.0)

[中文版](TRY.md)

Sinter is a simplified Rust: it has ownership, moves, borrowing and regions (arenas), but no lifetime
annotations, and the core rules the compiler checks are proved in Lean 4. `sinterc` compiles `.sn` source
files to C and hands the result to your system C compiler.

Error messages are in Chinese by default. Pass `--lang=en`, or set it once for your shell:

```sh
export SINTER_LANG=en
```

## Install

You need OCaml 5.1 or later (installed with opam), dune 3, menhir, and GCC or Clang.

```sh
opam install dune menhir
cd compiler
dune build
dune install        # installs sinterc into the current opam switch; or use dune exec sinterc -- …
sinterc --version   # sinterc 0.1.0
```

## Your first program

`examples/tour/tour.sn` is a small program that covers the main features:

```sh
sinterc run examples/tour/tour.sn      # compile and run
sinterc build examples/tour/tour.sn    # produce the executable examples/tour/tour
sinterc examples/tour/tour.sn -o t.c   # emit C only
```

The C compiler is taken from the `CC` environment variable (default `cc`).

The comments and printed text in the examples and tests are in Chinese; the code itself reads the same in any language.

## The language at a glance

- Types: `I64`, `Bool`, `()`, structs, enums, `Box<T>`, `Vec<T>`, `Result<T, E>`, `Option<T>`, and references `&T in a`.
- Variables: `let` cannot be reassigned, `var` can.
- Parameter modes: `let` by default (read-only borrow; the argument is still usable after the call); `inout` (modifies the caller's variable or field); `sink` (takes ownership).
- Ownership: `Box`, `Vec` and structs containing them are moved, and dropped automatically at the end of their scope; using a value after moving it is a compile error.
  You can move out a single field of a struct (`var a = q.p.a`) and put it back later (`q.p.a = box(7)`).
- `linear struct`: must be explicitly `consume`d; forgetting to handle one is a compile error. Use `Result`, `?` and `errdefer` to write error paths.
- Enums and `match`: `.Some(x)`, `.None`.
- Generics and traits: `fn same<T: Eq>(a: T, b: T) -> Bool`, monomorphized and statically dispatched. Method-call syntax `v.push(x)` is the same as `push(v, x)`.
- References come only from arenas: `arena |a| { let r = alloc(a, 1) … }`; a reference cannot outlive its arena.
  Functions can take region parameters: `fn get(o: Option<&I64 in x>) -> I64`.
- Output: `print("text")` (a string literal; escapes are `\n`, `\t`, `\\`, `\"`), `print_i64`, `print_bool`.

More complete examples of each feature are in `test/c*.t`. Each file starts by saying what that step added, followed by programs that run and programs that must be rejected.

## Known limitations

- Only integers and booleans. Strings can only be arguments to `print`; they are not a type yet. No floats, closures or modules; a program is a single file.
- Error messages are Chinese by default; `--lang=en` (or `SINTER_LANG=en`) switches to English. A few messages are not translated yet and still appear in Chinese. Only the first error is reported.
- Type parameters of `Box` and `Vec` are invariant across instances, which is stricter than Rust: some programs Rust accepts are rejected here.
- Runtime errors (for example an index out of bounds) print `panic: …` and exit with code 101.
- The generated C needs GCC or Clang (it uses GNU C extensions).

## Feedback

The most useful feedback:
- programs you think should compile but are rejected (please include the source file and the error message);
- programs you think should be rejected but compile, or crash at run time. These are the most serious problems, so please always tell us;
- error messages you cannot understand. If an English message still shows Chinese, tell us that too.
