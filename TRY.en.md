# Trying Sinter (0.2.0-pre)

[中文版](TRY.md)

Sinter is a simplified Rust: it has ownership, moves, borrowing and regions (arenas), but no lifetime
annotations, and the core rules the compiler checks are proved in Lean 4. `sinterc` compiles `.sn` source
files to C and hands the result to your system C compiler.

This is a **pre-release**: the language and standard library will still change. Please try it and tell us
what you find (see the last section).

Error messages are in Chinese by default. Pass `--lang=en`, or set it once for your shell:

```sh
export SINTER_LANG=en
```

## Install

You need OCaml 5.1 or later (via opam), dune 3, menhir, and GCC or Clang.

```sh
opam install dune menhir
git clone https://github.com/clawbolt/sinterc.git compiler
cd compiler
dune build
dune install        # installs sinterc into the current opam switch; or use dune exec sinterc -- …
sinterc --version   # sinterc 0.2.0-pre
```

To check the proofs yourself you also need Lean 4 (install it with elan). The proofs live in a separate
repository, [sinter-formal](https://github.com/clawbolt/sinter-formal); clone it as `lean` next to `compiler`:

```sh
git clone https://github.com/clawbolt/sinter-formal.git lean   # in the same directory as compiler
cd lean && lake build              # all proofs (the Lean version is in lean-toolchain)
cd ../compiler && sh validate.sh   # control-flow graphs of the examples, checked independently by Lean
sh diff.sh                         # 42,000 programs: Lean, sinterc and runtime behaviour compared
```

## A first program

`examples/tour/` has two tour programs:

```sh
sinterc run examples/tour/tour.sn       # structs, parameter modes, Box, Vec, match, arenas
echo "a b a" | sinterc run examples/tour/words.sn   # word count: files, strings, HashMap, sorting, closures
sinterc build examples/tour/tour.sn     # builds the executable examples/tour/tour
sinterc examples/tour/tour.sn -o t.c    # emits C only
sinterc fmt -w examples/tour/*.sn       # formats the files
```

The C compiler is taken from `$CC` (default: `cc`).

## The language at a glance

- **Types**: `I64`, `F64`, `Bool`, `Char`, `Str`, `Bytes`, `()`, structs, enums, tuples `(A, B)`, `Box<T>`, `Vec<T>`,
  `Result<T, E>`, `Option<T>`, `Fn(T) -> U`, references `&T in a`. No implicit numeric conversions (use `to_f64`, `to_i64`);
  integer overflow and division by zero panic.
- **Variables**: `let` cannot be reassigned, `var` can.
- **Loops**: `while c { … }`; `for i in 0..n { … }`; `for x in v { … }` borrows each element of a `Vec`
  (`v` is locked during the loop), `for x in inout v` can modify them; `v` may also be a call (`for c in s.chars()`);
  `break`, `continue`.
- **Indexing**: `v[i]` is the i-th element (the same as `vec_get(v, i)`). You can read it, borrow it, assign a field
  (`v[i].x = 1`) or replace it with `v[i] = x` (the old value is dropped first); out of bounds panics. Elements
  cannot be moved out directly; use `v.pop()` to take one.
- **Parameter modes**: `let` by default (read-only borrow; the argument is still usable afterwards);
  `inout` (modifies the caller's variable or field); `sink` (takes ownership).
- **Ownership**: `Box`, `Vec`, `Str` and values containing them are moved and dropped automatically at the end
  of their scope; using a moved value is a compile error. You can move a single field out of a struct and put
  it back later.
- **`linear struct`**: must be `consume`d explicitly; forgetting is a compile error. Combine with `Result`, `?`
  and `errdefer` for error paths. `panic(msg)` ends the program immediately (exit code 101).
- **Enums and `match`**: `.Some(x)`, `.None`; a `_` arm; literal arms for integers, characters, strings and
  `true`/`false`. Patterns nest: `.Some((0, s))`, `Ok(.None)`, `(0, _)`; `match (a, b) { … }` matches tuples.
  The compiler checks that every case is covered (or reports an example that isn't) and that no arm is
  unreachable. `match let x { … }` binds by borrowing; so does matching on a `let` parameter.
- **Generics and traits**: `fn same<T: math.Eq>(a: T, b: T) -> Bool`, monomorphised, statically dispatched.
  Method syntax `v.push(x)` means `push(v, x)`. A trait method with a body is a default method that an impl may
  omit; impls can be generic: `impl<T: ToStr> ToStr for Vec<T> { … }`.
- **Closures**: a parameter of type `fn(T) -> U` takes a closure `|x| e`, which can read outer variables and modify
  outer `var`s. To store a closure in a variable, field or `Vec`, or to return it, use `Fn(T) -> U`: outer variables
  are taken into the closure by value (a `Str` is moved), and the closure may change its own environment (a counter),
  so it must be in a `var` when called (`var f: Fn(I64) -> I64 = |x| x + k`, `f(1)`; for fields and `Vec` elements
  write `fs[i].call(1)`).
- **References only come from arenas**: `arena |a| { let r = alloc(a, 1) … }`; a reference cannot outlive its arena.
- **Modules**: one file per module; `use geom` brings in `geom.sn` from the same directory, and `geom.area(…)`
  accesses its `pub` items.
- **Strings**: `Str` holds UTF-8 bytes. `s.len()` counts bytes, `s.chars()` decodes `Char`s, `s.char_count()`
  counts characters; `s.push(t)`, `s.push_char(c)`, `s.slice(a, b)`, `s.to_i64()`, `s.to_f64()`.
  Interpolation: `"x = {x}, sum = {a + b}"`, with `{{` and `}}` for literal braces. It uses the built-in trait
  `ToStr`: I64, F64, Char, Bool and Str implement it; for other types write `fn to_str(x: T) -> Str` (one per type
  is fine), and generic code needs the bound `T: ToStr`. Escapes are `\n`, `\r`, `\t`, `\\`, `\"`; character
  literals look like `'a'`, `'中'`, `'\n'`.
- **I/O**: `print(s)`, `print_i64`, `print_f64`, `print_char` (all add a newline); `print_str(s)` does not, and
  `print_byte(b)` writes one raw byte (0–255); `eprint(s)`, `eprint_str(s)` write to standard error; `read_line()`, `args()`, `read_file`, `write_file`.
- **Byte strings `Bytes`**: arbitrary bytes (I64 values 0–255), owned like `Str`. `bytes_new()`, `b.len()`, `b.get(i)`,
  `b.set(i, x)`, `b.push(x)`, `b.append(c)`, `b.slice(a, b)`, `b.clone()`, `b.eq(c)`; `read_bytes` and `write_bytes`
  read and write binary files, `print_bytes(b)` writes them as they are; `s.to_bytes()` copies the bytes of a string,
  and `str_from_utf8(b)` gives `.Some(s)` only for valid UTF-8.

## Standard library

Import a module with `use std.name` and call it as `name.function(…)`. The sources are in `std/`.

| Module | Contents |
|---|---|
| `std.math` | traits `Eq`, `Ord`; `min`, `max`, `abs`, `pow`, `gcd`, `lcm`; F64 `sqrt`, `floor`, `round`, `pi`, … |
| `std.option`, `std.result` | `is_some`, `unwrap_or`, `unwrap`, `expect`, … |
| `std.vec` | `map`, `filter`, `fold`, `any`, `all`, `count`, `sum`, `max`, `min`, `reverse`, `contains`, `range`, `sort`, `sort_by`, `binary_search`, `dedup` |
| `std.str` | `split`, `join`, `trim`, `find`, `contains`, `starts_with`, `replace`, `lines`, `to_upper`, `to_lower`, `repeat` |
| `std.char` | `is_digit`, `is_alpha`, `is_whitespace`, `to_upper`, `to_digit`, … (ASCII only) |
| `std.io` | `read_lines`, `write_lines`, `input_lines` |
| `std.os` | `env`, `list_dir` (sorted by name), `exists`, `is_dir`, `make_dir`, `remove`, `run` (starts a process without a shell and returns its exit code), `output` (also collects its standard output), `exit` |
| `std.map` | ordered map (two sorted Vecs, binary search) |
| `std.treemap` | ordered map (AVL tree, logarithmic insert and remove) |
| `std.hashmap` | hash map (open addressing; keys implement `Eq` and `hashmap.Hash`; I64 and Str already do) |
| `std.bigint` | arbitrary-size integers: `from_i64`, `parse`, `to_str`, `add`, `sub`, `mul`, `pow`, `divmod_small` (division by a number up to 10^9); implements `Eq` and `Ord` |

The core algorithms of sorting, binary search, the three maps (including AVL balance) and big integers are proved in Lean (S4–S11 and S6b in the sinter-formal README).

## Tools

- `sinterc run FILE [ARGS…]`, `sinterc build FILE [-o EXE]`, `sinterc FILE [-o OUT.c]`; with `--release`, `build` and `run` compile with -O2 and skip the leak check at exit.
- `sinterc fmt [--check | -w] FILE…`: formatting. It only changes whitespace (indentation, spaces around
  operators, blank lines); tokens and line breaks stay where they are, and comments are kept.
- Errors show the offending line with a `^` under the position.
- Editors: `editor/` has syntax highlighting for VS Code and Vim; see `editor/README.md` to install.

Fuller examples of each feature are in `test/*.t`: each file starts with what that step added, followed by
programs that run and programs that must be rejected.

## Known limitations

- Closures cannot be nested, and the environment of an `Fn(…)` closure cannot contain references.
- There are no struct patterns yet (`P { x, y }` only works in `let`); patterns cannot look inside a `Box`.
- `std.char` and case conversion only know ASCII; `sort` is not stable.
- Type arguments of `Box` and `Vec` are invariant, which is stricter than Rust: some programs Rust accepts are rejected here.
- The generated C needs GCC or Clang (it uses GNU C extensions). There is no language server yet (editors get syntax highlighting only).
- Not there yet: trait objects (dynamic dispatch), dividing a big integer by a big integer, regular expressions, threads and networking,
  a package manager. `read_file` does not check UTF-8 (invalid bytes
  come out as U+FFFD when iterating characters); use `read_bytes` with `str_from_utf8` to check. `Bytes` has no
  `b[i]` and no `for x in b` yet (use `b.get(i)` and `0..b.len()`).

## Feedback

The most useful reports:
- programs you think should be accepted but are rejected (with the source and the error message);
- programs you think should be rejected but are accepted, or crash or leak at run time — these are the most
  serious problems, please do tell us;
- error messages you cannot understand.

## License

MIT or Apache-2.0, at your option ([LICENSE-MIT](LICENSE-MIT), [LICENSE-APACHE](LICENSE-APACHE)).
