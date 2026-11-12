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
  `Result<T, E>`, `Option<T>`, `Fn(T) -> U`, references `&T in a`. No implicit numeric conversions (use `n.to_f64()`, `x.to_i64()`);
  integer overflow and division by zero panic.
- **Variables**: `let` cannot be reassigned, `var` can.
- **Loops**: `while c { … }`; `for i in 0..n { … }`; `for x in v { … }` borrows each element of a `Vec`
  (`v` is locked during the loop), `for x in inout v` can modify them; `v` may also be a call (`for c in s.chars()`);
  `break`, `continue`.
- **Indexing**: `v[i]` is the i-th element (also written `v.get(i)`). You can read it, borrow it, assign a field
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
- **Generics and traits**: `fn same<T: Eq>(a: T, b: T) -> Bool`, monomorphised, statically dispatched.
  Methods go in `impl Type { … }` and are called as `x.m()`; plain functions are only called as `f(x)`. A trait method with a body is a default method that an impl may
  omit; impls can be generic: `impl<T: ToStr> ToStr for Vec<T> { … }`.
- **Closures**: a parameter of type `fn(T) -> U` takes a closure `|x| e`, which can read outer variables and modify
  outer `var`s. To store a closure in a variable, field or `Vec`, or to return it, use `Fn(T) -> U`: outer variables
  are taken into the closure by value (a `Str` is moved), and the closure may change its own environment (a counter),
  so it must be in a `var` when called (`var f: Fn(I64) -> I64 = |x| x + k`, `f(1)`; for fields and `Vec` elements
  write `fs[i].call(1)`).
- **References only come from arenas**: `arena |a| { let r = alloc(a, 1) … }`; a reference cannot outlive its arena.
- **String slices `&str in a`**: `text(a, s)` hands a string to an arena and gives a slice of the whole of it; `v.sub(i, j)`, `v.byte(i)`, `v.find(p)` (an `Option`), `v.eq(s)`, `v.len()` do not allocate (out of bounds panics), and `v.to_str()` copies into a `Str`. Like references, slices cannot outlive their arena. String literals do not allocate; they are copied only when modified. `fs.read_in(a, path)`, `io.read_all_in(a)` and `io.read_chunk(a, n)` read a file or standard input (any bytes) straight into an arena and give a slice. `io.next_block_in(r, a, n)` reads standard input chunk by chunk and each time gives a run of complete lines ending in a newline; with each chunk in an inner arena, memory does not grow with the input.
- **Modules**: one file per module; `use geom` brings in `geom.sn` from the same directory, and `geom.area(…)`
  accesses its `pub` items.
- **Strings**: `Str` holds UTF-8 bytes. `s.len()` counts bytes, `s.chars()` decodes `Char`s, `s.char_count()`
  counts characters; `s.push(t)`, `s.push_char(c)`, `s.slice(a, b)`, `s.parse_i64()`, `s.parse_f64()` (an `Option`).
  Interpolation: `"x = {x}, sum = {a + b}"`, with `{{` and `}}` for literal braces. It uses the built-in trait
  `ToStr`: I64, F64, Char, Bool and Str implement it; for other types write `fn to_str(x: T) -> Str` (one per type
  is fine), and generic code needs the bound `T: ToStr`. Escapes are `\n`, `\r`, `\t`, `\\`, `\"`; character
  literals look like `'a'`, `'中'`, `'\n'`.
- **I/O**: `println(s)` adds a newline, `print(s)` does not, other types go through interpolation `println("{x}")`; `io.write_byte(b)` writes one raw byte (0–255);
  `eprintln(s)`, `eprint(s)` write to standard error; `io.read_line()`, `env.args()`, `fs.read`, `fs.write` (see the standard library below).
- **Byte strings `Bytes`**: arbitrary bytes (I64 values 0–255), owned like `Str`. `Bytes.new()`, `b.len()`, `b.get(i)`,
  `b.set(i, x)`, `b.push(x)`, `b.append(c)`, `b.slice(a, b)`, `b.clone()`, `b.eq(c)`; `fs.read_bytes` and `fs.write_bytes`
  read and write binary files, `io.write_bytes(b)` writes them as they are; `s.to_bytes()` copies the bytes of a string,
  and `b.decode()` gives `.Some(s)` only for valid UTF-8.

## Standard library

The sources are in `std/`, written in Sinter. Built-in functions implemented by the compiler (`str_len`, `vec_push`, `read_line`, …) can only be called directly inside the standard library; programs use methods or standard library functions instead.

The core part (`std/core/`) is preloaded, no `use` needed:

| Contents | |
|---|---|
| traits | `Eq` (`a.eq(b)`), `Ord` (`a.cmp(b)` returns `.Less`, `.Equal`, `.Greater`), `Hash`; generic `min(a, b)`, `max(a, b)` |
| printing | `println(s)` adds a newline, `print(s)` does not; `eprintln`, `eprint` write to standard error. Other types go through interpolation: `println("{x}")` |
| I64, F64 | `abs`, `min`, `max`, `clamp`, `pow`, `gcd`, `lcm`, `to_f64`, `to_char`; `sqrt`, `floor`, `round`, `powi`, `to_i64`, … |
| Char | `is_digit`, `is_alpha`, `is_whitespace`, `to_upper`, `to_digit`, … (ASCII only) |
| Option, Result | `is_some`, `unwrap_or`, `unwrap`, `expect`, `map`, `ok`, … |
| Str | `split`, `trim`, `find` (`Option`), `contains`, `starts_with`, `replace`, `lines`, `to_upper`, `to_lower`, `repeat`, `parse_i64`; `join` on `Vec<Str>` |
| slices `&str in a` | `split`, `lines`, `trim`, `find`, `starts_with`, `parse_i64`, plus `line_end`, `trim_cr`, `field` that walk lines and fields without building a Vec; none of them allocate |
| Vec | `map`, `filter`, `fold`, `any`, `all`, `count`, `for_each`, `sum`, `max`, `min`, `reverse`, `contains`, `index_of`, `sort`, `sort_by`, `binary_search`, `dedup` |

Import the rest with `use std.name` and call it as `name.function(…)`; constructors live in the module, everything else is a method:

| Module | Contents |
|---|---|
| `std.io` | `read_line`, `read_all`, `input_lines`; `read_all_in(a)`, `read_chunk(a, n)` read into an arena; `lines()`, `next_block_in(r, a, n)` read lines chunk by chunk |
| `std.fs` | `read`, `write`, `read_lines`, `write_lines`, `read_bytes`, `write_bytes`, `read_in(a, path)`, `exists`, `is_dir`, `list_dir` (sorted by name), `make_dir`, `remove` |
| `std.env` | `args`, `get` (environment variables), `exit` |
| `std.process` | `run` (starts a process without a shell and returns its exit code), `output` (also collects its standard output) |
| `std.vec`, `std.math` | `range(n)`; `pi()`, `e()` |
| `std.map` | ordered map (AVL tree): `map.new()`, `m.insert(k, v)`, `m.get(k, \|v\| …)`, `m.remove(k)`, `m.for_each(\|k, v\| …)` |
| `std.hashmap` | hash map (open addressing; keys implement `Eq` and `Hash`; I64 and Str already do), same methods as `Map` |
| `std.bigint` | arbitrary-size integers: `bigint.from_i64(x)`, `parse(s)`; `a.add(b)`, `sub`, `mul`, `pow`, `divmod_small` (division by a number up to 10^9); implements `Eq`, `Ord`, `ToStr` |

The core algorithms of sorting, binary search, the two maps (including AVL balance) and big integers are proved in Lean (`Core/Library/` in the sinter-formal repository).

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
- Char classification and case conversion only know ASCII; `sort` is not stable.
- Type arguments of `Box` and `Vec` are invariant, which is stricter than Rust: some programs Rust accepts are rejected here.
- The generated C needs GCC or Clang (it uses GNU C extensions). There is no language server yet (editors get syntax highlighting only).
- Not there yet: trait objects (dynamic dispatch), dividing a big integer by a big integer, regular expressions, threads and networking,
  a package manager. `fs.read` does not check UTF-8 (invalid bytes
  come out as U+FFFD when iterating characters); use `fs.read_bytes` with `b.decode()` to check. `Bytes` has no
  `b[i]` and no `for x in b` yet (use `b.get(i)` and `0..b.len()`).

## Feedback

The most useful reports:
- programs you think should be accepted but are rejected (with the source and the error message);
- programs you think should be rejected but are accepted, or crash or leak at run time — these are the most
  serious problems, please do tell us;
- error messages you cannot understand.

## License

MIT or Apache-2.0, at your option ([LICENSE-MIT](LICENSE-MIT), [LICENSE-APACHE](LICENSE-APACHE)).
