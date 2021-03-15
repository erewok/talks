# Let's Do Some Rust:
### Learning Rust with Some Haskell Inspiration

Erik Aker
@erewok

----

## Administrivia

All code for this talk available here:

github.com/erewok/talks/tree/main/rust_with_haskell_lessons

----

## Setting Up Our Coding Environment for Rust

- Install Rustup
- Use VSCode
- Install VSCode Extension: `rust-analyzer` (disable deprecated RLS, "Rust")
- Install VSCode Extension: Crates
- Install VSCode Extension: CodeLLDB

----

## Debugging Rust with CodeLLDB

- VSCode Launch.json:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "lldb",
            "request": "launch",
            "name": "Debug",
            "cargo": {
                "args": [
                    "build",
                    "--bin=lendio-rs",
                    "--package=lendio-rs"
                ],
                "filter": {
                    "name": "lendio-rs",
                    "kind": "bin"
                }
            },
            "args": [],
            "cwd": "${workspaceFolder}",
            "sourceLanguages": [
                "rust"
            ],
            "sourceMap": {
                "/rustc/*": "${env:HOME}/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust"
            }
        }
    ]
}
```
----

## Setting Up Our Coding Environment for Haskell

- Install ghcup
- ghcup install 8.10.2  # install a recent Haskell compiler
- Use VSCode
- Install VSCode Extension: Haskell (Haskell Language Server)
- Install VSCode Extension: Haskell Syntax Highlighting

---

## For Haskell Use the REPL (no debugger)

```sh
❯ cabal repl
Build profile: -w ghc-8.10.2 -O1
In order, the following will be built (use -v for more details):
 - talk-hs-0.1.0.0 (lib) (file MyLib.hs changed)
Preprocessing library for talk-hs-0.1.0.0..
GHCi, version 8.10.2: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/eaker/.ghc/ghci.conf
[1 of 1] Compiling MyLib            ( MyLib.hs, interpreted )
λ>  1 * 2 + 3
5
```

---

## Rust Basics

```sh
❯ cargo new talk-rs
     Created binary (application) `talk-rs` package
```

```rust
fn some_func(an_argument: String) -> i32 {
  println!("Here's a rust function!");
  println!("My argument was {}", an_argument);
  // We're returning the last expression: NO SEMICOLON!
  4
}

fn main() {
  let some_arg: String = "the emperor of ice cream".to_string();
  some_func();
  // this function doesn't return anything...
}
```

Use `cargo run` to run it.

----

## Haskell Basics

```sh
❯ mkdir talk-hs

❯ cd talk-hs

❯ cabal init --interactive
Should I generate a simple project with sensible defaults? [default: y] y

Guessing dependencies...
...

❯ cabal run
Resolving dependencies...
Build profile: -w ghc-8.10.2 -O1
...
Hello, Haskell!
someFunc
```

## Sum Types, Product Types

```haskell
data OfferType =
  Conditional
  | Regular
  deriving (Generic, Show, Eq)

offerTypeToLendio :: OfferType -> String
offerTypeToLendio Conditional = "soft"
offerTypeToLendio Regular     = "hard"

data Vehicle = Vehicle {
  wheelCount :: Int,
  isATruck :: Bool,
  modelName :: String
  } deriving (Show)

a_vehicle = Vehicle {
    wheelCount=4,
    isATruck=True,
    modelName="tacoma"}
```

----

## Very Common Types: Option, and Result

```haskell
data Option a =
  Some a
  | None
  deriving (Show)

data Result t e =
  Ok t
  | Err e
  deriving (Show)
```

----

## What about generic types?

```haskell
some_func :: a -> a
some_func val = ...?
```

----

## What is known about a generic type?

`show` is a function that turns a value into a string.

```haskell
some_string :: a -> String
some_string val = show val
```

But this doesn't compile!

----

## In Rust, These are "Traits"


```rust
fn generics<T: std::fmt::Display>(some_generic_type: T) -> String {
    format!("We're building a string with a generic thing [ => {} <= ]!", some_generic_type)
}
```

**Note**: The _caller_ decides what this `T` will be.

---

## The Elephant in the Room: Rust's Borrow Checker

```rust
    let s1 = String::from("hello");
    let s2 = s1;
    println!("{}, world!", s1);
```

```cargo run
58 |     let s1 = String::from("hello");
   |         -- move occurs because `s1` has type `String`, which does not implement the `Copy` trait
59 |     let s2 = s1;
   |              -- value moved here
60 |
61 |     println!("{}, world!", s1);
   |                            ^^ value borrowed here after move
```

----

# Examples of Ownership from Rust Book

https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html#ownership-and-functions

```rust
fn takes_ownership(some_string: String) { // some_string comes into scope
    println!("{}", some_string);
} // Here, some_string goes out of scope and `drop` is called. The backing
  // memory is freed.

fn makes_copy(some_integer: i32) { // some_integer comes into scope
    println!("{}", some_integer);
} // Here, some_integer goes out of scope. Nothing special happens.
```

----

## What is borrowing?

```rust
fn main() {
    let s1 = String::from("hello");

    let len = calculate_length(&s1);

    println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
    s.len()
}
```

---

## Why is Haskell interesting?

- Focus on _correctness_ enforced at compile-time.
- Amazing Concurrency.
- Moving some business logic into the type system.
- Thinking of programs as algebraic relationships.

**Downsides**:

- Remains a small community, pretty niche.
- For problems you're often on your own.
- Academic aspects can be offputting to newcomers.

----

## Why is Rust interesting?

- Inspired by langs like Haskell to pursue _correctness_ at compile time.
- Insanely fast.
- Large and friendly community, growing fast.
- Great story for inter-operating with Python.

**Downsides**:

- Borrow-checker takes some time to learn.
- Still pretty new: things are changing fast.

----

## Correctness at Compile Time

Q: What can go wrong with this function?

```python

def calculate_interest_on_loan(loan_amount, years, interest_rate):
  return (loan_amount * years * (interest_rate/100))/100
```

---

## Some Useful Links

**Haskell**

- Learn You a Haskell for Great Good: http://learnyouahaskell.com/chapters

- Learn For Haskell: https://github.com/kowainik/learn4haskell

- What I Wish I Knew When Learning Haskell: http://dev.stephendiehl.com/hask/

- A Tour of Go in Haskell: https://a-tour-of-go-in-haskell.syocy.net/en_US/index.html

**Rust**

- Rust Book 2nd Edition: https://doc.rust-lang.org/book/title-page.html

- Rust by Example: https://doc.rust-lang.org/rust-by-example/