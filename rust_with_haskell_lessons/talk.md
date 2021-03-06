# Let's Do Some Rust:
### Learning Rust with Some Haskell Inspiration

Erik Aker
@erewok

----

## Administrivia

All code for this talk available here:

github.com/erewok/talks/tree/master/rust_with_haskell_lessons

----

## Setting Up Our Coding Environment for Rust

- Install Rustup
- Use VSCode
- Install VSCode Extension: rust-analyzer (disable deprecated RLS)
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

## Use the REPL

```sh
❯ cabal repl
Resolving dependencies...
Build profile: -w ghc-8.10.2 -O1
In order, the following will be built (use -v for more details):
 - fake-package-0 (lib) (first run)
Configuring library for fake-package-0..
Preprocessing library for fake-package-0..
Warning: No exposed modules
GHCi, version 8.10.2: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/eaker/.ghc/ghci.conf
λ>
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

----

## Sum Types, Product Types

```haskell
data OfferType =
  Conditional
  | Regular
  deriving (Generic, Show, Eq)

offerTypeToLendio :: OfferType -> Value
offerTypeToLendio Conditional = String "soft"
offerTypeToLendio Regular     = String "hard"


data Vehicle = Vehicle {
  wheelCount :: Int,
  isATruck :: Bool,
  modelName :: String
  } deriving (Show)

a_vehicle = Vehicle {wheelCount=4, isATruck=True, modelName="tacoma"}
```

----

## Extremely Common Types: Option, and Result

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

```haskell
some_func :: (Show a) => a -> String
some_func val = show val
```

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

----

## Some Useful Links

**Haskell**

- Learn You a Haskell for Great Good: http://learnyouahaskell.com/chapters
-

**Rust**

- Rust Book 2nd Edition: https://doc.rust-lang.org/book/title-page.html

