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
- Install VSCode Extension: `rust-analyzer` (disable deprecated RLS, `Rust`)
- Install VSCode Extension: `Crates`
- Install VSCode Debugger Extension: `CodeLLDB`

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

- Install `ghcup`
- `ghcup install 8.10.2`  # install Haskell compiler
- Use VSCode
- Install VSCode Extension: `Haskell`
- Install VSCode Extension: `Haskell Syntax Highlighting`

----

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
❯ mkdir talk-hs && cd talk-hs

❯ cabal init --interactive
Guessing dependencies...

❯ cabal run
Build profile: -w ghc-8.10.2 -O1
	 🚀 Hello, Haskell! 🚀
```

```haskell
some_func :: Int -> String
some_func 0 = ""
some_func 1 = "Only one"
some_func a_number = show a_number

main :: IO ()
main = do
  putStrLn "\t 🚀 Hello, Haskell! 🚀 "
```

---

## Types, Values, and Cardinality

- Type: abstract set of things.
- Value: an example within a set of types.
- Cardinality: how many possible instances inahbit a type?

----

## Sum Types and Pattern Matching

```rust
pub enum OfferType {
  Conditional,
  Regular,
}

fn offer_type_to_str(otype: OfferType) -> String {
  match otype {
    OfferType::Conditional => "soft".to_string(),
    OfferType::Regular => "hard".to_string()
  }
}

```

----

## AKA Algebraic Data Types

```haskell
data OfferType =
  Conditional
  | Regular

offerTypeToLendio :: OfferType -> String
offerTypeToLendio Conditional = "soft"
offerTypeToLendio Regular     = "hard"
```

(This is the **best** feature I've ever found in a programming language.)

----

## How To Represent JSON?

```rust

// fn parse_json(some_str: String) -> ? {
//   // parse some_str into different things...??
// }

enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(Map<String, Value>),
}
```

---

## Product Types ("Records")

```rust
pub enum BodyDamage {
  DeepDent,
  LightScratches,
  CrumpledZone,
  MissingBumper,
}
struct SalvageVehicle {
  damage: BodyDamage,
  is_running: bool,
}
fn make_a_vehicle(damage: BodyDamage, is_running: bool) -> Vehicle {
    Vehicle {
        damage, is_running: is_running
    }
}
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

showMaybeInt :: Option Int -> String
showMaybeInt (Some number) = show number
showMaybeInt None = ""
```

----

## You can pattern match arbitrary or deeply nested things!

```rust
struct Point {
    x: i32,
    y: i32,
}
let p = Point { x: 0, y: 7 };

match p {
    Point { x, y: 0 } => println!("On the x axis at {}", x),
    Point { x: 0, y } => println!("On the y axis at {}", y),
    Point { x, y } => println!("On neither axis: ({}, {})", x, y),
}

// type is Result<Result<u32, error2> Error1>
match processed_data {
    Err(err) => "An Error1!",
    Ok(Err(some_string)) => "An Error2!",
    Ok(Ok(success)) => format!("{}", success * 4),
}
```

----

## What about functions over generic types?

```haskell
some_func :: a -> a
some_func val = ...?
```

Q: What is *known* about a generic type?

----

## `show` is a function that turns a value into a string


```haskell
some_string :: a -> String
some_string val = show val
```

But this doesn't compile!

----

## Adding a type Constraint Makes it Compile

```haskell
some_string :: (Show a) => a -> String
some_string val = show val
```

This is called a "typeclass"

----

## In Rust, These are called "Traits"

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

## Ownership Examples

```rust
fn takes_ownership(some_string: String) { // some_string comes into scope
    println!("{}", some_string);
} // Here, some_string goes out of scope and `drop` is called. The backing
  // memory is freed.

fn makes_copy(some_integer: i32) { // some_integer comes into scope
    println!("{}", some_integer);
} // Here, some_integer goes out of scope. Nothing special happens.
```

https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html#ownership-and-functions


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

## What can go wrong with this function?

```python

def calculate_interest_on_loan(
  loan_amount, years, interest_rate
):
  init_val = loan_amount * years * (interest_rate / 100)
  return init_val / 100
```

----

## Correctness at Compile Time

```rust

struct Interest(f32);
struct Years(u8);
struct LoanAmount(u32);

fn calculate_interest(loan_amt: LoanAmount, years: Years, int: Interest) -> f32 {
  // function goes here
}

```

---

## Why is Haskell interesting?

- Focus on _correctness_ enforced at compile-time.
- Move some business logic into the type system.
- Think of programs and types as algebraic relationships.
- Immutability. Referential transparency.
- Great for Concurrency.

**Downsides**:

- Remains a small community, pretty niche.
- With problems you're often on your own.
- Academic aspects can be offputting to newcomers.

----

## Why is Rust interesting?

- Also focus on _correctness_ at compile time.
- Insanely fast.
- Large and friendly community, growing fast.
- Great story for inter-operating with Python.
- Great tooling.

**Downsides**:

- Borrow-checker takes some time to learn.
- Still pretty new: things are changing fast.

---

## Some Useful Links

**Haskell**

- Learn You a Haskell for Great Good: http://learnyouahaskell.com/chapters

- Learn For Haskell: https://github.com/kowainik/learn4haskell

- What I Wish I Knew When Learning Haskell: http://dev.stephendiehl.com/hask/

- A Tour of Go in Haskell: https://a-tour-of-go-in-haskell.syocy.net/en_US/index.html

----

**Rust**

- Rust Book 2nd Edition: https://doc.rust-lang.org/book/title-page.html

- Rust by Example: https://doc.rust-lang.org/rust-by-example/