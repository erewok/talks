/// Options
pub fn some_option1() -> Option<String> {
    Some("Ice Cream!".to_string())
}

pub fn some_option2() -> Option<String> {
    None
}

/// Accepting an option means evaluating/unpacking it
pub fn accepts_option(testval: Option<String>) {
    let newstr = "hello! ".to_string();

    let test_resolved = match testval {
        Some(stringy) => stringy,
        None => "[argument was empty :(]".to_string(),
    };

    println!("{}{}", newstr, test_resolved);
}

/// Results
pub fn some_result1() -> Result<Vec<u32>, String> {
    let v1 = vec![1, 2, 3, 4];
    Ok(v1)
}

pub fn some_result2() -> Result<Vec<u32>, String> {
    Err("BROKEN".to_string())
}

pub fn accepts_result(testval: Result<Vec<u32>, String>) -> () {
    let printstr: String = match testval {
        Ok(somevec) => format!("It's this long! {}", somevec.len()),
        Err(err_string) => err_string,
    };
    println!("{}", printstr);
}


fn option_and_result() {
    let val = some_option2();
    accepts_option(val);

    accepts_result(some_result1());
    accepts_result(some_result2());
}


fn some_func(an_argument: String) -> i32 {
  println!("Here's a rust function!");
  println!("My argument was {}", an_argument);
  // notice: we're returning the last expression: NO SEMICOLON!
  4
}

fn ownership() -> () {
    let s1 = String::from("hello");
    let s2 = s1;

    // println!("{}, world!", s1);
}

fn generics<T: std::fmt::Display>(some_generic_type: T) -> String {
    format!("We're building a string with a generic thing [ => {} <= ]!", some_generic_type)
}

fn main() {
  let some_arg: String = "the emperor of ice cream".to_string();
  some_func(some_arg);
  option_and_result();
  // this function doesn't return anything...
  let res = generics(true);
  println!("{}", res);
}