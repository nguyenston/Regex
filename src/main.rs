pub mod regex_fa;

use std::env;

fn main() {
    let mut args : Vec<String>= env::args().collect();
    if args.len() < 3 {
        println!("invalid arguments!");
        println!("cargo run <regex> <string>");
        std::process::exit(1);
    }
    let regex = &args[1];
    let str = &args[2];

    regex_fa::test_nfa(regex, str);
}
