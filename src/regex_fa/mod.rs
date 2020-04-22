// [ab]         : a or b
// [^ab]        : not (a or b)
// [a-b]        : match c where a <= c <= b
// ()           : grouping
// ?            : match 0 or 1 times
// *            : match at least 0 times
// +            : match at least 1 times
// {n}          : preceeding item match n times
// {min,}       : preceeding item match at least min times
// {min, max}   : preceding item match at least min times and at most max times
// .            : wild card

mod nfa;
mod dfa;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum StateCode {
    Char,
    WildCard,
    Branch,
    Exit,
}

pub fn test_nfa(regex: &str, str: &str) {
    use nfa::ToNFA;

    let nfa = regex.to_nfa();
    println!("Regex is {}", regex);
    for i in nfa.states().iter() {
        println!("{}", i);
    }
    println!("{}", nfa.entry());
    println!("Match len: {}", nfa.match_first(str));
}
