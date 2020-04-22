use super::StateCode;
use std::fmt;
use std::collections::VecDeque;

pub trait ToNFA {
    fn expand(&self) -> String;
    fn to_nfa(&self) -> NFA;
}

pub struct NFA {
    states: Vec<NFAState>,
    entry: usize,
}

#[derive(Clone)]
struct NFATracker {
    state: usize,
    loop_stack: Vec<(u32, usize)>,
    length: u32,
}

impl NFA {
    pub fn entry(&self) -> usize { self.entry }
    pub fn states(&self) -> &Vec<NFAState> { &self.states }
    pub fn match_first(&self, str: &str) -> u32 {
        let mut result = 0;

        let mut stack1 = vec![NFATracker{ state: self.entry(), loop_stack: vec![], length: 0 }];
        let mut stack2 = vec![];
        let mut stack_filled = &mut stack1;
        let mut stack_empty = &mut stack2;
        let mut queue = VecDeque::new();

        for c in str.chars() {
            if stack_filled.len() == 0 { break; }
            for _ in 0..stack_filled.len() {
                queue.push_back(stack_filled.pop().unwrap());
                loop {
                    match queue.pop_front() {
                        None => break,
                        Some(mut tracker) => {
                            let state = &self.states[tracker.state];
                            match state.code {
                                // if state is char or wildcard, match with current character
                                StateCode::Char => {
                                    if (c == state.c) == state.bool_flag {
                                        tracker.length += 1;
                                        tracker.state = state.next;
                                        stack_empty.push(tracker);
                                    }
                                },
                                StateCode::WildCard => {
                                    tracker.length += 1;
                                    tracker.state = state.next;
                                    stack_empty.push(tracker);
                                },
                                // if state is branch forward state to the options
                                StateCode::Branch => {
                                    match state.min {
                                        None => {
                                            let mut alt_tracker = tracker.clone();
                                            tracker.state = state.next;
                                            alt_tracker.state = state.alt;
                                            queue.push_back(tracker);
                                            queue.push_back(alt_tracker);
                                        },
                                        Some(min) => {
                                            if tracker.loop_stack.len() == 0 { tracker.loop_stack.push((0, state.id)); }
                                            let (_count_ref, id_ref) = tracker.loop_stack.last().unwrap();
                                            if *id_ref != state.id { tracker.loop_stack.push((0, state.id)); }

                                            let (count_ref, _id_ref) = tracker.loop_stack.last_mut().unwrap();
                                            if *count_ref < min {
                                                *count_ref += 1;
                                                tracker.state = state.alt;
                                                queue.push_back(tracker);
                                            }
                                            else {
                                                let max = match state.max {
                                                    None => std::u32::MAX,
                                                    Some(m) => m,
                                                };
                                                if *count_ref < max {
                                                    *count_ref += 1;

                                                    let mut alt_tracker = tracker.clone();
                                                    tracker.loop_stack.pop();

                                                    tracker.state = state.next;
                                                    alt_tracker.state = state.alt;
                                                    queue.push_back(alt_tracker);
                                                    queue.push_back(tracker);
                                                }
                                                else {
                                                    tracker.loop_stack.pop();
                                                    tracker.state = state.next;
                                                    queue.push_back(tracker);
                                                }
                                            }
                                        },
                                    };
                                },
                                StateCode::Exit => {
                                    if tracker.length > result { result = tracker.length; }
                                },
                            }; // match StateCode
                        },
                    }; // match queue
                }
            } // pop stack
            std::mem::swap(&mut stack_filled, &mut stack_empty);
        }

        for _ in 0..stack_filled.len() {
            let tracker = stack_filled.pop().unwrap();
            let state = &self.states[tracker.state];
            match state.code {
                StateCode::Exit => {
                    if tracker.length > result { result = tracker.length; }
                },
                StateCode::Branch => {
                    queue.push_back(tracker);
                    loop {
                        match queue.pop_front() {
                            None => break,
                            Some(mut tracker) => {
                                let state = &self.states[tracker.state];
                                match state.code {
                                    StateCode::Branch => {
                                        match state.min {
                                            Some(0) => {
                                                tracker.state = state.next;
                                                queue.push_back(tracker);
                                            },
                                            Some(min) => {
                                                match tracker.loop_stack.pop() {
                                                    Some((counter, id)) => {
                                                        if id == state.id {
                                                            if counter >= min {
                                                                tracker.state = state.next;
                                                                queue.push_back(tracker);
                                                            }
                                                        }
                                                    },
                                                    None => (),
                                                };
                                            }
                                            None => {
                                                let mut alt_tracker = tracker.clone();
                                                tracker.state = state.next;
                                                alt_tracker.state = state.alt;
                                                queue.push_back(tracker);
                                                queue.push_back(alt_tracker);
                                            },
                                        };
                                    },
                                    StateCode::Exit => {
                                        if tracker.length > result { result = tracker.length; }
                                    },
                                    _ => (),
                                };
                            },
                        };
                    }
                },
                _ => (),
            }
        }
        return result;
    } // fn match first
}

#[derive(Debug)]
pub struct NFAState {
    id: usize,
    code: StateCode,

    // universal
    next: usize,

    // if state code is Char
    c: char,
    bool_flag: bool,

    // if state code is Branch
    alt: usize,
    min: Option<u32>,
    max: Option<u32>,
}

impl NFAState {
    fn new_wildcard(id: usize, bool_flag: bool) -> NFAState {
        NFAState {
            id: id,
            code: StateCode::WildCard,
            next: 0,
            c: '\0',
            bool_flag: bool_flag,
            alt: 0,
            min: None,
            max: None,
        }
    }

    fn new_char(id: usize, c: char, bool_flag: bool) -> NFAState {
        NFAState {
            id: id,
            code: StateCode::Char,
            next: 0,
            c: c,
            bool_flag: bool_flag,
            alt: 0,
            min: None,
            max: None,
        }
    }

    fn new_branch(id: usize, min: Option<u32>, max: Option<u32>) -> NFAState {
        NFAState {
            id: id,
            code: StateCode::Branch,
            next: 0,
            c: '\0',
            bool_flag: false,
            alt: 0,
            min: min,
            max: max,
        }
    }

    fn new_exit(id: usize) -> NFAState {
        NFAState {
            id: id,
            code: StateCode::Exit,
            next: 0,
            c: '\0',
            bool_flag: false,
            alt: 0,
            min: None,
            max: None,
        }
    }
}

impl fmt::Display for NFAState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.code {
            StateCode::Char => write!(f, "{:>5} NFAS({} '{}', {})", self.id, self.bool_flag, self.c, self.next),
            StateCode::WildCard => write!(f, "{:>5} NFAS(WC, {})", self.id, self.next),
            StateCode::Branch => {
                match self.min {
                    Some(mi) => {
                        match self.max {
                            Some(ma) => write!(f, "{:>5} NFAS(BR, {}, {}, min: {}, max: {})", self.id, self.next, self.alt, mi, ma),
                            None => write!(f, "{:>5} NFAS(BR, {}, {}, min: {}, max: None)", self.id, self.next, self.alt, mi),
                        }
                    },
                    None => write!(f, "{:>5} NFAS(BR, {}, {})", self.id, self.next, self.alt),
                }
            },
            StateCode::Exit => write!(f, "{:>5} NFAS(Ex)", self.id),
        }
    }
}

const SEQ     : u32 = 0;
const OR      : u32 = 1;
const MINMAX  : u32 = 2;
const ESCAPED : u32 = 3;
const RANGE   : u32 = 4;

impl ToNFA for str {
    fn expand(&self) -> String {
        let mut ret = String::new();
        let mut chars = self.chars();
        loop {
            let c = match chars.next() {
                Some(c) => c,
                None => break,
            };
            match c {
                '*' => ret.push_str("{,}"),
                '?' => ret.push_str("{,1}"),
                '+' => ret.push_str("{1,}"),
                '\\' => {
                    let nc = match chars.next() {
                        Some(nc) => nc,
                        None => { ret.push('\\'); break; },
                    };
                    match nc {
                        '*' => ret.push('*'),
                        '?' => ret.push('?'),
                        '+' => ret.push('+'),
                        'd' => ret.push_str("[0-9]"),
                        'n' => ret.push('\n'),
                        't' => ret.push('\t'),
                        's' => ret.push_str("[\n\t ]"),
                        x   => { ret.push('\\'); ret.push(x); },
                    };
                },
                x => ret.push(x),
            };
        }
        return ret;
    }

    fn to_nfa(&self) -> NFA {
        let mut nfa : Vec<NFAState> = vec![];
        let mut counter = 0;

        let mut group : Option<(usize, Vec<usize>)> = None;
        let mut mode = SEQ;
        let mut bool_flag = true; // for regexes like "[^abc]"
        let mut modes = vec![];
        let mut layers = vec![vec![], vec![]];

        let mut min : u32 = 0;
        let mut max : Option<u32> = None;
        let mut borrowing_max = false;

        let _nor = false;

        for c in self.expand().chars() {
            match mode {
                SEQ => {
                    assert_ne!(c, ']', "illegal ]");
                    assert_ne!(c, '}', "illegal }}");

                    let seq = layers.last_mut().unwrap();
                    match c {
                        // Function call: Parse branches
                        // Argument: none
                        '[' => {
                            // commiting group
                            match group {
                                Some(g) => seq.push(g),
                                None    => (),
                            };
                            group = None;

                            mode = OR;
                            modes.push(SEQ);
                            layers.push(vec![]);
                        },
                        // Function call: Parse Branch_Sequence
                        // Argument: None
                        '(' => {
                            // commiting group
                            match group {
                                Some(g) => seq.push(g),
                                None    => (),
                            };
                            group = None;

                            modes.push(SEQ);
                            layers.push(vec![]);
                            layers.push(vec![]);
                        },
                        // Return from: Parse Branch_Sequence
                        // Return: Branch_o_Sequences
                        ')' => {
                            let pop_seq = layers.pop().unwrap();
                            let pop_or = layers.pop().unwrap();
                            let val_ret;
                            let mut vec_ret;

                            match group {
                                Some(( mut val_temp, vec_temp)) => {
                                    vec_ret = vec_temp;

                                    // weaving sequence
                                    for (val_r, vec_r) in pop_seq.into_iter().rev() {
                                        for &r in vec_r.iter() {
                                            nfa[r].next = val_temp;
                                        }
                                        val_temp = val_r;
                                    }

                                    // weaving branches
                                    for (val_r, mut vec_r) in pop_or.into_iter().rev() {
                                        vec_ret.append(&mut vec_r);

                                        let val_or = counter;
                                        counter += 1;

                                        nfa.push(NFAState::new_branch(val_or, None, None));
                                        nfa[val_or].next = val_r;

                                        nfa[val_or].alt = val_temp;
                                        val_temp = val_or;
                                    }

                                    val_ret = val_temp;

                                    group = Some((val_ret, vec_ret));
                                },
                                None => (),
                            };
                            mode = modes.pop().unwrap();
                        },
                        // Return from Sequence and call Sequence
                        // Return: a sequence
                        // Argument: None
                        '|' => {
                            let pop_seq = layers.pop().unwrap();
                            let or = layers.last_mut().unwrap();
                            let val_ret;
                            let vec_ret;

                            match group {
                                Some(( mut val_temp, vec_temp)) => {
                                    vec_ret = vec_temp;

                                    // weaving sequence
                                    for (val_r, vec_r) in pop_seq.into_iter().rev() {
                                        for &r in vec_r.iter() {
                                            nfa[r].next = val_temp;
                                        }
                                        val_temp = val_r;
                                    }
                                    val_ret = val_temp;

                                    or.push((val_ret, vec_ret));
                                    group = None;
                                },
                                None => panic!("epsilon or"),
                            };

                            layers.push(vec![]);
                        },
                        '{' => {
                            mode = MINMAX;
                        },
                        '.' => {
                            //commiting group
                            match group {
                                Some(g) => seq.push(g),
                                None    => (),
                            };

                            nfa.push(NFAState::new_wildcard(counter, true));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '\\' => {
                            //commiting group
                            match group {
                                Some(g) => seq.push(g),
                                None    => (),
                            };
                            group = None;

                            mode = ESCAPED;
                            modes.push(SEQ);
                        },
                        x   => {
                            //commiting group
                            match group {
                                Some(g) => seq.push(g),
                                None    => (),
                            };

                            nfa.push(NFAState::new_char(counter, x, true));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                    };
                },
                OR => {
                    let or = layers.last_mut().unwrap();
                    match c {
                        ']' => {
                            let pop_or = layers.pop().unwrap();
                            let val_ret;
                            let mut vec_ret;

                            match group {
                                Some(( mut val_temp, vec_temp)) => {
                                    vec_ret = vec_temp;

                                    for (val_r, mut vec_r) in pop_or.into_iter().rev() {
                                        vec_ret.append(&mut vec_r);

                                        let val_or = counter;
                                        counter += 1;

                                        nfa.push(NFAState::new_branch(val_or, None, None));
                                        nfa[val_or].next = val_r;

                                        nfa[val_or].alt = val_temp;
                                        val_temp = val_or;
                                    }

                                    val_ret = val_temp;

                                    group = Some((val_ret, vec_ret));
                                },
                                None => panic!("zero length or"),
                            };
                            mode = modes.pop().unwrap();
                            bool_flag = true;
                        },
                        '^' => {
                            match group {
                                Some(g) => {
                                    or.push(g);
                                    nfa.push(NFAState::new_char(counter, '^', bool_flag));
                                    group = Some((counter, vec![counter]));
                                    counter += 1;
                                },
                                None    => bool_flag = false,
                            };
                        },
                        '\\' => {
                            //commiting group
                            match group {
                                Some(g) => or.push(g),
                                None    => (),
                            };
                            group = None;

                            mode = ESCAPED;
                            modes.push(OR);
                        },
                        '-' => {
                            mode = RANGE;
                            modes.push(OR);
                        },
                        x   => {
                            //commiting group
                            match group {
                                Some(g) => or.push(g),
                                None    => (),
                            };

                            nfa.push(NFAState::new_char(counter, x, bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                    };
                },
                MINMAX => {
                    assert_ne!(c, ')', "illegal )");
                    assert_ne!(c, '(', "illegal (");
                    assert_ne!(c, ']', "illegal ]");
                    assert_ne!(c, '[', "illegal [");
                    assert_ne!(c, '{', "illegal {{");
                    match c {
                        ',' => {
                            borrowing_max = true;
                        },
                        '}' => {
                            if !borrowing_max {
                                max = Some(min);
                            }
                            
                            // commiting group
                            match group {
                                Some((val, vec)) => {
                                    if val != vec[0] {
                                        nfa.push(NFAState::new_branch(counter, Some(min), max));
                                        group = Some((counter, vec![counter]));

                                        nfa[counter].alt = val;
                                        for &r in vec.iter() {
                                            nfa[r].next = counter;
                                        }
                                        counter += 1;
                                    }
                                    else {
                                        let state = &mut nfa[val];
                                        match state.code {
                                            StateCode::Branch => {
                                                group = Some((val, vec));
                                                let temp = state.min.unwrap();
                                                state.min = Some(temp * min);
                                                match max {
                                                    Some(m1) => {
                                                        match state.max {
                                                            Some(m2) => state.max = Some(m1 * m2),
                                                            None => (),
                                                        };
                                                    },
                                                    None => state.max = None,
                                                };
                                            },
                                            _ => {
                                                nfa.push(NFAState::new_branch(counter, Some(min), max));
                                                group = Some((counter, vec![counter]));

                                                nfa[counter].alt = val;
                                                for &r in vec.iter() {
                                                    nfa[r].next = counter;
                                                }
                                                counter += 1;
                                            }
                                        };
                                    }
                                },
                                None    => panic!("illegal minmax, no modifyee"),
                            };

                            mode = SEQ;
                            min = 0;
                            max = None;
                            borrowing_max = false;
                        },
                        x   => {
                            assert!(x.is_digit(10), "illegal minmax, non-digit found");
                            if borrowing_max {
                                let val = match max {
                                    Some(v) => v,
                                    None => 0,
                                };
                                max = Some(val * 10 + x.to_digit(10).unwrap() as u32);
                            }
                            else {
                                min *= 10;
                                min += x.to_digit(10).unwrap() as u32;
                            }
                        },
                    };
                },
                ESCAPED => {
                    match c {
                        '(' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '(', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        ')' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, ')', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '[' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '[', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        ']' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, ']', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '{' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '{', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '}' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '}', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '.' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '.', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '-' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '-', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        '^' => {
                            mode = modes.pop().unwrap();
                            nfa.push(NFAState::new_char(counter, '^', bool_flag));
                            group = Some((counter, vec![counter]));
                            counter += 1;
                        },
                        _ => panic!("illegal escape"),
                    };
                },
                RANGE => {
                    mode = modes.pop().unwrap();
                    assert_eq!(mode, OR, "illegal range");

                    let or = layers.last_mut().unwrap();
                    let last_c = match group {
                        Some((val, vec)) => {
                            assert_eq!(val, vec[0], "wrong format");
                            assert_eq!(nfa[val].code, StateCode::Char, "wrong format");
                            or.push((val, vec));
                            nfa[val].c
                        },
                        _ => panic!("wrong format"),
                    };

                    assert!(last_c < c, "range tail is smaller than head");
                    for i in (last_c as u8 + 1)..(c as u8) {
                        nfa.push(NFAState::new_char(counter, i as char, bool_flag));
                        or.push((counter, vec![counter]));
                        counter += 1;
                    }
                    nfa.push(NFAState::new_char(counter, c, bool_flag));
                    group = Some((counter, vec![counter]));
                    counter += 1;
                },
                _ => panic!("illegal mode"),
            }; // match mode
        } // for c in string

        assert_eq!(modes.pop(), None, "not enough closing parens");
        let pop_seq = layers.pop().unwrap();
        let pop_or = layers.pop().unwrap();
        let mut val_ret = 0;
        let mut vec_ret = vec![];

        match group {
            Some(( mut val_temp, vec_temp)) => {
                vec_ret = vec_temp;

                for (val_r, vec_r) in pop_seq.into_iter().rev() {
                    for &r in vec_r.iter() {
                        nfa[r].next = val_temp;
                    }
                    val_temp = val_r;
                }

                for (val_r, mut vec_r) in pop_or.into_iter().rev() {
                    vec_ret.append(&mut vec_r);

                    let val_or = counter;
                    counter += 1;

                    nfa.push(NFAState::new_branch(val_or, None, None));
                    nfa[val_or].next = val_r;

                    nfa[val_or].alt = val_temp;
                    val_temp = val_or;
                }

                val_ret = val_temp;
            },
            None => (),
        };

        nfa.push(NFAState::new_exit(counter));

        for &r in vec_ret.iter() {
            nfa[r].next = counter;
        }
        return NFA{states: nfa, entry: val_ret};
    } // fn to_regex
}
