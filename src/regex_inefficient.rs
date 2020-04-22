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


pub trait RegexConvertable {
    fn to_regex(&self) -> Regex;
}

pub trait RangeCompress: RangeAdd where Self: Sized {
    fn range_compress(&mut self);
}

pub trait RangeAdd<Rhs = Self> {
    type Output;
    fn add(self, other: Rhs) -> Self::Output;
}

pub trait RangeMul<Rhs = Self> {
    type Output;
    fn mul(self, other: Rhs) -> Self::Output;
}


// Failed(true) means self is larger than other
impl RangeAdd for std::ops::Range<u32> {
    type Output = Option<Self>;

    fn add(self, other: Self) -> Option<Self> {
        if self.start < other.start {
            if self.end < other.start { return None; }
            else { return Some(self.start..other.end); }
        }
        else {
            if self.start > other.end { return None; }
            else { return Some(other.start..self.end); }
        }
    }
}

impl RangeMul for std::ops::Range<u32> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let max = match (self.end).checked_add(other.end) {
            Some(val) => val - 1,
            None => std::u32::MAX,
        };
        return (self.start + other.start)..max;
    }
}

impl RangeMul<u32> for std::ops::Range<u32> {
    type Output = Vec<std::ops::Range<u32>>;

    fn mul(self, other: u32) -> Vec<std::ops::Range<u32>> {
        let mut ret = vec![];
        for i in self { 
            let min = match i.checked_mul(other) {
                Some(val) => {
                    if val < std::u32::MAX { val }
                    else { continue; }
                },
                None => continue,
            };
            ret.push(min..(min + 1));
        }
        return ret;
    }
}

impl RangeAdd for Vec<std::ops::Range<u32>> {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut ret = self.clone();
        ret.append(&mut other.clone());
        ret.range_compress();
        return ret;
    }
}

impl RangeMul for Vec<std::ops::Range<u32>> {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        let mut ret = vec![];
        for i in self.iter() {
            for j in other.iter() {
                ret.push(i.clone().mul(j.clone()));
            }
        }
        ret.range_compress();
        return ret;
    }
}

impl RangeMul<u32> for Vec<std::ops::Range<u32>> {
    type Output = Self;

    fn mul(self, other: u32) -> Self {
        let mut ret = vec![];
        for i in self.into_iter() {
            ret.append(&mut i.mul(other));
        }
        ret.range_compress();
        return ret;
    }
}

impl RangeMul<std::ops::Range<u32>> for Vec<std::ops::Range<u32>> {
    type Output = Self;

    fn mul(self, other: std::ops::Range<u32>) -> Self {
        let mut ret = vec![];
        for i in other {
            ret.append(&mut self.clone().mul(i));
        }
        ret.range_compress();
        return ret;
    }
}



impl RangeCompress for Vec<std::ops::Range<u32>> {
    fn range_compress(&mut self) {
        let mut ret : Vec<std::ops::Range<u32>> = vec![];
        let mut current = self.pop().unwrap();
        let mut trig = true;
        loop {
            for i in 0..ret.len() {
                match current.clone().add(ret[i].clone()) {
                    Some(r) => {
                        current = r;
                        ret.remove(i);
                        trig = false;
                        break;
                    },
                    None => (),
                };
            }

            if trig {
                ret.push(current);
                match self.pop() {
                    Some(r) => current = r,
                    None => break,
                };
            }
            else {
                trig = true;
            }
        }
        self.append(&mut ret);
    }
}



const SEQ     : u32 = 0;
const OR      : u32 = 1;
const MINMAX  : u32 = 2;
const ESCAPED : u32 = 3;
const RANGE   : u32 = 4;


impl RegexConvertable for str {
    fn to_regex(&self) -> Regex {
        let mut group = None;
        let mut mode = SEQ;
        let mut modes = vec![];

        let mut layers = vec![vec![], vec![]];

        let mut min : i32 = 0;
        let mut max : i32 = -1;
        let mut borrowing_max = false;

        let _nor = false;

        for c in self.chars() {
            match mode {
                SEQ => {
                    assert_ne!(c, ']', "illegal ]");
                    assert_ne!(c, '}', "illegal }}");

                    let seq = layers.last_mut().unwrap();
                    match c {
                        // Push a new stack frame for the OR layer
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
                        // Push a new stack frame for the OR_SEQ layer
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
                        ')' => {
                            let pop_seq = layers.pop().unwrap();
                            let pop_or = layers.pop().unwrap();
                            let mut temp = match group {
                                Some(g) => g,
                                None    => Regex::Epsilon,
                            };

                            for r in pop_seq.into_iter().rev() {
                                let templen = temp.len();
                                let rlen = r.len();
                                temp = Regex::Sequence(Box::new(r), Box::new(temp), rlen.mul(templen));
                            }

                            for r in pop_or.into_iter().rev() {
                                let templen = temp.len();
                                let rlen = r.len();
                                temp = Regex::Or(Box::new(r), Box::new(temp), rlen.add(templen));
                            }

                            group = Some(temp);
                            mode = modes.pop().unwrap();
                        },
                        '|' => {
                            let pop_seq = layers.pop().unwrap();
                            let or = layers.last_mut().unwrap();

                            let mut temp = match group {
                                Some(g) => g,
                                None    => panic!("zero length sub-sequence"),
                            };

                            for r in pop_seq.into_iter().rev() {
                                let templen = temp.len();
                                let rlen = r.len();
                                temp = Regex::Sequence(Box::new(r), Box::new(temp), rlen.mul(templen));
                            }
                            or.push(temp);

                            group = None;
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
                            group = Some(Regex::WildCard);
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
                            group = Some(Regex::Char(x));
                        },
                    };
                },
                OR => {
                    let or = layers.last_mut().unwrap();
                    match c {
                        ']' => {
                            let pop_or = layers.pop().unwrap();
                            let mut temp = match group {
                                Some(g) => g,
                                None    => panic!("zero length or"),
                            };

                            for r in pop_or.into_iter().rev() {
                                let templen = temp.len();
                                let rlen = r.len();
                                temp = Regex::Or(Box::new(r), Box::new(temp), rlen.add(templen));
                            }

                            group = Some(temp);
                            mode = modes.pop().unwrap();
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
                        x  => {
                            // commiting group
                            match group {
                                Some(g) => or.push(g),
                                None    => (),
                            };
                            group = Some(Regex::Char(x));
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
                                max = min;
                            }
                            
                            let opt_max = if max < 0 {
                                None 
                            }
                            else {
                                Some(max as u32)
                            };

                            // commiting group
                            match group {
                                Some(Regex::MinMax(_, _, _)) => panic!("illegal minmax, minmax after minmax"),
                                Some(g) => {
                                    group = Some(Regex::MinMax(Box::new(g), min as u32, opt_max));
                                },
                                None    => panic!("illegal minmax, no modifyee"),
                            };

                            mode = SEQ;
                            max = -1;
                            min = 0;
                            borrowing_max = false;
                        },
                        x   => {
                            assert!(x.is_digit(10), "illegal minmax, non-digit found");
                            if borrowing_max {
                                if max < 0 {
                                    max = 0;
                                }
                                max *= 10;
                                max += x.to_digit(10).unwrap() as i32;
                            }
                            else {
                                min *= 10;
                                min += x.to_digit(10).unwrap() as i32;
                            }
                        },
                    };
                },
                ESCAPED => {
                    match c {
                        '(' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('('));
                        },
                        ')' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char(')'));
                        },
                        '[' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('['));
                        },
                        ']' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char(']'));
                        },
                        '{' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('{'));
                        },
                        '}' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('{'));
                        },
                        '.' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('.'));
                        },
                        '-' => {
                            mode = modes.pop().unwrap();
                            group = Some(Regex::Char('-'));
                        },
                        'd' => {
                            mode = modes.pop().unwrap();
                            group = Some("[0-9]".to_regex());
                        },
                        _ => panic!("illegal escape"),
                    };
                },
                RANGE => {
                    mode = modes.pop().unwrap();
                    assert_eq!(mode, OR, "illegal range");

                    let or = layers.last_mut().unwrap();
                    let last_c = match group {
                        Some(Regex::Char(lc)) => lc,
                        _ => panic!("wrong format"),
                    };

                    assert!(last_c < c, "range tail is smaller than head");
                    for i in (last_c as u8)..(c as u8) {
                        or.push(Regex::Char(i as char));
                    }
                    group = Some(Regex::Char(c));
                },
                _ => panic!("illegal mode"),
            }; // match mode
        } // for c in string

        assert_eq!(modes.pop(), None);
        let pop_seq = layers.pop().unwrap();
        let pop_or = layers.pop().unwrap();
        let mut temp = match group {
            Some(g) => g,
            None    => Regex::Epsilon,
        };

        for r in pop_seq.into_iter().rev() {
            let templen = temp.len();
            let rlen = r.len();
            temp = Regex::Sequence(Box::new(r), Box::new(temp), rlen.mul(templen));
        }

        for r in pop_or.into_iter().rev() {
            let templen = temp.len();
            let rlen = r.len();
            temp = Regex::Or(Box::new(r), Box::new(temp), rlen.add(templen));
        }

        return temp;
    } // fn to_regex
}


type RegexP = Box<Regex>;

#[derive(Debug, Clone)]
pub enum Regex {
    Epsilon,
    WildCard,
    Char(char),
    MinMax(RegexP, u32, Option<u32>),

    Sequence(RegexP, RegexP, Vec<std::ops::Range<u32>>),
    Or(RegexP, RegexP, Vec<std::ops::Range<u32>>),
}

impl Regex {
    pub fn len(&self) -> Vec<std::ops::Range<u32>> {
        match self {
            Regex::Epsilon => return vec![0..1],
            Regex::WildCard => return vec![1..2],
            Regex::Char(_) => return vec![1..2],
            Regex::MinMax(regexp, min, maxopt) => {
                let max = match maxopt {
                    Some(m) => *m,
                    None => std::u32::MAX - 1,
                };
                match **regexp {
                    Regex::Epsilon => return vec![0..1],
                    Regex::WildCard => {
                        return vec![*min..(max + 1)];
                    },
                    Regex::Char(_) => {
                        return vec![*min..(max + 1)];
                    },
                    Regex::Sequence(_, _, ref vec) => {
                        return vec.clone().mul(*min..(max + 1));
                    },
                    Regex::Or(_, _, ref vec) => {
                        return vec.clone().mul(*min..(max + 1));
                    },
                    _ => panic!("no minmax a minmax"),
                };
            },
            Regex::Sequence(_, _, ref vec) => return vec.clone(),
            Regex::Or(_, _, ref vec) => return vec.clone(),
        };
    }
}

impl RegexConvertable for Regex {
    fn to_regex(&self) -> Regex {
        return self.clone();
    }
}


    




