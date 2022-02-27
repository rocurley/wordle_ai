#![feature(type_alias_impl_trait)]

use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::{Index, IndexMut};
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Letter(u8);

impl Letter {
    fn to_char(self) -> char {
        (b'a' + self.0) as char
    }
    fn from_char(c: char) -> Option<Self> {
        let ascii: u8 = (c as u32).try_into().ok()?;
        if ascii >= b'a' && ascii <= b'z' {
            Some(Letter(ascii - b'a'))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word([Letter; 5]);
impl Debug for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: String = self.0.into_iter().map(Letter::to_char).collect();
        f.write_str(&s)
    }
}

impl FromStr for Word {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let letters_opt: Option<Vec<Letter>> = s.chars().map(Letter::from_char).collect();
        let letters = letters_opt.ok_or(())?;
        Ok(Word(letters.try_into().map_err(|_| ())?))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ResponseCell {
    Correct,
    Moved,
    Wrong,
}

impl ResponseCell {
    fn emoji(self) -> char {
        match self {
            ResponseCell::Correct => '🟩',
            ResponseCell::Moved => '🟨',
            ResponseCell::Wrong => '⬛',
        }
    }
    fn from_int(x: usize) -> Option<Self> {
        match x {
            0 => Some(ResponseCell::Correct),
            1 => Some(ResponseCell::Moved),
            2 => Some(ResponseCell::Wrong),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Response([ResponseCell; 5]);

impl Response {
    fn as_int(&self) -> usize {
        self.0.iter().fold(0, |i, &cell| i * 3 + cell as usize)
    }
    fn from_int(mut x: usize) -> Self {
        let mut out = [
            ResponseCell::Correct,
            ResponseCell::Correct,
            ResponseCell::Correct,
            ResponseCell::Correct,
            ResponseCell::Correct,
        ];
        for i in 0..5 {
            out[4 - i] = ResponseCell::from_int(x % 3).unwrap();
            x /= 3;
        }
        Response(out)
    }
}

impl Debug for Response {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: String = self.0.into_iter().map(ResponseCell::emoji).collect();
        f.write_str(&s)
    }
}

struct ResponseMap<T>(Vec<T>);

impl<T> Index<Response> for ResponseMap<T> {
    type Output = T;
    fn index(&self, index: Response) -> &Self::Output {
        &self.0[index.as_int()]
    }
}

impl<T> IndexMut<Response> for ResponseMap<T> {
    fn index_mut(&mut self, index: Response) -> &mut Self::Output {
        &mut self.0[index.as_int()]
    }
}

impl<T: Clone> ResponseMap<T> {
    fn new_cloned(init: T) -> ResponseMap<T> {
        ResponseMap(vec![init; 3usize.pow(5)])
    }
}

impl<T> IntoIterator for ResponseMap<T> {
    type Item = (Response, T);
    type IntoIter = impl Iterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0
            .into_iter()
            .enumerate()
            .map(|(i, x)| (Response::from_int(i), x))
    }
}

fn alist_get_or_insert<K: Eq, V>(alist: &mut Vec<(K, V)>, target: K, default: V) -> &mut V {
    match alist.iter().position(|(k, _)| *k == target) {
        Some(i) => &mut alist[i].1,
        None => {
            alist.push((target, default));
            &mut alist.last_mut().unwrap().1
        }
    }
}

fn check_guess(guess: Word, answer: Word) -> Response {
    let mut used = [false, false, false, false, false];
    let mut out = [
        ResponseCell::Wrong,
        ResponseCell::Wrong,
        ResponseCell::Wrong,
        ResponseCell::Wrong,
        ResponseCell::Wrong,
    ];
    for (((x, y), letter_used), out_cell) in guess
        .0
        .iter()
        .zip(answer.0.iter())
        .zip(used.iter_mut())
        .zip(out.iter_mut())
    {
        if x == y {
            *letter_used = true;
            *out_cell = ResponseCell::Correct;
        }
    }
    for i in 0..5 {
        if out[i] == ResponseCell::Correct {
            continue;
        }
        for j in 0..5 {
            if guess.0[i] == answer.0[j] && !used[j] {
                out[i] = ResponseCell::Moved;
                used[j] = true;
                break;
            }
        }
    }
    Response(out)
}

fn bucket_answers_by_response(
    guess: Word,
    possible_answers: &[Word],
) -> Vec<(Response, Vec<Word>)> {
    let mut out = Vec::new();
    for &answer in possible_answers {
        let response = check_guess(guess, answer);
        alist_get_or_insert(&mut out, response, Vec::new()).push(answer);
    }
    out
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct MinimaxTrace {
    frames: Vec<MinimaxFrame>,
    score: usize,
}

impl MinimaxTrace {
    fn push(&mut self, frame: MinimaxFrame) {
        self.frames.push(frame);
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct MinimaxFrame {
    guess: Word,
    response: Response,
    remaining_answers: Vec<Word>,
}

fn merge_min(min: &mut Option<MinimaxTrace>, new: MinimaxTrace) {
    let swap = min.as_ref().map_or(true, |min| min.score > new.score);
    if swap {
        *min = Some(new);
    }
}

fn merge_max(max: &mut Option<MinimaxTrace>, new: MinimaxTrace) {
    let swap = max.as_ref().map_or(true, |max| max.score < new.score);
    if swap {
        *max = Some(new);
    }
}

pub fn minimax(
    depth: usize,
    possible_guesses: &[Word],
    possible_answers: &[Word],
    min_min: Option<usize>,
    verbose: bool,
) -> Option<MinimaxTrace> {
    if depth == 0 {
        return Some(MinimaxTrace {
            frames: Vec::new(),
            score: possible_answers.len(),
        });
    }
    let len = possible_guesses.len();
    let mut min_trace: Option<MinimaxTrace> = None;
    'find_guess: for (i, &guess) in possible_guesses.into_iter().enumerate() {
        if verbose {
            println!("{}/{}", i, len);
        }
        let possible_responses = bucket_answers_by_response(guess, possible_answers);
        let mut max_trace = None;
        for (response, remaining_answers) in possible_responses {
            let child_trace_option =
                minimax(depth - 1, possible_guesses, &remaining_answers, None, false);
            let mut child_trace = if let Some(tr) = child_trace_option {
                tr
            } else {
                continue;
            };
            let new_frame = MinimaxFrame {
                guess,
                response,
                remaining_answers,
            };
            child_trace.push(new_frame);
            if let Some(min_trace) = min_trace.as_ref() {
                if child_trace.score > min_trace.score {
                    if verbose {
                        println!("pruned!");
                    }
                    continue 'find_guess;
                }
            }
            merge_max(&mut max_trace, child_trace);
        }
        if let Some(max_trace) = max_trace {
            if let Some(min_min) = min_min {
                if max_trace.score <= min_min {
                    return None;
                }
            }
            merge_min(&mut min_trace, max_trace);
        }
    }
    min_trace
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_check_guess() {
        use ResponseCell::*;
        assert_eq!(
            check_guess("opens".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Wrong, Wrong, Moved, Wrong, Wrong])
        );
        assert_eq!(
            check_guess("babes".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Moved, Moved, Correct, Correct, Wrong])
        );
        assert_eq!(
            check_guess("kebab".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Wrong, Moved, Correct, Moved, Moved])
        );

        assert_eq!(
            check_guess("algae".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Correct, Wrong, Wrong, Wrong, Moved])
        );
        assert_eq!(
            check_guess("keeps".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Wrong, Moved, Wrong, Wrong, Wrong])
        );
        assert_eq!(
            check_guess("orbit".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Wrong, Wrong, Correct, Wrong, Wrong])
        );
        assert_eq!(
            check_guess("abate".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Correct, Correct, Wrong, Wrong, Moved])
        );
        assert_eq!(
            check_guess("abbey".parse().unwrap(), "abbey".parse().unwrap()),
            Response([Correct, Correct, Correct, Correct, Correct])
        );
    }

    #[test]
    fn test_response_int_roundtrip() {
        for i in 0..3usize.pow(5) {
            let response = Response::from_int(i);
            let i2 = response.as_int();
            assert_eq!(i, i2);
        }
    }
}
