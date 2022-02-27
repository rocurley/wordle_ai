#![feature(type_alias_impl_trait)]

use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};
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
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Response([ResponseCell; 5]);

impl Debug for Response {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: String = self.0.into_iter().map(ResponseCell::emoji).collect();
        f.write_str(&s)
    }
}

fn alist_get_or_else<K: Eq, V, F: FnOnce() -> V>(
    alist: &mut Vec<(K, V)>,
    target: K,
    default: F,
) -> &mut V {
    match alist.iter().position(|(k, _)| *k == target) {
        Some(i) => &mut alist[i].1,
        None => {
            alist.push((target, default()));
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

pub struct VecPool<T> {
    vecs: RefCell<Vec<Vec<T>>>,
}

impl<'a, T> VecPool<T> {
    fn take_vec(&'a self) -> PoolVec<T> {
        let vec = self.vecs.borrow_mut().pop().unwrap_or_else(Vec::new);
        PoolVec { pool: self, vec }
    }
    pub fn new() -> Self {
        VecPool {
            vecs: RefCell::new(Vec::new()),
        }
    }
}

struct PoolVec<'a, T> {
    pool: &'a VecPool<T>,
    vec: Vec<T>,
}

impl<'a, T> Drop for PoolVec<'a, T> {
    fn drop(&mut self) {
        let mut vec = std::mem::replace(&mut self.vec, Vec::new());
        vec.clear();
        self.pool.vecs.borrow_mut().push(vec);
    }
}

impl<'a, T> Deref for PoolVec<'a, T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<'a, T> DerefMut for PoolVec<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
}

impl<'a, T: Debug> Debug for PoolVec<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

fn bucket_answers_by_response<'pool>(
    pool: &'pool VecPool<Word>,
    guess: Word,
    possible_answers: &[Word],
) -> Vec<(Response, PoolVec<'pool, Word>)> {
    let mut out = Vec::new();
    for &answer in possible_answers {
        let response = check_guess(guess, answer);
        alist_get_or_else(&mut out, response, || pool.take_vec()).push(answer);
    }
    out
}

#[derive(Debug)]
pub struct MinimaxTrace<'pool> {
    pub frames: Vec<MinimaxFrame<'pool>>,
    pub score: usize,
}

impl<'pool> MinimaxTrace<'pool> {
    fn push(&mut self, frame: MinimaxFrame<'pool>) {
        self.frames.push(frame);
    }
}

#[derive(Debug)]
pub struct MinimaxFrame<'pool> {
    guess: Word,
    response: Response,
    remaining_answers: PoolVec<'pool, Word>,
}

fn merge_min<'pool>(min: &mut Option<MinimaxTrace<'pool>>, new: MinimaxTrace<'pool>) {
    let swap = min.as_ref().map_or(true, |min| min.score > new.score);
    if swap {
        min.replace(new);
    }
}

fn merge_max<'pool>(max: &mut Option<MinimaxTrace<'pool>>, new: MinimaxTrace<'pool>) {
    let swap = max.as_ref().map_or(true, |max| max.score < new.score);
    if swap {
        max.replace(new);
    }
}

pub fn minimax<'pool>(
    words_pool: &'pool VecPool<Word>,
    depth: usize,
    possible_guesses: &[Word],
    possible_answers: &[Word],
    min_min: Option<usize>,
    verbose: bool,
) -> Option<MinimaxTrace<'pool>> {
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
        let possible_responses = bucket_answers_by_response(words_pool, guess, possible_answers);
        let mut max_trace: Option<MinimaxTrace> = None;
        for (response, remaining_answers) in possible_responses {
            let child_trace_option = minimax(
                words_pool,
                depth - 1,
                possible_guesses,
                &remaining_answers,
                None,
                false,
            );
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
}
