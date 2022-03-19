#![feature(type_alias_impl_trait)]
#![feature(portable_simd)]
#![feature(decl_macro)]

use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut, Index};
use std::str::FromStr;
pub mod simd_word;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Letter(u8);

impl Letter {
    fn to_char(self) -> char {
        (b'a' + self.0) as char
    }
    fn to_wide_char(self) -> char {
        char::from_u32(self.0 as u32 + 0xff21).unwrap()
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
impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: String = self.0.into_iter().map(Letter::to_wide_char).collect();
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
    fn from_int(x: u8) -> Option<Self> {
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

impl Debug for ResponseInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(&Response::from_int(*self), f)
    }
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResponseInt(pub u8);

impl Debug for Response {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s: String = self.0.into_iter().map(ResponseCell::emoji).collect();
        f.write_str(&s)
    }
}

impl Response {
    pub fn as_int(&self) -> ResponseInt {
        ResponseInt(self.0.iter().fold(0, |acc, &cell| acc * 3 + cell as u8))
    }
    fn from_int(ResponseInt(mut x): ResponseInt) -> Self {
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

pub fn alist_get_or_else<K: Eq, V, F: FnOnce() -> V>(
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

pub fn check_guess(guess: Word, answer: Word) -> Response {
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

pub struct ResponseLUT {
    table: Vec<ResponseInt>,
    n_answers: usize,
}

impl ResponseLUT {
    pub fn new(guesses: &[Word], answers: &[Word]) -> Self {
        let table = guesses
            .iter()
            .flat_map(|guess| {
                answers
                    .iter()
                    .map(|answer| check_guess(*guess, *answer).as_int())
            })
            .collect();
        let n_answers = answers.len();
        ResponseLUT { table, n_answers }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GuessIx(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AnswerIx(pub usize);

impl Index<(GuessIx, AnswerIx)> for ResponseLUT {
    type Output = ResponseInt;
    fn index(&self, (GuessIx(i), AnswerIx(j)): (GuessIx, AnswerIx)) -> &Self::Output {
        &self.table[i * self.n_answers + j]
    }
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

#[derive(Clone)]
pub struct PoolVec<'a, T> {
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

impl<'a, T> IntoIterator for PoolVec<'a, T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;
    fn into_iter(mut self) -> Self::IntoIter {
        std::mem::replace(&mut self.vec, Vec::new()).into_iter()
    }
}

impl<'a, T: Hash> Hash for PoolVec<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vec.hash(state);
    }
}

impl<'a, T: PartialEq> PartialEq for PoolVec<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.vec == other.vec
    }
}

impl<'a, T: Eq> Eq for PoolVec<'a, T> {}

impl<'a, T> PoolVec<'a, T> {
    pub fn from_vec(pool: &'a VecPool<T>, vec: Vec<T>) -> Self {
        PoolVec { pool, vec }
    }
}

pub fn bucket_answers_by_response<'pool>(
    pools: &'pool AllPools<'pool>,
    guess: GuessIx,
    possible_answers: &[AnswerIx],
    lut: &ResponseLUT,
) -> PoolVec<'pool, (ResponseInt, PoolVec<'pool, AnswerIx>)> {
    let mut out = pools.buckets_pool.take_vec();
    for &answer in possible_answers {
        let response = lut[(guess, answer)];
        alist_get_or_else(&mut out, response, || pools.words_pool.take_vec()).push(answer);
    }
    out
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Score {
    Complete { depth: usize },
    Incomplete { words: usize },
}

#[derive(Debug, Clone)]
pub struct MinimaxTrace<'pool> {
    pub frames: PoolVec<'pool, MinimaxFrame<'pool>>,
    pub score: Score,
}

impl<'pool> MinimaxTrace<'pool> {
    fn push(&mut self, frame: MinimaxFrame<'pool>) {
        self.frames.push(frame);
    }
}

impl<'pool> MinimaxTrace<'pool> {
    fn hydrate(self, guesses: &[Word], answers: &[Word]) -> HydratedMinimaxTrace {
        let MinimaxTrace { frames, score } = self;
        let frames = frames
            .into_iter()
            .rev()
            .map(|frame| frame.hydrate(guesses, answers))
            .collect();
        HydratedMinimaxTrace { score, frames }
    }
}

#[derive(Debug, Clone)]
pub struct MinimaxFrame<'pool> {
    guess: GuessIx,
    response: ResponseInt,
    remaining_answers: PoolVec<'pool, AnswerIx>,
}

impl<'pool> MinimaxFrame<'pool> {
    fn hydrate(self, guesses: &[Word], answers: &[Word]) -> HydratedMinimaxFrame {
        let MinimaxFrame {
            guess,
            response,
            remaining_answers,
        } = self;
        let guess = guesses[guess.0];
        let remaining_answers = remaining_answers
            .into_iter()
            .map(|answer| answers[answer.0])
            .collect();
        HydratedMinimaxFrame {
            guess,
            response,
            remaining_answers,
        }
    }
}

#[derive(Debug, Clone)]
pub struct HydratedMinimaxTrace {
    pub frames: Vec<HydratedMinimaxFrame>,
    pub score: Score,
}

impl Display for HydratedMinimaxTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for frame in &self.frames {
            write!(
                f,
                "Guess:     {}\nResponse:  {:?}\nRemaining: {}\n",
                frame.guess,
                frame.response,
                frame.remaining_answers.len()
            )?
        }
        if matches!(self.score, Score::Complete { .. }) {
            write!(
                f,
                "Answer:    {}\n",
                self.frames.last().unwrap().remaining_answers[0]
            )?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct HydratedMinimaxFrame {
    guess: Word,
    response: ResponseInt,
    remaining_answers: Vec<Word>,
}

fn merge_min<'pool>(min: &mut Option<MinimaxTrace<'pool>>, new: MinimaxTrace<'pool>) {
    let swap = min.as_ref().map_or(true, |min| min.score > new.score);
    if swap {
        min.replace(new);
    }
}

pub struct AllPools<'pool> {
    pub buckets_pool: VecPool<(ResponseInt, PoolVec<'pool, AnswerIx>)>,
    pub words_pool: VecPool<AnswerIx>,
    pub frames_pool: VecPool<MinimaxFrame<'pool>>,
}

impl AllPools<'static> {
    pub fn new() -> &'static Self {
        Box::leak(Box::new(AllPools {
            words_pool: VecPool::new(),
            buckets_pool: VecPool::new(),
            frames_pool: VecPool::new(),
        }))
    }
}

pub struct Minimaxer {
    pools: &'static AllPools<'static>,
    lut: ResponseLUT,
    guesses: Vec<Word>,
    answers: Vec<Word>,
    guess_ixs: Vec<GuessIx>,
    answer_ixs: Vec<AnswerIx>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct MinCacheKey {
    depth: usize,
    possible_answers: Vec<AnswerIx>,
}

#[derive(Clone, Debug)]
pub enum MinimaxResult {
    Complete { trace: MinimaxTrace<'static> },
    Pruned { score: Score },
}

struct Cache {
    map: HashMap<MinCacheKey, MinimaxResult>,
    hits: [usize; 6],
    misses: [usize; 6],
}

impl Cache {
    fn new() -> Self {
        Cache {
            map: HashMap::new(),
            hits: [0; 6],
            misses: [0; 6],
        }
    }
    fn len(&self) -> usize {
        self.map.len()
    }
    fn get(&mut self, k: &MinCacheKey) -> Option<&MinimaxResult> {
        let res = self.map.get(k);
        if res.is_some() {
            self.hits[k.depth] += 1;
        } else {
            self.misses[k.depth] += 1;
        }
        res
    }
    fn insert(&mut self, k: MinCacheKey, v: MinimaxResult) {
        self.map.insert(k, v);
    }
}

fn pop_line() {
    print!("\x1b[2K");
    print!("\x1b[1F");
}

impl Minimaxer {
    pub fn new(answers: Vec<Word>, guesses: Vec<Word>) -> Self {
        let pools = AllPools::new();
        let lut = ResponseLUT::new(&guesses, &answers);
        let guess_ixs: Vec<GuessIx> = (0..guesses.len()).map(|i| GuessIx(i)).collect();
        let answer_ixs: Vec<AnswerIx> = (0..answers.len()).map(|i| AnswerIx(i)).collect();
        Minimaxer {
            pools,
            lut,
            guesses,
            answers,
            guess_ixs,
            answer_ixs,
        }
    }
    pub fn run(&self, search_depth: usize, verbose: bool) -> HydratedMinimaxTrace {
        let mut cache = Cache::new();
        let raw = self.minimize(&mut cache, 0, search_depth, &self.answer_ixs, None, verbose);
        let trace = if let MinimaxResult::Complete { trace } = raw {
            trace
        } else {
            unreachable!();
        };
        dbg!(cache.len(), cache.hits, cache.misses);
        //dbg!(cache.map);
        trace.hydrate(&self.guesses, &self.answers)
    }
    pub fn book(&self, search_depth: usize, verbose: bool) -> Vec<(Word, HydratedMinimaxTrace)> {
        let len = self.guess_ixs.len();
        let mut cache = Cache::new();
        let mut out = Vec::new();
        for (i, &guess) in self.guess_ixs[..1].iter().enumerate() {
            let max_trace = self
                .maximize(
                    &mut cache,
                    guess,
                    0,
                    search_depth,
                    &self.answer_ixs,
                    None,
                    verbose,
                )
                .unwrap()
                .hydrate(&self.guesses, &self.answers);
            let guess = self.guesses[guess.0];
            println!("################");
            println!("{}/{}", i + 1, len);
            print!("{}", max_trace);
            out.push((guess, max_trace));
        }
        out
    }
    fn minimize(
        &self,
        minimize_cache: &mut Cache,
        depth: usize,
        mut remaining_depth: usize,
        possible_answers: &[AnswerIx],
        min_min: Option<Score>,
        verbose: bool,
    ) -> MinimaxResult {
        if possible_answers.len() == 1 {
            return MinimaxResult::Complete {
                trace: MinimaxTrace {
                    frames: self.pools.frames_pool.take_vec(),
                    score: Score::Complete { depth },
                },
            };
        }
        if remaining_depth == 0 || possible_answers.len() == 1 {
            return MinimaxResult::Complete {
                trace: MinimaxTrace {
                    frames: self.pools.frames_pool.take_vec(),
                    score: Score::Incomplete {
                        words: possible_answers.len(),
                    },
                },
            };
        }
        let cache_key = MinCacheKey {
            depth,
            // TODO: try to avoid this copy
            possible_answers: possible_answers.to_vec(),
        };
        if let Some(cached) = minimize_cache.get(&cache_key) {
            match cached {
                MinimaxResult::Complete { .. } => return cached.clone(),
                MinimaxResult::Pruned { score } => {
                    if let Some(min_min) = min_min {
                        if *score < min_min {
                            return cached.clone();
                        }
                    }
                }
            }
        }
        let len = self.guess_ixs.len();
        let mut min_trace: Option<MinimaxTrace> = None;
        // Find min score over all guesses
        if verbose {
            println!("");
        }
        let guesses_sorted: Vec<GuessIx>;
        let guesses = if remaining_depth > 1 {
            let mut scored_guesses: Vec<(Score, GuessIx)> = self
                .guess_ixs
                .iter()
                .filter_map(|&guess| {
                    let single_ply = self.maximize(
                        minimize_cache,
                        guess,
                        depth,
                        1,
                        possible_answers,
                        None,
                        false,
                    )?;
                    Some((single_ply.score, guess))
                })
                .collect();
            scored_guesses.sort_by_key(|&(score, _)| score);
            guesses_sorted = scored_guesses.into_iter().map(|(_, guess)| guess).collect();
            &guesses_sorted
        } else {
            &self.guess_ixs
        };
        for (i, &guess) in guesses.into_iter().enumerate() {
            if verbose {
                print!("\x1b[2k\r Depth {}: {:0>5}/{:0>5}", depth, i, len);
            }
            let max_max = min_trace.as_ref().map(|tr| tr.score);
            let max_trace = self.maximize(
                minimize_cache,
                guess,
                depth,
                remaining_depth,
                possible_answers,
                max_max,
                verbose,
            );
            if let Some(max_trace) = max_trace {
                if let Some(min_min) = min_min {
                    if max_trace.score <= min_min {
                        let result = MinimaxResult::Pruned {
                            score: max_trace.score,
                        };
                        minimize_cache.insert(cache_key, result.clone());
                        if verbose {
                            pop_line();
                        }
                        return result;
                    }
                }
                if let Score::Complete {
                    depth: completion_depth,
                } = max_trace.score
                {
                    // No point in going as deep as the already-found victory
                    remaining_depth = completion_depth - depth - 1;
                }
                merge_min(&mut min_trace, max_trace);
                if remaining_depth == 0 {
                    break;
                }
            }
        }
        let trace = min_trace.expect("min_trace must get set at least once");
        let result = MinimaxResult::Complete { trace };
        minimize_cache.insert(cache_key, result.clone());
        if verbose {
            pop_line();
        }
        result
    }

    fn maximize(
        &self,
        minimize_cache: &mut Cache,
        guess: GuessIx,
        depth: usize,
        remaining_depth: usize,
        possible_answers: &[AnswerIx],
        max_max: Option<Score>,
        verbose: bool,
    ) -> Option<MinimaxTrace<'static>> {
        let mut possible_responses =
            bucket_answers_by_response(self.pools, guess, possible_answers, &self.lut);
        if possible_responses.len() == 1 {
            // Useless guess
            return None;
        }
        let mut max_trace: Option<MinimaxTrace> = None;
        let len = possible_responses.len();
        if verbose {
            println!("");
        }
        possible_responses.sort_by_key(|(_, remaining_answers)| Reverse(remaining_answers.len()));
        // Find max score over all responses
        for (i, (response, remaining_answers)) in possible_responses.into_iter().enumerate() {
            if verbose {
                print!("\x1b[2k\r Depth {}: {:0>5}/{:0>5}", depth, i, len);
            }
            let min_min = max_trace.as_ref().map(|tr| tr.score);
            let child_result = self.minimize(
                minimize_cache,
                depth + 1,
                remaining_depth - 1,
                &remaining_answers,
                min_min,
                verbose,
            );
            let mut child_trace = if let MinimaxResult::Complete { trace } = child_result {
                trace
            } else {
                continue;
            };
            if let Some(max_max) = max_max {
                if child_trace.score > max_max {
                    // Note that this can only trigger if min_trace is populated, so min_trace
                    // must be set at least once.
                    if verbose {
                        pop_line();
                    }
                    return None;
                }
            }
            let swap = max_trace
                .as_ref()
                .map_or(true, |max| max.score < child_trace.score);
            if swap {
                let new_frame = MinimaxFrame {
                    guess,
                    response,
                    remaining_answers,
                };
                child_trace.push(new_frame);
                max_trace.replace(child_trace);
            }
        }
        if verbose {
            pop_line();
        }
        max_trace
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ResponseCell::*;
    const TEST_CHECK_GUESS_CASES: [(&str, &str, Response); 8] = [
        (
            "opens",
            "abbey",
            Response([Wrong, Wrong, Moved, Wrong, Wrong]),
        ),
        (
            "babes",
            "abbey",
            Response([Moved, Moved, Correct, Correct, Wrong]),
        ),
        (
            "kebab",
            "abbey",
            Response([Wrong, Moved, Correct, Moved, Moved]),
        ),
        (
            "algae",
            "abbey",
            Response([Correct, Wrong, Wrong, Wrong, Moved]),
        ),
        (
            "keeps",
            "abbey",
            Response([Wrong, Moved, Wrong, Wrong, Wrong]),
        ),
        (
            "orbit",
            "abbey",
            Response([Wrong, Wrong, Correct, Wrong, Wrong]),
        ),
        (
            "abate",
            "abbey",
            Response([Correct, Correct, Wrong, Wrong, Moved]),
        ),
        (
            "abbey",
            "abbey",
            Response([Correct, Correct, Correct, Correct, Correct]),
        ),
    ];
    #[test]
    fn test_check_guess() {
        for &(guess, answer, expected) in &TEST_CHECK_GUESS_CASES {
            assert_eq!(
                check_guess(guess.parse().unwrap(), answer.parse().unwrap()),
                expected
            );
        }
    }

    #[test]
    fn test_response_int_roundtrip() {
        for i in 0..3u8.pow(5) {
            let i = ResponseInt(i);
            let response = Response::from_int(i);
            let i2 = response.as_int();
            assert_eq!(i, i2);
        }
    }
}
