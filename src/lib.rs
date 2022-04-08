#![feature(type_alias_impl_trait)]
#![feature(portable_simd)]
#![feature(decl_macro)]

use std::cell::{Cell, RefCell};
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Bound;
use std::ops::{Deref, DerefMut, Index};
use std::str::FromStr;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::thread::LocalKey;
use std::time::Instant;
pub mod simd_word;

thread_local! {
    static BAR: RefCell<f32> = RefCell::new(1.0);
    static BUCKETS_POOL_INNER : VecPoolInner<(ResponseInt, PoolVec<AnswerIx>)> = VecPoolInner::new();
    static WORDS_POOL_INNER : VecPoolInner<AnswerIx> = VecPoolInner::new();
    static FRAMES_POOL_INNER : VecPoolInner<MinimaxFrame> = VecPoolInner::new();
}

static BUCKETS_POOL: VecPool<(ResponseInt, PoolVec<AnswerIx>)> = VecPool(&BUCKETS_POOL_INNER);
static WORDS_POOL: VecPool<AnswerIx> = VecPool(&WORDS_POOL_INNER);
static FRAMES_POOL: VecPool<MinimaxFrame> = VecPool(&FRAMES_POOL_INNER);

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
    fn from_emoji(c: char) -> Option<Self> {
        match c {
            '🟩' => Some(ResponseCell::Correct),
            '🟨' => Some(ResponseCell::Moved),
            '⬛' => Some(ResponseCell::Wrong),
            _ => None,
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

impl FromStr for Response {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cells_opt: Option<Vec<ResponseCell>> =
            s.chars().map(ResponseCell::from_emoji).collect();
        let cells = cells_opt.ok_or(())?;
        Ok(Response(cells.try_into().map_err(|_| ())?))
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

#[derive(Clone)]
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
pub struct GuessIx(pub u16);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AnswerIx(pub u16);

impl Index<(GuessIx, AnswerIx)> for ResponseLUT {
    type Output = ResponseInt;
    fn index(&self, (GuessIx(i), AnswerIx(j)): (GuessIx, AnswerIx)) -> &Self::Output {
        &self.table[i as usize * self.n_answers + j as usize]
    }
}

struct VecPoolInner<T> {
    vecs: RefCell<Vec<Vec<T>>>,
    take_count: Cell<usize>,
    new_count: Cell<usize>,
}

impl<T> VecPoolInner<T> {
    pub fn new() -> Self {
        VecPoolInner {
            vecs: RefCell::new(Vec::new()),
            take_count: Cell::new(0),
            new_count: Cell::new(0),
        }
    }
}

pub struct VecPool<T: 'static>(&'static LocalKey<VecPoolInner<T>>);

impl<T> VecPool<T> {
    fn take_vec(&'static self) -> PoolVec<T> {
        self.0.with(|inner| {
            let mut vecs = inner.vecs.borrow_mut();
            if vecs.len() == 0 {
                inner.new_count.set(inner.new_count.get() + 1);
            }
            let vec = vecs.pop().unwrap_or_else(Vec::new);
            inner.take_count.set(inner.take_count.get() + 1);
            PoolVec { pool: self, vec }
        })
    }
    fn debug_info(&'static self) {
        self.0.with(|inner| {
            println!("Pool type: {}", std::any::type_name::<T>());
            println!("Available vectors: {}", inner.vecs.borrow().len());
            println!("Used vectors: {}", inner.take_count.get());
            println!("Allocated vectors: {}", inner.new_count.get());
            let total_size: usize = inner.vecs.borrow().iter().map(|v| v.capacity()).sum();
            println!("Total vector size: {}", total_size);
        });
    }
    fn return_vec(&'static self, vec: Vec<T>) {
        self.0.with(|inner| {
            inner.vecs.borrow_mut().push(vec);
        });
    }
}

pub struct PoolVec<T: 'static> {
    pool: &'static VecPool<T>,
    vec: Vec<T>,
}

impl<T> Drop for PoolVec<T> {
    fn drop(&mut self) {
        let mut vec = std::mem::replace(&mut self.vec, Vec::new());
        vec.clear();
        self.pool.return_vec(vec);
    }
}

impl<T: Clone> Clone for PoolVec<T> {
    fn clone(&self) -> Self {
        let mut cloned = self.pool.take_vec();
        cloned.vec.extend_from_slice(&self.vec);
        cloned
    }
}

impl<T> Deref for PoolVec<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl<T> DerefMut for PoolVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
}

impl<T: Debug> Debug for PoolVec<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl<T> IntoIterator for PoolVec<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;
    fn into_iter(mut self) -> Self::IntoIter {
        std::mem::replace(&mut self.vec, Vec::new()).into_iter()
    }
}

impl<T: Hash> Hash for PoolVec<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vec.hash(state);
    }
}

impl<T: PartialEq> PartialEq for PoolVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.vec == other.vec
    }
}

impl<T: Eq> Eq for PoolVec<T> {}

impl<T> PoolVec<T> {
    pub fn from_vec(pool: &'static VecPool<T>, vec: Vec<T>) -> Self {
        PoolVec { pool, vec }
    }
}

pub fn bucket_answers_by_response(
    guess: GuessIx,
    possible_answers: &[AnswerIx],
    lut: &ResponseLUT,
) -> PoolVec<(ResponseInt, PoolVec<AnswerIx>)> {
    let mut out = BUCKETS_POOL.take_vec();
    for &answer in possible_answers {
        let response = lut[(guess, answer)];
        alist_get_or_else(&mut out, response, || WORDS_POOL.take_vec()).push(answer);
    }
    out
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Score {
    Complete { depth: usize },
    Incomplete { words: usize },
}

#[derive(Debug, Clone)]
pub struct MinimaxTrace {
    pub frames: PoolVec<MinimaxFrame>,
    pub score: Score,
}

impl MinimaxTrace {
    fn push(&mut self, frame: MinimaxFrame) {
        self.frames.push(frame);
    }
}

impl MinimaxTrace {
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
pub struct MinimaxFrame {
    guess: GuessIx,
    response: ResponseInt,
    remaining_answers: PoolVec<AnswerIx>,
}

impl MinimaxFrame {
    fn hydrate(self, guesses: &[Word], answers: &[Word]) -> HydratedMinimaxFrame {
        let MinimaxFrame {
            guess,
            response,
            remaining_answers,
        } = self;
        let guess = guesses[guess.0 as usize];
        let remaining_answers = remaining_answers
            .into_iter()
            .map(|answer| answers[answer.0 as usize])
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
    pub guess: Word,
    pub response: ResponseInt,
    pub remaining_answers: Vec<Word>,
}

fn merge_min(min: &mut Option<MinimaxTrace>, new: MinimaxTrace) {
    let swap = min.as_ref().map_or(true, |min| min.score > new.score);
    if swap {
        min.replace(new);
    }
}

pub struct Minimaxer {
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
    Complete { trace: MinimaxTrace },
    Pruned { score: Score },
}

struct Cache {
    map: HashMap<MinCacheKey, MinimaxResult>,
    hits: [usize; 6],
    bad_hits: [usize; 6],
    misses: [usize; 6],
}

impl Cache {
    fn new() -> Self {
        Cache {
            map: HashMap::new(),
            hits: [0; 6],
            bad_hits: [0; 6],
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

#[derive(Debug, Copy, Clone)]
enum MaximizeAbort {
    UselessGuess(ResponseInt),
    Pruned,
}
impl Minimaxer {
    pub fn new(answers: Vec<Word>, guesses: Vec<Word>) -> Self {
        let lut = ResponseLUT::new(&guesses, &answers);
        let guess_ixs: Vec<GuessIx> = (0..guesses.len() as u16).map(|i| GuessIx(i)).collect();
        let answer_ixs: Vec<AnswerIx> = (0..answers.len() as u16).map(|i| AnswerIx(i)).collect();
        Minimaxer {
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
    pub fn book(
        &self,
        search_depth: usize,
        verbose: bool,
        range: (Bound<usize>, Bound<usize>),
    ) -> Vec<(Word, HydratedMinimaxTrace)> {
        let len = self.guess_ixs.len();
        let mut out = Vec::new();
        let start = match range.0 {
            Bound::Unbounded => 0,
            Bound::Included(x) => x,
            Bound::Excluded(x) => x + 1,
        };
        for (i, &guess) in self.guess_ixs[range].iter().enumerate() {
            let t_start = Instant::now();
            let mut cache = Cache::new();
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
            let guess = self.guesses[guess.0 as usize];
            println!("################");
            println!("{}/{} ({:#?})", i + start + 1, len, t_start.elapsed());
            WORDS_POOL.debug_info();
            print!("{}", max_trace);
            out.push((guess, max_trace));
        }
        /*
        dbg!(cache.len(), cache.hits, cache.misses, cache.bad_hits);
        let cache_total_size: usize = cache.map.keys().map(|k| k.possible_answers.len()).sum();
        dbg!(cache_total_size);
        */
        out
    }
    pub fn book_threads(
        self,
        search_depth: usize,
        verbose: bool,
        guesses_skipped: &[Word],
        n_threads: usize,
    ) -> mpsc::Receiver<(Word, HydratedMinimaxTrace)> {
        let (book_entry_send, book_entry_recv) = mpsc::channel();
        let input: Vec<GuessIx> = self
            .guess_ixs
            .iter()
            .copied()
            .filter(|guess| !guesses_skipped.contains(&self.guesses[guess.0 as usize]))
            .collect();
        let input = Arc::new(Mutex::new(input.into_iter()));
        let minimaxer = Arc::new(self);
        for _ in 0..n_threads {
            let thread_minimaxer = minimaxer.clone();
            let output = book_entry_send.clone();
            let input = input.clone();
            thread::spawn(move || {
                (thread_minimaxer).book_thread_worker(search_depth, verbose, &input, output);
            });
        }
        book_entry_recv
    }
    fn book_thread_worker(
        &self,
        search_depth: usize,
        verbose: bool,
        input: &Mutex<impl Iterator<Item = GuessIx>>,
        output: mpsc::Sender<(Word, HydratedMinimaxTrace)>,
    ) {
        loop {
            let mut guard = input.lock().unwrap();
            let guess = if let Some(guess) = guard.next() {
                guess
            } else {
                break;
            };
            drop(guard);
            let mut cache = Cache::new();
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
            let guess = self.guesses[guess.0 as usize];
            output.send((guess, max_trace)).unwrap();
        }
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
                    frames: FRAMES_POOL.take_vec(),
                    score: Score::Complete { depth },
                },
            };
        }
        if remaining_depth == 0 || possible_answers.len() == 1 {
            return MinimaxResult::Complete {
                trace: MinimaxTrace {
                    frames: FRAMES_POOL.take_vec(),
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
                        } else {
                            minimize_cache.bad_hits[depth] += 1;
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
                    let single_ply = self
                        .maximize(
                            minimize_cache,
                            guess,
                            depth,
                            1,
                            possible_answers,
                            None,
                            false,
                        )
                        .ok()?;
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
            if let Ok(max_trace) = max_trace {
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
    ) -> Result<MinimaxTrace, MaximizeAbort> {
        let mut possible_responses = bucket_answers_by_response(guess, possible_answers, &self.lut);
        if possible_responses.len() == 1 {
            // Useless guess
            return Err(MaximizeAbort::UselessGuess(possible_responses[0].0));
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
                    return Err(MaximizeAbort::Pruned);
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
        Ok(max_trace.unwrap())
    }
}

pub struct Interactive {
    minimaxer: Minimaxer,
    history: Vec<MinimaxFrame>,
    cache: Cache,
    book: Option<HashMap<Word, ResponseInt>>,
}

#[derive(Clone, Copy, Debug)]
pub enum UserError {
    InvalidWord,
    InvalidGuess,
}

pub const SOLVED: ResponseInt = ResponseInt(0);
impl Interactive {
    pub fn new(
        answers: Vec<Word>,
        guesses: Vec<Word>,
        book: Option<HashMap<Word, ResponseInt>>,
    ) -> Self {
        Interactive {
            minimaxer: Minimaxer::new(answers, guesses),
            history: Vec::new(),
            cache: Cache::new(),
            book,
        }
    }
    pub fn interact(&mut self, input: &str) -> Result<ResponseInt, UserError> {
        let guess = Word::from_str(input).map_err(|()| UserError::InvalidWord)?;
        let guess_ix_raw = self
            .minimaxer
            .guesses
            .iter()
            .position(|&g| g == guess)
            .ok_or(UserError::InvalidGuess)?;
        let guess_ix = GuessIx(guess_ix_raw as u16);
        let answers = match self.history.last() {
            None => &self.minimaxer.answer_ixs,
            Some(last_frame) => &last_frame.remaining_answers,
        };
        if let Some(book) = self.book.take() {
            let response = book[&guess];
            let answers_bucketed =
                bucket_answers_by_response(guess_ix, answers, &self.minimaxer.lut);
            let (_, remaining_answers) = answers_bucketed
                .into_iter()
                .find(|(r, _)| *r == response)
                .expect("Invalid response in book");
            let frame = MinimaxFrame {
                remaining_answers,
                guess: guess_ix,
                response,
            };
            self.history.push(frame);
            return Ok(response);
        }
        let trace = self
            .minimaxer
            .maximize(&mut self.cache, guess_ix, 0, 6, answers, None, false);
        match trace {
            Ok(mut trace) => {
                let frame = trace.frames.pop().unwrap();
                assert_eq!(guess_ix, frame.guess);
                let response = frame.response;
                self.history.push(frame);
                assert_ne!(response, SOLVED);
                Ok(response)
            }
            Err(MaximizeAbort::Pruned) => {
                panic!("interact top level maximize call should never be pruned")
            }
            Err(MaximizeAbort::UselessGuess(response)) => {
                let mut remaining_answers = WORDS_POOL.take_vec();
                remaining_answers.extend_from_slice(&answers);
                let frame = MinimaxFrame {
                    remaining_answers,
                    guess: guess_ix,
                    response,
                };
                self.history.push(frame);
                Ok(response)
            }
        }
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
