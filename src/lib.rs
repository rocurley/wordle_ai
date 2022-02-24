use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Formatter};
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
) -> HashMap<Response, Vec<Word>> {
    let mut out = HashMap::new();
    for &answer in possible_answers {
        let response = check_guess(guess, answer);
        out.entry(response).or_insert(Vec::new()).push(answer);
    }
    out
}

// TODO: Move score out of MinimaxFrame and use this.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct MinimaxTrace {
    frames: Vec<MinimaxFrame>,
    score: usize,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct MinimaxFrame {
    guess: Word,
    response: Response,
    remaining_answers: Vec<Word>,
    score: usize,
}

pub fn minimax(
    depth: usize,
    possible_guesses: &[Word],
    possible_answers: &[Word],
) -> Vec<MinimaxFrame> {
    if depth == 0 {
        let frame = heuristic(possible_guesses, possible_answers);
        return vec![frame];
    }
    let len = possible_guesses.len();
    let mut i = 0;
    possible_guesses
        .into_iter()
        .map(|&guess| {
            let possible_responses = bucket_answers_by_response(guess, possible_answers);
            possible_responses
                .into_iter()
                .map(|(response, remaining_answers)| {
                    let mut child_frames = minimax(depth - 1, possible_guesses, &remaining_answers);
                    let new_frame = MinimaxFrame {
                        guess,
                        response,
                        remaining_answers,
                        score: child_frames.last().unwrap().score,
                    };
                    child_frames.push(new_frame);
                    child_frames
                })
                .max_by_key(|child_frames| child_frames.last().unwrap().score)
                .unwrap()
        })
        .inspect(|_| {
            i += 1;
            println!("{}/{}", i, len);
        })
        .min_by_key(|child_frames| child_frames.last().unwrap().score)
        .unwrap()
}

pub fn heuristic(possible_guesses: &[Word], possible_answers: &[Word]) -> MinimaxFrame {
    possible_guesses
        .into_iter()
        .map(|&guess| {
            let possible_responses = bucket_answers_by_response(guess, possible_answers);
            let (response, remaining_answers) = possible_responses
                .into_iter()
                .max_by_key(|(_, answers)| answers.len())
                .unwrap();
            let score = remaining_answers.len();
            MinimaxFrame {
                guess,
                response,
                remaining_answers,
                score,
            }
        })
        .min_by_key(|frame| frame.score)
        .unwrap()
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
