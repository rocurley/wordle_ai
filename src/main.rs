use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, AllPools, AnswerIx, GuessIx, MinimaxSharedState, ResponseLUT, Word};

fn main() {
    let args: Vec<String> = env::args().collect();
    let guesses = read_words(&args[1]);
    let answers = read_words(&args[2]);
    let answer_ixs: Vec<AnswerIx> = (0..answers.len()).map(|i| AnswerIx(i)).collect();
    let minimaxer = MinimaxSharedState::new(&answers, &guesses);
    {
        let out = minimax(&minimaxer, 2, &answer_ixs, None, true);
        dbg!(out);
    }
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}
