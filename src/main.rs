use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, AllPools, AnswerIx, GuessIx, ResponseLUT, Word};

fn main() {
    let args: Vec<String> = env::args().collect();
    let guesses = read_words(&args[1]);
    let answers = read_words(&args[2]);
    let pools = AllPools::new();
    let lut = ResponseLUT::new(&guesses, &answers);
    let guess_ixs: Vec<GuessIx> = (0..guesses.len()).map(|i| GuessIx(i)).collect();
    let answer_ixs: Vec<AnswerIx> = (0..answers.len()).map(|i| AnswerIx(i)).collect();
    {
        let out = minimax(pools, &lut, 2, &guess_ixs, &answer_ixs, None, true);
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
