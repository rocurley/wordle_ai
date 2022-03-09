use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{Minimaxer, Word};

fn main() {
    let args: Vec<String> = env::args().collect();
    let guesses = read_words(&args[1]);
    let answers = read_words(&args[2]);
    let minimaxer = Minimaxer::new(answers, guesses);
    let out = minimaxer.run(2, true);
    dbg!(out);
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}
