use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, Word};

fn main() {
    let guesses = read_words("guesses.txt");
    let answers = read_words("answers.txt");
    dbg!(minimax(1, &guesses, &answers));
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}
