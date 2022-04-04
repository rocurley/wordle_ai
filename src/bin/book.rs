use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{Minimaxer, Word};

fn main() {
    let args: Vec<String> = env::args().collect();
    let depth = args[1].parse().unwrap();
    let guesses = read_words(&args[2]);
    let answers = read_words(&args[3]);
    let minimaxer = Minimaxer::new(answers, guesses);
    let resp = minimaxer.book_threads(depth, false, &[], 4);
    for (_guess, trace) in resp {
        println!("################");
        print!("{}", trace);
    }
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}
