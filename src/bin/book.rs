use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{Minimaxer, Word};

fn main() {
    let args: Vec<String> = env::args().collect();
    let depth = args[1].parse().unwrap();
    let guesses = read_words(&args[2]);
    let answers = read_words(&args[3]);
    let to_skip = if args.len() >= 3 {
        read_existing_guesses(&args[4])
    } else {
        Vec::new()
    };
    let minimaxer = Minimaxer::new(answers, guesses);
    let resp = minimaxer.book_threads(depth, false, &to_skip, 4);
    for (_guess, trace) in resp {
        let frame = &trace.frames[0];
        println!("{:?}:{:?}", frame.guess, frame.response);
    }
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}

fn read_existing_guesses(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().split(":").next().unwrap().parse().unwrap())
        .collect()
}
