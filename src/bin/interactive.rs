use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{Interactive, Response, ResponseInt, UserError, Word, SOLVED};

fn main() {
    let args: Vec<String> = env::args().collect();
    let guesses = read_words(&args[1]);
    let answers = read_words(&args[2]);
    let book = read_book(&args[3]);
    let mut interactive = Interactive::new(answers, guesses, Some(book));
    let mut buffer = String::new();
    let stdin = io::stdin();
    loop {
        stdin.read_line(&mut buffer).expect("error reading input");
        match interactive.interact(buffer.trim()) {
            Err(UserError::InvalidWord) => {
                println!("Error: guesses must be 5 english letters");
            }
            Err(UserError::InvalidGuess) => {
                println!("Error: guess not found in dictionary");
            }
            Ok(result) => {
                println!("{:?}", result);
                if result == SOLVED {
                    return;
                }
            }
        }
        buffer.clear();
    }
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}

fn read_book(path: &str) -> HashMap<Word, ResponseInt> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let mut split = line.split(":");
            let word = split.next().unwrap().parse().unwrap();
            let response: Response = split.next().unwrap().parse().unwrap();
            (word, response.as_int())
        })
        .collect()
}
