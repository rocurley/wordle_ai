use iai::black_box;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, MinimaxTrace, Word};

fn iai_benchmark() -> Option<MinimaxTrace> {
    let file = File::open("short.txt").unwrap();
    let words: Vec<Word> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    minimax(2, &words, &words, None, false)
}

iai::main!(iai_benchmark);
