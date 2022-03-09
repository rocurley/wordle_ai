use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{Minimaxer, Word};

fn iai_benchmark() -> Option<usize> {
    let file = File::open("short.txt").unwrap();
    let words: Vec<Word> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    let minimaxer = Minimaxer::new(words.clone(), words.clone());
    minimaxer.run(2, false).map(|tr| tr.score)
}

iai::main!(iai_benchmark);
