use iai::black_box;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, AllPools, Word};

fn iai_benchmark() -> Option<usize> {
    let file = File::open("short.txt").unwrap();
    let words: Vec<Word> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    let pools = AllPools::new();
    minimax(pools, 2, &words, &words, None, false).map(|tr| tr.score)
}

iai::main!(iai_benchmark);
