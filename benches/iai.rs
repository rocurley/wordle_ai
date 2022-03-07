use iai::black_box;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{minimax, AllPools, AnswerIx, GuessIx, ResponseLUT, Word};

fn iai_benchmark() -> Option<usize> {
    let file = File::open("short.txt").unwrap();
    let words: Vec<Word> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    let pools = AllPools::new();
    let lut = ResponseLUT::new(&words, &words);
    let guess_ixs: Vec<GuessIx> = (0..words.len()).map(|i| GuessIx(i)).collect();
    let answer_ixs: Vec<AnswerIx> = (0..words.len()).map(|i| AnswerIx(i)).collect();
    minimax(pools, &lut, 2, &guess_ixs, &answer_ixs, None, false).map(|tr| tr.score)
}

iai::main!(iai_benchmark);
