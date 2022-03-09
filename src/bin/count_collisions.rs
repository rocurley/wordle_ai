use std::collections::HashSet;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{
    bucket_answers_by_response, minimax, AllPools, AnswerIx, GuessIx, PoolVec, ResponseLUT, Word,
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let guesses = read_words(&args[1]);
    let answers = read_words(&args[2]);
    let pools = AllPools::new();
    let lut = ResponseLUT::new(&guesses, &answers);
    let guess_ixs: Vec<GuessIx> = (0..guesses.len()).map(|i| GuessIx(i)).collect();
    let answer_ixs = PoolVec::from_vec(
        &pools.words_pool,
        (0..answers.len()).map(|i| AnswerIx(i)).collect(),
    );
    let mut prior_answers: HashSet<PoolVec<AnswerIx>> = HashSet::new();
    prior_answers.insert(answer_ixs);
    for depth in 1..6 {
        let new_answers: Vec<_> = guess_ixs
            .iter()
            .flat_map(|guess| {
                prior_answers.iter().flat_map(|possible_answers| {
                    bucket_answers_by_response(&pools, *guess, possible_answers, &lut)
                })
            })
            .map(|(response, answers)| answers)
            .collect();
        let undeduped = new_answers.len();
        prior_answers = new_answers.into_iter().collect();
        println!(
            "Depth {}: {}/{} = {}",
            depth,
            prior_answers.len(),
            undeduped,
            prior_answers.len() as f64 / undeduped as f64
        );
    }
}

fn read_words(path: &str) -> Vec<Word> {
    let file = File::open(path).unwrap();
    BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect()
}
