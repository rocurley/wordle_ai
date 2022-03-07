use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::{check_guess, check_guess_simd, SimdWord, Word};

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("check_guess", |b| {
        let file = File::open("short.txt").unwrap();
        let words: Vec<Word> = BufReader::new(file)
            .lines()
            .map(|line| line.unwrap().parse().unwrap())
            .collect();
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &words {
                for &answer in &words {
                    let response = check_guess(guess, answer);
                    prevent_noop += response.as_int().0 as i32;
                }
            }
            prevent_noop
        })
    });
    c.bench_function("check_guess_simd", |b| {
        let file = File::open("short.txt").unwrap();
        let words: Vec<SimdWord> = BufReader::new(file)
            .lines()
            .map(|line| SimdWord::from_word(line.unwrap().parse().unwrap()))
            .collect();
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &words {
                for &answer in &words {
                    let response = check_guess_simd(guess, answer);
                    prevent_noop += response.0 as i32;
                }
            }
            prevent_noop
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
