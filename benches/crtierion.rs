#![feature(slice_group_by)]
use bit_set::BitSet;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::io::{BufRead, BufReader};
use wordle_ai_lib::simd_word::{
    check_guess_simd, check_guess_simd_simple_answer, check_guess_simd_simple_guess,
    check_guess_simd_slow, SimdWord,
};
use wordle_ai_lib::{alist_get_or_else, check_guess, Word};

fn consume_buckets<'a, I: Iterator<Item = (usize, &'a [usize])>>(iter: I) -> usize {
    iter.into_iter()
        .map(|(k, vs)| k * vs.into_iter().sum::<usize>())
        .sum()
}

fn consume_pair_buckets<'a, I: Iterator<Item = &'a [(usize, usize)]>>(iter: I) -> usize {
    iter.into_iter()
        .map(|kvs| {
            let k = kvs[0].0;
            k * kvs.into_iter().map(|(_, v)| v).sum::<usize>()
        })
        .sum()
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let file = File::open("short.txt").unwrap();
    let words: Vec<Word> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    let simd_words: Vec<SimdWord> = words.iter().copied().map(SimdWord::from_word).collect();
    let mut check_guess_group = c.benchmark_group("check_guess");
    check_guess_group.bench_function("non-simd", |b| {
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
    check_guess_group.bench_function("simd", |b| {
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &simd_words {
                for &answer in &simd_words {
                    let response = check_guess_simd(guess, answer);
                    prevent_noop += response.0 as i32;
                }
            }
            prevent_noop
        })
    });
    check_guess_group.bench_function("simd_slow", |b| {
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &simd_words {
                for &answer in &simd_words {
                    let response = check_guess_simd_slow(guess, answer);
                    prevent_noop += response.0 as i32;
                }
            }
            prevent_noop
        })
    });
    check_guess_group.bench_function("simd_simple_guess", |b| {
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &simd_words {
                for &answer in &simd_words {
                    // Note that this won't actually be right, since we don't check the
                    // precondition. But that's okay, we just care about speed.
                    let response = check_guess_simd_simple_guess(guess, answer);
                    prevent_noop += response.0 as i32;
                }
            }
            prevent_noop
        })
    });
    check_guess_group.bench_function("simd_simple_answer", |b| {
        b.iter(|| {
            let mut prevent_noop = 0;
            for &guess in &simd_words {
                for &answer in &simd_words {
                    // Note that this won't actually be right, since we don't check the
                    // precondition. But that's okay, we just care about speed.
                    let response = check_guess_simd_simple_answer(guess, answer);
                    prevent_noop += response.0 as i32;
                }
            }
            prevent_noop
        })
    });
    check_guess_group.finish();
    const N_BUCKETS: usize = 3usize.pow(5);
    const BUCKET_SIZE: usize = 2314;
    let values: Vec<usize> = (0..100).map(|i| i * 23).collect();
    let bucket_alist = |size: usize| {
        let mut out = Vec::new();
        for &x in &values {
            alist_get_or_else(&mut out, x % size, Vec::new).push(x);
        }
        out
    };
    let bucket_hashmap = |size: usize| {
        let mut out = HashMap::new();
        for &x in &values {
            out.entry(x % size).or_insert_with(Vec::new).push(x);
        }
        out
    };
    let bucket_btreemap = |size: usize| {
        let mut out = BTreeMap::new();
        for &x in &values {
            out.entry(x % size).or_insert_with(Vec::new).push(x);
        }
        out
    };
    let bucket_vec = |size: usize| {
        let mut out = vec![Vec::new(); N_BUCKETS];
        for &x in &values {
            out[x % size].push(x);
        }
        out
    };
    let bucket_bitset = |size: usize| {
        let mut out = BitSet::with_capacity(BUCKET_SIZE * N_BUCKETS);
        for &x in &values {
            out.insert((x % size) * BUCKET_SIZE + x);
        }
        out
    };
    let bucket_flat_vec = |size: usize| {
        let mut out = Vec::new();
        for &x in &values {
            out.push((x % size, x));
        }
        out.sort_unstable_by_key(|(bucket, _)| *bucket);
        out
    };

    let mut write_buckets = c.benchmark_group("write_buckets");
    for size in [1, 2, 4, 8, 16, 32, 64, 128] {
        write_buckets.bench_with_input(BenchmarkId::new("Alist", size), &size, |b, &size| {
            b.iter(|| bucket_alist(size))
        });
        /*
        write_buckets.bench_with_input(BenchmarkId::new("Hashmap", size), &size, |b, &size| {
            b.iter(|| bucket_hashmap(size))
        });
        write_buckets.bench_with_input(BenchmarkId::new("BTreeMap", size), &size, |b, &size| {
            b.iter(|| bucket_btreemap(size))
        });
        */
        write_buckets.bench_with_input(BenchmarkId::new("Vec", size), &size, |b, &size| {
            b.iter(|| bucket_vec(size))
        });
        write_buckets.bench_with_input(BenchmarkId::new("BitSet", size), &size, |b, &size| {
            b.iter(|| bucket_bitset(size))
        });
        write_buckets.bench_with_input(BenchmarkId::new("FlatVec", size), &size, |b, &size| {
            b.iter(|| bucket_flat_vec(size))
        });
    }
    write_buckets.finish();

    let mut read_buckets = c.benchmark_group("read_buckets");
    for size in [1, 2, 4, 8, 16, 32, 64, 128] {
        read_buckets.bench_with_input(BenchmarkId::new("Alist", size), &size, |b, &size| {
            let buckets = bucket_alist(size);
            b.iter(|| {
                let iter = buckets.iter().map(|(k, v)| (*k, v.as_slice()));
                consume_buckets(iter)
            })
        });
        /*
        read_buckets.bench_with_input(BenchmarkId::new("Hashmap", size), &size, |b, &size| {
            let buckets = bucket_hashmap(size);
            b.iter(|| {
                let iter = buckets.iter().map(|(k, v)| (*k, v.as_slice()));
                consume_buckets(iter)
            })
        });
        read_buckets.bench_with_input(BenchmarkId::new("BTreeMap", size), &size, |b, &size| {
            let buckets = bucket_btreemap(size);
            b.iter(|| {
                let iter = buckets.iter().map(|(k, v)| (*k, v));
                consume_buckets(iter)
            })
        });
        read_buckets.bench_with_input(BenchmarkId::new("BitSet", size), &size, |b, &size| {
            let buckets = bucket_bitset(size);
            b.iter(|| {
                let iter = buckets.iter().map(|i| (i / BUCKET_SIZE, i % BUCKET_SIZE));
                consume_ungrouped_buckets(iter)
            })
        });
        */
        read_buckets.bench_with_input(BenchmarkId::new("Vec", size), &size, |b, &size| {
            let buckets = bucket_vec(size);
            b.iter(|| {
                let iter = buckets.iter().map(Vec::as_slice).enumerate();
                consume_buckets(iter)
            })
        });
        read_buckets.bench_with_input(BenchmarkId::new("FlatVec", size), &size, |b, &size| {
            let buckets = bucket_flat_vec(size);
            b.iter(|| {
                let iter = buckets.group_by(|(k1, _), (k2, _)| k1 == k2);
                consume_pair_buckets(iter)
            })
        });
    }
    read_buckets.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
