use super::{Letter, ResponseInt, Word};
use std::simd::{
    i8x8, mask8x8, simd_swizzle, u8x8, LaneCount, Mask, MaskElement, SupportedLaneCount,
};

const POW_3: u8x8 = u8x8::from_array([
    3u8.pow(4),
    3u8.pow(3),
    3u8.pow(2),
    3u8.pow(1),
    3u8.pow(0),
    0,
    0,
    0,
]);
const POW_3X2: u8x8 = u8x8::from_array([
    2 * 3u8.pow(4),
    2 * 3u8.pow(3),
    2 * 3u8.pow(2),
    2 * 3u8.pow(1),
    2 * 3u8.pow(0),
    2 * 0,
    0,
    0,
]);

const ZERO: u8x8 = u8x8::splat(0);

pub fn rotate_mask<T: MaskElement, const LANES: usize, const N: usize>(
    m: Mask<T, LANES>,
) -> Mask<T, LANES>
where
    T: MaskElement,
    LaneCount<LANES>: SupportedLaneCount,
{
    let v = m.to_int();
    let vr = v.rotate_lanes_right::<N>();
    unsafe { Mask::<T, LANES>::from_int_unchecked(vr) }
}

pub fn first<T: MaskElement, const LANES: usize>(m: Mask<T, LANES>) -> Mask<T, LANES>
where
    T: MaskElement + std::fmt::Debug,
    LaneCount<LANES>: SupportedLaneCount,
{
    let mut to_clear = rotate_mask::<T, LANES, 1>(m);
    to_clear.set(5, false);
    to_clear |= rotate_mask::<T, LANES, 1>(to_clear);
    to_clear |= rotate_mask::<T, LANES, 2>(to_clear);
    m & !to_clear
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SimdWord(u8x8);
impl SimdWord {
    pub fn from_word(word: Word) -> Self {
        let Word([Letter(l0), Letter(l1), Letter(l2), Letter(l3), Letter(l4)]) = word;
        SimdWord(u8x8::from_array([
            l0 + 1,
            l1 + 1,
            l2 + 1,
            l3 + 1,
            l4 + 1,
            0,
            0,
            0,
        ]))
    }
    pub fn to_word(self) -> Word {
        let &[l0, l1, l2, l3, l4, _, _, _] = self.0.as_array();
        Word([
            Letter(l0 - 1),
            Letter(l1 - 1),
            Letter(l2 - 1),
            Letter(l3 - 1),
            Letter(l4 - 1),
        ])
    }
}

pub fn check_guess_simd(guess: SimdWord, answer: SimdWord) -> ResponseInt {
    if !repeats(guess).any() {
        return check_guess_simd_simple_guess(guess, answer);
    }

    if !repeats(answer).any() {
        return check_guess_simd_simple_answer(guess, answer);
    }
    check_guess_simd_slow(guess, answer)
}

pub fn check_guess_simd_slow(guess: SimdWord, answer: SimdWord) -> ResponseInt {
    // Refers to both guess and answer
    let correct = guess.0.lanes_eq(answer.0);
    // Refers to guess
    let mut moved = mask8x8::splat(false);
    // i refers to answer
    for i in 0..5 {
        if correct.test(i) {
            continue;
        }
        let matches = u8x8::splat(answer.0.as_array()[i]).lanes_eq(guess.0) & !(correct | moved);
        let first_match = first(matches);
        moved |= first_match;
    }
    let cell_values = correct.select(ZERO, moved.select(POW_3, POW_3X2));
    ResponseInt(cell_values.horizontal_sum())
}

pub fn check_guess_simd_simple_guess(guess: SimdWord, answer: SimdWord) -> ResponseInt {
    // Refers to both guess and answer
    let correct = guess.0.lanes_eq(answer.0);
    // Refers to guess
    let moved = moved(guess, answer);
    let cell_values = correct.select(ZERO, moved.select(POW_3, POW_3X2));
    ResponseInt(cell_values.horizontal_sum())
}

fn from_signed(x: i8x8) -> u8x8 {
    let arr = x.as_array();
    let ptr = (arr as *const [i8; 8]) as *const [u8; 8];
    u8x8::from_array(unsafe { *ptr })
}

pub fn check_guess_simd_simple_answer(guess: SimdWord, answer: SimdWord) -> ResponseInt {
    // Refers to both guess and answer
    let correct = guess.0.lanes_eq(answer.0);

    let unused_answer = from_signed((!correct).to_int()) & answer.0;
    // Refers to guess
    let mut moved = moved(guess, SimdWord(unused_answer));
    moved &= !repeats(guess);
    let cell_values = correct.select(ZERO, moved.select(POW_3, POW_3X2));
    ResponseInt(cell_values.horizontal_sum())
}

pub fn moved_or_correct(guess: SimdWord, answer: SimdWord) -> mask8x8 {
    let mut moved = guess.0.lanes_eq(u8x8::splat(answer.0.to_array()[0]));
    moved |= guess.0.lanes_eq(u8x8::splat(answer.0.to_array()[1]));
    moved |= guess.0.lanes_eq(u8x8::splat(answer.0.to_array()[2]));
    moved |= guess.0.lanes_eq(u8x8::splat(answer.0.to_array()[3]));
    moved |= guess.0.lanes_eq(u8x8::splat(answer.0.to_array()[4]));
    moved
}

pub fn moved(guess: SimdWord, answer: SimdWord) -> mask8x8 {
    let rot1 = simd_swizzle!(answer.0, [4, 0, 1, 2, 3, 5, 6, 7]);
    // Refers to guess
    let mut moved = guess.0.lanes_eq(rot1);
    let rot2 = simd_swizzle!(answer.0, [3, 4, 0, 1, 2, 5, 6, 7]);
    moved |= guess.0.lanes_eq(rot2);
    let rot3 = simd_swizzle!(answer.0, [2, 3, 4, 0, 1, 5, 6, 7]);
    moved |= guess.0.lanes_eq(rot3);
    let rot4 = simd_swizzle!(answer.0, [1, 2, 3, 4, 0, 5, 6, 7]);
    moved |= guess.0.lanes_eq(rot4);
    moved
}

fn repeats(SimdWord(word): SimdWord) -> mask8x8 {
    let rot1 = word.rotate_lanes_right::<1>();
    let mut repeated = word.lanes_eq(rot1);
    let rot2 = word.rotate_lanes_right::<2>();
    repeated |= word.lanes_eq(rot2);
    let rot3 = word.rotate_lanes_right::<3>();
    repeated |= word.lanes_eq(rot3);
    // Can't rotate by 4 or we'll bring the last letter around to first.
    let first_last = repeated.test(4) || word.as_array()[0] == word.as_array()[4];
    repeated.set(4, first_last);
    repeated & mask8x8::from_array([true, true, true, true, true, false, false, false])
}

#[cfg(test)]
mod tests {
    use super::super::{check_guess, Response};
    use super::*;
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    fn safe_rotate_mask<T: MaskElement, const LANES: usize, const N: usize>(
        m: Mask<T, LANES>,
    ) -> Mask<T, LANES>
    where
        T: MaskElement,
        LaneCount<LANES>: SupportedLaneCount,
    {
        let v = m.to_int();
        let vr = v.rotate_lanes_right::<N>();
        Mask::<T, LANES>::from_int(vr)
    }

    #[test]
    fn test_rotate_mask_safety() {
        for i in 0..256 {
            let bits: Vec<bool> = (0..8).map(|shift| i & 1 << shift > 0).collect();
            let bits: [bool; 8] = bits.try_into().unwrap();
            let m = mask8x8::from_array(bits);
            safe_rotate_mask::<_, 8, 0>(m);
            safe_rotate_mask::<_, 8, 1>(m);
            safe_rotate_mask::<_, 8, 2>(m);
            safe_rotate_mask::<_, 8, 3>(m);
            safe_rotate_mask::<_, 8, 4>(m);
            safe_rotate_mask::<_, 8, 5>(m);
            safe_rotate_mask::<_, 8, 6>(m);
            safe_rotate_mask::<_, 8, 7>(m);
        }
    }

    #[test]
    fn test_first() {
        for i in 0..256 {
            let bits: Vec<bool> = (0..5)
                .map(|shift| i & 1 << shift > 0)
                .chain([false, false, false])
                .collect();
            let bits: [bool; 8] = bits.try_into().unwrap();
            let m = mask8x8::from_array(bits);
            let first_ix = m.to_array().into_iter().position(|bit| bit);
            let mut expected = mask8x8::splat(false);
            if let Some(i) = first_ix {
                expected.set(i, true);
            }
            assert_eq!(expected, first(m), "\ninput: {:?}", m);
        }
    }
    #[test]
    fn test_check_guess_simd() {
        let file = File::open("short.txt").unwrap();
        let words: Vec<Word> = BufReader::new(file)
            .lines()
            .map(|line| line.unwrap().parse().unwrap())
            .collect();
        let simd_words: Vec<SimdWord> = words.iter().copied().map(SimdWord::from_word).collect();
        for (&guess, &simd_guess) in words.iter().zip(&simd_words) {
            for (&answer, &simd_answer) in words.iter().zip(&simd_words) {
                assert_eq!(
                    Response::from_int(check_guess_simd(simd_guess, simd_answer)),
                    check_guess(guess, answer),
                    "Guess: {}, Answer: {}",
                    guess,
                    answer
                );
            }
        }
    }
    #[test]
    fn test_check_guess_simd_simple_guess() {
        let file = File::open("short.txt").unwrap();
        let words: Vec<Word> = BufReader::new(file)
            .lines()
            .map(|line| line.unwrap().parse().unwrap())
            .collect();
        let simd_words: Vec<SimdWord> = words.iter().copied().map(SimdWord::from_word).collect();
        for (&guess, &simd_guess) in words.iter().zip(&simd_words) {
            for (&answer, &simd_answer) in words.iter().zip(&simd_words) {
                if repeats(simd_guess).any() {
                    continue;
                }
                assert_eq!(
                    Response::from_int(check_guess_simd_simple_guess(simd_guess, simd_answer)),
                    check_guess(guess, answer),
                    "Guess: {}, Answer: {}",
                    guess,
                    answer
                );
            }
        }
    }
    #[test]
    fn test_check_guess_simd_simple_answer() {
        let file = File::open("short.txt").unwrap();
        let words: Vec<Word> = BufReader::new(file)
            .lines()
            .map(|line| line.unwrap().parse().unwrap())
            .collect();
        let simd_words: Vec<SimdWord> = words.iter().copied().map(SimdWord::from_word).collect();
        for (&guess, &simd_guess) in words.iter().zip(&simd_words) {
            for (&answer, &simd_answer) in words.iter().zip(&simd_words) {
                if repeats(simd_answer).any() {
                    continue;
                }
                assert_eq!(
                    Response::from_int(check_guess_simd_simple_answer(simd_guess, simd_answer)),
                    check_guess(guess, answer),
                    "Guess: {}, Answer: {}",
                    guess,
                    answer
                );
            }
        }
    }

    #[test]
    fn test_response_int_roundtrip() {
        for i in 0..3u8.pow(5) {
            let i = ResponseInt(i);
            let response = Response::from_int(i);
            let i2 = response.as_int();
            assert_eq!(i, i2);
        }
    }
}
