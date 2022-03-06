use std::simd::{mask8x8, LaneCount, Mask, MaskElement, SupportedLaneCount};

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
