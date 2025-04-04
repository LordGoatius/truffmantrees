use std::collections::VecDeque;
use std::vec::Vec;

//== Types ==//

// Why use a bit Enum type? Well, while it does still use a byte of space, we gain the benefits of
// strong typing, exclusive pattern matching, and more obvious intent. A bool would *work*, but
// would be less explicit in what it is trying to accomplish. A u8 would *work*, but it would not
// be clear we are expecting only 2 possible values out of 256. This allows the compiler to not
// need a catchall match arm for invalid numbers as well.
/// An enum representing a singular bit, to be used with the [`Biterator`] iterator
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Bit {
    Zero,
    One,
}

/// An iterator over bits, constructed from a [`Vec<u8>`].
/// It internally turns it into a [`VecDeque`], and consumes the [`VecDeque<u8>`] over time
#[derive(Debug, Clone)]
pub struct Biterator {
    pub(in crate::biterator) data: VecDeque<u8>,
    pub(in crate::biterator) curr: Option<u8>,
    pub(in crate::biterator) index: usize,
}

/// An iterator created from a [`Biterator`] that iterates over the bits without consuming the
/// data, or modifying the internal [`Biterator`]
pub struct BitPeeker<'a> {
    biterator: &'a Biterator,
    advance: usize,
}

pub struct Byterator {
    biterator: Biterator,
}

//== Impls ==//

impl Bit {
    /// Convert a list of [`Bit`]s to a u8. If the list is longer than 8 bits long, it will
    /// truncate the list
    pub fn to_u8(value: Vec<Bit>) -> u8 {
        value
            .iter()
            .enumerate()
            .take(8)
            .map(|(i, &val)| (1u8 << i) * <Bit as Into<u8>>::into(val))
            .sum()
    }

    /// Convert a list of [`Bit`]s to a u8. If the list is longer than `size_of::<usize>()` bits
    /// long it will truncate the list
    pub fn to_usize(value: Vec<Bit>) -> usize {
        value
            .iter()
            .enumerate()
            .take(size_of::<usize>())
            .map(|(i, &val)| (1usize << i) * <Bit as Into<usize>>::into(val))
            .sum()
    }
}

impl Biterator {
    pub fn new(data: Vec<u8>) -> Biterator {
        let mut data: VecDeque<u8> = data.into();
        let curr = data.pop_front();
        Biterator {
            data,
            curr,
            index: 0,
        }
    }
}

impl BitPeeker<'_> {
    pub fn new(biterator: &Biterator) -> BitPeeker<'_> {
        BitPeeker {
            biterator,
            advance: 0,
        }
    }
}

impl Byterator {
    pub fn new(biterator: Biterator) -> Byterator {
        Byterator { biterator }
    }
}

//== Trait Impls ==//

impl Iterator for Biterator {
    type Item = Bit;

    fn next(&mut self) -> Option<Bit> {
        match self.data.len() {
            0 => self.curr.and_then(|curr| match self.index {
                7 => {
                    let ret = Some((curr & (0b1 << self.index)) >> self.index);
                    self.index = 0;
                    self.curr = self.data.pop_front();
                    ret.map(|x| <u8 as TryInto<Bit>>::try_into(x).unwrap())
                }
                0..=6 => {
                    let ret = Some((curr & (0b1 << self.index)) >> self.index);
                    self.index += 1;
                    ret.map(|x| <u8 as TryInto<Bit>>::try_into(x).unwrap())
                }
                _ => unreachable!(),
            }),
            _ => match self.curr {
                None => {
                    self.curr = self.data.pop_front();
                    let curr = self.curr.unwrap();
                    let ret = Some((curr & (0b1 << self.index)) >> self.index);
                    self.index += 1;
                    self.index %= 8;
                    ret.map(|x| <u8 as TryInto<Bit>>::try_into(x).unwrap())
                }
                Some(curr) => match self.index {
                    7 => {
                        let ret = Some((curr & (0b1 << self.index)) >> self.index);
                        self.index = 0;
                        self.curr = self.data.pop_front();
                        ret.map(|x| <u8 as TryInto<Bit>>::try_into(x).unwrap())
                    }
                    0..=6 => {
                        let ret = Some((curr & (0b1 << self.index)) >> self.index);
                        self.index += 1;
                        ret.map(|x| <u8 as TryInto<Bit>>::try_into(x).unwrap())
                    }
                    _ => unreachable!(),
                },
            },
        }
    }
}

impl Iterator for BitPeeker<'_> {
    type Item = Bit;

    fn next(&mut self) -> Option<Self::Item> {
        let val = self.biterator.index + self.advance;
        let ind_into_vec = val % 8;
        let ind_into_u8 = val / 8;
        let item = self.biterator.data.get(ind_into_vec);
        item.map(|val| {
            <u8 as TryInto<Bit>>::try_into((val & (0b1 << ind_into_u8)) >> ind_into_u8).unwrap()
        })
    }
}

impl Iterator for Byterator {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let bits: Vec<_> = (&mut self.biterator).take(8).collect();
        if bits.len() == 0 {
            None
        } else {
            Some(Bit::to_u8(bits))
        }
    }
}

//== Macros ==//

macro_rules! impl_bit_int_conversion {
    ($x:ty) => {
        impl TryFrom<$x> for Bit {
            type Error = ();
            fn try_from(value: $x) -> Result<Bit, ()> {
                match value {
                    0 => Ok(Bit::Zero),
                    1 => Ok(Bit::One),
                    _ => Err(()),
                }
            }
        }

        impl From<Bit> for $x {
            fn from(val: Bit) -> $x {
                match val {
                    Bit::Zero => 0,
                    Bit::One => 1,
                }
            }
        }
    };
}

impl_bit_int_conversion!(u8);
impl_bit_int_conversion!(u16);
impl_bit_int_conversion!(u32);
impl_bit_int_conversion!(u64);
impl_bit_int_conversion!(usize);

impl_bit_int_conversion!(i8);
impl_bit_int_conversion!(i16);
impl_bit_int_conversion!(i32);
impl_bit_int_conversion!(i64);
impl_bit_int_conversion!(isize);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bit_works() {
        let data = vec![0b10101010, 0b00001111];
        // I would usually avoid this pattern in actual code to avoid namespace conflicts, but for
        // one test that doens't get imported, I think it's fine
        use Bit::*;
        let check = vec![
            Zero, One, Zero, One, Zero, One, Zero, One, One, One, One, One, Zero, Zero, Zero, Zero,
        ];
        let biterator = Biterator::new(data.into());
        assert!(biterator.zip(check).all(|(bit, check)| bit == check));
    }

    #[test]
    fn peek_works() {
        let mut biterator = Biterator::new(vec![0b10101010, 0b00001111]);
        {
            let _: Vec<_> = (&mut biterator).take(8).collect();
        }
        // I would usually avoid this pattern in actual code to avoid namespace conflicts, but for
        // one test that doens't get imported, I think it's fine
        use Bit::*;
        let check = vec![
            One, One, One, One, One, One, One, One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero,
        ];

        let peeker = BitPeeker::new(&biterator);
        assert!(peeker.zip(check).all(|(bit, check)| bit == check));
    }
}
