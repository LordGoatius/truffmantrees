use std::{collections::VecDeque, hash::Hash};

use crate::{biterator::Bit, huffman_tree::{HuffmanTable, HuffmanTree}};

pub fn binary_canonical_from_lengths<T: Hash + Eq + Clone>(tree: Vec<(T, usize)>) -> HuffmanTree<T> {
    let mut codes = vec![0; tree.len()];

    let max = tree.iter().map(|x| x.1).clone().into_iter().max().unwrap() as usize;
    let mut bl_count = vec![0; max + 1];

    for i in tree.iter().map(|x| x.1).clone() {
        bl_count[i as usize] += 1;
    }

    let mut code = 0;
    bl_count[0] = 0;

    let mut next_code = vec![0; max + 1];

    for bits in 1..=max {
        code = (code + bl_count[bits - 1]) << 1;
        next_code[bits] = code;
    }

    for n in 0..18 {
        let len = tree[n].1;
        if len != 0 {
            codes[n] = next_code[len];
            next_code[len] += 1;
        }
    }

    let table = HuffmanTable(tree.into_iter().zip(codes).map(|((t, len), code)| {
        let mut vec_bits: VecDeque<_> = vec![].into();
        for i in 0..len {
            let bit: Bit = ((code & (1 << i)) >> i).try_into().unwrap();
            vec_bits.push_front(bit);
        }
        (t, vec_bits.into())
    }).collect());

    HuffmanTree::from(table)
}
