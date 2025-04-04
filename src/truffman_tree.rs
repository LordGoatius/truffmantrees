use std::{
    collections::{BinaryHeap, HashMap, VecDeque},
    fs::File,
    io::Read, usize,
};

use ternary::trits::Trit;

use crate::triterator::Triterator;

#[derive(Debug, PartialEq, Eq)]
pub enum TruffmanTree<T> {
    None,
    Leaf(usize, T),
    Node(Box<TruffmanTree<T>>, Box<TruffmanTree<T>>, Box<TruffmanTree<T>>),
}

impl<T> TruffmanTree<T> {
    pub fn create_file_tree(mut path: File) -> TruffmanTree<u8> {
        let mut char_map: HashMap<u8, usize> = HashMap::new();
        let mut u8_buff = Vec::new();
        path.read_to_end(&mut u8_buff).unwrap();
        for byte in u8_buff.into_iter() {
            let val = char_map.get(&byte).unwrap_or(&0);
            char_map.insert(byte, val + 1);
        }

        let mut bin_heap = char_map
            .into_iter()
            .map(|(char, count)| TruffmanTree::Leaf(count, char))
            .collect::<BinaryHeap<_>>(); //BinaryHeap::new();


        // We want 5 at the end. Each loop the heap takes 3 and turns it into one.
        while bin_heap.len() % 3 != 2 {
            bin_heap.push(TruffmanTree::None);
        }

        while bin_heap.len() > 2 {
            let left = bin_heap.pop().unwrap();
            let mid = bin_heap.pop().unwrap();
            let right = bin_heap.pop().unwrap();

            bin_heap.push(TruffmanTree::Node(Box::new(left), Box::new(mid), Box::new(right)));
        }

        bin_heap.pop().unwrap()
    }

    pub fn traverse(&self, mut code: VecDeque<Trit>) -> &T {
        match self {
            TruffmanTree::Leaf(_, val) => val,
            TruffmanTree::Node(l, m, r) => match code.pop_front().unwrap() {
                Trit::NOne => l.traverse(code),
                Trit::Zero => m.traverse(code),
                Trit::POne => r.traverse(code),
            },
            TruffmanTree::None => panic!()
        }
    }

    pub fn traverse_triterator(&self, triterator: &mut Triterator) -> &T {
        match self {
            TruffmanTree::Leaf(_, val) => val,
            TruffmanTree::Node(l, m, r) => match triterator.next().unwrap() {
                Trit::NOne => l.traverse_triterator(triterator),
                Trit::Zero => m.traverse_triterator(triterator),
                Trit::POne => r.traverse_triterator(triterator),
            },
            TruffmanTree::None => panic!()
        }
    }

    fn value(&self) -> usize {
        match self {
            &TruffmanTree::Leaf(val, _) => val,
            TruffmanTree::Node(l, m, r) => l.value() + m.value() + r.value(),
            TruffmanTree::None => 0,
        }
    }
}

impl TruffmanTree<u8> {
    pub fn to_table(&self) -> HashMap<u8, Vec<Trit>> {
        let mut curr: Vec<Trit> = Vec::new();
        let mut table = Vec::new();

        fn int_rec(tree: &TruffmanTree<u8>, curr: &mut Vec<Trit>, table: &mut Vec<(u8, Vec<Trit>)>) {
            match tree {
                &TruffmanTree::Leaf(_, val) => {
                    table.push((val, curr.clone()));
                }
                TruffmanTree::Node(l, m, r) => {
                    curr.push(Trit::NOne);
                    int_rec(l, curr, table);
                    curr.pop();
                    curr.push(Trit::Zero);
                    int_rec(m, curr, table);
                    curr.pop();
                    curr.push(Trit::POne);
                    int_rec(r, curr, table);
                    curr.pop();
                }
                TruffmanTree::None => return,
            }
        }

        int_rec(&self, &mut curr, &mut table);
        table.into_iter().collect()
    }
}

impl PartialOrd for TruffmanTree<u8> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.value().partial_cmp(&self.value())
    }
}

impl Ord for TruffmanTree<u8> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.value().cmp(&self.value())
    }
}

#[cfg(test)]
pub mod tests {
//     use std::{
//         collections::{BinaryHeap, HashMap},
//         fs::File,
//         io::{Read, Write},
//     };
// 
//     use crate::biterator::{Bit, Biterator};
// 
//     use super::TruffmanTree;
// 
    //#[test]
    //fn bin_heap_tree() {
    //    let mut char_map: HashMap<u8, usize> = HashMap::new();
    //    let u8_buff: Vec<u8> = (0..=255).chain(0..=127).chain(64..=127).collect();
    //    for byte in u8_buff.into_iter() {
    //        let val = char_map.get(&byte).unwrap_or(&0);
    //        char_map.insert(byte, val + 1);
    //    }

    //    let mut bin_heap = char_map
    //        .into_iter()
    //        .map(|(char, count)| TruffmanTree::Leaf(count, char))
    //        .collect::<BinaryHeap<_>>();

    //    bin_heap.push(TruffmanTree::Node(
    //        Box::new(TruffmanTree::Leaf(2, 0)),
    //        Box::new(TruffmanTree::Leaf(2, 2)),
    //    ));

    //    while let Some(i) = bin_heap.pop() {
    //        eprintln!("{i:?}");
    //    }
    //}

    //#[test]
    //fn test_file_compression() {
    //    let file = File::open("./testfile.txt").unwrap();
    //    let tree = TruffmanTree::<u8>::create_file_tree(file);
    //    let table = tree.to_table();
    //    for (num, code) in table.iter() {
    //        assert_eq!(num, tree.traverse(code.clone().into()))
    //    }

    //    let mut file = File::open("./testfile.txt").unwrap();
    //    let mut compressed = File::create("./testfile.comp").unwrap();
    //    let mut data = Vec::new();
    //    file.read_to_end(&mut data).unwrap();
    //    let compressed_data: Vec<Bit> = data.into_iter().map(|byte| table[&byte].clone()).flatten().collect();

    //    let bytes: Vec<_> = compressed_data.chunks(8).map(|chunk| Bit::to_u8(chunk.to_vec())).collect();
    //    compressed.write(&bytes).unwrap();
    //    drop(compressed);

    //    let mut file = File::open("./testfile.txt").unwrap();
    //    let mut compressed = File::open("./testfile.comp").unwrap();
    //    let mut data = Vec::new();
    //    let mut compressed_data = Vec::new();
    //    file.read_to_end(&mut data).unwrap();
    //    compressed.read_to_end(&mut compressed_data).unwrap();
    //    let mut biterator = Biterator::new(compressed_data);
    //    let mut decompressed = Vec::new();
    //    while decompressed.len() < data.len() {
    //        decompressed.push(*tree.traverse_biterator(&mut biterator));
    //    }

    //    assert_eq!(decompressed, data);

    //    eprintln!("{decompressed:?}");
    //    eprintln!("{data:?}");
    //}
}
