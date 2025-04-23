use std::{
    collections::{BinaryHeap, HashMap, VecDeque},
    fmt::{Debug, Display},
    fs::File,
    hash::Hash,
    io::Read,
    ops::{Deref, DerefMut},
};

use itertools::{Either, Itertools};

use crate::biterator::{Bit, Biterator};

#[derive(Debug, PartialEq, Eq)]
pub enum HuffmanTree<T> {
    Leaf(usize, T),
    Node(Box<HuffmanTree<T>>, Box<HuffmanTree<T>>),
}

#[derive(Debug, Clone)]
pub struct HuffmanTable<T>(pub(super) HashMap<T, Vec<Bit>>);

impl<T> Deref for HuffmanTable<T> {
    type Target = HashMap<T, Vec<Bit>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for HuffmanTable<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Clone> HuffmanTree<T> {
    pub fn from(symbol_code_vec: HuffmanTable<T>) -> HuffmanTree<T> {
        let scv = symbol_code_vec
            .iter()
            .filter_map(|val| {
                if !val.1.is_empty() {
                    Some((val.0.clone(), val.1.clone().into()))
                } else {
                    None
                }
            })
            .collect();

        // We have determined it is valid, so we can use an infallible internal recursive
        fn internal_create<T: Clone>(symbol_code_vec: Vec<(T, VecDeque<Bit>)>) -> HuffmanTree<T> {
            let empty: Vec<_> = symbol_code_vec
                .iter()
                .filter_map(|s_c| if s_c.1.is_empty() { Some(s_c) } else { None })
                .collect();

            if empty.is_empty() {
                let (left, right): (Vec<_>, Vec<_>) =
                    symbol_code_vec.into_iter().partition_map(|mut s_c| {
                        let check = s_c.1[0];
                        s_c.1.pop_front();
                        if check == Bit::Zero {
                            Either::Left(s_c)
                        } else {
                            Either::Right(s_c)
                        }
                    });
                HuffmanTree::Node(
                    Box::new(internal_create(left)),
                    Box::new(internal_create(right)),
                )
            } else {
                HuffmanTree::Leaf(0, empty[0].0.clone())
            }
        }

        internal_create(scv)
    }

    pub fn create_file_tree(mut path: File) -> HuffmanTree<u8> {
        let mut char_map: HashMap<u8, usize> = HashMap::new();
        let mut u8_buff = Vec::new();
        path.read_to_end(&mut u8_buff).unwrap();
        for byte in u8_buff.into_iter() {
            let val = char_map.get(&byte).unwrap_or(&0);
            char_map.insert(byte, val + 1);
        }

        let mut bin_heap = char_map
            .into_iter()
            .map(|(char, count)| HuffmanTree::Leaf(count, char))
            .collect::<BinaryHeap<_>>(); //BinaryHeap::new();

        while bin_heap.len() > 1 {
            let left = bin_heap.pop().unwrap();
            let right = bin_heap.pop().unwrap();

            bin_heap.push(HuffmanTree::Node(Box::new(left), Box::new(right)));
        }

        bin_heap.pop().unwrap()
    }

    pub fn traverse(&self, mut code: VecDeque<Bit>) -> &T {
        match self {
            HuffmanTree::Leaf(_, val) => val,
            HuffmanTree::Node(l, r) => match code.pop_front().unwrap() {
                Bit::Zero => l.traverse(code),
                Bit::One => r.traverse(code),
            },
        }
    }

    pub fn traverse_biterator(&self, biterator: &mut Biterator) -> &T {
        match self {
            HuffmanTree::Leaf(_, val) => val,
            HuffmanTree::Node(l, r) => match biterator.next().unwrap() {
                Bit::Zero => l.traverse_biterator(biterator),
                Bit::One => r.traverse_biterator(biterator),
            },
        }
    }

    fn value(&self) -> usize {
        match self {
            &HuffmanTree::Leaf(val, _) => val,
            HuffmanTree::Node(l, r) => l.value() + r.value(),
        }
    }

    pub fn kraft_mcmillan(&self) -> f64 {
        let mut data: Vec<usize> = Vec::new();

        fn rec_int<V>(depth: usize, data: &mut Vec<usize>, curr: &HuffmanTree<V>) {
            match curr {
                HuffmanTree::Leaf(_, _) => data.push(depth),
                HuffmanTree::Node(l, r) => {
                    rec_int(depth + 1, data, &l);
                    rec_int(depth + 1, data, &r);
                }
            }
        }

        rec_int(0, &mut data, self);

        data.into_iter()
            .map(|depth| f64::powi(2.0, -(depth as i32)))
            .sum()
    }
}

impl<T: Hash + Clone + Eq> HuffmanTree<T> {
    pub fn to_table(&self) -> HuffmanTable<T> {
        let mut curr: Vec<Bit> = Vec::new();
        let mut table = Vec::new();

        fn int_rec<V: Clone>(
            tree: &HuffmanTree<V>,
            curr: &mut Vec<Bit>,
            table: &mut Vec<(V, Vec<Bit>)>,
        ) {
            match tree {
                HuffmanTree::Leaf(_, val) => {
                    table.push((val.clone(), curr.clone()));
                }
                HuffmanTree::Node(l, r) => {
                    curr.push(Bit::Zero);
                    int_rec(l, curr, table);
                    curr.pop();
                    curr.push(Bit::One);
                    int_rec(r, curr, table);
                    curr.pop();
                }
            }
        }

        int_rec(&self, &mut curr, &mut table);
        HuffmanTable(table.into_iter().collect())
    }
}

impl<T: Display> Display for HuffmanTable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Huffman Table:")?;
        writeln!(f, "{: >11} | {}", "Symbol", "Code")?;
        writeln!(f, "     -------|-----")?;
        for (symbol, code) in self.iter() {
            let code_string: String = code
                .iter()
                .map(|bit| <Bit as Into<char>>::into(*bit))
                .collect();
            writeln!(f, "{symbol: >11} | {code_string}")?
        }

        Ok(())
    }
}

impl<T> HuffmanTable<T> {
    pub fn canonical(lens: &[u8]) -> Vec<Vec<Bit>> {
        let mut codes: Vec<u16> = std::iter::repeat(0).take(lens.len()).collect::<Vec<_>>();
        let max = *lens.iter().max().unwrap();

        let histogram = &mut vec![0u16; max as usize + 1];
        let next_code = &mut vec![0u16; max as usize + 1];
        for i in lens {
            histogram[*i as usize] += 1;
        }

        let mut code = 0;
        histogram[0] = 0;

        for i in 1..=max {
            code = (code + histogram[i as usize - 1]) << 1;
            next_code[i as usize] = code;
        }

        for n in 0..lens.len() {
            let len = lens[n as usize];
            if len != 0 {
                codes[n as usize] = next_code[len as usize];
                next_code[len as usize] += 1;
            }
        }

        let codes: Vec<_> = codes
            .into_iter()
            .enumerate()
            .map(|(i, code)| {
                let mut bits: VecDeque<_> = vec![].into();
                for j in 0..lens[i as usize] {
                    let bit: Bit = ((code >> j) & 0b1).try_into().unwrap();
                    bits.push_front(bit);
                }
                bits.into()
            })
            .collect();
        codes
    }
}

impl PartialOrd for HuffmanTree<u8> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.value().partial_cmp(&self.value())
    }
}

impl Ord for HuffmanTree<u8> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.value().cmp(&self.value())
    }
}

#[cfg(test)]
pub mod tests {
    use std::{
        collections::{BinaryHeap, HashMap},
        fs::File,
        io::{Read, Write},
        path::Path,
    };

    use owo_colors::OwoColorize;

    use crate::biterator::{Bit, Biterator};

    use super::{HuffmanTable, HuffmanTree};

    #[test]
    fn bin_canonical() {
        let lens = [4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 0];
        let tbl = HuffmanTable::<u8>::canonical(&lens);
        let tbl = HuffmanTable((1..).zip(tbl.into_iter()).collect());
        eprintln!("{tbl}");
    }

    #[test]
    fn test_tree_table_display() {
        let file = File::open("./testfile.txt").unwrap();
        let tree = HuffmanTree::<u8>::create_file_tree(file);
        let table = tree.to_table();
        eprintln!("{table}");
    }

    #[test]
    fn kraft() {
        let file = File::open("./testfile.txt").unwrap();
        let tree = HuffmanTree::<u8>::create_file_tree(file);
        let kraft = tree.kraft_mcmillan();
        assert_eq!(1., kraft);
        eprintln!("{}", kraft);
    }

    #[test]
    fn bin_heap_tree() {
        let mut char_map: HashMap<u8, usize> = HashMap::new();
        let u8_buff: Vec<u8> = (0..=255).chain(0..=127).chain(64..=127).collect();
        for byte in u8_buff.into_iter() {
            let val = char_map.get(&byte).unwrap_or(&0);
            char_map.insert(byte, val + 1);
        }

        let mut bin_heap = char_map
            .into_iter()
            .map(|(char, count)| HuffmanTree::Leaf(count, char))
            .collect::<BinaryHeap<_>>();

        bin_heap.push(HuffmanTree::Node(
            Box::new(HuffmanTree::Leaf(2, 0)),
            Box::new(HuffmanTree::Leaf(2, 2)),
        ));

        while let Some(i) = bin_heap.pop() {
            eprintln!("{i:?}");
        }
    }

    #[test]
    fn test_file_compression() {
        let file = File::open("./testfile.txt").unwrap();
        let tree = HuffmanTree::<u8>::create_file_tree(file);
        let table = tree.to_table();
        for (num, code) in table.iter() {
            assert_eq!(num, tree.traverse(code.clone().into()))
        }

        let mut file = File::open("./testfile.txt").unwrap();
        let mut compressed = File::create("./testfile.comp").unwrap();
        let mut data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        let compressed_data: Vec<Bit> = data
            .into_iter()
            .map(|byte| table[&byte].clone())
            .flatten()
            .collect();

        let bytes: Vec<_> = compressed_data
            .chunks(8)
            .map(|chunk| Bit::to_u8(chunk.to_vec()))
            .collect();
        compressed.write(&bytes).unwrap();
        drop(compressed);

        let mut file = File::open("./testfile.txt").unwrap();
        let mut compressed = File::open("./testfile.comp").unwrap();
        let mut data = Vec::new();
        let mut compressed_data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        compressed.read_to_end(&mut compressed_data).unwrap();
        let len_compressed = compressed_data.len();
        let mut biterator = Biterator::new(compressed_data);
        let mut decompressed = Vec::new();
        while decompressed.len() < data.len() {
            decompressed.push(*tree.traverse_biterator(&mut biterator));
        }

        assert_eq!(decompressed, data);

        let len_data = data.len();

        eprintln!("Kraft McMillan: {}", tree.kraft_mcmillan().blue());
        eprintln!("Data Length (Bits): {}", (len_data * 8).yellow());
        eprintln!(
            "Compressed Length (Bits): {}",
            (len_compressed * 8).bright_red()
        );
        eprintln!();
        eprintln!("Data Length (Bytes): {}", len_data.yellow());
        eprintln!("Compressed Length (Bytes): {}", len_compressed.bright_red());
        eprintln!();
        eprintln!(
            "Compression Ratio: {}",
            (len_compressed as f64 / len_data as f64).green()
        );
        eprintln!();
    }

    #[test]
    fn test_file_compression_2() {
        let file_name = Path::new("./random.txt");
        let file_comp = Path::new("./random.comp");

        let file = File::open(file_name).unwrap();
        let tree = HuffmanTree::<u8>::create_file_tree(file);
        let table = tree.to_table();
        for (num, code) in table.iter() {
            assert_eq!(num, tree.traverse(code.clone().into()))
        }

        let mut file = File::open(file_name).unwrap();
        let mut compressed = File::create(file_comp).unwrap();
        let mut data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        let compressed_data: Vec<Bit> = data
            .into_iter()
            .map(|byte| table[&byte].clone())
            .flatten()
            .collect();

        let bytes: Vec<_> = compressed_data
            .chunks(8)
            .map(|chunk| Bit::to_u8(chunk.to_vec()))
            .collect();
        compressed.write(&bytes).unwrap();
        drop(compressed);

        let mut file = File::open(file_name).unwrap();
        let mut compressed = File::open(file_comp).unwrap();
        let mut data = Vec::new();
        let mut compressed_data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        compressed.read_to_end(&mut compressed_data).unwrap();
        let len_compressed = compressed_data.len();
        let mut biterator = Biterator::new(compressed_data);
        let mut decompressed = Vec::new();
        while decompressed.len() < data.len() {
            decompressed.push(*tree.traverse_biterator(&mut biterator));
        }

        assert_eq!(decompressed, data);

        let len_data = data.len();

        eprintln!("Kraft McMillan: {}", tree.kraft_mcmillan().blue());
        eprintln!("Data Length (Bits): {}", (len_data * 8).yellow());
        eprintln!(
            "Compressed Length (Bits): {}",
            (len_compressed * 8).bright_red()
        );
        eprintln!();
        eprintln!("Data Length (Bytes): {}", len_data.yellow());
        eprintln!("Compressed Length (Bytes): {}", len_compressed.bright_red());
        eprintln!();
        eprintln!(
            "Compression Ratio: {}",
            (len_compressed as f64 / len_data as f64).green()
        );
        eprintln!();
    }
}
