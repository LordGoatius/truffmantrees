use std::{
    collections::{BinaryHeap, HashMap, VecDeque},
    fs::File,
    hash::Hash,
    io::Read,
    usize,
};

use ternary::{trits::Trit, tryte::Tryte};

use crate::triterator::Triterator;

#[derive(Debug, PartialEq, Eq)]
pub enum TruffmanTree<T> {
    None,
    Leaf(usize, T),
    Node(
        Box<TruffmanTree<T>>,
        Box<TruffmanTree<T>>,
        Box<TruffmanTree<T>>,
    ),
}

impl<T> TruffmanTree<T> {
    pub fn create_file_tree(mut path: File) -> TruffmanTree<Tryte> {
        let mut char_map: HashMap<Tryte, usize> = HashMap::new();
        let mut u8_buff = Vec::new();
        path.read_to_end(&mut u8_buff).unwrap();
        for tryte in u8_buff
            .into_iter()
            .map(|char| <isize as Into<Tryte>>::into(char as isize))
        {
            let val = char_map.get(&tryte).unwrap_or(&0);
            char_map.insert(tryte, val + 1);
        }

        let mut bin_heap = char_map
            .into_iter()
            .map(|(char, count)| TruffmanTree::Leaf(count, char))
            .collect::<BinaryHeap<_>>(); //BinaryHeap::new();

        // We want 5 at the end. Each loop the heap takes 3 and turns it into one.
        while bin_heap.len() % 2 != 1 {
            bin_heap.push(TruffmanTree::None);
        }

        while bin_heap.len() > 2 {
            let left = bin_heap.pop().unwrap();
            let mid = bin_heap.pop().unwrap();
            let right = bin_heap.pop().unwrap();

            bin_heap.push(TruffmanTree::Node(
                Box::new(left),
                Box::new(mid),
                Box::new(right),
            ));
        }

        debug_assert_eq!(bin_heap.len(), 1);
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
            TruffmanTree::None => panic!(),
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
            TruffmanTree::None => panic!(),
        }
    }

    fn value(&self) -> usize {
        match self {
            &TruffmanTree::Leaf(val, _) => val,
            TruffmanTree::Node(l, m, r) => l.value() + m.value() + r.value(),
            TruffmanTree::None => 0,
        }
    }

    pub fn kraft_mcmillan(&self) -> f64 {
        let mut data: Vec<usize> = Vec::new();

        fn rec_int<V>(depth: usize, data: &mut Vec<usize>, curr: &TruffmanTree<V>) {
            match curr {
                TruffmanTree::None => return,
                TruffmanTree::Leaf(_, _) => data.push(depth),
                TruffmanTree::Node(l, m, r) => {
                    rec_int(depth + 1, data, &l);
                    rec_int(depth + 1, data, &m);
                    rec_int(depth + 1, data, &r);
                }
            }
        }

        rec_int(0, &mut data, self);

        data.into_iter()
            .map(|depth| f64::powi(3.0, -(depth as i32)))
            .sum()
    }
}

impl<T: Hash + Clone + Eq> TruffmanTree<T> {
    pub fn to_table(&self) -> HashMap<T, Vec<Trit>> {
        let mut curr: Vec<Trit> = Vec::new();
        let mut table = Vec::new();

        fn rec_int<V: Clone>(
            tree: &TruffmanTree<V>,
            curr: &mut Vec<Trit>,
            table: &mut Vec<(V, Vec<Trit>)>,
        ) {
            match tree {
                TruffmanTree::Leaf(_, val) => {
                    table.push((val.clone(), curr.clone()));
                }
                TruffmanTree::Node(l, m, r) => {
                    curr.push(Trit::NOne);
                    rec_int(l, curr, table);
                    curr.pop();

                    curr.push(Trit::Zero);
                    rec_int(m, curr, table);
                    curr.pop();

                    curr.push(Trit::POne);
                    rec_int(r, curr, table);
                    curr.pop();
                }
                TruffmanTree::None => return,
            }
        }

        rec_int(&self, &mut curr, &mut table);
        table.into_iter().collect()
    }
}

impl<T: PartialOrd> PartialOrd for TruffmanTree<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        other.value().partial_cmp(&self.value())
    }
}

impl<T: Ord> Ord for TruffmanTree<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.value().cmp(&self.value())
    }
}

#[cfg(test)]
pub mod tests {
    use std::{
        collections::{BinaryHeap, HashMap},
        fs::File,
        io::Read, path::Path,
    };

    use owo_colors::OwoColorize;
    use ternary::{trits::Trit, tryte::Tryte};

    use crate::triterator::Triterator;

    use super::TruffmanTree;

    #[test]
    fn kraft() {
        let file = File::open("./testfile.txt").unwrap();
        let tree = TruffmanTree::<Tryte>::create_file_tree(file);
        let kraft = tree.kraft_mcmillan();
        assert_eq!(1., kraft);
        eprintln!("{}", kraft);
    }

    #[test]
    fn tern_bin_heap_tree() {
        let mut char_map: HashMap<Tryte, usize> = HashMap::new();
        let tryte_buff: Vec<Tryte> = (0isize..=255)
            .chain(0..=127)
            .chain(64..=127)
            .map(|x| x.into())
            .collect();
        for tryte in tryte_buff.into_iter() {
            let val = char_map.get(&tryte).unwrap_or(&0);
            char_map.insert(tryte, val + 1);
        }

        let mut bin_heap = char_map
            .into_iter()
            .map(|(char, count)| TruffmanTree::Leaf(count, char))
            .collect::<BinaryHeap<_>>();

        bin_heap.push(TruffmanTree::Node(
            Box::new(TruffmanTree::None),
            Box::new(TruffmanTree::Leaf(1, 4.into())),
            Box::new(TruffmanTree::Leaf(1, 6.into())),
        ));

        while let Some(i) = bin_heap.pop() {
            eprintln!("{i:?}");
        }
    }

    #[test]
    fn tern_test_file_compression() {
        let file = File::open("./testfile.txt").unwrap();
        let tree = TruffmanTree::<Tryte>::create_file_tree(file);
        let table = tree.to_table();
        for (num, code) in table.iter() {
            assert_eq!(num, tree.traverse(code.clone().into()))
        }

        let mut file = File::open("./testfile.txt").unwrap();
        let mut data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        let data_tryte: Vec<Tryte> = data
            .iter()
            .map(|byte| <isize as Into<Tryte>>::into(*byte as isize))
            .collect();

        let compressed_data: Vec<Trit> = data
            .iter()
            .map(|byte| table[&<isize as Into<Tryte>>::into(*byte as isize)].clone())
            .flatten()
            .collect();

        let len_data = data_tryte.len() * 9;
        let len_compressed = compressed_data.len();

        assert!(len_data > len_compressed);

        let compressed_data: Vec<Tryte> = compressed_data
            .chunks(9)
            .map(|chunk| {
                if chunk.len() == 9 {
                    return Tryte(chunk.try_into().unwrap());
                } else {
                    let mut tryte = [Trit::Zero; 9];
                    for i in (0..chunk.len()).rev() {
                        tryte[i] = chunk[i];
                    }
                    Tryte(tryte)
                }
            })
            .collect();

        let mut triterator = Triterator::new(compressed_data);
        let mut decompressed = Vec::new();
        while decompressed.len() < data.len() {
            decompressed.push(*tree.traverse_triterator(&mut triterator));
        }

        assert_eq!(decompressed, data_tryte);

        eprintln!("Kraft McMillan: {}", tree.kraft_mcmillan().blue());
        eprintln!("Data Length (Trits): {}", len_data.yellow());
        eprintln!("Compressed Length (Trits): {}", len_compressed.bright_red());
        eprintln!();
        eprintln!("Data Length (Trytes): {}", data.len().yellow());
        eprintln!(
            "Compressed Length (Trytes): {}",
            (len_compressed / 9).bright_red()
        );
        eprintln!();
        eprintln!(
            "Compression Ratio: {}",
            (len_compressed as f64 / len_data as f64).green()
        );
        eprintln!();
    }

    #[test]
    fn tern_test_file_compression_2() {
        let file_name = Path::new("./random.txt");

        let file = File::open(file_name).unwrap();
        let tree = TruffmanTree::<Tryte>::create_file_tree(file);
        let table = tree.to_table();
        for (num, code) in table.iter() {
            assert_eq!(num, tree.traverse(code.clone().into()))
        }

        let mut file = File::open(file_name).unwrap();
        let mut data = Vec::new();
        file.read_to_end(&mut data).unwrap();
        let data_tryte: Vec<Tryte> = data
            .iter()
            .map(|byte| <isize as Into<Tryte>>::into(*byte as isize))
            .collect();

        let compressed_data: Vec<Trit> = data
            .iter()
            .map(|byte| table[&<isize as Into<Tryte>>::into(*byte as isize)].clone())
            .flatten()
            .collect();

        let len_data = data_tryte.len() * 9;
        let len_compressed = compressed_data.len();

        assert!(len_data > len_compressed);

        let compressed_data: Vec<Tryte> = compressed_data
            .chunks(9)
            .map(|chunk| {
                if chunk.len() == 9 {
                    return Tryte(chunk.try_into().unwrap());
                } else {
                    let mut tryte = [Trit::Zero; 9];
                    for i in (0..chunk.len()).rev() {
                        tryte[i] = chunk[i];
                    }
                    Tryte(tryte)
                }
            })
            .collect();

        let mut triterator = Triterator::new(compressed_data);
        let mut decompressed = Vec::new();
        while decompressed.len() < data.len() {
            decompressed.push(*tree.traverse_triterator(&mut triterator));
        }

        assert_eq!(decompressed, data_tryte);

        eprintln!("Kraft McMillan: {}", tree.kraft_mcmillan().blue());
        eprintln!("Data Length (Trits): {}", len_data.yellow());
        eprintln!("Compressed Length (Trits): {}", len_compressed.bright_red());
        eprintln!();
        eprintln!("Data Length (Trytes): {}", data.len().yellow());
        eprintln!(
            "Compressed Length (Trytes): {}",
            (len_compressed / 9).bright_red()
        );
        eprintln!();
        eprintln!(
            "Compression Ratio: {}",
            (len_compressed as f64 / len_data as f64).green()
        );
        eprintln!();
    }
}
