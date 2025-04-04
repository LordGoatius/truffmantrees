use ternary::tryte::Tryte;
use ternary::trits::Trit;

#[derive(Debug, Clone)]
pub struct Triterator {
    pub(in crate::triterator) data: Vec<Tryte>,
    pub(in crate::triterator) curr: usize
}

impl Triterator {
    fn new(data: Vec<Tryte>) -> Triterator {
        Triterator {
            data,
            curr: 0
        }
    }
}

impl Iterator for Triterator {
    type Item = Trit;
    fn next(&mut self) -> Option<Self::Item> {
        let ind = self.curr / 9;
        let int = self.curr % 9;
        self.curr += 1;
        self.data.get(ind).map(|tryte| tryte[int])
    }
}
