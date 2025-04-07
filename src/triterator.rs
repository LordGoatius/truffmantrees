use ternary::tryte::Tryte;
use ternary::trits::Trit;

#[derive(Debug, Clone)]
pub struct Triterator {
    pub(in crate::triterator) data: Vec<Tryte>,
    pub(in crate::triterator) curr: usize
}

#[derive(Debug, Clone)]
pub struct TritPeeker {
    triterator: Triterator,
    offset: usize
}

impl Triterator {
    pub fn new(data: Vec<Tryte>) -> Triterator {
        Triterator {
            data,
            curr: 0
        }
    }
}

impl TritPeeker {
    pub fn from(triterator: Triterator) -> TritPeeker {
        TritPeeker { triterator, offset: 0 }
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

impl Iterator for TritPeeker {
    type Item = Trit;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.triterator.curr + self.offset;
        let ind = curr / 9;
        let int = curr % 9;
        self.offset+= 1;
        self.triterator.data.get(ind).map(|tryte| tryte[int])
    }
}
