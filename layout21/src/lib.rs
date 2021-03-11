use serde::{Deserialize, Serialize};

pub enum Unit {
    Nano,
    Micro,
}
pub enum Dir {
    Horiz,
    Vert,
}
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq)]
pub enum Entry {
    Sig(usize),
    Pwr(usize),
    Gnd(usize),
    Gap(usize),
    Pat(Pattern),
}
#[derive(Default, Clone, Debug, Deserialize, Serialize, PartialEq)]
pub struct Pattern {
    pub entries: Vec<Entry>,
    pub nrep: usize,
}
impl Pattern {
    pub fn new(e: impl Into<Vec<Entry>>, nrep: usize) -> Self {
        Self {
            entries: e.into(),
            nrep,
        }
    }
    pub fn expand(&self) -> Vec<Entry> {
        if self.nrep == 0 {
            return Vec::new();
        }
        let mut rv = self.entries.clone();
        for _i in 1..self.nrep {
            rv.extend(self.entries.clone());
        }
        rv
    }
}
pub struct Stack {
    pub units: Unit,
    pub xpitch: usize,
    pub ypitch: usize,
    pub layers: Vec<Layer>,
}

pub struct Layer {
    pub index: usize,
    pub name: String,
    pub dir: Dir,
    pub entries: Vec<Entry>,
    pub offset: (i32, i32),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        use Entry::*;

        let hs_stack = Stack {
            units: Unit::Nano,
            xpitch: 0,
            ypitch: 3300,
            layers: vec![
                Layer {
                    index: 1,
                    name: "M1".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                },
                Layer {
                    index: 2,
                    name: "M2".into(),
                    entries: vec![
                        Gnd(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                        Pwr(490),
                        Pat(Pattern::new(vec![Gap(230), Sig(140)], 7)),
                        Gap(230),
                    ],
                    dir: Dir::Horiz,
                    offset: (0, -245),
                },
            ],
        };
    }
}
