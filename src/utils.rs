#![allow(unused)]
use indexmap::IndexMap;
use std::{cell::Cell, hash::Hash, mem::swap, ops::Index, rc::Rc};

pub fn map<T, R>(xs: impl IntoIterator<Item = T>, f: impl FnMut(T) -> R) -> Vec<R> {
    xs.into_iter().map(f).collect()
}

pub fn try_map<T, R, E>(
    xs: impl IntoIterator<Item = T>,
    f: impl FnMut(T) -> Result<R, E>,
) -> Result<Vec<R>, E> {
    xs.into_iter().map(f).collect::<Result<_, _>>()
}

pub fn try_imap<T, K, V, E>(
    xs: impl IntoIterator<Item = T>,
    f: impl Fn(T) -> Result<(K, V), E>,
) -> Result<IndexMap<K, V>, E>
where
    K: Hash + Eq,
{
    xs.into_iter().map(f).collect::<Result<IndexMap<K, V>, E>>()
}

pub struct Dsu {
    leaders: Vec<Cell<u32>>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct DsuElementId(u32);

impl Dsu {
    pub fn new() -> Self {
        Self { leaders: vec![] }
    }

    pub fn get_set(&self, id: DsuElementId) -> usize {
        let i = id.0 as usize;
        let mut leader = self.leaders[i].get();
        if self.leaders[leader as usize].get() != leader {
            leader = self.get_set(DsuElementId(leader)) as u32;
            self.leaders[i].set(leader);
        }
        leader as usize
    }

    pub fn new_element(&mut self) -> DsuElementId {
        let id = self.leaders.len() as u32;
        self.leaders.push(Cell::new(id));
        DsuElementId(id)
    }

    pub fn merge_sets(&mut self, mut u: usize, mut v: usize) -> usize {
        // TODO: check if rank system improves performance in my case
        if u > v {
            swap(&mut u, &mut v);
        }
        self.leaders[v].set(u as u32);
        u
    }
}

pub fn stringify_lossy(bytes: &[u8]) -> String {
    let mut string = String::new();
    for chunk in bytes.utf8_chunks() {
        for (i, ch) in chunk.valid().char_indices() {
            if ch.escape_debug().count() == 1 {
                string.push(ch);
            } else {
                for byte in chunk.valid()[i..i + ch.len_utf8()].bytes() {
                    string.push_str(&format!("\\{:02X}", byte));
                }
            }
        }
        for byte in chunk.invalid() {
            string.push_str(&format!("\\{:02X}", byte));
        }
    }
    string
}

pub trait Leb128<T> {
    fn pack(&mut self, value: T);
}

impl Leb128<i64> for Vec<u8> {
    fn pack(&mut self, value: i64) {
        let mut value = value;
        loop {
            self.push(0x7f & value as u8 + (value > 0x7f) as u8);
            value >>= 7;
            if value == 0 {
                break;
            }
        }
        todo!("Wrong sign");
    }
}

impl Leb128<usize> for Vec<u8> {
    fn pack(&mut self, value: usize) {
        let mut value = value;
        loop {
            self.push(0x7f & value as u8 + (value > 0x7f) as u8);
            value >>= 7;
            if value == 0 {
                break;
            }
        }
    }
}

impl Leb128<i32> for Vec<u8> {
    fn pack(&mut self, value: i32) {
        self.pack(value as i64);
    }
}

impl Leb128<u32> for Vec<u8> {
    fn pack(&mut self, value: u32) {
        self.pack(value as usize);
    }
}

impl Leb128<&str> for Vec<u8> {
    fn pack(&mut self, string: &str) {
        self.pack(string.as_bytes());
    }
}

impl Leb128<Rc<str>> for Vec<u8> {
    fn pack(&mut self, string: Rc<str>) {
        self.pack(string.as_bytes());
    }
}

impl Leb128<Vec<Vec<u8>>> for Vec<u8> {
    fn pack(&mut self, array: Vec<Vec<u8>>) {
        self.pack(array.len());
        for bytes in array.into_iter() {
            self.extend(bytes);
        }
    }
}

impl Leb128<&[u8]> for Vec<u8> {
    fn pack(&mut self, bytes: &[u8]) {
        self.pack(bytes.len());
        self.extend(bytes);
    }
}
