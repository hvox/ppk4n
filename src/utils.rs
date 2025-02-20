#![allow(unused)]
use indexmap::IndexMap;
use std::{cell::Cell, hash::Hash, mem::swap, ops::Index};

pub fn try_map<T, R, E>(xs: impl IntoIterator<Item = T>, f: impl FnMut(T) -> Result<R, E>) -> Result<Vec<R>, E> {
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
