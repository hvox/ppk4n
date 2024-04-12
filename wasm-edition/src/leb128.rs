use leb128::read; // TODO: use my own errorless implementation without panics

pub trait Reader: std::io::Read {
	fn u8(&mut self) -> u8;
	fn i32(&mut self) -> i32;
	fn i64(&mut self) -> i64;
	fn u32(&mut self) -> u32;
	fn u64(&mut self) -> u64;
	fn skip(&mut self, n: usize);
	fn next(&mut self) -> Option<u8>;
	fn vec<T: Readable<T>>(&mut self) -> Vec<T>;
	fn u8s<const N: usize>(&mut self) -> [u8; N];
}

impl<R: std::io::Read> Reader for R {
	fn u8(&mut self) -> u8 {
		self.next().unwrap_or(0)
	}

	fn i32(&mut self) -> i32 {
		read::signed(self).unwrap() as i32
	}

	fn i64(&mut self) -> i64 {
		read::signed(self).unwrap()
	}

	fn u32(&mut self) -> u32 {
		read::unsigned(self).unwrap() as u32
	}

	fn u64(&mut self) -> u64 {
		read::unsigned(self).unwrap()
	}

	fn skip(&mut self, n: usize) {
		for _ in 0..n {
			self.next();
		}
	}

	fn next(&mut self) -> Option<u8> {
		let mut buffer = [0u8; 1];
		self.read_exact(&mut buffer).ok()?;
		Some(buffer[0])
	}

	fn vec<T: Readable<T>>(&mut self) -> Vec<T> {
		let size = self.u32();
		let mut vec = vec![];
		for _ in 0..size {
			vec.push(T::read_from(self));
		}
		vec
	}

	fn u8s<const N: usize>(&mut self) -> [u8; N] {
		let mut buffer = [0u8; N];
		self.read_exact(&mut buffer).unwrap();
		buffer
	}
}

pub trait Readable<T> {
	fn read_from<R: Reader>(reader: &mut R) -> T;
}
impl Readable<u8> for u8 {
	fn read_from<R: Reader>(reader: &mut R) -> u8 {
		reader.u8()
	}
}
impl Readable<i32> for i32 {
	fn read_from<R: Reader>(reader: &mut R) -> i32 {
		reader.i32()
	}
}
impl Readable<i64> for i64 {
	fn read_from<R: Reader>(reader: &mut R) -> i64 {
		reader.i64()
	}
}
impl Readable<u32> for u32 {
	fn read_from<R: Reader>(reader: &mut R) -> u32 {
		reader.u32()
	}
}
impl Readable<u64> for u64 {
	fn read_from<R: Reader>(reader: &mut R) -> u64 {
		reader.u64()
	}
}
