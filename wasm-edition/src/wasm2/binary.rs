use leb128::read; // TODO: use my own errorless implementation without panics

fn f(x: u32) -> u32 {
	5
}

pub trait Reader: std::io::Read {
	fn u8(&mut self) -> u8;
	fn i32(&mut self) -> i32;
	fn i64(&mut self) -> i64;
	fn u32(&mut self) -> u32;
	fn u64(&mut self) -> u64;
	fn str(&mut self) -> String;
	fn size(&mut self) -> usize;
	fn skip(&mut self, n: usize);
	fn next(&mut self) -> Option<u8>;
	fn u8s<const N: usize>(&mut self) -> [u8; N];
	fn check(&mut self, expected_value: u8) -> &mut Self;
	fn head<T: Readable<T> + Eq>(&mut self, expected_value: T) -> &mut Self;
	fn vec_v1<T: Readable<T>, V, F: Fn(T) -> V>(&mut self, f: F) -> Vec<V>;
	fn vec<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> Vec<T>;
	fn vec2<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> (Vec<T>, Vec<T>);
	fn elif<T, F: Fn(&mut Self) -> T>(&mut self, els: F, then: F) -> T;
	fn vec_v3<S: Readable<S>, T: From<S>>(&mut self) -> Vec<T>;
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

	fn str(&mut self) -> String {
		String::from_utf8(self.vec(|r| r.u8())).unwrap()
	}

	fn size(&mut self) -> usize {
		self.u32() as usize
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

	fn u8s<const N: usize>(&mut self) -> [u8; N] {
		let mut buffer = [0u8; N];
		self.read_exact(&mut buffer).unwrap();
		buffer
	}

	fn check(&mut self, expected_value: u8) -> &mut Self {
		if self.next() != Some(expected_value) {
			todo!();
		}
		self
	}

	fn head<T: Readable<T> + Eq>(&mut self, header: T) -> &mut Self {
		if T::read_from(self) != header {
			todo!();
		}
		self
	}

	fn vec_v1<T: Readable<T>, V, F: Fn(T) -> V>(&mut self, f: F) -> Vec<V> {
		(0..self.size()).map(|_| f(T::read_from(self))).collect()
	}

	fn vec<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> Vec<T> {
		(0..self.size()).map(|_| f(self)).collect()
	}

	fn vec2<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> (Vec<T>, Vec<T>) {
		((0..self.size()).map(|_| f(self)).collect(), (0..self.size()).map(|_| f(self)).collect())
	}

	fn elif<T, F: Fn(&mut Self) -> T>(&mut self, els: F, then: F) -> T {
		if self.u8() != 0 {
			then(self)
		} else {
			els(self)
		}
	}

	fn vec_v3<S: Readable<S>, T: From<S>>(&mut self) -> Vec<T> {
		(0..self.size()).map(|_| T::from(S::read_from(self))).collect()
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
