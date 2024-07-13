use std::io::Result;

macro_rules! unsigned_leb128 {
	( $reader:expr, ($type:ty) ) => {{
		let mut value = 0;
		let mut shift = 0;
		while {
			let byte = $reader.u8()?;
			value |= (byte as $type & 0x7f) << shift;
			shift += 7;
			byte > 0x80
		} {}
		Ok(value)
	}};
	( $writer:expr, $value:expr ) => {{
		while {
			let byte = ($value & 0x7f) as u8;
			$writer.u8(byte | if $value > 0x7f { 0x80 } else { 0 })?;
			$value >>= 7;
			$value > 0
		} {}
		Ok(())
	}};
}
macro_rules! signed_leb128 {
	( $reader:expr, ($type:ty) ) => {{
		let mut value = 0;
		let mut shift = 0;
		let mut byte;
		while {
			byte = $reader.u8()?;
			value |= (byte as $type & 0x7f) << shift;
			shift += 7;
			byte > 0x80
		} {}
		Ok(value | if byte & 0x40 == 0x40 { -1 << shift } else { 0 })
	}};
	( $writer:expr, $value:expr ) => {{
		let mut more = true;
		while more {
			let byte = ($value & 0x7f) as u8;
			$value >>= 7;
			more = !($value == 0 && byte & 0x40 == 0 || $value == -1 && byte & 0x40 == 0x40);
			$writer.u8(byte | (more as u8) << 7)?;
		}
		Ok(())
	}};
}

pub trait Writer: std::io::Write {
	fn u8(&mut self, value: u8) -> Result<()>;
	fn i8(&mut self, value: i8) -> Result<()>;
	fn u16(&mut self, value: u16) -> Result<()>;
	fn i16(&mut self, value: i16) -> Result<()>;
	fn u32(&mut self, value: u32) -> Result<()>;
	fn i32(&mut self, value: i32) -> Result<()>;
	fn u64(&mut self, value: u64) -> Result<()>;
	fn i64(&mut self, value: i64) -> Result<()>;
	fn u128(&mut self, value: u128) -> Result<()>;
	fn i128(&mut self, value: i128) -> Result<()>;
	fn array(&mut self, array: &[u8]) -> Result<()>;
}

#[rustfmt::skip]
impl<W: std::io::Write> Writer for W {
	fn u8(&mut self, value: u8) -> Result<()> { self.write_all(&[value; 1]) }
	fn i8(&mut self, value: i8) -> Result<()> { self.write_all(&[value as u8; 1]) }
	fn u16(&mut self, mut value: u16) -> Result<()> { unsigned_leb128!(self, value) }
	fn i16(&mut self, mut value: i16) -> Result<()> { signed_leb128!(self, value) }
	fn u32(&mut self, mut value: u32) -> Result<()> { unsigned_leb128!(self, value) }
	fn i32(&mut self, mut value: i32) -> Result<()> { signed_leb128!(self, value) }
	fn u64(&mut self, mut value: u64) -> Result<()> { unsigned_leb128!(self, value) }
	fn i64(&mut self, mut value: i64) -> Result<()> { signed_leb128!(self, value) }
	fn u128(&mut self, mut value: u128) -> Result<()> { unsigned_leb128!(self, value) }
	fn i128(&mut self, mut value: i128) -> Result<()> { signed_leb128!(self, value) }
	fn array(&mut self, array: &[u8]) -> Result<()> { self.write_all(array) }
}

pub trait Reader: std::io::Read {
	fn u8(&mut self) -> Result<u8>;
	fn i8(&mut self) -> Result<i8>;
	fn u16(&mut self) -> Result<u16>;
	fn i16(&mut self) -> Result<i16>;
	fn u32(&mut self) -> Result<u32>;
	fn i32(&mut self) -> Result<i32>;
	fn u64(&mut self) -> Result<u64>;
	fn i64(&mut self) -> Result<i64>;
	fn u128(&mut self) -> Result<u128>;
	fn i128(&mut self) -> Result<i128>;
	fn array<const N: usize>(&mut self) -> Result<[u8; N]>;
}

#[rustfmt::skip]
impl<R: std::io::Read> Reader for R {
	fn u8(&mut self) -> Result<u8> { Ok(self.array::<1>()?[0]) }
	fn i8(&mut self) -> Result<i8> { Ok(self.array::<1>()?[0] as i8) }
	fn u16(&mut self) -> Result<u16> { unsigned_leb128!(self, (u16)) }
	fn i16(&mut self) -> Result<i16> { signed_leb128!(self, (i16)) }
	fn u32(&mut self) -> Result<u32> { unsigned_leb128!(self, (u32)) }
	fn i32(&mut self) -> Result<i32> { signed_leb128!(self, (i32)) }
	fn u64(&mut self) -> Result<u64> { unsigned_leb128!(self, (u64)) }
	fn i64(&mut self) -> Result<i64> { signed_leb128!(self, (i64)) }
	fn u128(&mut self) -> Result<u128> { unsigned_leb128!(self, (u128)) }
	fn i128(&mut self) -> Result<i128> { signed_leb128!(self, (i128)) }
	fn array<const N: usize>(&mut self) -> Result<[u8; N]> {
		let mut buffer = [0u8; N];
		self.read_exact(&mut buffer)?;
		Ok(buffer)
	}
}
