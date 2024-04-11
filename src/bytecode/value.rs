use std::convert::{From, Into};
use std::fmt::{Debug, Display};
use std::mem::transmute;

#[derive(Clone, Copy, Debug)]
pub struct Value(u64);

impl Value {
	pub const NONE: Value = Value(0);
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.0 as i64)
	}
}

impl From<i32> for Value {
	fn from(value: i32) -> Self {
		unsafe { Value(transmute(value as i64)) }
	}
}

impl From<i64> for Value {
	fn from(value: i64) -> Self {
		unsafe { Value(transmute(value)) }
	}
}

impl From<f32> for Value {
	fn from(value: f32) -> Self {
		unsafe { Value::from(transmute::<f32, i32>(value)) }
	}
}

impl From<f64> for Value {
	fn from(value: f64) -> Self {
		unsafe { Value(transmute(value)) }
	}
}

impl Into<i32> for Value {
	fn into(self) -> i32 {
		self.0 as i32
	}
}

impl Into<i64> for Value {
	fn into(self) -> i64 {
		self.0 as i64
	}
}

impl Into<f32> for Value {
	fn into(self) -> f32 {
		unsafe { transmute(self.0 as u32) }
	}
}

impl Into<f64> for Value {
	fn into(self) -> f64 {
		unsafe { transmute(self.0) }
	}
}
