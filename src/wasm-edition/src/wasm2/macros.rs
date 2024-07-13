macro_rules! check {
	($err:expr, $cond:expr) => {
		if !$cond {
			return Err($err);
		}
	};
}

pub(crate) use check;
