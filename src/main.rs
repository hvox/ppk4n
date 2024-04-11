mod bytecode;
use std::collections::HashMap;

use crate::bytecode::*;
use indexmap::IndexMap;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	if args.len() != 2 {
		panic!("Are you stupid?");
	}
	use BinaryOp::*;
	use Type::*;
	let program = Mod {
		imports: IndexMap::new(),
		exports: HashMap::from([("main".to_string(), 0)]),
		functions: IndexMap::from([(
			"main".to_string(),
			Fn {
				params: vec![],
				result: vec![],
				locals: vec![],
				body: vec![Op::i32(123), Op::i32(321), Op::Binary(Add, I32)],
			},
		)]),
	};
	let result = program.run();
	println!("Program returned: {:?}", result);
}
