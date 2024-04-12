#[allow(dead_code)]
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
	let program = Program {
		imports: IndexMap::new(),
		exports: HashMap::from([("main".to_string(), 0)]),
		functions: IndexMap::from([
			(
				"main".to_string(),
				Fn {
					params: vec![],
					result: vec![I32],
					locals: vec![Var { id: "x".to_string(), typ: I32 }],
					body: vec![
						Op::i32(100),
						Op::i32(1000),
						Op::Drop,
						Op::Call(1),
						Op::i32(123456789),
						Op::IfElse(
							vec![
								Op::i32(100),
								Op::i32(42),
								Op::Binary(Add, I32),
								Op::i32(100),
								Op::Binary(Sub, I32),
								Op::Tee(0),
							],
							vec![Op::i32(666)],
						),
					],
				},
			),
			(
				"f".to_string(),
				Fn {
					params: vec![],
					result: vec![I32],
					locals: vec![],
					body: vec![
						Op::i32(42),
						Op::i32(1),
						Op::Binary(Mul, I32),
						Op::Return,
						Op::i32(987654321),
					],
				},
			),
		]),
	};
	let result = program.run();
	println!("Program returned: {:?}", result);
}
