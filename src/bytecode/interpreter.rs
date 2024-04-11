use crate::bytecode::value::Value;
use crate::bytecode::*;

struct State<'a> {
	stack: Vec<Value>,
	code: &'a Mod,
}

fn run(program: &Mod) -> Vec<Value> {
	let main_id = program.exports["main"];
	let mut state = State::new(program);
	call_fn(&mut state, main_id);
	state.stack
}

fn call_fn(state: &mut State, fn_id: usize) {
	let f = &state.code.functions[fn_id];
	let mut locals = vec![Value::NONE; f.params.len() + f.locals.len()];
	for i in (0..f.params.len()).rev() {
		locals[i] = state.stack.pop().unwrap();
	}
	eval_block(state, &mut locals, &f.body);
}

fn eval_block(state: &mut State, locals: &mut Vec<Value>, block: &Vec<Op>) {
	for instruction in block {
		match instruction {
			Op::Drop => state.stack.truncate(state.stack.len() - 1),
			Op::Call(i) => call_fn(state, *i),
			Op::Set(i) => locals[*i] = state.stack.pop().unwrap(),
			Op::Tee(i) => locals[*i] = *state.stack.last().unwrap(),
			Op::Get(i) => state.stack.push(locals[*i]),
			Op::Const(value, _) => state.stack.push(*value),
			Op::IfElse(then, otherwise) => {
				let condition: i32 = state.stack.pop().unwrap().into();
				if condition != 0 {
					eval_block(state, locals, then);
				} else {
					eval_block(state, locals, otherwise);
				}
			}
			Op::Binary(op, typ) => match typ {
				Type::I32 => {
					let y: i32 = state.stack.pop().unwrap().into();
					let x: i32 = state.stack.pop().unwrap().into();
					let z = match op {
						BinaryOp::Add => x.wrapping_add(y),
						BinaryOp::Sub => x.wrapping_sub(y),
						BinaryOp::Mul => x.wrapping_mul(y),
					};
					state.stack.push(z.into());
				}
			},
		}
	}
}

impl<'a> State<'a> {
	fn new(program: &'a Mod) -> State<'a> {
		State { stack: vec![], code: program }
	}
}

impl Mod {
	pub fn run(self: &Mod) -> Vec<Value> {
		run(self)
	}
}
