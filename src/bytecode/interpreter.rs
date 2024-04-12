use crate::bytecode::value::Value;
use crate::bytecode::*;

struct State<'a> {
	stack: Vec<Value>,
	code: &'a Program,
}

fn run(program: &Program) -> Vec<Value> {
	let main_id = program.exports["main"];
	let mut state = State::new(program);
	call_fn(program, &mut state.stack, main_id);
	state.stack
}

fn call_fn(program: &Program, stack: &mut Vec<Value>, fn_id: usize) {
	let f = &program.functions[fn_id];
	let mut locals = vec![Value::NONE; f.params.len() + f.locals.len()];
	for i in (0..f.params.len()).rev() {
		locals[i] = stack.pop().unwrap();
	}
	let mut result = match eval_block(program, &mut locals, &f.body) {
		EvalResult::Return(stack) => stack,
		EvalResult::End(stack) => stack,
	};
	stack.extend(result.split_off(result.len() - f.result.len()));
}

fn eval_block(program: &Program, locals: &mut Vec<Value>, block: &Vec<Op>) -> EvalResult {
	let mut stack = vec![];
	for instruction in block {
		#[cfg(debug_assertions)] println!("{:?} \x1b[92m{:?}\x1b[0m", stack, instruction);
		match instruction {
			Op::Drop => stack.truncate(stack.len() - 1),
			Op::Return => return EvalResult::Return(stack),
			Op::Call(i) => call_fn(program, &mut stack, *i),
			Op::Set(i) => locals[*i] = stack.pop().unwrap(),
			Op::Tee(i) => locals[*i] = *stack.last().unwrap(),
			Op::Get(i) => stack.push(locals[*i]),
			Op::Const(value, _) => stack.push(*value),
			Op::IfElse(then, otherwise) => {
				let condition: i32 = stack.pop().unwrap().into();
				let block = if condition != 0 { then } else { otherwise };
				if let EvalResult::Return(result) = eval_block(program, locals, block) {
					return EvalResult::Return(result);
				}
			}
			Op::Binary(op, typ) => match typ {
				Type::I32 => {
					let y: i32 = stack.pop().unwrap().into();
					let x: i32 = stack.pop().unwrap().into();
					let z = match op {
						BinaryOp::Add => x.wrapping_add(y),
						BinaryOp::Sub => x.wrapping_sub(y),
						BinaryOp::Mul => x.wrapping_mul(y),
					};
					stack.push(z.into());
				}
			},
		}
	}
	return EvalResult::End(stack);
}

enum EvalResult {
	Return(Vec<Value>),
	End(Vec<Value>),
}

impl<'a> State<'a> {
	fn new(program: &'a Program) -> State<'a> {
		State { stack: vec![], code: program }
	}
}

impl Program {
	pub fn run(self: &Program) -> Vec<Value> {
		run(self)
	}
}
