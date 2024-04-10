use crate::bytecode::*;
macro_rules! cast {
	($target: expr, $pat: path) => {{
		let $pat(a) = $target else {
			panic!("Cast to {} failed", stringify!($pat)); // #2
		};
		a
	}};
}

struct State<'a> {
	stack: Vec<Value>,
	code: &'a Mod,
}

fn run(program: &Mod) -> Vec<Value>{
	let main_id = program.exports["main"];
	// let main = &program.functions[main_id];
	let mut state = State::new(program);
	call_fn(&mut state, main_id);
    state.stack
}

fn call_fn(state: &mut State, fn_id: usize) {
	let f = &state.code.functions[fn_id];
	let mut locals: Vec<Value> = vec![Value::None; f.params.len() + f.locals.len()];
	for i in (0..locals.len()).rev() {
		locals[i] = state.stack.pop().unwrap();
	}
	let mut pc = 0;
	use Value::*;
	while pc < f.body.len() {
		match &f.body[pc] {
			Op::Set(id) => {
				locals[*id] = state.stack.pop().unwrap();
			}
			Op::Get(id) => {
				state.stack.push(locals[*id]);
			}
			Op::Tee(id) => {
				locals[*id] = state.stack[state.stack.len() - 1];
			}
			Op::Const(value) => {
				state.stack.push(I32(*value));
			}
			Op::Add => {
				let y: i32 = cast!(state.stack.pop().unwrap(), I32);
				let x: i32 = cast!(state.stack.pop().unwrap(), I32);
				state.stack.push(I32(x.wrapping_add(y)));
			}
			Op::Sub => {
				let y: i32 = cast!(state.stack.pop().unwrap(), I32);
				let x: i32 = cast!(state.stack.pop().unwrap(), I32);
				state.stack.push(I32(x.wrapping_sub(y)));
			}
			Op::Mult => {
				let y: i32 = cast!(state.stack.pop().unwrap(), I32);
				let x: i32 = cast!(state.stack.pop().unwrap(), I32);
				state.stack.push(I32(x.wrapping_mul(y)));
			}
		}
		pc += 1;
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
