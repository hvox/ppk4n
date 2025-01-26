use std::{fs, process::Command};

macro_rules! current_function_name {
	() => {{
		fn f() {}
		let name = std::any::type_name_of_val(&f);
		match &name[..name.len() - 3].rfind(':') {
			Some(pos) => &name[pos + 1..name.len() - 3],
			None => &name[..name.len() - 3],
		}
	}};
}

fn test_source(source_name: &str) {
	let mut ppkn = Command::new(env!("CARGO_BIN_EXE_ppkn"));
	ppkn.arg("into-python");
	ppkn.arg(format!("tests/{}.ppkn", source_name));
	let output = String::from_utf8(ppkn.output().unwrap().stdout).unwrap();
	let expected_output = fs::read_to_string(format!("tests/{}.py", source_name)).unwrap();
	assert_eq!(output, expected_output);
}

#[test]
fn functions() {
	test_source(current_function_name!());
}

#[test]
fn hello_world() {
	test_source(current_function_name!());
}

#[test]
fn shadowing() {
	test_source(current_function_name!());
}

#[test]
fn simple_arithmetic() {
	test_source(current_function_name!());
}

#[test]
fn typeless() {
	test_source(current_function_name!());
}

#[test]
fn vectors() {
	test_source(current_function_name!());
}
