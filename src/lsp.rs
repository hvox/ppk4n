// #![allow(unused)]
use super::ppkn::hir::Program;
use super::ppkn::hir::InstrKind;
use std::collections::VecDeque;
use std::fs::OpenOptions;
use std::io::{stdin, stdout, Read, Write};
use std::rc::Rc;
use std::time::Instant;
use std::usize;
use json::{array, object, parse, JsonValue};

thread_local! {
    static START_TIME: Instant = Instant::now();
}

pub fn main() {
    std::fs::write("lsp.log", "").unwrap();
    let default_panic = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        log(format!("{}", info));
        default_panic(info);
    }));
    log("LSP START");
    let mut big_buffer = Vec::new();
    let mut state = State::default();
    loop {
        let mut buffer = [0; 4096];
        let size = stdin().lock().read(&mut buffer).unwrap();
        big_buffer.extend(&buffer[0..size]);
        let Some(header_end) = find(&big_buffer[..], b"\r\n\r\n") else { continue };
        let message_size: usize =
            String::from_utf8_lossy(&big_buffer[16..header_end]).parse().unwrap();
        if header_end + 4 + message_size > big_buffer.len() {
            continue;
        }
        let suffix = big_buffer.split_off(header_end + 4 + message_size);
        let message = String::from_utf8(big_buffer.split_off(header_end + 4)).unwrap();
        big_buffer = suffix;

        let message = read_message(message);
        // log(format!("Got {:?}", message));
        match message {
            Message::Request { id, method, params } => {
                log(format!("Got request \"{}\"", method));
                state.process_request(id, method, params);
            }
            Message::Response { id: _, result } => {
                log(format!("Got response {:?}", result));
            }
            Message::Notification { method, params } => {
                log(format!("Got notification \"{}\"", method));
                if method == "exit" {
                    log("LSP STOP");
                    break;
                }
                state.process_notification(method, params);
            }
        }
        while let Some(message) = state.queue.pop_back() {
            println!("{}", encode_message(message));
        }
        _ = stdout().flush();
    }
}

#[derive(Default)]
struct State {
    project: Program,
    queue: VecDeque<Message>,
}

impl State {
    fn process_request(&mut self, id: JsonValue, method: String, mut params: JsonValue) {
        match method.as_ref() {
            "initialize" => {
                // TODO check if field "clientInfo" exists
                log("Connected to ".to_string() + &params["clientInfo"].to_string());
                // log(params["capabilities"].stringify().unwrap());
                let result = object! {
                    "capabilities": object! {
                        "positionEncoding": "utf-8",
                        "textDocumentSync": 1,
                        "completionProvider": object! {"completionItem": object! {"labelDetailsSupport": true}},
                        "hoverProvider": true,
                    },
                    "serverInfo": object! {
                        "name": "ppkn-lsp",
                        "version": "0.0.1-dev",
                    },
                };
                self.send_response(id, result);
            }
            "textDocument/hover" => {
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(uri);
                let line = params["position"]["line"].as_usize().unwrap();
                let column = params["position"]["character"].as_usize().unwrap();
                let position = self.resolve_position(&module, line, column);
                log(format!(
                    "{}:{}: {}[{}]{}",
                    line,
                    column,
                    &self.project.sources[&module][position - 5..position],
                    &self.project.sources[&module][position..position + 1],
                    &self.project.sources[&module][position + 1..position + 6]
                ));
                let mut pos_fname: Rc<str> = "".into();
                let mut min_dist: usize = usize::MAX;
                for (fname, f) in &self.project.functions {
                    let (start, end) = f.span;
                    if f.module == module && start as usize <= position && position < end as usize {
                        pos_fname = fname.clone();
                        min_dist = 0;
                    } else if f.module == module && start as usize <= position {
                        let dist = position - end as usize;
                        if dist < min_dist {
                            pos_fname = fname.clone();
                            min_dist = dist;
                        }
                    }
                }
                let result = if !pos_fname.is_empty() {
                    use InstrKind::*;
                    let hover_info = self.project.functions[&pos_fname].at_position(
                        position,
                        |variables, instr| match &instr.kind {
                            Identifier(var) | Assignment(var, _) => {
								log(var);
                                format!("{}: {:?}", var, variables[var].1)
                            }
                            MethodCall(_, name, _) => format!("fun {}(self, ...)", name),
                            FnCall(fname, _) => format!("fun {}(...)", fname),
                            Unreachable => "Unreachable code".to_string(),
                            NoOp => "Do nothing at all".to_string(),
                            _ => "Aboba".to_string(),
                        },
                    );
                    object! { "contents": hover_info }
                } else {
                    JsonValue::Null
                };
                self.send_response(id, result);
            }
            "textDocument/completion" => {
                // log(format!("{}", params));
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(uri);
                let line = params["position"]["line"].as_usize().unwrap();
                let column = params["position"]["character"].as_usize().unwrap();
                let position = self.resolve_position(&module, line, column);
                // log(format!(
                //     "{}${}",
                //     &self.project.sources[&module][position - 5..position],
                //     &self.project.sources[&module][position..position + 5]
                // ));
                let mut completions = array![];
                for (gname, (typ, _)) in &self.project.globals {
                    let item = object! {"label": gname.to_string(), "kind": 6, "detail": format!("{:?}", typ)};
                    completions.push(item).unwrap();
                }
                let mut pos_fname: Rc<str> = "".into();
                let mut min_dist: usize = usize::MAX;
                for (fname, f) in &self.project.functions {
                    let (start, end) = f.span;
                    if f.module == module && start as usize <= position && position < end as usize {
                        pos_fname = fname.clone();
                        min_dist = 0;
                    } else if f.module == module && start as usize <= position {
                        let dist = position - end as usize;
                        if dist < min_dist {
                            pos_fname = fname.clone();
                            min_dist = dist;
                        }
                    }
                    let fname = fname.to_string();
                    let label = fname.strip_prefix(&format!("{}:", module)).unwrap_or(&fname);
                    let item = object! {"label": label, "kind": 3, "detail": "(...) -> (...)"};
                    completions.push(item).unwrap();
                }
                if !pos_fname.is_empty() {
                    for (var, typ) in self.project.functions[&pos_fname].variables(position) {
                        let item = object! {"label": var.to_string(), "kind": 6, "detail": typ.to_string()};
                        completions.push(item).unwrap();
                    }
                }
                log(format!("Send completions: {}", completions));
                self.send_response(id, completions);
            }
            "shutdown" => {
                log("We had nothing to do anyway, bye!");
                self.send_response(id, JsonValue::Null);
            }
            unknown_method => {
                log(format!("unknown request {:?}", unknown_method));
            }
        }
    }

    fn process_notification(&mut self, method: String, mut params: JsonValue) {
        match method.as_ref() {
            "initialized" => {}
            "textDocument/didOpen" => {
                let uri: String = params["textDocument"]["uri"].take_string().unwrap();
                let text: String = params["textDocument"]["text"].take_string().unwrap();
                log(format!("{}: {}", uri, text.len()));
                self.update_source(uri, text);
            }
            "textDocument/didChange" => {
                let uri: String = params["textDocument"]["uri"].take_string().unwrap();
                let text: String = params["contentChanges"][0]["text"].take_string().unwrap();
                self.update_source(uri, text);
            }
            "textDocument/didSave" => {}
            unknown_method => {
                log(format!("unknown notification {:?}", unknown_method));
            }
        }
    }

    fn update_source(&mut self, uri: String, content: String) {
        let module = self.resolve_path(uri);
        self.project.sources.insert(module.clone(), content.into());
        self.project = Program::default();
        if let Err(errors) = self.project.load_and_typecheck(module) {
            log(format!("{:?}", errors));
        }
        // log(format!("{:?}", self.project.functions.keys().collect::<Vec<_>>()))
    }

    fn resolve_path(&mut self, uri: String) -> Rc<str> {
        let path = uri.strip_prefix("file://").unwrap();
        let module = path.split("/").last().unwrap().split_once(".").unwrap().0;
        module.into()
    }

    fn resolve_position(&self, module: &str, line: usize, character: usize) -> usize {
        let mut position = 0;
        let mut current_line = 0;
        let mut current_column = 0;
        for chr in self.project.sources[module].chars() {
            position += chr.len_utf8();
            if chr == '\n' {
                current_line += 1;
                current_column = 0;
            } else {
                current_column += chr.len_utf8();
            }
            // log(format!("{}:{} =?= {}:{}", current_line, current_column, line, character));
            if (current_line, current_column) >= (line, character) {
                break;
            }
        }
        position
    }

    fn send_response(&mut self, id: JsonValue, result: JsonValue) {
        self.queue.push_back(Message::Response { id, result });
    }
}

fn read_message(message: impl AsRef<str>) -> Message {
    // TODO: less clones
    let mut message: JsonValue = parse(message.as_ref()).unwrap();
    if message.has_key("id") {
        let id = message["id"].take();
        if message.has_key("method") {
            let method = message["method"].take_string().unwrap();
            let params = message["params"].take();
            Message::Request { id, method, params }
        } else {
            let result = message["result"].take();
            Message::Response { id, result }
        }
    } else {
        let method = message["method"].take_string().unwrap();
        let params = message["params"].take();
        Message::Notification { method, params }
    }
}

fn encode_message(message: Message) -> String {
    let message = match message {
        Message::Request { id, method, params } => object! {
            "id":  id,
            "method": method,
            "params": params,
        },
        Message::Response { id, result } => object! {
            "id": id,
            "result": result,
        },
        Message::Notification { method, params } => object! {
            "method": method,
            "params": params,
        },
    };
    let message = message.dump();
    format!("Content-Length: {}\r\n\r\n{}", message.len(), message)
}

#[derive(Debug)]
enum Message {
    Request { id: JsonValue, method: String, params: JsonValue },
    Response { id: JsonValue, result: JsonValue },
    Notification { method: String, params: JsonValue },
}

fn find(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|window| window == needle)
}

fn log(message: impl AsRef<str>) {
    // TODO: figure out how to print time without chrono
    let time = START_TIME.with(|time| time.elapsed().as_micros());
    let Ok(mut file) = OpenOptions::new().append(true).open("lsp.log") else {
        return;
    };
    _ = writeln!(file, "{:>3}: {}", time, message.as_ref());
}
