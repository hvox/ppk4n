// #![allow(unused)]
use super::ppkn::hir::Program;
use std::collections::{HashMap, VecDeque};
use std::fs::OpenOptions;
use std::io::{stdin, stdout, Read, Write};
use std::time::Instant;
use tinyjson::JsonValue;

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
            print!("{}", encode_message(message));
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
    fn process_request(&mut self, id: JsonValue, method: String, params: JsonValue) {
        match method.as_ref() {
            "initialize" => {
                // TODO check if field "clientInfo" exists
                log("Connected to ".to_string() + &params["clientInfo"].stringify().unwrap());
                // log(params["capabilities"].stringify().unwrap());
                let server_capabilites = JsonValue::Object(HashMap::from([
                    ("positionEncoding".into(), "utf-8".to_string().into()),
                    ("textDocumentSync".into(), 1.0f64.into()),
                ]));
                let server_info = JsonValue::Object(HashMap::from([
                    ("name".into(), JsonValue::String("ppkn-lsp".into())),
                    ("version".into(), JsonValue::String("0.0.1-dev".into())),
                ]));
                let result = JsonValue::Object(HashMap::from([
                    ("capabilities".into(), server_capabilites),
                    ("serverInfo".into(), server_info),
                ]));
                self.send_response(id, result);
            }
            "shutdown" => {
                log("We had nothing to do anyway, bye!");
                self.send_response(id, JsonValue::Null);
            }
            unknown_method => {
                log(format!("unknown method {:?}", unknown_method));
            }
        }
    }

    fn process_notification(&mut self, method: String, params: JsonValue) {
        match method.as_ref() {
            "textDocument/didOpen" => {
                let uri: String = params["textDocument"]["uri"].clone().try_into().unwrap();
                let text: String = params["textDocument"]["text"].clone().try_into().unwrap();
                log(format!("{}: {}", uri, text.len()));
                self.update_source(uri, text);
            }
            "textDocument/didChange" => {
                let uri: String = params["textDocument"]["uri"].clone().try_into().unwrap();
                let text: String = params["contentChanges"][0]["text"].clone().try_into().unwrap();
                self.update_source(uri, text);
            }
            "textDocument/didSave" => {}
            unknown_method => {
                log(format!("unknown method {:?}", unknown_method));
            }
        }
    }

    fn update_source(&mut self, uri: String, content: String) {
        let Some(path) = uri.strip_prefix("file://") else { return };
        let module = path.split("/").last().unwrap().split_once(".").unwrap().0;
        self.project.sources.insert(module.into(), content.into());
        self.project = Program::default();
        if let Err(errors) = self.project.load_and_typecheck(module.into()) {
            log(format!("{:?}", errors));
        }
        log(format!("{:?}", self.project.functions.keys().collect::<Vec<_>>()))
    }

    fn send_response(&mut self, id: JsonValue, result: JsonValue) {
        self.queue.push_back(Message::Response { id, result });
    }
}

fn read_message(message: impl AsRef<str>) -> Message {
    // TODO: less clones
    let message: JsonValue = message.as_ref().parse().unwrap();
    let message: &HashMap<String, JsonValue> = message.get().unwrap();
    if let Some(id) = message.get("id") {
        if let Some(method) = message.get("method") {
            let method: String = method.clone().try_into().unwrap();
            let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
            Message::Request { id: id.clone(), method, params: params.clone() }
        } else {
            let result = message["result"].clone();
            Message::Response { id: id.clone(), result }
        }
    } else {
        let method: String = message["method"].clone().try_into().unwrap();
        let params = message.get("params").cloned().unwrap_or(JsonValue::Null);
        Message::Notification { method, params }
    }
}

fn encode_message(message: Message) -> String {
    let message = match message {
        Message::Request { id, method, params } => HashMap::from([
            ("id".into(), id),
            ("method".into(), method.into()),
            ("params".into(), params),
        ]),
        Message::Response { id, result } => {
            HashMap::from([("id".into(), id), ("result".into(), result)])
        }
        Message::Notification { method, params } => {
            HashMap::from([("method".into(), method.into()), ("params".into(), params)])
        }
    };
    let message = JsonValue::Object(message).stringify().unwrap();
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
