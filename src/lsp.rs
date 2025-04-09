// #![allow(unused)]
use super::ppkn::hir::Program;
use super::ppkn::hir::InstrKind;
use super::ppkn::lexer::*;
use std::collections::VecDeque;
use std::fs::OpenOptions;
use std::io::{stdin, stdout, Read, Write};
use std::path::PathBuf;
use std::rc::Rc;
use std::time::Instant;
use std::usize;
use json::{array, object, parse, JsonValue};

const SPOONLESS_MODE_TEST: bool = false;

thread_local! {
    static START_TIME: Instant = Instant::now();
}

static TOKEN_TYPES: &[&str] = &[
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
];

pub fn get_log_path() -> Option<PathBuf> {
    // Documentation of `home` crate states that `home_dir()`
    // is absolutely safe starting from Rust 1.85 but still
    // has irrelevant deprecation warning.
    let log_path = ".local/state/ppkn/lsp.log";
    #[allow(deprecated)]
    std::env::home_dir().map(|path| path.join(log_path))
}

pub fn main() {
    if let Some(log_path) = get_log_path() {
        if let Some(parent) = log_path.parent() {
            println!("{:?}", parent);
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(log_path, "").unwrap();
    }
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
                log(format!("Got request \"{}\" id={}", method, id));
                state.process_request(id, method, params);
            }
            Message::Response { id, result } => {
                log(format!("Got response {} id={}", result, id));
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
    source: SpoonlessText,
}

// There is no text, Neo
#[derive(Default)]
struct SpoonlessText {
    // It should have been LinkedList, but Rust currently
    // does not have working LinkedList in its std.    :(
    groups: Vec<Vec<String>>,
    current_line: usize,
    current_group: usize,
}

impl SpoonlessText {
    fn from(text: &str) -> Self {
        let mut groups = Vec::new();
        for line in text.lines() {
            let line = line.to_string();
            if !line.is_empty() && !line.starts_with("\t") || groups.is_empty() {
                groups.push(vec![line]);
            } else {
                groups.last_mut().unwrap().push(line);
            }
        }
        Self { groups, current_line: 0, current_group: 0 }
    }

    fn to_string(&self) -> String {
        let mut text = String::new();
        for (group_idx, group) in self.groups.iter().enumerate() {
            for (line_idx, line) in group.iter().enumerate() {
                text.push_str(&format!(
                    "{}:{:<2} {}\n",
                    group_idx,
                    line_idx,
                    line.replace("\t", "    "),
                ));
            }
        }
        text
    }

    fn replace_range(&mut self, start: (usize, usize), end: (usize, usize), text: &str) {
        let (y0, x0) = start;
        let (y1, x1) = end;
        while self.group_last_line() < y0 {
            self.goto_next_group();
        }
        while self.current_line >= y1 {
            self.goto_prev_group();
        }
        let y0 = y0 - self.current_line;
        let y1 = y1 - self.current_line;
        if y0 == y1 {
            let group = &mut self.groups[self.current_group];
            if text.contains("\n") {
                let mut lines: Vec<_> = text.lines().map(|s| s.to_string()).collect();
                lines[0] = group[y0][0..x0].to_string() + &lines[0];
                lines.last_mut().unwrap().push_str(&group[y1][x1..]);
                // group.replace_range(y0..=y1, lines);
                group.splice(y0..=y1, lines);
            } else {
                group[y0].replace_range(x0..x1, text);
            }
        } else {
            for y in y0..=y1 {
                self.groups[self.current_group][y].replace_range(0..0, &"ERROR-".repeat(10));
            }
        }
    }

    fn goto_prev_group(&mut self) {
        self.current_group -= 1;
        self.current_line -= self.groups[self.current_group].len();
    }

    fn goto_next_group(&mut self) {
        self.current_line += self.groups[self.current_group].len();
        self.current_group += 1;
    }

    fn group_len(&self) -> usize {
        self.groups[self.current_group].len()
    }

    fn group_last_line(&self) -> usize {
        self.current_line + self.group_len()
    }

    fn log(&self) {
        if SPOONLESS_MODE_TEST {
            std::fs::write("spoonless.txt", &self.to_string()).unwrap();
        }
    }
}

impl State {
    fn process_request(&mut self, id: JsonValue, method: String, mut params: JsonValue) {
        match method.as_ref() {
            "initialize" => {
                // TODO check if field "clientInfo" exists
                log("Connected to ".to_string() + &params["clientInfo"].to_string());
                // log(format!("{}", params["capabilities"]));
                let mut encodings = params["capabilities"]["general"]["positionEncodings"].take();
                log(format!("Supported positon encodings: {}", encodings));
                // Encoding does not matter because Ppkn LSP currently
                // does not support non-ASCII characters...
                let encoding = if encodings.is_array() && !encodings.is_empty() {
                    encodings[0].take_string().unwrap()
                } else {
                    "utf-16".to_string()
                };
                let result = object! {
                    "capabilities": object! {
                        "positionEncoding": encoding,
                        "textDocumentSync": 2,
                        "completionProvider": object! {"completionItem": object! {"labelDetailsSupport": true}},
                        "hoverProvider": true,
                        "definitionProvider": true,
                        "renameProvider": true,
                        "semanticTokensProvider": object! {"legend": object! {
                            "tokenTypes": TOKEN_TYPES,
                            "tokenModifiers": array![],
                        }, "full": true, "range": false },
                    },
                    "serverInfo": object! {
                        "name": "ppkn-lsp",
                        "version": "0.0.1-dev",
                    },
                };
                log(format!("{}", result["capabilities"]["semanticTokensProvider"]["legend"]));
                self.send_response(id, result);
            }
            "textDocument/completion" => {
                // log(format!("{}", params));
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
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
                // log(format!("Send completions: {}", completions));
                self.send_response(id, completions);
            }
            "textDocument/hover" => {
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
                let line = params["position"]["line"].as_usize().unwrap();
                let column = params["position"]["character"].as_usize().unwrap();
                let position = self.resolve_position(&module, line, column);
                // log(format!(
                //     "{}:{}: {}[{}]{}",
                //     line,
                //     column,
                //     &self.project.sources[&module][position - 5..position],
                //     &self.project.sources[&module][position..position + 1],
                //     &self.project.sources[&module][position + 1..position + 6]
                // ));
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
            "textDocument/definition" => {
                // log(format!("{}", params));
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
                let line = params["position"]["line"].as_usize().unwrap();
                let column = params["position"]["character"].as_usize().unwrap();
                let position = self.resolve_position(&module, line, column);
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
                if pos_fname.is_empty() {
                    return self.send_response(id, JsonValue::Null);
                }
                let definition_location =
                    self.project.functions[&pos_fname].at_position(position, |variables, instr| {
                        use InstrKind::*;
                        match &instr.kind {
                            Identifier(var) | Assignment(var, _) => variables[var].0 as usize,
                            FnCall(name, _) => {
                                self.project.functions[name].body.value.span.0 as usize
                            }
                            _ => position,
                        }
                    });
                self.send_response(id, self.encode_position(&module, definition_location));
            }
            "textDocument/rename" => {
                let new_name = params["newName"].take_string().unwrap();
                // log(format!("{}", params));
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
                let line = params["position"]["line"].as_usize().unwrap();
                let column = params["position"]["character"].as_usize().unwrap();
                let position = self.resolve_position(&module, line, column);
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
                if pos_fname.is_empty() {
                    return self.send_response(id, JsonValue::Null);
                }
                let definition =
                    self.project.functions[&pos_fname].at_position(position, |variables, instr| {
                        use InstrKind::*;
                        match &instr.kind {
                            Identifier(var) | Assignment(var, _) => Some(variables[var].0),
                            _ => None,
                        }
                    });
                let Some(definition) = definition else {
                    return self.send_response(id, JsonValue::Null);
                };
                let definition = definition as usize;
                let references = self.project.functions[&pos_fname].find_references(definition);
                let mut changes = array![];
                for (start, end) in references {
                    let range = self.encode_range(&module, start as usize, end as usize);
                    // log(format!("-> {}", range));
                    changes.push(object! {"range": range, "newText": new_name.clone()}).unwrap();
                }
                // object! {"range": object! { "start": object! {"line": 2, "character": 1}, "end": object! {"line": 2, "character": 2}}, "newText": "aboba"},
                let mut result = object! { "changes": object! { } };
                result["changes"].insert(&uri, changes).unwrap();
                self.send_response(id, result);
            }
            "textDocument/semanticTokens/full" => {
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
                // TODO: This info should go from AST not from tokens
                let mut tokens_encoded = vec![];
                let mut position = 0;
                let source = &self.project.sources[&module];
                for token in tokenize_with_comments(source) {
                    use TokenKind::*;
                    let token_type = match token.kind {
                        Comment => "comment",
                        UnterminatedString | String => "string",
                        Identifier => match &source[token.position as usize
                            ..token.position as usize + token.length as usize]
                        {
                            "print" | "println" | "malloc" | "free" | "main" => "function",
                            _ => "variable",
                        },
                        Integer | Decimal => "number",
                        Use | Let | Mut | And | Class | Else | False | For | Fun | If | Or
                        | Return | Super | This | True | While => "keyword",
                        LeftBrace | RightBrace | LeftBracket | RightBracket | Comma | Dot
                        | Minus | Plus | Colon | Semicolon | Slash | Star | Rem | Bang
                        | BangEqual | Equal | EqualEqual | Greater | GreaterEqual | Less
                        | LessEqual | RightArrow | LeftParen | RightParen => "operator",
                        Eof | Indent | Dedent | Linend => continue,
                    };
                    let length = token.length as usize;
                    let column = token.position as usize
                        - position
                        - source[position..token.position as usize]
                            .char_indices()
                            .rfind(|(_, ch)| *ch == '\n')
                            .map(|(i, _)| i + 1)
                            .unwrap_or(0);
                    let line = source[position..token.position as usize]
                        .chars()
                        .filter(|ch| *ch == '\n')
                        .count();
                    position = token.position as usize;
                    // log(format!("{}:{}:{} {:?}", line, column, length, token));
                    let (token_type, _) = TOKEN_TYPES
                        .iter()
                        .enumerate()
                        .find(|(_, typ)| **typ == token_type)
                        .unwrap();
                    tokens_encoded.extend([line, column, length, token_type, 0]);
                }
                self.send_response(id, object! {"data": tokens_encoded});
            }
            "textDocument/semanticTokens/range" => {
                let uri = params["textDocument"]["uri"].take_string().unwrap();
                let module = self.resolve_path(&uri);
                log(format!("range: {}", params));
                self.send_response(id, object! { "data": [0, 0, 5, 8, 0] });
                _ = module;
            }
            "shutdown" => {
                log("We had nothing to do anyway, bye!");
                self.project = Default::default();
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
                self.source = SpoonlessText::from(&text);
                self.update_source(&uri, text);
                self.source.log();
            }
            "textDocument/didChange" => {
                let uri: String = params["textDocument"]["uri"].take_string().unwrap();
                for change in params["contentChanges"].members_mut() {
                    let text: String = change["text"].take_string().unwrap();
                    let range = change["range"].take();
                    if range.is_null() {
                        self.update_source(&uri, text);
                    } else {
                        let start = (
                            range["start"]["line"].as_usize().unwrap(),
                            range["start"]["character"].as_usize().unwrap(),
                        );
                        let end = (
                            range["end"]["line"].as_usize().unwrap(),
                            range["end"]["character"].as_usize().unwrap(),
                        );
                        self.source.replace_range(start, end, &text);
                        self.source.log();

                        let module = self.resolve_path(&uri);
                        let range = self.decode_range(&module, &range);
                        let mut source = self.project.sources[&module].to_string();
                        source.replace_range(range.0..range.1, &text);
                        self.update_source(&uri, source);
                    }
                }
            }
            "textDocument/didSave" => {}
            unknown_method => {
                log(format!("unknown notification {:?}", unknown_method));
            }
        }
    }

    fn update_source(&mut self, uri: &str, content: String) {
        let module = self.resolve_path(uri);
        let sources = std::mem::take(&mut self.project.sources);
        self.project = Program::default();
        self.project.sources = sources;
        self.project.sources.insert(module.clone(), content.into());
        if let Err(errors) = self.project.load_and_typecheck(module) {
            // log(format!("{:?}", errors));
            _ = errors;
        } else {
            // log("updated");
        }
        // log(format!("{:?}", self.project.functions.keys().collect::<Vec<_>>()))
    }

    fn resolve_path(&mut self, uri: &str) -> Rc<str> {
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

    fn encode_position(&self, module: &str, position: usize) -> JsonValue {
        let source = &self.project.sources[module][..position];
        let uri =
            format!("file://{}/{}.ppkn", self.project.root.as_os_str().to_string_lossy(), module);
        let line = source.lines().count() - 1;
        let column = source.lines().last().unwrap_or("").len();
        object! {
            "uri": uri,
            "range": object! {
                "start": { "line": line, "character": column },
                "end": { "line": line, "character": column + 1 }
            }
        }
    }

    fn encode_range(&self, module: &str, start: usize, end: usize) -> JsonValue {
        let source = &self.project.sources[module][..start];
        let line = source.lines().count() - 1;
        let column = source.lines().last().unwrap_or("").len();
        let start = object! { "line": line, "character": column };
        let source = &self.project.sources[module][..end];
        let line = source.lines().count() - 1;
        let column = source.lines().last().unwrap_or("").len();
        let end = object! { "line": line, "character": column };
        object! { "start": start, "end": end }
    }

    fn decode_range(&self, module: &str, range: &JsonValue) -> (usize, usize) {
        let start = self.decode_position(module, &range["start"]);
        let end = self.decode_position(module, &range["end"]);
        (start, end)
    }

    fn decode_position(&self, module: &str, position: &JsonValue) -> usize {
        let line = position["line"].as_usize().unwrap();
        let column = position["character"].as_usize().unwrap();
        let mut y = 0;
        let mut x = 0;
        for (i, ch) in self.project.sources[module].char_indices() {
            if (y, x) == (line, column) {
                return i;
            }
            if ch == '\n' {
                x = 0;
                y += 1;
            } else {
                x += 1;
            }
        }
        unreachable!()
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
    let Some(log_path) = get_log_path() else { return };
    // TODO: figure out how to print time without chrono
    let time = START_TIME.with(|time| time.elapsed().as_micros());
    let Ok(mut file) = OpenOptions::new().append(true).open(log_path) else { return };
    _ = writeln!(file, "{:>3}: {}", time, message.as_ref());
}
