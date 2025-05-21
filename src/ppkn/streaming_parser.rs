#![allow(dead_code, unused_mut)]
use std::borrow::Cow;

use super::mir::*;
use super::common::*;
use super::streaming_lexer::*;

const MULTILEVEL_ERROR_REPORTING: bool = true;

#[derive(Debug, PartialEq, Eq)]
struct Parser<'p, 'l, L: Iterator<Item = &'l str>> {
    pub program: &'p mut Program,
    pub module_name: Str,
    pub module: Module,
    module_id: usize,
    // current_line: usize,
    tokens: Lexer<'l, L>,
}

impl<'p, 'l, L: Iterator<Item = &'l str>> Parser<'p, 'l, L> {
    pub fn parse(program: &'p mut Program, module_name: Str, lines: L) -> Module {
        let lexer = Lexer::new(lines);
        let module = Module::default();
        let id = program.modules.get_index_of(&module_name).unwrap_or(program.modules.len());
        let mut parser = Self {
            tokens: lexer,
            module_name,
            program,
            module,
            module_id: id,
            // current_line: 0,
        };
        parser.parse_module();
        parser.module
    }

    fn parse_module(&mut self) {
        use TokenKind::*;
        loop {
            // self.current_line += self.tokens.reset_line_position();
            let token = self.tokens.peek();
            match token.kind {
                Eof => break,
                Linend | Comment => self.tokens.skip(),
                Pub | Use | Let | Fun | Import => self.parse_module_item(),
                _ => {
                    self.error(token.span(), "expected import or definition");
                    self.tokens.skip();
                }
            }
        }
    }

    fn parse_module_item(&mut self) {
        // let module = self.module_name.clone();
        let module = self.module_id;
        use TokenKind::*;
        let (public, keyword_span, token) = match self.tokens.next() {
            pub_word @ Token { kind: Pub, .. } => {
                let start = pub_word.span().start;
                let token = self.tokens.next();
                let span = Span::new(start, token.span().end);
                (true, span, token)
            }
            token => (false, token.span(), token),
        };
        let (name, kind) = match token.kind {
            Use => {
                let (_path_span, path) = self.parse_path();
                let linend = self.tokens.next();
                if linend.kind != Linend {
                    self.error(linend.span(), "expected end of line");
                    while self.tokens.next().kind != Linend {}
                }
                let name = Str::from(&path[path.rfind(':').unwrap_or(0)..]);
                self.parse_end_of_line();
                (name, ItemKind::Dependency(path.into()))
            }
            Let => {
                let (span, name) = self.parse_identifier();
                let colon = self.tokens.peek();
                let typ = if matches!(colon.kind, Colon | ColonSpace) {
                    self.tokens.skip();
                    let (span, typ) = self.parse_type();
                    Spanned { span, value: typ }
                } else {
                    self.error(colon.span(), "expected colon and type");
                    Spanned { span, value: Type::Builtin(BuiltinType::Bool) }
                };
                self.parse_token(Equal);
                let name = Str::from(name);
                let value = self.parse_body(typ.value.clone());
                let global = Global { name: name.clone(), module, value };
                let global = self.program.globals.insert(global);
                (name, ItemKind::Global(global))
            }
            Fun => {
                let (_, name) = self.parse_identifier();
                let name = Str::from(name);
                let (params, result) = self.parse_signature();
                let body = self.parse_body(result);
                let function =
                    // Function { name: name.clone(), module: self.module_name.clone(), params, body };
                    Function { name: name.clone(), module, params, body };
                let function = self.program.functions.insert(function);
                (name, ItemKind::Function(function))
            }
            Import => {
                let (_, name) = self.parse_identifier();
                self.parse_token(Colon);
                let (_, namespace) = self.parse_identifier();
                let (params, result) = self.parse_signature();
                self.parse_end_of_line();
                let name = Str::from(name);
                let namespace = Str::from(namespace);
                let import =
                    FunctionImport { name: name.clone(), module, namespace, params, result };
                let import = self.program.imports.insert(import);
                (name, ItemKind::Import(import))
            }
            _ => {
                return self.error(token.span(), "expected let/use/fun/import");
            }
        };
        let item = Item { public, keyword_span, kind };
        self.module.items.insert(name, item);
    }

    fn parse_body(&mut self, typ: Type) -> Body {
        // let value = if typ == Type::Unit {
        //     self.parse_stmt()
        // } else {
        //     self.parse_expression()
        // };
        let value = self.parse_expression();
        let types = Types::empty();
        Body { value, types, typ }
    }

    fn parse_block(&mut self) -> (Span, Block) {
        while self.tokens.peek().kind == Linend {
            self.tokens.skip();
        }
        use TokenKind::*;
        let token = self.tokens.peek();
        let mut span = token.span();
        if token.kind != Indent {
            self.error(span, "expected block of code");
            return (span, Block { stmts: Vec::new(), result: Expr::noop(span) });
        } else {
            self.tokens.skip();
            self.tokens.skip_while(|tok| tok.kind == Linend);
        }
        let mut stmts: Vec<Stmt> = vec![];
        while self.tokens.peek().kind != Dedent {
            let start = self.tokens.peek().span().start;
            let target = match self.tokens.peek().kind {
                Let | Mut => {
                    let mutable = self.tokens.next().kind == Mut;
                    let (_, name) = self.parse_identifier();
                    let typ = if matches!(self.tokens.peek().kind, Colon | ColonSpace) {
                        self.tokens.skip();
                        let (span, typ) = self.parse_type();
                        Some(Spanned { span, value: typ })
                    } else {
                        None
                    };
                    self.parse_token(Equal);
                    Some(VarDef { name: name.into(), mutable, typ })
                }
                _ => None,
            };
            let value = self.parse_expression();
            let span = Span::new(start, value.span.end);
            let stmt = Stmt { target, value, span };
            self.parse_end_of_line();
            stmts.push(stmt);
            self.tokens.skip_while(|tok| tok.kind == Linend);
        }
        let result = if stmts.last().is_some_and(|x| x.target.is_none()) {
            stmts.pop().unwrap().value
        } else {
            Expr::noop(stmts.last().unwrap().span)
        };
        (span, Block { stmts, result })
    }

    fn parse_expression(&mut self) -> Expr {
        use TokenKind::*;
        let peek = self.tokens.peek();
        match peek.kind {
            If => todo!(),
            While => todo!(),
            Return => todo!(),
            // Linend => {
            // 	if self.peek_long().kind == Indent {
            // 		self.parse_block()
            // 	}
            // }
            _ => self.parse_arithmetic(),
        }
    }

    fn parse_arithmetic(&mut self) -> Expr {
        use TokenKind::*;
        fn precedence(operator: Token) -> u32 {
            match operator.kind {
                Dot => 110,
                // LeftParen => 100,
                Star | Slash | Rem => 90,
                Plus | Minus => 80,
                // LeftShift | RightShift => 70,
                // BinAnd => 60,
                // BinOr => 50,
                EqualEqual | BangEqual | Less | Greater | LessEqual | GreaterEqual => 40,
                And => 30,
                Or => 20,
                Equal => 10,
                _ => 0,
            }
        }
        let mut args: Vec<Expr> = vec![self.parse_unary()];
        let mut ops: Vec<Token> = vec![];
        let mut next_op = self.peek_long();
        while precedence(next_op) > 0 {
            ops.push(next_op);
            while self.tokens.next().kind == TokenKind::Linend {}
            args.push(self.parse_unary());
            next_op = self.peek_long();
            while ops.last().is_some_and(|op| precedence(*op) >= precedence(next_op)) {
                let op = ops.pop().unwrap();
                let y = args.pop().unwrap();
                let x = args.pop().unwrap();
                let span = Span::new(x.span.start, y.span.end);
                let op_name = match op.kind {
                    Dot => todo!(),
                    // LeftParen => todo!(),
                    Star => "mul",
                    Slash => "div",
                    Rem => "rem",
                    Plus => "add",
                    Minus => "sub",
                    BangEqual => "ne",
                    EqualEqual => "eq",
                    Greater => "gt",
                    GreaterEqual => "ge",
                    Less => "lt",
                    LessEqual => "le",
                    And => todo!(),
                    Or => todo!(),
                    _ => unreachable!(),
                };
                let op_result = Expr {
                    span,
                    typ: VOID,
                    kind: InstrKind::Call(Box::new(FnCall {
                        has_receiver: true,
                        fname_span: op.span(),
                        func: op_name.into(),
                        args: vec![x, y],
                        hndl: None,
                    })),
                };
                args.push(op_result);
            }
        }
        debug_assert!(ops.len() == 0);
        debug_assert!(args.len() == 1);
        args.pop().unwrap()
    }

    fn parse_unary(&mut self) -> Expr {
        use InstrKind::*;
        let start = self.tokens.peek();
        let mut end = start;
        let kind = match start.kind {
            boolean @ (TokenKind::True | TokenKind::False) => {
                self.tokens.skip();
                Boolean(boolean == TokenKind::True)
            }
            TokenKind::Integer => {
                let digits = Str::from(self.tokens.literal());
                self.tokens.skip();
                Integer(digits)
            }
            TokenKind::Identifier => {
                let name = Str::from(self.tokens.literal());
                self.tokens.skip();
                let next_token = self.tokens.peek();
                if next_token.kind == TokenKind::LeftParen
                    && end.span().end == next_token.span().start
                {
                    self.tokens.skip();
                    let mut arguments = vec![self.parse_expression()];
                    while self.tokens.peek().kind == TokenKind::Comma {
                        self.tokens.skip();
                        arguments.push(self.parse_expression());
                    }
                    end = self.tokens.peek();
                    self.parse_token(TokenKind::RightParen);
                    Call(Box::new(FnCall {
                        has_receiver: false,
                        fname_span: start.span(),
                        func: name,
                        args: arguments,
                        hndl: None,
                    }))
                } else {
                    Identifier(name, None)
                }
            }
            TokenKind::String => {
                let string = Str::from(self.tokens.literal());
                self.tokens.skip();
                String(string)
            }
            _ => {
                let span = start.span();
                self.error(span, "expected expression");
                return Expr { span, typ: VOID, kind: InstrKind::Unreachable };
            }
        };
        let span = Span::new(start.span().start, end.span().end);
        Expr { typ: VOID, span, kind }
    }

    fn parse_signature(&mut self) -> (Vec<(Str, Type)>, Type) {
        use TokenKind::*;
        let left_parenthesis = self.tokens.next();
        if left_parenthesis.kind != LeftParen {
            self.error(left_parenthesis.span(), "expected function signature");
            return (Vec::new(), Type::Tuple(Vec::new()));
        }
        let mut parameters = Vec::new();
        while self.tokens.peek().kind == Identifier {
            let name = if self.tokens.peek_nth(1).kind == ColonSpace {
                let (_, name) = self.parse_identifier();
                self.tokens.next();
                name
            } else {
                ""
            };
            let (_, typ) = self.parse_type();
            parameters.push((name.into(), typ));
            // self.tokens.skip_if(|token| token.kind == Comma);
            if self.tokens.peek().kind == Comma {
                self.tokens.skip();
            } else {
                break;
            }
        }
        self.parse_token(RightParen);
        let result = if self.tokens.peek().kind == RightArrow {
            self.tokens.skip();
            let (_, typ) = self.parse_type();
            typ
        } else {
            Type::Tuple(Vec::new())
        };
        (parameters, result)
    }

    fn parse_type(&mut self) -> (Span, Type) {
        use TokenKind::*;
        let token = self.tokens.peek();
        match token.kind {
            LeftParen => {
                let mut fields = Vec::new();
                while matches!(self.tokens.peek().kind, Identifier | LeftBracket | LeftParen) {
                    let (_, typ) = self.parse_type();
                    fields.push(typ);
                    if self.tokens.peek().kind == Comma {
                        self.tokens.skip();
                    }
                }
                let right_paren = self.parse_token(RightParen);
                let span = Span::new(token.span().start, right_paren.end);
                (span, Type::Tuple(fields))
            }
            LeftBracket => {
                self.tokens.next();
                let (_, typ) = self.parse_type();
                let right_bracket = self.parse_token(RightBracket);
                let span = Span::new(token.span().start, right_bracket.end);
                (span, Type::Array(typ.into()))
            }
            Identifier => {
                let (span, name) = self.parse_path();
                use BuiltinType::*;
                let typ = Type::Builtin(match name {
                    "bool" => Bool,
                    "char" => Char,
                    "str" => Str,
                    "int8" => Int8,
                    "int16" => Int16,
                    "int32" => Int32,
                    "int64" => Int64,
                    "uint8" => Uint8,
                    "uint16" => Uint16,
                    "uint32" => Uint32,
                    "uint64" => Uint64,
                    "float32" => Float32,
                    "float64" => Float64,
                    "void" => Void,
                    _ => todo!(),
                });
                (span, typ)
            }
            _ => {
                self.error(token.span(), "expected type");
                (token.span(), Type::Tuple(Vec::new()))
            }
        }
    }

    fn parse_path(&mut self) -> (Span, &'l str) {
        let (span, path_start) = self.parse_identifier();
        if self.tokens.peek().kind != TokenKind::Colon {
            return (span, path_start);
        }
        let mut path_end = path_start;
        let start = span.start;
        let mut end = span.end;
        while self.tokens.peek().kind == TokenKind::Colon {
            self.tokens.next();
            let (span, name) = self.parse_identifier();
            path_end = name;
            end = span.end;
        }
        let path = unsafe { str_from_borders(path_start, path_end) };
        (Span::new(start, end), path)
    }

    fn parse_end_of_line(&mut self) {
        self.tokens.skip_if(|tok| tok.kind == TokenKind::Comment);
        let token = self.tokens.peek();
        if token.kind == TokenKind::Linend {
            self.tokens.skip();
        } else {
            self.error(token.span(), "expected end of line");
            while self.tokens.next().kind != TokenKind::Linend {}
        }
    }

    fn parse_identifier(&mut self) -> (Span, &'l str) {
        let token = self.tokens.peek();
        let name = if token.kind == TokenKind::Identifier {
            let name = self.tokens.literal();
            self.tokens.next();
            name
        } else {
            self.error(token.span(), "expected identifier");
            ""
        };
        (token.span(), name)
    }

    fn parse_token(&mut self, expected: TokenKind) -> Span {
        let token = self.tokens.peek();
        if token.kind != expected {
            self.error(token.span(), "invalid syntax");
        } else {
            self.tokens.skip();
        }
        token.span()
    }

    fn peek_long(&mut self) -> Token {
        let mut i = 0;
        loop {
            let token = self.tokens.peek_nth(i);
            if token.kind != TokenKind::Linend {
                return token;
            } else {
                i += 1;
            }
        }
    }

    fn skip_line(&mut self) {
        use TokenKind::*;
        while !matches!(self.tokens.next().kind, Linend | Eof) {}
    }

    fn error(&mut self, span: Span, message: impl Into<Cow<'static, str>>) {
        if self.module.errors.last().is_none_or(|error| error.cause != span)
            || MULTILEVEL_ERROR_REPORTING
        {
            let error = Error { cause: span, message: message.into(), kind: ErrorKind::Syntax };
            self.module.errors.push(error);
        }
    }
}

pub fn main() {
    let mut _program = Program::default();
    let paresr = Parser::parse(&mut _program, "".into(), "".lines());
    _ = paresr;
}
