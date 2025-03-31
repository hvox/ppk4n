use std::collections::HashMap;
use std::process::exit;
use std::rc::Rc;

use indexmap::IndexMap;
use indexmap::IndexSet;

use super::error::SyntaxError;
use super::lexer::tokenize;
use super::lexer::Token;
use super::lexer::TokenKind;

pub fn parse(source: &str) -> (Ast, Vec<SyntaxError>) {
    Parser::new(source).parse()
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ast {
    pub location: Str,
    pub globals: IndexMap<Str, VarDef>,
    pub dependencies: Vec<Dependency>,
    pub functions: IndexMap<Str, FunDef>,
    pub imported_types: IndexSet<Str>,
    pub imported_functions: IndexMap<Str, FunSignature>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarDef {
    pub location: (u32, u32),
    pub name: Str,
    pub mutable: bool,
    pub typename: Typename,
    pub value: Option<Rc<Expr>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Dependency {
    pub location: (u32, u32),
    pub name: Str,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunDef {
    pub location: (u32, u32),
    pub name: Str,
    pub params: Vec<FunParameter>,
    pub result: Typename,
    pub body: Rc<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunSignature {
    pub location: (u32, u32),
    pub name: Str,
    pub params: Vec<FunParameter>,
    pub result: Typename,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunParameter {
    pub location: usize,
    pub name: Str,
    pub typ: Typename,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Typename {
    pub location: (u32, u32),
    pub kind: TypenameKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypenameKind {
    Tuple(Vec<Typename>),
    Array(Box<Typename>),
    Name(Str),
    Unknown,
    Void,
}

#[derive(PartialEq, Eq, Clone)]
pub struct Expr {
    pub location: (u32, u32),
    pub kind: ExprKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind {
    String(Str),
    Integer(Str),
    Identifier(Str),
    Tuple(Vec<Expr>),
    Assignment(Str, Rc<Expr>),
    Block(Indented),
    While(Rc<Expr>, Rc<Expr>),
    If(Rc<Expr>, Rc<Expr>, Rc<Expr>),
    MethodCall(Rc<Expr>, Str, Vec<Expr>),
    FnCall(Str, Vec<Expr>),
    Return(Rc<Expr>),
    Field(Rc<Expr>, Str),
    Unreachable,
    Nothing,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Indented {
    pub location: (u32, u32),
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Stmt {
    DefLocal(VarDef),
    Expression(Expr),
}

type Str = Rc<str>;

struct Parser<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    errors: Vec<SyntaxError>,
    cache: HashMap<usize, (Rc<Expr>, usize)>,
}

const DEBUG_LOGGING: bool = false;
const DIE_ON_FIRST_ERROR_REPORT: bool = false;

impl<'s> Parser<'s> {
    fn new(source: &'s str) -> Self {
        let tokens = tokenize(source);
        Self { source, tokens, errors: vec![], cache: HashMap::new() }
    }

    fn parse(mut self) -> (Ast, Vec<SyntaxError>) {
        let mut dependencies: Vec<Dependency> = vec![];
        let mut functions: IndexMap<Str, FunDef> = IndexMap::new();
        let mut globals: IndexMap<Rc<str>, VarDef> = IndexMap::new();
        let mut imported_functions = IndexMap::new();
        let mut imported_types = IndexSet::new();
        let mut position = 0;

        use TokenKind::*;
        while self.tokens[position].kind != Eof {
            match self.tokens[position].kind {
                Identifier => {
                    let (keyword, pos) = self.parse_identifier_soft(position);
                    match keyword.as_ref() {
                        "import" => {
                            let (name, pos) = self.parse_identifier_soft(pos);
                            if let Ok(pos) = self.try_parse_token(pos, Colon) {
                                let (signature, pos) = self.parse_imported_function_signature(pos);
                                let path = format!("{}:{}", name, signature.name).into();
                                imported_functions.insert(path, signature);
                                position = self.expect_end_of_line(pos);
                            } else {
                                imported_types.insert(name);
                                position = self.expect_end_of_line(pos);
                            }
                        }
                        _ => {
                            self.error(position, "Unexpected identifier");
                            position = self.skip_line(pos);
                        }
                    }
                }
                Use => {
                    if let Ok((name, pos)) = self.try_parse_identifier(position + 1) {
                        dependencies
                            .push(Dependency { location: self.tokens[position + 1].span(), name });
                        position = pos;
                    } else {
                        self.error(position + 1, "Expected module name");
                    }
                }
                Let | Mut => {
                    if let Ok((mut definiton, pos)) = self.parse_definition(position) {
                        definiton.mutable = self.tokens[position].kind == Mut;
                        globals.insert(definiton.name.clone(), definiton);
                        position = pos;
                    } else {
                        position = self.skip_line(position);
                    }
                }
                Fun => {
                    if let Ok((function, pos)) = self.parse_function(position) {
                        functions.insert(function.name.clone(), function);
                        position = pos;
                    } else {
                        position = self.skip_line(position);
                    }
                }
                Linend => position += 1,
                Indent => {
                    self.error(position, "Unexpected indent");
                    position = self.skip_current_block(position + 1);
                }
                _ => {
                    self.error(position, "Unexpected unfunny code");
                    position = self.skip_line(position + 1);
                }
            }
        }
        let ast = Ast {
            location: "main".into(),
            globals,
            dependencies,
            functions,
            imported_types,
            imported_functions,
        };
        (ast, self.errors)
    }

    fn parse_imported_function_signature(&mut self, position: usize) -> (FunSignature, usize) {
        self.log("imported function signature", position);
        use TokenKind::*;
        let (name, pos) = self.parse_identifier_soft(position);
        let mut params: Vec<FunParameter> = vec![];
        let mut param_pos = self.parse_token_soft(pos, LeftParen);
        while !matches!(
            self.tokens[param_pos].kind,
            RightParen | Linend | Dedent | Indent | RightArrow
        ) {
            let (typ, pos) = self.parse_typename(param_pos).unwrap_or_else(|_| {
                (
                    Typename {
                        location: self.tokens[param_pos].span(),
                        kind: TypenameKind::Unknown,
                    },
                    pos,
                )
            });
            params.push(FunParameter { location: param_pos, name: "".into(), typ });
            param_pos = self.try_parse_token(pos, Comma).unwrap_or(pos).max(param_pos + 1);
        }
        let pos = self.parse_token_soft(param_pos, RightParen);
        let (result, pos) = if let Ok(pos) = self.try_parse_token(pos, RightArrow) {
            self.parse_typename(pos).unwrap_or_else(|_| {
                (Typename { location: self.tokens[pos].span(), kind: TypenameKind::Void }, pos)
            })
        } else {
            (Typename { location: self.tokens[pos].span(), kind: TypenameKind::Void }, pos)
        };
        let location = self.tokens[position].span();
        let signature = FunSignature { location, name, params, result };
        (signature, pos)
    }

    fn parse_function(&mut self, position: usize) -> Result<(FunDef, usize), ()> {
        self.log("function", position);
        use TokenKind::*;
        let mut params: Vec<FunParameter> = vec![];
        let pos = self.parse_token(position, Fun)?;
        let (name, pos) = self.parse_identifier(pos)?;
        let mut param_pos = self.parse_token(pos, LeftParen)?;
        while let Ok((name, pos)) = self.try_parse_identifier(param_pos) {
            let pos = self.parse_token(pos, Colon)?;
            let (typ, pos) = self.parse_typename(pos)?;
            params.push(FunParameter { location: param_pos, name, typ });
            param_pos = self.try_parse_token(pos, Comma).unwrap_or(pos);
        }
        let pos = self.parse_token(param_pos, RightParen)?;
        let (result, pos) = if let Ok(pos) = self.try_parse_token(pos, RightArrow) {
            self.parse_typename(pos)?
        } else {
            (Typename { location: self.tokens[pos].span(), kind: TypenameKind::Void }, pos)
        };
        let pos = self.try_parse_token(pos, Linend).unwrap_or(pos);
        let (body, pos) = self.parse_expr(pos)?;
        let location = (self.tokens[position].span().0, self.tokens[pos].span().0);
        let function = FunDef { location, name, params, result, body };
        Ok((function, pos))
    }

    fn parse_expr(&mut self, position: usize) -> Result<(Rc<Expr>, usize), ()> {
        self.log("expression", position);
        use TokenKind::*;
        match self.tokens[position].kind {
            If => {
                let (condition, pos) = self.parse_expr(position + 1)?;
                let pos = self.try_parse_token(pos, Linend).unwrap_or(pos);
                let (then, pos) = self.parse_expr(pos)?;
                let (els, pos) = if let Ok(pos) = self.try_parse_token(pos, Else) {
                    self.parse_expr(pos)?
                } else {
                    let expr = Expr { location: then.location, kind: ExprKind::Nothing };
                    (expr.into(), pos)
                };
                let expr = Expr {
                    location: (self.tokens[position].span().0, self.tokens[pos - 1].span().1),
                    kind: ExprKind::If(condition, then, els),
                };
                Ok((expr.into(), pos))
            }
            While => {
                let (condition, pos) = self.parse_expr(position + 1)?;
                let pos = self.try_parse_token(pos, Linend).unwrap_or(pos);
                let (body, pos) = self.parse_expr(pos)?;
                let expr = Expr {
                    location: self.tokens[position].span(),
                    kind: ExprKind::While(condition, body),
                };
                Ok((expr.into(), pos))
            }
            Return => {
                let (result, pos) = self.parse_expr(position + 1)?;
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Return(result) };
                Ok((expr.into(), pos))
            }
            _ => self.parse_arithmetic(position),
        }
    }

    fn parse_arithmetic(&mut self, position: usize) -> Result<(Rc<Expr>, usize), ()> {
        self.log("arithmetic", position);
        fn precedence(operator: TokenKind) -> u32 {
            match operator {
                Dot => 110,
                LeftParen => 100,
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
        use TokenKind::*;
        if let Some((expr, pos)) = self.cache.get(&position) {
            return Ok((expr.clone(), *pos));
        }
        let (expr, mut pos) = self.parse_unary(position)?;
        let mut ops: Vec<Token> = vec![];
        let mut args = vec![expr];
        while args.len() != 1 || precedence(self.tokens[pos].kind) > 0 {
            if ops.last().is_some_and(|op| precedence(op.kind) >= precedence(self.tokens[pos].kind))
            {
                let op = ops.pop().unwrap();
                let y = args.pop().unwrap();
                let x = args.pop().unwrap();
                let method_name = match op.kind {
                    Dot => {
                        match &y.kind {
                            ExprKind::Identifier(name) => {
                                let location = (x.location.0, y.location.1);
                                let kind = ExprKind::Field(x, name.clone());
                                let expr = Expr { location, kind };
                                args.push(expr.into());
                                continue;
                            }
                            ExprKind::FnCall(name, fn_args) => {
                                let location = (x.location.0, y.location.1);
                                let kind = ExprKind::MethodCall(x, name.clone(), fn_args.clone());
                                let expr = Expr { location, kind };
                                args.push(expr.into());
                                continue;
                            }
                            _ => {
                                self.error_loc(op.span(), "invalid field access");
                                args.push(x);
                                continue;
                            }
                        };
                    }
                    Minus => "sub",
                    Plus => "add",
                    Slash => "div",
                    Star => "mul",
                    Rem => "rem",
                    BangEqual => "ne",
                    EqualEqual => "eq",
                    Greater => "gt",
                    GreaterEqual => "ge",
                    Less => "lt",
                    LessEqual => "le",
                    And => todo!(),
                    Or => todo!(),
                    Equal => {
                        let name = match &x.kind {
                            ExprKind::Identifier(name) => name,
                            _ => todo!(),
                        };
                        let kind = ExprKind::Assignment(name.clone(), y);
                        let z = Expr { location: x.location, kind };
                        args.push(Rc::new(z));
                        continue;
                    }
                    LeftParen => {
                        let kind = match &x.kind {
                            ExprKind::Identifier(name) => {
                                ExprKind::FnCall(name.clone(), vec![(*y).clone()])
                            }
                            _ => todo!(),
                        };
                        let z = Expr { location: x.location, kind };
                        args.push(Rc::new(z));
                        continue;
                    }
                    _ => unreachable!("{:?}", op),
                };
                let z = Expr {
                    location: op.span(),
                    kind: ExprKind::MethodCall(x, method_name.into(), vec![(*y).clone()]),
                };
                args.push(Rc::new(z));
            } else {
                ops.push(self.tokens[pos]);
                let expr;
                (expr, pos) = if self.tokens[pos].kind == LeftParen {
                    let (expr, pos) = self.parse_expr(pos + 1)?;
                    let pos = self.parse_token(pos, RightParen)?;
                    (expr, pos)
                } else {
                    self.parse_unary(pos + 1)?
                };
                args.push(expr);
            }
        }
        let expr = args.pop().unwrap();
        self.cache.insert(position, (expr.clone(), pos));
        Ok((expr, pos))
    }

    fn parse_unary(&mut self, position: usize) -> Result<(Rc<Expr>, usize), ()> {
        self.log("unary", position);
        use TokenKind::*;
        match self.tokens[position].kind {
            LeftParen => {
                if self.tokens[position + 1].kind == RightParen {
                    let expr = Expr {
                        location: (self.tokens[position].span().0, self.tokens[position].span().1),
                        kind: ExprKind::Nothing,
                    };
                    return Ok((expr.into(), position + 2));
                }
                let (expr, pos) = self.parse_expr(position + 1)?;
                if let Ok(pos) = self.try_parse_token(pos, RightParen) {
                    return Ok((expr, pos));
                }
                let mut fields = vec![(*expr).clone()];
                let mut field_pos = self.try_parse_token(pos, Comma).unwrap_or(pos);
                while !matches!(self.tokens[field_pos].kind, RightParen | Linend) {
                    let (expr, pos) = self.parse_expr(field_pos)?;
                    fields.push((*expr).clone());
                    field_pos = self.try_parse_token(pos, Comma).unwrap_or(pos);
                }
                let pos = self.parse_token(field_pos, RightParen).unwrap_or(field_pos);
                let expr = Expr {
                    location: (self.tokens[position].span().0, self.tokens[pos].span().1),
                    kind: ExprKind::Tuple(fields),
                };
                Ok((expr.into(), pos))
            }
            LeftBrace => todo!(),   // Map
            LeftBracket => todo!(), // Array
            Minus => {
                let (expr, pos) = self.parse_unary(position + 1)?;
                let expr = Expr {
                    location: self.tokens[position].span(),
                    kind: ExprKind::MethodCall(expr, "neg".into(), vec![]),
                };
                Ok((expr.into(), pos))
            }
            Plus => {
                let (expr, pos) = self.parse_unary(position + 1)?;
                Ok((expr, pos))
            }
            Identifier => {
                let (mut name, mut pos) = self.parse_identifier(position)?;
                let mut location = self.tokens[position].span();
                while self.tokens[pos].kind == Colon {
                    let suffix;
                    (suffix, pos) = self.parse_identifier_soft(pos + 1);
                    location.1 = self.tokens[pos - 1].span().1;
                    name = format!("{}:{}", name, suffix).into();
                }
                if self.tokens[pos].kind == LeftParen {
                    let (args, pos) = self.parse_arguments(pos);
                    let expr = Expr { location, kind: ExprKind::FnCall(name, args) };
                    return Ok((expr.into(), pos));
                }
                let expr = Expr { location, kind: ExprKind::Identifier(name) };
                Ok((expr.into(), pos))
            }
            Integer => {
                let (value, pos) = self.parse_integer(position);
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Integer(value) };
                Ok((expr.into(), pos))
            }
            String => {
                let value = self.parse_string(position);
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::String(value) };
                Ok((expr.into(), position + 1))
            }
            Decimal => {
                let (value, pos) = self.parse_identifier(position)?;
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Integer(value) };
                Ok((expr.into(), pos))
            }
            UnterminatedString => {
                let value = self.parse_string(position);
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::String(value) };
                Ok((expr.into(), position + 1))
            }
            Indent => {
                let (block, pos) = self.parse_block(position + 1);
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Block(block) };
                Ok((expr.into(), pos))
            }
            Linend | Dedent => {
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable };
                self.error(position, "Expected expression");
                Ok((expr.into(), position))
            }
            _ => {
                let expr =
                    Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable };
                self.error(position, "Expected expression");
                Ok((expr.into(), position + 1))
            }
        }
    }

    fn parse_typename(&mut self, position: usize) -> Result<(Typename, usize), ()> {
        self.log("typename", position);
        use TokenKind::*;
        match self.tokens[position].kind {
            Identifier => {
                let (name, pos) = self.parse_identifier(position)?;
                let typ = Typename {
                    location: self.tokens[position].span(),
                    kind: TypenameKind::Name(name),
                };
                Ok((typ, pos))
            }
            LeftParen => {
                if self.tokens[position + 1].kind == RightParen {
                    let typ = Typename {
                        location: (
                            self.tokens[position].span().0,
                            self.tokens[position + 1].span().1,
                        ),
                        kind: TypenameKind::Void,
                    };
                    return Ok((typ, position + 2));
                }
                let mut fields: Vec<Typename> = vec![];
                let mut variable_pos = position + 1;
                while self.tokens[variable_pos].kind != RightParen {
                    let (field, pos) = self.parse_typename(variable_pos)?;
                    fields.push(field);
                    variable_pos = self.try_parse_token(pos, Comma).unwrap_or(pos);
                }
                let end_position = variable_pos + 1;
                let typ = Typename {
                    location: (self.tokens[position].span().0, self.tokens[variable_pos].span().1),
                    kind: TypenameKind::Tuple(fields),
                };
                Ok((typ, end_position))
            }
            LeftBracket => {
                let (typ, pos) = self.parse_typename(position + 1)?;
                let pos = self.parse_token(pos, RightBracket)?;
                let typ = Typename {
                    location: (self.tokens[position].span().0, self.tokens[pos - 1].span().1),
                    kind: TypenameKind::Array(typ.into()),
                };
                Ok((typ, pos))
            }
            _ => {
                self.error(position, "Expected typename");
                Err(())
            }
        }
    }

    fn parse_block(&mut self, mut position: usize) -> (Indented, usize) {
        self.log("block", position);
        let mut stmts = Vec::new();
        let mut location = self.tokens[position].span();
        while self.tokens[position].kind != TokenKind::Dedent {
            let (stmt, pos) = self.parse_stmt(position);
            location = (self.tokens[position].span().0, self.tokens[pos].span().1);
            position = pos;
            stmts.push(stmt);
            // if !matches!(self.tokens[pos].kind, Indent | Linend | Dedent) {
            // 	self.error(position, "expected end of line");
            // 	position = self.skip_line(position);
            // }
            while self.tokens[position].kind == TokenKind::Linend {
                position += 1;
            }
        }
        self.log("block end", position);
        (Indented { stmts, location }, position + 1)
    }

    fn parse_definition(&mut self, position: usize) -> Result<(VarDef, usize), ()> {
        self.log("variable definition", position);
        use TokenKind::*;
        if !matches!(self.tokens[position].kind, Mut | Let) {
            self.error(position, "expected 'let' or 'mut' keyword");
            return Err(());
        }
        let mutable = self.tokens[position].kind == Mut;
        let (name, pos) = self.parse_identifier(position + 1)?;
        let (typename, pos) = if self.tokens[pos].kind == Colon {
            self.parse_typename(pos + 1)?
        } else {
            (
                Typename {
                    location: self.tokens[position + 1].span(),
                    kind: TypenameKind::Unknown,
                },
                pos,
            )
        };
        let (value, pos) = if self.tokens[pos].kind == Equal {
            let (expr, pos) = self.parse_expr(pos + 1)?;
            (Some(expr), pos)
        } else {
            (None, pos)
        };
        let definition =
            VarDef { location: self.tokens[position].span(), name, mutable, typename, value };
        Ok((definition, pos))
    }

    fn parse_stmt(&mut self, position: usize) -> (Stmt, usize) {
        self.log("statement", position);
        use TokenKind::*;
        if matches!(self.tokens[position].kind, Let | Mut) {
            let id_pos = position + 1;
            let Ok((name, pos)) = self.parse_identifier(id_pos) else {
                return (
                    Stmt::Expression(Expr {
                        location: self.tokens[id_pos].span(),
                        kind: ExprKind::Unreachable,
                    }),
                    self.skip_line(id_pos),
                );
            };
            let (typename, pos) = if self.tokens[pos].kind == Colon {
                self.parse_typename(pos + 1).unwrap()
            } else {
                (
                    Typename { location: self.tokens[id_pos].span(), kind: TypenameKind::Unknown },
                    pos,
                )
            };
            if self.tokens[pos].kind != Equal {
                self.error(pos, "Expected `=`");
                return (
                    Stmt::Expression(Expr {
                        location: self.tokens[id_pos].span(),
                        kind: ExprKind::Unreachable,
                    }),
                    self.skip_line(pos),
                );
            }
            let (expr, pos) = self.parse_expr(pos + 1).unwrap_or((
                Expr { location: self.tokens[pos].span(), kind: ExprKind::Unreachable }.into(),
                pos + 1,
            ));
            let definition = VarDef {
                name,
                location: self.tokens[id_pos].span(),
                mutable: self.tokens[position].kind == Mut,
                typename,
                value: Some(expr),
            };
            return (Stmt::DefLocal(definition), pos);
        }
        let (expr, pos) = self.parse_expr(position).unwrap_or((
            Expr { location: self.tokens[position].span(), kind: ExprKind::Unreachable }.into(),
            position,
        ));
        (Stmt::Expression((*expr).clone()), pos)
    }

    fn parse_arguments(&mut self, position: usize) -> (Vec<Expr>, usize) {
        self.log("function arguments", position);
        use TokenKind::*;
        if self.tokens[position].kind != LeftParen {
            self.error(position, "expected '('");
            return (vec![], position + (self.tokens[position].kind != Eof) as usize);
        }
        let mut args = vec![];
        let mut position = position + 1;
        while !matches!(self.tokens[position].kind, RightParen | Linend) {
            let Ok((argument, pos)) = self.parse_expr(position) else {
                break;
            };
            args.push((*argument).clone());
            position = self.try_parse_token(pos, Comma).unwrap_or(pos);
        }
        if self.tokens[position].kind == RightParen {
            (args, position + 1)
        } else {
            self.error(position, "expected ')'");
            (args, position)
        }
    }

    fn try_parse_token(&mut self, position: usize, token: TokenKind) -> Result<usize, ()> {
        if self.tokens[position].kind == token {
            Ok(position + 1)
        } else {
            Err(())
        }
    }

    fn parse_token(&mut self, position: usize, token: TokenKind) -> Result<usize, ()> {
        if self.tokens[position].kind == token {
            Ok(position + 1)
        } else {
            self.error(position, "Invalid syntax");
            Err(())
        }
    }

    fn parse_token_soft(&mut self, position: usize, token: TokenKind) -> usize {
        use TokenKind::*;
        if self.tokens[position].kind == token {
            position + 1
        } else {
            let error_msg = match token {
                RightParen => "expected ')'",
                _ => "invalid syntax",
            };
            self.error(position, error_msg);
            position
        }
    }

    fn try_parse_identifier(&mut self, position: usize) -> Result<(Str, usize), ()> {
        let token = self.tokens[position];
        if token.kind == TokenKind::Identifier {
            let token_start = usize::from(token.position);
            let token_end = token_start + usize::from(token.length);
            let name = self.source[token_start..token_end].into();
            Ok((name, position + 1))
        } else {
            Err(())
        }
    }

    fn parse_identifier(&mut self, position: usize) -> Result<(Str, usize), ()> {
        self.try_parse_identifier(position)
            .map_err(|_| self.error(position, "Expected an identifier"))
    }

    fn parse_identifier_soft(&mut self, position: usize) -> (Str, usize) {
        self.parse_identifier(position).unwrap_or_else(|_| ("".into(), position))
    }

    fn parse_integer(&mut self, position: usize) -> (Str, usize) {
        let token = self.tokens[position];
        if token.kind == TokenKind::Integer {
            let token_start = usize::from(token.position);
            let token_end = token_start + usize::from(token.length);
            let name = self.source[token_start..token_end].into();
            (name, position + 1)
        } else {
            self.error(position, "expected integer literal");
            ("".into(), position)
        }
    }

    fn parse_string(&mut self, position: usize) -> Str {
        let mut result = String::new();
        let (start, end) = self.tokens[position].span();
        let chars = self.source[start as usize..end as usize].chars().skip(1);
        for chr in chars {
            // TODO escapes
            result.push(chr);
        }
        result.pop();
        result.into()
    }

    fn expect_end_of_line(&mut self, position: usize) -> usize {
        use TokenKind::*;
        if self.tokens[position].kind != Linend {
            self.error(position, "expected end of line");
        }
        self.skip_line(position)
    }

    fn skip_line(&mut self, mut position: usize) -> usize {
        use TokenKind::*;
        while !matches!(self.tokens[position].kind, Linend | Dedent) {
            if self.tokens[position].kind == Indent {
                position = self.skip_current_block(position + 1);
            } else {
                position += 1
            }
        }
        position + 1
    }

    fn skip_current_block(&mut self, mut position: usize) -> usize {
        use TokenKind::*;
        let mut indent = 1;
        while indent > 0 {
            match self.tokens[position].kind {
                Indent => indent += 1,
                Dedent => indent -= 1,
                _ => {}
            }
            position += 1;
        }
        position
    }

    fn error(&mut self, position: usize, message: &'static str) {
        self.error_loc(self.tokens[position].span(), message);
    }

    fn error_loc(&mut self, location: (u32, u32), message: &'static str) {
        if DEBUG_LOGGING {
            eprintln!("SYNTAX ERROR: {:?}", message);
            if DIE_ON_FIRST_ERROR_REPORT {
                exit(1);
            }
        }
        let error = SyntaxError::new(location, message);
        self.errors.push(error);
    }

    fn prettify(&self, position: usize) -> String {
        let position = self.tokens[position].position as usize;
        let (before, after) = self.source.split_at(position);
        let line_no = before.split('\n').count().max(1);
        let before = before.split('\n').last().unwrap_or(" ");
        let after = after.split('\n').next().unwrap_or(" ");
        format!("\x1b[90m{:3}\x1b[0m {}\x1b[91m▶\x1b[0m{}", line_no, before, after)
            .replace("\t", "     ")
    }

    fn log(&self, msg: &str, position: usize) {
        if DEBUG_LOGGING {
            let line = self.prettify(position);
            eprintln!("{} \x1b[90m# {} /{:?}\x1b[0m", line, msg, self.tokens[position].kind);
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}
