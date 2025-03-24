use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub module: Str,
    pub cause_location: (u32, u32),
    pub message: String,
    pub kind: PpknErrorKind,
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PpknErrorKind {
    // LoadError, TODO
    SyntaxError,
    TypeError,
    NameError,
    RuntimeError,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SyntaxError {
    pub cause_location: (u32, u32),
    pub message: &'static str,
}

type Str = Rc<str>;

impl SyntaxError {
    pub fn new(location: (u32, u32), message: &'static str) -> Self {
        Self {
            cause_location: location,
            message,
        }
    }
}

impl SyntaxError {
    pub fn into_error(self, module: Str) -> Error {
        let cause_location = self.cause_location;
        let message = self.message.into();
        let kind = PpknErrorKind::SyntaxError;
        Error {
            module,
            cause_location,
            message,
            kind,
        }
    }
}
