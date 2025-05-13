#![allow(dead_code)]
use std::{
    fmt::Debug,
    marker::PhantomData,
    ops::{AddAssign, Index, IndexMut, SubAssign},
    rc::Rc,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Storage<T> {
    elements: Vec<T>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Handle<T> {
    index: u32,
    phantom: PhantomData<T>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Position {
    line: u16,
    column: u16,
}

pub type Str = Rc<str>;

pub type TODO = ();

// This struct was supposed to be an arena implementation,
// but I ended up never really needing remove() method,
// so it is just Vec<T> with typed indexes now.
impl<T> Storage<T> {
    pub fn new() -> Self {
        Self { elements: Vec::new() }
    }

    pub fn insert(&mut self, value: T) -> Handle<T> {
        let index = self.elements.len();
        self.elements.push(value);
        Handle::from_index(index)
    }

    // TODO: pub fn remove()
    // using freelist
}

impl<T> Default for Storage<T> {
    fn default() -> Self {
        Storage { elements: Vec::new() }
    }
}

impl<T> Index<Handle<T>> for Storage<T> {
    type Output = T;

    fn index(&self, index: Handle<T>) -> &Self::Output {
        &self.elements[index.index as usize]
    }
}

impl<T> IndexMut<Handle<T>> for Storage<T> {
    fn index_mut(&mut self, index: Handle<T>) -> &mut Self::Output {
        &mut self.elements[index.index as usize]
    }
}

impl<T> Handle<T> {
    fn from_index(index: usize) -> Self {
        Self { index: index as u32, phantom: PhantomData }
    }

    pub fn default() -> Self {
        Self { index: u32::MAX, phantom: PhantomData }
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self { index: self.index, phantom: PhantomData }
    }
}

impl<T> Copy for Handle<T> {}

impl Span {
    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl Position {
    pub const fn new(line: usize, column: usize) -> Self {
        let line = line as _;
        let column = column as _;
        Self { line, column }
    }

    pub const fn into(&self) -> (usize, usize) {
        (self.line as usize, self.column as usize)
    }

    pub const fn line(&self) -> usize {
        self.line as usize
    }

    pub const fn column(&self) -> usize {
        self.column as usize
    }
}

impl SubAssign for Position {
    fn sub_assign(&mut self, rhs: Self) {
        self.line -= rhs.line;
        self.column -= rhs.column;
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        self.line += rhs.line;
        self.column += rhs.column;
    }
}

pub unsafe fn str_from_borders<'a>(start: &'a str, end: &'a str) -> &'a str {
    let end = end.as_ptr() as usize + end.len();
    let length = end - start.as_ptr() as usize;
    unsafe {
        let bytes = std::slice::from_raw_parts(start.as_ptr(), length);
        std::str::from_utf8_unchecked(bytes)
    }
}

// I think it's currently impossible to implement
// in Rust PhantomData, that can be field of struct
// with derived Clone, Eq and others trait.
mod clonable_phantom {
    use super::*;

    pub struct Phantom<T>(PhantomData<T>);

    impl<T> Clone for Phantom<T> {
        fn clone(&self) -> Self {
            Self(PhantomData)
        }
    }

    impl<T> Default for Phantom<T> {
        fn default() -> Self {
            Self(PhantomData)
        }
    }

    impl<T> Debug for Phantom<T> {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            Ok(())
        }
    }

    impl<T> PartialEq for Phantom<T> {
        fn eq(&self, _: &Self) -> bool {
            true
        }
    }

    impl<T> Eq for Phantom<T> {}
    impl<T> Copy for Phantom<T> {}
}
