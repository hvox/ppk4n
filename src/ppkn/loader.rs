#![allow(unused)]

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::rc::Rc;

use indexmap::IndexMap;

use super::error::Error;
use super::error::PpknErrorKind;
use super::parser::parse;
use super::parser::Ast;
use super::parser::Dependency;

pub fn load(root: PathBuf, name: Str, source: Str) -> Loader {
    let mut loader = Loader {
        root_path: root,
        sources: HashMap::new(),
        modules: IndexMap::new(),
        errors: Vec::new(),
    };
    loader.sources.insert(name.clone(), source.clone());
    let _ = loader.load(name);
    loader
}

pub struct Loader {
    root_path: PathBuf,
    sources: HashMap<Str, Str>,
    pub modules: IndexMap<Str, Ast>,
    pub errors: Vec<Error>,
}

type Str = Rc<str>;

const LOG_LOADING: bool = false;

impl Loader {
    fn load(&mut self, name: Str) -> io::Result<()> {
        if LOG_LOADING {
            println!("--- loading {}.ppkn ---", name);
        }
        if self.modules.contains_key(&name) {
            return Ok(());
        }

        let source = match self.sources.get(&name) {
            Some(source) => source.clone(),
            None => {
                let path = self.root_path.join(format!("{}.ppkn", name));
                let source = Str::from(fs::read_to_string(path)?);
                self.sources.insert(name.clone(), source.clone());
                source
            }
        };

        let (ast, errors) = parse(&source);
        let deps = ast.dependencies.clone();
        self.errors.extend(
            errors
                .into_iter()
                .map(|error| error.into_error(name.clone())),
        );
        self.modules.insert(name.clone(), ast);
        for Dependency {
            location,
            name: dependency_name,
        } in deps
        {
            if let Err(_) = self.load(dependency_name) {
                self.errors.push(Error::new(
                    name.clone(),
                    location,
                    PpknErrorKind::LoadError,
                    "Failed to load module".into(),
                ));
            }
        }
        return Ok(());
    }
}
