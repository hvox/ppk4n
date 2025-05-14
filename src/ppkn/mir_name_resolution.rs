#![allow(unused)]
use std::collections::HashMap;

use super::mir::*;
use super::common::*;

impl Program {
    pub fn resolve_names(&mut self, module_name: &str) {
        let (id, _, module) = self.modules.get_full(module_name).unwrap();
        if !module.resolved_names.is_empty() {
            return;
        }
        let mut deps = vec![];
        let mut names = HashMap::new();
        names.insert(Str::from("self"), Definition::Module(id));
        for (name, item) in &module.items {
            use Definition::*;
            let name = name.clone();
            match &item.kind {
                ItemKind::Dependency(name) => {
                    deps.push(name.clone());
                }
                ItemKind::Import(handle) => {
                    names.insert(name, Import(*handle));
                }
                ItemKind::Function(handle) => {
                    names.insert(name, Function(*handle));
                }
                ItemKind::Global(handle) => {
                    names.insert(name, Global(*handle));
                }
            }
        }
        self.modules.get_mut(module_name).unwrap().resolved_names = names;
        for dependency in deps {
            if let Some(i) = dependency.rfind(":") {
                let module = module_name.to_string() + ":" + &dependency[..i];
                let name = &dependency[i + 1..];
                self.resolve_names(&module[..]);
                let def = self.modules[&module[..]].resolved_names[name];
                self.modules[module_name].resolved_names.insert(Str::from(name), def);
            } else {
                self.resolve_names(&dependency);
                let def = Definition::Module(self.modules.get_index_of(&dependency).unwrap());
                self.modules[module_name].resolved_names.insert(dependency, def);
            }
        }
    }

    pub fn resolve_name_in_module(&mut self, module: &str, name: &str) -> Definition {
        if let Some(def) = self.modules[module].resolved_names.get(name) {
            return *def;
        }
        use ItemKind::*;
        let definition = match &self.modules[module].items[name].kind {
            Dependency(_) => self.resolve_name_in_module(module, name),
            Import(handle) => Definition::Import(*handle),
            Function(handle) => Definition::Function(*handle),
            Global(handle) => Definition::Global(*handle),
        };
        self.modules.get_mut(module).unwrap().resolved_names.insert(name.into(), definition);
        definition
    }
}
