use std::collections::HashMap;

type SymbolScope = String;

const GLOBALSCOPE: &str = "GLOBAL";

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Debug)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: u16,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: GLOBALSCOPE.to_string(),
            index: self.num_definitions,
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: GLOBALSCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: GLOBALSCOPE.to_string(),
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();
        let a = global.define("a".to_string());
        assert_eq!(&a, expected.get("a").unwrap());

        let b = global.define("b".to_string());
        assert_eq!(&b, expected.get("b").unwrap());
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a".to_string());
        global.define("b".to_string());

        let expected = HashMap::from([
            (
                "a".to_string(),
                Symbol {
                    name: "a".to_string(),
                    scope: GLOBALSCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "b".to_string(),
                Symbol {
                    name: "b".to_string(),
                    scope: GLOBALSCOPE.to_string(),
                    index: 1,
                },
            ),
        ]);

        for (_, symbol) in expected {
            let result = global.resolve(&symbol.name);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), &symbol);
        }
    }
}
