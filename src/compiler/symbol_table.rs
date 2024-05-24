use std::collections::HashMap;

type SymbolScope = String;

pub const GLOBALSCOPE: &str = "GLOBAL";
pub const LOCALSCOPE: &str = "LOCAL";

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    pub num_definitions: u16,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> Self {
        Self {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }

    pub fn define(&mut self, name: String) -> Symbol {
        let scope: &str;
        if self.outer.is_none() {
            scope = GLOBALSCOPE;
        } else {
            scope = LOCALSCOPE;
        }

        let symbol = Symbol {
            name: name.clone(),
            scope: scope.to_string(),
            index: self.num_definitions,
        };

        self.store.insert(name, symbol.clone());
        self.num_definitions += 1;

        symbol
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        let obj = self.store.get(name);

        if obj.is_none() && self.outer.is_some() {
            let outer = self.outer.as_ref().unwrap();
            return outer.resolve(name);
        }

        obj.cloned()
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
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 1,
                },
            ),
            (
                "e".to_string(),
                Symbol {
                    name: "e".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "f".to_string(),
                Symbol {
                    name: "f".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 1,
                },
            ),
        ]);

        let mut global = SymbolTable::new();
        let a = global.define("a".to_string());
        assert_eq!(&a, expected.get("a").unwrap());

        let b = global.define("b".to_string());
        assert_eq!(&b, expected.get("b").unwrap());

        let mut first_local = SymbolTable::new_enclosed(global);
        let c = first_local.define("c".to_string());
        assert_eq!(&c, expected.get("c").unwrap());

        let d = first_local.define("d".to_string());
        assert_eq!(&d, expected.get("d").unwrap());

        let mut second_local = SymbolTable::new_enclosed(first_local);
        let e = second_local.define("e".to_string());
        assert_eq!(&e, expected.get("e").unwrap());

        let f = second_local.define("f".to_string());
        assert_eq!(&f, expected.get("f").unwrap());
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
            assert_eq!(result.unwrap(), symbol);
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();

        global.define("a".to_string());
        global.define("b".to_string());

        let mut local = SymbolTable::new_enclosed(global);
        local.define("c".to_string());
        local.define("d".to_string());

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
            (
                "c".to_string(),
                Symbol {
                    name: "c".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 0,
                },
            ),
            (
                "d".to_string(),
                Symbol {
                    name: "d".to_string(),
                    scope: LOCALSCOPE.to_string(),
                    index: 1,
                },
            ),
        ]);

        for (_, symbol) in expected {
            let result = local.resolve(&symbol.name);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), symbol);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();

        global.define("a".to_string());
        global.define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());
        first_local.define("d".to_string());

        let mut second_local = SymbolTable::new_enclosed(first_local.clone());
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let tests = vec![
            (
                first_local,
                HashMap::from([
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
                    (
                        "c".to_string(),
                        Symbol {
                            name: "c".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 0,
                        },
                    ),
                    (
                        "d".to_string(),
                        Symbol {
                            name: "d".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 1,
                        },
                    ),
                ]),
            ),
            (
                second_local,
                HashMap::from([
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
                    (
                        "f".to_string(),
                        Symbol {
                            name: "e".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 0,
                        },
                    ),
                    (
                        "e".to_string(),
                        Symbol {
                            name: "f".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 1,
                        },
                    ),
                ]),
            ),
        ];

        for (local, expected) in tests {
            for (_, symbol) in expected {
                let result = local.resolve(&symbol.name);
                assert!(result.is_some());
                assert_eq!(result.unwrap(), symbol);
            }
        }
    }
}
