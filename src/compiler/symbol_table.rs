use std::{cell::RefCell, collections::HashMap, rc::Rc};

type SymbolScope = String;

pub const GLOBALSCOPE: &str = "GLOBAL";
pub const LOCALSCOPE: &str = "LOCAL";
pub const BUILTINSCOPE: &str = "BUILTIN";
pub const FREESCOPE: &str = "FREE";

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,

    pub store: HashMap<String, Symbol>,
    pub num_definitions: u16,
    pub free_symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }

    pub fn new_enclosed(outer: Rc<RefCell<SymbolTable>>) -> Self {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
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

    pub fn define_builtin(&mut self, index: u16, name: String) -> Symbol {
        let symbol = Symbol {
            name: name.clone(),
            scope: BUILTINSCOPE.to_string(),
            index,
        };

        self.store.insert(name, symbol.clone());

        symbol
    }

    pub fn define_free(&mut self, original: Symbol) -> Symbol {
        self.free_symbols.push(original.clone());

        let symbol = Symbol {
            name: original.name.clone(),
            scope: FREESCOPE.to_string(),
            index: self.free_symbols.len() as u16 - 1,
        };

        self.store.insert(symbol.name.clone(), symbol.clone());

        symbol
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let obj = self.store.get(name);

        if obj.is_none() && self.outer.is_some() {
            let outer = self.outer.clone().unwrap();
            let obj_outer = outer.borrow_mut().resolve(name);

            if let Some(obj) = obj_outer {
                if obj.scope == GLOBALSCOPE || obj.scope == BUILTINSCOPE {
                    return Some(obj);
                }

                let free = self.define_free(obj.clone());

                dbg!(outer);
                return Some(free);
            } else {
                return None;
            }
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

        let mut global = Rc::new(RefCell::new(SymbolTable::new()));
        let a = global.borrow_mut().define("a".to_string());
        assert_eq!(&a, expected.get("a").unwrap());

        let b = global.borrow_mut().define("b".to_string());
        assert_eq!(&b, expected.get("b").unwrap());

        let mut first_local = SymbolTable::new_enclosed(global);
        let c = first_local.define("c".to_string());
        assert_eq!(&c, expected.get("c").unwrap());

        let d = first_local.define("d".to_string());
        assert_eq!(&d, expected.get("d").unwrap());

        let mut second_local = SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local)));
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
        let global = Rc::new(RefCell::new(SymbolTable::new()));

        global.borrow_mut().define("a".to_string());
        global.borrow_mut().define("b".to_string());

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
        let global = Rc::new(RefCell::new(SymbolTable::new()));

        global.borrow_mut().define("a".to_string());
        global.borrow_mut().define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());
        first_local.define("d".to_string());

        let mut second_local =
            SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));
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
                ]),
            ),
        ];

        for (mut local, expected) in tests {
            for (_, symbol) in expected {
                let result = local.resolve(&symbol.name);
                assert!(result.is_some());
                assert_eq!(result.unwrap(), symbol);
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));
        let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(global.clone())));

        let second_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(first_local.clone())));

        let expected = [
            Symbol {
                name: "a".to_string(),
                scope: BUILTINSCOPE.to_string(),
                index: 0,
            },
            Symbol {
                name: "c".to_string(),
                scope: BUILTINSCOPE.to_string(),
                index: 1,
            },
            Symbol {
                name: "e".to_string(),
                scope: BUILTINSCOPE.to_string(),
                index: 2,
            },
            Symbol {
                name: "f".to_string(),
                scope: BUILTINSCOPE.to_string(),
                index: 3,
            },
        ];

        for (idx, symbol) in expected.iter().enumerate() {
            global
                .borrow_mut()
                .define_builtin(idx as u16, symbol.name.clone());
        }

        for symbol in &expected {
            let result = global.borrow_mut().resolve(&symbol.name);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *symbol);
        }

        for symbol in &expected {
            let result: Option<Symbol> = first_local.borrow_mut().resolve(&symbol.name);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *symbol);
        }

        for symbol in &expected {
            let result: Option<Symbol> = second_local.borrow_mut().resolve(&symbol.name);
            assert!(result.is_some());
            assert_eq!(result.unwrap(), *symbol);
        }
    }

    #[test]
    fn test_resolve_free() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));

        global.borrow_mut().define("a".to_string());
        global.borrow_mut().define("b".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());
        first_local.define("d".to_string());

        let mut second_local =
            SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let mut third_local =
            SymbolTable::new_enclosed(Rc::new(RefCell::new(second_local.clone())));
        third_local.define("g".to_string());
        third_local.define("h".to_string());

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
                        "c".to_string(),
                        Symbol {
                            name: "c".to_string(),
                            scope: FREESCOPE.to_string(),
                            index: 0,
                        },
                    ),
                    (
                        "d".to_string(),
                        Symbol {
                            name: "d".to_string(),
                            scope: FREESCOPE.to_string(),
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
                ]),
            ),
            (
                third_local.clone(),
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
                            scope: FREESCOPE.to_string(),
                            index: 0,
                        },
                    ),
                    (
                        "d".to_string(),
                        Symbol {
                            name: "d".to_string(),
                            scope: FREESCOPE.to_string(),
                            index: 1,
                        },
                    ),
                    (
                        "e".to_string(),
                        Symbol {
                            name: "e".to_string(),
                            scope: FREESCOPE.to_string(),
                            index: 2,
                        },
                    ),
                    (
                        "f".to_string(),
                        Symbol {
                            name: "f".to_string(),
                            scope: FREESCOPE.to_string(),
                            index: 3,
                        },
                    ),
                    (
                        "g".to_string(),
                        Symbol {
                            name: "g".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 0,
                        },
                    ),
                    (
                        "h".to_string(),
                        Symbol {
                            name: "h".to_string(),
                            scope: LOCALSCOPE.to_string(),
                            index: 1,
                        },
                    ),
                ]),
            ),
        ];

        for (mut local, expected) in tests {
            for (_, symbol) in expected {
                let result = local.resolve(&symbol.name);
                assert!(result.is_some());
                assert_eq!(result.clone().unwrap().name, symbol.name);
                assert_eq!(result.unwrap().scope, symbol.scope);
            }
        }
    }

    #[test]
    fn test_resolve_unresolvable_free() {
        let global = Rc::new(RefCell::new(SymbolTable::new()));

        global.borrow_mut().define("a".to_string());

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c".to_string());

        let mut second_local =
            SymbolTable::new_enclosed(Rc::new(RefCell::new(first_local.clone())));
        second_local.define("e".to_string());
        second_local.define("f".to_string());

        let tests = vec![(
            second_local.clone(),
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
                    "c".to_string(),
                    Symbol {
                        name: "c".to_string(),
                        scope: FREESCOPE.to_string(),
                        index: 0,
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
            ]),
        )];

        for (mut local, expected) in tests {
            for (_, symbol) in expected {
                let result = local.resolve(&symbol.name);
                dbg!(&symbol);
                dbg!(&result);
                assert!(result.is_some());
                assert_eq!(result.unwrap(), symbol);
            }
        }

        let unresolvable = ["b".to_string(), "d".to_string()];

        for unresolvable_symbol in unresolvable {
            let result = second_local.resolve(&unresolvable_symbol);
            assert!(result.is_none());
        }
    }
}
