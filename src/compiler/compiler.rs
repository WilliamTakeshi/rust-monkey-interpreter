use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::code::code::{make, Instructions, OpCode};
use crate::object::buildin::{get_builtin_functions, BUILTINS};
use crate::object::object::Object;
use anyhow::{anyhow, Result};

use crate::compiler::symbol_table::SymbolTable;

use super::symbol_table::{Symbol, BUILTINSCOPE, GLOBALSCOPE, LOCALSCOPE};

#[derive(Debug, Clone, PartialEq)]
struct EmittedInstruction {
    opcode: OpCode,
    position: usize,
}

#[derive(Debug)]
pub struct Compiler {
    constants: Vec<Object>,

    pub symbol_table: Rc<RefCell<SymbolTable>>,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

#[derive(Debug, Clone)]
pub struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

#[derive(Debug, Clone)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl CompilationScope {
    pub fn new() -> Self {
        CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        }
    }
}

impl Compiler {
    pub fn new() -> Self {
        let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

        for (idx, name) in BUILTINS.iter().enumerate() {
            symbol_table
                .borrow_mut()
                .define_builtin(idx as u16, name.to_string());
        }

        Compiler {
            constants: vec![],
            symbol_table: symbol_table,
            scopes: vec![CompilationScope::new()],
            scope_index: 0,
        }
    }

    pub fn new_with_state(symbol_table: Rc<RefCell<SymbolTable>>, constants: Vec<Object>) -> Self {
        Compiler {
            constants: constants,
            symbol_table: symbol_table,
            scopes: vec![CompilationScope::new()],
            scope_index: 0,
        }
    }
    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.scopes[self.scope_index].instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    pub fn compile_program(&mut self, program: Program) -> Result<()> {
        for stmt in program {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, stmt: Statement) -> Result<()> {
        match stmt {
            Statement::ExpressionStmt(expression) => {
                self.compile_expression(expression)?;
                self.emit(OpCode::OpPop, vec![]);
                Ok(())
            }
            Statement::Let(ident, expr) => {
                self.compile_expression(expr)?;

                let symbol = self.symbol_table.borrow_mut().define(ident);

                if symbol.scope == GLOBALSCOPE {
                    self.emit(OpCode::OpSetGlobal, vec![symbol.index]);
                } else {
                    self.emit(OpCode::OpSetLocal, vec![symbol.index]);
                }

                Ok(())
            }
            Statement::Return(expr) => {
                self.compile_expression(expr)?;
                self.emit(OpCode::OpReturnValue, vec![]);
                Ok(())
            }
        }
    }

    fn compile_block(&mut self, block: Vec<Statement>) -> Result<()> {
        for stmt in block {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<()> {
        match expression {
            Expression::IntLiteral(value) => {
                let integer = Object::Integer(value as i64);
                let constant = self.add_constant(integer);
                self.emit(OpCode::OpConstant, vec![constant]);
                Ok(())
            }
            Expression::Boolean(b) => {
                match b {
                    true => self.emit(OpCode::OpTrue, vec![]),
                    false => self.emit(OpCode::OpFalse, vec![]),
                };
                Ok(())
            }
            Expression::PrefixExpr(op, right) => {
                self.compile_expression(*right)?;
                match op {
                    Prefix::Bang => self.emit(OpCode::OpBang, vec![]),
                    Prefix::Minus => self.emit(OpCode::OpMinus, vec![]),
                };
                Ok(())
            }
            Expression::InfixExpr(op, left, right) => {
                // Special case, we still use greaterThan, but evaluate right then left
                if op == Infix::Lt {
                    self.compile_expression(*right)?;
                    self.compile_expression(*left)?;
                    self.emit(OpCode::OpGreaterThan, vec![]);
                    return Ok(());
                }

                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match op {
                    Infix::Plus => self.emit(OpCode::OpAdd, vec![]),
                    Infix::Minus => self.emit(OpCode::OpSub, vec![]),
                    Infix::Asterisk => self.emit(OpCode::OpMult, vec![]),
                    Infix::Slash => self.emit(OpCode::OpDiv, vec![]),
                    Infix::Gt => self.emit(OpCode::OpGreaterThan, vec![]),
                    Infix::Eq => self.emit(OpCode::OpEqual, vec![]),
                    Infix::Neq => self.emit(OpCode::OpNotEqual, vec![]),
                    _ => unimplemented!(),
                };
                Ok(())
            }
            Expression::IfExpr(condition, consequence, alternative) => {
                self.compile_expression(*condition)?;

                // Emit an `OpJumpNotTruthy` with a bogus value
                let jump_not_truthy_pos = self.emit(OpCode::OpJumpNotTruthy, vec![9999]);

                self.compile_block(consequence)?;

                if self.last_instruction_is(OpCode::OpPop) {
                    self.remove_last_pop();
                }

                // Emit an `OpJump` with a bogus value
                let jump_pos = self.emit(OpCode::OpJump, vec![9999]);
                let after_consequence_pos = self.scopes[self.scope_index].instructions.len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos as u16);

                if alternative.is_empty() {
                    self.emit(OpCode::OpNull, vec![]);
                } else {
                    self.compile_block(alternative)?;
                    if self.last_instruction_is(OpCode::OpPop) {
                        self.remove_last_pop();
                    }
                }

                let after_alternative_pos = self.scopes[self.scope_index].instructions.len();
                self.change_operand(jump_pos, after_alternative_pos as u16);

                Ok(())
            }
            Expression::Ident(ident) => {
                let mut maybe_symbol = None;
                {
                    maybe_symbol = self.symbol_table.borrow().resolve(&ident).clone();
                }

                if let Some(symbol) = maybe_symbol {
                    self.load_symbol(symbol);
                    Ok(())
                } else {
                    Err(anyhow!("undefined variable {}", ident))
                }
            }
            Expression::StringLiteral(s) => {
                let string = Object::String(s);
                let constant = self.add_constant(string);
                self.emit(OpCode::OpConstant, vec![constant]);
                Ok(())
            }
            Expression::ArrayLiteral(elements) => {
                let elements_len = elements.len();
                for elem in elements {
                    self.compile_expression(elem)?;
                }

                self.emit(OpCode::OpArray, vec![elements_len as u16]);
                Ok(())
            }
            Expression::HashLiteral(pairs) => {
                let pairs_len = pairs.len();

                for (key, value) in pairs {
                    self.compile_expression(key)?;
                    self.compile_expression(value)?;
                }

                self.emit(OpCode::OpHash, vec![(pairs_len * 2) as u16]);

                Ok(())
            }
            Expression::IndexExpr(left, idx) => {
                self.compile_expression(*left)?;
                self.compile_expression(*idx)?;
                self.emit(OpCode::OpIndex, vec![]);

                Ok(())
            }
            Expression::FnLiteral(params, body) => {
                self.enter_scope();

                for param in params.clone() {
                    match param {
                        Expression::Ident(param) => {
                            self.symbol_table.borrow_mut().define(param);
                        }
                        _ => panic!("Function parameter should be a Identifier, got {:?}", param),
                    }
                }

                self.compile_block(body)?;

                if self.last_instruction_is(OpCode::OpPop) {
                    self.replace_last_pop_with_return();
                }

                if !self.last_instruction_is(OpCode::OpReturnValue) {
                    self.emit(OpCode::OpReturn, vec![]);
                }

                let num_locals = self.symbol_table.borrow().num_definitions;
                let instructions = self.leave_scope();
                let constant = self.add_constant(Object::CompiledFunction {
                    instructions,
                    num_locals,
                    num_params: params.len() as u16,
                });

                self.emit(OpCode::OpClosure, vec![constant, 0]);

                Ok(())
            }
            Expression::Call(function, arguments) => {
                self.compile_expression(*function)?;

                for arg in arguments.clone() {
                    self.compile_expression(arg)?;
                }

                self.emit(OpCode::OpCall, vec![arguments.len() as u16]);

                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn replace_last_pop_with_return(&mut self) {
        let last_pos = self.scopes[self.scope_index]
            .last_instruction
            .as_ref()
            .expect("Never called on empty scope")
            .position
            .clone();

        self.replace_instruction(last_pos, make(OpCode::OpReturnValue, vec![]));

        self.scopes[self.scope_index].last_instruction = Some(EmittedInstruction {
            opcode: OpCode::OpReturnValue,
            position: last_pos,
        })
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for i in 0..new_instruction.len() {
            self.scopes[self.scope_index].instructions[pos + i] = new_instruction[i];
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: u16) {
        let op = self.scopes[self.scope_index].instructions[op_pos];
        let new_instruction = make(
            OpCode::try_from(op).expect("Not valid operand"),
            vec![operand],
        );

        self.replace_instruction(op_pos, new_instruction);
    }

    fn last_instruction_is(&self, op: OpCode) -> bool {
        match &self.scopes[self.scope_index].last_instruction {
            Some(instr) => instr.opcode == op,
            None => false,
        }
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].instructions.pop();
        self.scopes[self.scope_index].last_instruction =
            self.scopes[self.scope_index].previous_instruction.clone();
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    fn emit(&mut self, op: OpCode, operands: Vec<u16>) -> usize {
        let instruction = make(op.clone(), operands);
        let pos = self.add_instruction(instruction);

        self.set_last_instruction(op, pos);

        pos
    }

    fn set_last_instruction(&mut self, opcode: OpCode, position: usize) {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last = EmittedInstruction { opcode, position };

        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = Some(last);
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.scopes[self.scope_index].instructions.len();
        for instr in instruction {
            self.scopes[self.scope_index].instructions.push(instr);
        }

        pos_new_instruction
    }

    fn enter_scope(&mut self) {
        let scope = CompilationScope::new();

        self.scopes.push(scope);
        self.scope_index += 1;

        let outer = self.symbol_table.clone();

        self.symbol_table = Rc::new(RefCell::new(SymbolTable::new_enclosed(outer)));
    }

    fn leave_scope(&mut self) -> Instructions {
        let instructions = self.scopes[self.scope_index].instructions.clone();
        self.scopes.pop();
        self.scope_index -= 1;

        let outer = self.symbol_table.borrow().outer.clone().unwrap();
        self.symbol_table = outer;

        instructions
    }

    fn load_symbol(&mut self, s: Symbol) {
        match s.scope.as_str() {
            GLOBALSCOPE => self.emit(OpCode::OpGetGlobal, vec![s.index]),
            LOCALSCOPE => self.emit(OpCode::OpGetLocal, vec![s.index]),
            BUILTINSCOPE => self.emit(OpCode::OpGetBuiltin, vec![s.index]),
            _ => unreachable!("Invalid scope"),
        };
    }
}

#[cfg(test)]
mod tests {
    use crate::code::code::string;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use super::*;

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for CompilerTestCase {
            input,
            expected_constants,
            expected_instructions,
        } in tests
        {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            let mut compiler = Compiler::new();
            compiler.compile_program(program);

            let bytecode = compiler.bytecode();
            assert_eq!(
                bytecode.constants.len(),
                expected_constants.len(),
                "constants length mismatch, got: {:?}, want: {:?}",
                bytecode.constants,
                expected_constants
            );

            for (i, constant) in expected_constants.iter().enumerate() {
                assert_eq!(
                    bytecode.constants[i], *constant,
                    "bytecode mismatch, got {:?}, want {:?}",
                    bytecode.constants[i], *constant
                );
            }
            test_instructions(expected_instructions, bytecode.instructions);
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(
            concatted.len(),
            actual.len(),
            "instructions length mismatch, want: {:?}, got: {:?}",
            string(concatted),
            string(actual),
        );

        dbg!(&concatted);
        dbg!(&actual);

        for (i, instr) in concatted.iter().enumerate() {
            assert_eq!(
                actual[i], *instr,
                "instructions mismatch at index: {}, got: {}. want: {}",
                i, actual[i], instr
            );
        }
    }

    fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
        let mut out = vec![];

        for instruction in instructions {
            for instr in instruction {
                out.push(instr);
            }
        }

        out
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<CompilerTestCase> = vec![
            CompilerTestCase {
                input: String::from("1 + 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1; 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 - 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpSub, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 * 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpMult, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("2 / 1"),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpDiv, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("-1"),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpMinus, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_boolean_expressions() {
        let tests: Vec<CompilerTestCase> = vec![
            CompilerTestCase {
                input: String::from("true"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpFalse, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 > 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpGreaterThan, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 < 2"),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpGreaterThan, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 == 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("1 != 2"),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpNotEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("true == false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpFalse, vec![]),
                    make(OpCode::OpEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("true != false"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpFalse, vec![]),
                    make(OpCode::OpNotEqual, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("!true"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpBang, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("if (true) { 10 } ; 3333;"),
                expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
                expected_instructions: vec![
                    // 0000
                    make(OpCode::OpTrue, vec![]),
                    // 0001
                    make(OpCode::OpJumpNotTruthy, vec![10]),
                    // 0004
                    make(OpCode::OpConstant, vec![0]),
                    // 0007
                    make(OpCode::OpJump, vec![11]),
                    // 0010
                    make(OpCode::OpNull, vec![]),
                    // 0011
                    make(OpCode::OpPop, vec![]),
                    // 0012
                    make(OpCode::OpConstant, vec![1]),
                    // 0015
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("if (true) { 10 } else { 20 }; 3333;"),
                expected_constants: vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                expected_instructions: vec![
                    make(OpCode::OpTrue, vec![]),
                    make(OpCode::OpJumpNotTruthy, vec![10]),
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpJump, vec![13]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            CompilerTestCase {
                input: String::from(
                    "let one = 1;
                    let two = 2;",
                ),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpSetGlobal, vec![1]),
                ],
            },
            CompilerTestCase {
                input: String::from(
                    "let one = 1;
                one;",
                ),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from(
                    "let one = 1;
                    let two = one;
                    two;",
                ),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpSetGlobal, vec![1]),
                    make(OpCode::OpGetGlobal, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }
    #[test]
    fn test_string_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: String::from(r#""monkey""#),
                expected_constants: vec![Object::String(String::from("monkey"))],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from(r#""mon" + "key""#),
                expected_constants: vec![
                    Object::String(String::from("mon")),
                    Object::String(String::from("key")),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("[]"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpArray, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("[1, 2, 3]"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpArray, vec![3]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("[1 + 2, 3 - 4, 5 * 6]"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpSub, vec![]),
                    make(OpCode::OpConstant, vec![4]),
                    make(OpCode::OpConstant, vec![5]),
                    make(OpCode::OpMult, vec![]),
                    make(OpCode::OpArray, vec![3]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("{}"),
                expected_constants: vec![],
                expected_instructions: vec![
                    make(OpCode::OpHash, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("{1: 2, 3: 4, 5: 6}"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpConstant, vec![4]),
                    make(OpCode::OpConstant, vec![5]),
                    make(OpCode::OpHash, vec![6]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("{1: 2 + 3, 4: 5 * 6}"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpConstant, vec![4]),
                    make(OpCode::OpConstant, vec![5]),
                    make(OpCode::OpMult, vec![]),
                    make(OpCode::OpHash, vec![4]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("[1, 2, 3][1 + 1]"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(1),
                    Object::Integer(1),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpArray, vec![3]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpConstant, vec![4]),
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpIndex, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("{1: 2}[2 - 1]"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(2),
                    Object::Integer(1),
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpHash, vec![2]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpSub, vec![]),
                    make(OpCode::OpIndex, vec![]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_functions() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("fn() { return 5 + 10 }"),
                expected_constants: vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]),
                            make(OpCode::OpConstant, vec![1]),
                            make(OpCode::OpAdd, vec![]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![2, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("fn() { 5 + 10 }"),
                expected_constants: vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]),
                            make(OpCode::OpConstant, vec![1]),
                            make(OpCode::OpAdd, vec![]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![2, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("fn() { 1; 2 }"),
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]),
                            make(OpCode::OpPop, vec![]),
                            make(OpCode::OpConstant, vec![1]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![2, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(
            compiler.scope_index, 0,
            "scope index wrong. got={}, want={}",
            compiler.scope_index, 0
        );

        compiler.emit(OpCode::OpMult, vec![]);
        let global_symbol_table = compiler.symbol_table.clone();

        compiler.enter_scope();
        assert_eq!(
            compiler.scope_index, 1,
            "scope index wrong. got={}, want={}",
            compiler.scope_index, 1
        );

        compiler.emit(OpCode::OpSub, vec![]);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.len(),
            1,
            "instructions length wrong. got={}, want={}",
            compiler.scopes[compiler.scope_index].instructions.len(),
            1
        );
        assert_eq!(
            compiler.scopes[compiler.scope_index].last_instruction,
            Some(EmittedInstruction {
                opcode: OpCode::OpSub,
                position: 0
            }),
        );
        assert_eq!(
            compiler.symbol_table.borrow().outer,
            Some(global_symbol_table.clone()),
            "compiler did not enclose symbol table"
        );

        compiler.leave_scope();
        assert_eq!(
            compiler.scope_index, 0,
            "scope index wrong. got={}, want={}",
            compiler.scope_index, 0
        );
        assert_eq!(
            compiler.symbol_table.borrow().clone(),
            global_symbol_table.borrow().clone(),
            "compiler did not restore global symbol table"
        );

        assert_eq!(
            compiler.symbol_table.borrow().outer,
            None,
            "compiler modified global symbol table incorrectly"
        );

        compiler.emit(OpCode::OpAdd, vec![]);
        assert_eq!(
            compiler.scopes[compiler.scope_index].instructions.len(),
            2,
            "instructions length wrong. got={}, want={}",
            compiler.scopes[compiler.scope_index].instructions.len(),
            2
        );

        assert_eq!(
            compiler.scopes[compiler.scope_index].last_instruction,
            Some(EmittedInstruction {
                opcode: OpCode::OpAdd,
                position: 1
            }),
        );

        assert_eq!(
            compiler.scopes[compiler.scope_index].previous_instruction,
            Some(EmittedInstruction {
                opcode: OpCode::OpMult,
                position: 0
            }),
        );
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![CompilerTestCase {
            input: String::from("fn() { }"),
            expected_constants: vec![Object::CompiledFunction {
                instructions: make(OpCode::OpReturn, vec![]),
                num_locals: 0,
                num_params: 0,
            }],
            expected_instructions: vec![
                make(OpCode::OpClosure, vec![0, 0]),
                make(OpCode::OpPop, vec![]),
            ],
        }];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_function_calls() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("fn() { 24 }();"),
                expected_constants: vec![
                    Object::Integer(24),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]), // The literal "24"
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![1, 0]), // The compiled function
                    make(OpCode::OpCall, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("let noArg = fn() { 24 }; noArg();"),
                expected_constants: vec![
                    Object::Integer(24),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]), // The literal "24"
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![1, 0]), // The compiled function
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpCall, vec![0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("let oneArg = fn(a) { }; oneArg(24);"),
                expected_constants: vec![
                    Object::CompiledFunction {
                        instructions: [make(OpCode::OpReturn, vec![])].concat(),
                        num_locals: 1,
                        num_params: 1,
                    },
                    Object::Integer(24),
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![0, 0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpCall, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from(
                    "
                let manyArg = fn(a, b, c) { };
                manyArg(24, 25, 26);
                ",
                ),
                expected_constants: vec![
                    Object::CompiledFunction {
                        instructions: [make(OpCode::OpReturn, vec![])].concat(),
                        num_locals: 3,
                        num_params: 3,
                    },
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![0, 0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpCall, vec![3]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("let oneArg = fn(a) { a }; oneArg(24);"),
                expected_constants: vec![
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpGetLocal, vec![0]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_params: 1,
                    },
                    Object::Integer(24),
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![0, 0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpCall, vec![1]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from(
                    "
                let manyArg = fn(a, b, c) { a; b; c };
                manyArg(24, 25, 26);
                ",
                ),
                expected_constants: vec![
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpGetLocal, vec![0]),
                            make(OpCode::OpPop, vec![]),
                            make(OpCode::OpGetLocal, vec![1]),
                            make(OpCode::OpPop, vec![]),
                            make(OpCode::OpGetLocal, vec![2]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 3,
                        num_params: 3,
                    },
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![0, 0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpGetGlobal, vec![0]),
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![3]),
                    make(OpCode::OpCall, vec![3]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_let_statement_scopes() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("let num = 55; fn() { num }"),
                expected_constants: vec![
                    Object::Integer(55),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpGetGlobal, vec![0]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 0,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpSetGlobal, vec![0]),
                    make(OpCode::OpClosure, vec![1, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("fn() { let num = 55; num }"),
                expected_constants: vec![
                    Object::Integer(55),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]),
                            make(OpCode::OpSetLocal, vec![0]),
                            make(OpCode::OpGetLocal, vec![0]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 1,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![1, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("fn() { let a = 55; let b = 77; a + b}"),
                expected_constants: vec![
                    Object::Integer(55),
                    Object::Integer(77),
                    Object::CompiledFunction {
                        instructions: [
                            make(OpCode::OpConstant, vec![0]),
                            make(OpCode::OpSetLocal, vec![0]),
                            make(OpCode::OpConstant, vec![1]),
                            make(OpCode::OpSetLocal, vec![1]),
                            make(OpCode::OpGetLocal, vec![0]),
                            make(OpCode::OpGetLocal, vec![1]),
                            make(OpCode::OpAdd, vec![]),
                            make(OpCode::OpReturnValue, vec![]),
                        ]
                        .concat(),
                        num_locals: 2,
                        num_params: 0,
                    },
                ],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![2, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }
    #[test]
    fn test_builtins() {
        let tests = vec![
            CompilerTestCase {
                input: String::from("len([]); push([], 1);"),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(OpCode::OpGetBuiltin, vec![0]),
                    make(OpCode::OpArray, vec![0]),
                    make(OpCode::OpCall, vec![1]),
                    make(OpCode::OpPop, vec![]),
                    make(OpCode::OpGetBuiltin, vec![5]),
                    make(OpCode::OpArray, vec![0]),
                    make(OpCode::OpConstant, vec![0]),
                    make(OpCode::OpCall, vec![2]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
            CompilerTestCase {
                input: String::from("fn() { len([]) }"),
                expected_constants: vec![Object::CompiledFunction {
                    instructions: [
                        make(OpCode::OpGetBuiltin, vec![0]),
                        make(OpCode::OpArray, vec![0]),
                        make(OpCode::OpCall, vec![1]),
                        make(OpCode::OpReturnValue, vec![]),
                    ]
                    .concat(),
                    num_locals: 0,
                    num_params: 0,
                }],
                expected_instructions: vec![
                    make(OpCode::OpClosure, vec![0, 0]),
                    make(OpCode::OpPop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }
}
