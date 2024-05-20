use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::ast::{Expression, Infix, Prefix, Program, Statement};
use crate::code::code::{make, Instructions, OpCode};
use crate::object::object::Object;
use anyhow::{anyhow, Result};

use crate::compiler::symbol_table::SymbolTable;

#[derive(Debug, Clone)]
struct EmittedInstruction {
    opcode: OpCode,
    position: usize,
}

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,

    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
    pub symbol_table: Rc<RefCell<SymbolTable>>,
}

#[derive(Debug, Clone)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: vec![],
            last_instruction: None,
            previous_instruction: None,
            symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
        }
    }

    pub fn new_with_state(symbol_table: Rc<RefCell<SymbolTable>>, constants: Vec<Object>) -> Self {
        Compiler {
            instructions: vec![],
            constants: constants,
            last_instruction: None,
            previous_instruction: None,
            symbol_table: symbol_table,
        }
    }
    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
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

                self.emit(OpCode::OpSetGlobal, vec![symbol.index]);
                Ok(())
            }
            _ => unimplemented!(),
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

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }

                // Emit an `OpJump` with a bogus value
                let jump_pos = self.emit(OpCode::OpJump, vec![9999]);
                let after_consequence_pos = self.instructions.len();
                self.change_operand(jump_not_truthy_pos, after_consequence_pos as u16);

                if alternative.is_empty() {
                    self.emit(OpCode::OpNull, vec![]);
                } else {
                    self.compile_block(alternative)?;
                    if self.last_instruction_is_pop() {
                        self.remove_last_pop();
                    }
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos as u16);

                Ok(())
            }
            Expression::Ident(ident) => {
                let mut maybe_symbol = None;
                {
                    maybe_symbol = self.symbol_table.borrow().resolve(&ident).clone();
                }

                if let Some(symbol) = maybe_symbol {
                    self.emit(OpCode::OpGetGlobal, vec![symbol.index]);
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
            _ => unimplemented!(),
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) {
        for i in 0..new_instruction.len() {
            self.instructions[pos + i] = new_instruction[i];
        }
    }

    fn change_operand(&mut self, op_pos: usize, operand: u16) {
        let op = self.instructions[op_pos];
        let new_instruction = make(
            OpCode::try_from(op).expect("Not valid operand"),
            vec![operand],
        );

        self.replace_instruction(op_pos, new_instruction);
    }

    fn last_instruction_is_pop(&self) -> bool {
        match &self.last_instruction {
            Some(instr) => instr.opcode == OpCode::OpPop,
            None => false,
        }
    }

    fn remove_last_pop(&mut self) {
        self.instructions.pop();
        self.last_instruction = self.previous_instruction.clone();
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
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction { opcode, position };

        self.previous_instruction = previous;
        self.last_instruction = Some(last);
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let pos_new_instruction = self.instructions.len();
        for instr in instruction {
            self.instructions.push(instr);
        }

        pos_new_instruction
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
            dbg!(&bytecode.constants);
            dbg!(&expected_constants);
            assert_eq!(bytecode.constants.len(), expected_constants.len());

            for (i, constant) in expected_constants.iter().enumerate() {
                assert_eq!(bytecode.constants[i], *constant);
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
            string(actual)
        );
        dbg!(string(concatted.clone()));
        dbg!(string(actual.clone()));

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
}
