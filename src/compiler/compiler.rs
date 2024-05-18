use crate::ast::ast::{Expression, Infix, Program, Statement};
use crate::code::code::{make, Instructions, OpCode};
use crate::object::object::Object;

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: vec![],
            constants: vec![],
        }
    }
    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }

    pub fn compile_program(&mut self, program: Program) -> () {
        for stmt in program {
            self.compile_statement(stmt);
        }
    }

    fn compile_statement(&mut self, stmt: Statement) -> () {
        match stmt {
            Statement::ExpressionStmt(expression) => {
                self.compile_expression(expression);
            }
            _ => unimplemented!(),
        }
    }

    fn compile_expression(&mut self, expression: Expression) -> () {
        match expression {
            Expression::IntLiteral(value) => {
                let integer = Object::Integer(value as i64);
                let constant = self.add_constant(integer);
                self.emit(OpCode::OpConstant, vec![constant]);
            }
            Expression::InfixExpr(op, left, right) => {
                self.compile_expression(*left);
                self.compile_expression(*right);
                match op {
                    Infix::Plus => self.emit(OpCode::OpAdd, vec![]),
                    // "-" => self.emit(OpCode::OpSub, vec![]),
                    // "*" => self.emit(OpCode::OpMul, vec![]),
                    // "/" => self.emit(OpCode::OpDiv, vec![]),
                    // ">" => self.emit(OpCode::OpGreater, vec![]),
                    // "<" => self.emit(OpCode::OpLess, vec![]),
                    // "==" => self.emit(OpCode::OpEqual, vec![]),
                    // "!=" => self.emit(OpCode::OpNotEqual, vec![]),
                    _ => unimplemented!(),
                };
                ()
            }
            _ => unimplemented!(),
        }
    }

    fn add_constant(&mut self, obj: Object) -> u16 {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    fn emit(&mut self, op: OpCode, operands: Vec<u16>) -> usize {
        let instruction = make(op, operands);
        self.add_instruction(instruction)
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
        expected_constants: Vec<u8>,
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

            assert_eq!(bytecode.constants.len(), expected_constants.len());

            for (i, constant) in expected_constants.iter().enumerate() {
                assert_eq!(bytecode.constants[i], Object::Integer(*constant as i64));
            }
            test_instructions(expected_instructions, bytecode.instructions);
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(concatted.len(), actual.len(), "instructions length mismatch, want: {:?}, got: {:?}", string(concatted), string(actual));

        for (i, instr) in concatted.iter().enumerate() {
            assert_eq!(actual[i], *instr);
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
        let tests: Vec<CompilerTestCase> = vec![CompilerTestCase {
            input: String::from("1 + 2"),
            expected_constants: vec![1, 2],
            expected_instructions: vec![
                make(OpCode::OpConstant, vec![0]),
                make(OpCode::OpConstant, vec![1]),
                make(OpCode::OpAdd, vec![]),
            ],
        }];

        run_compiler_tests(tests)
    }
}
