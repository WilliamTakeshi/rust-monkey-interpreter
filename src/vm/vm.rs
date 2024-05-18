use crate::code::code::{Instructions, OpCode};
use crate::compiler::compiler::Bytecode;
use crate::object::object::Object;
use anyhow::{anyhow, Result};

const STACK_SIZE: usize = 50;
// const STACK_SIZE: usize = 2048;

#[derive(Debug)]
struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize,
}

impl Vm {
    fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            return &Object::Null;
        }
        &self.stack[self.sp - 1]
    }

    fn run(&mut self) -> Result<()> {
        let mut ip: usize = 0;
        while ip < self.instructions.len() {
            let op = self.instructions[ip];
            match op.try_into() {
                Ok(OpCode::OpConstant) => {
                    let const_index =
                        u16::from_be_bytes(self.instructions[ip + 1..=ip + 2].try_into().unwrap());
                    ip += 2;

                    let result = self.push(self.constants[const_index as usize].clone());

                    if let Err(_) = result {
                        return result;
                    }
                }
                Ok(OpCode::OpAdd) => {
                    let right = self.pop();
                    let left = self.pop();

                    match (left, right) {
                        (Object::Integer(left), Object::Integer(right)) => {
                            self.push(Object::Integer(left + right))?;
                        }
                        _ => Err(anyhow!("unsupported types for OpAdd"))?,
                    }
                }
                _ => todo!(),
            }
            ip += 1;
        }
        Ok(())
    }

    fn push(&mut self, obj: Object) -> Result<()> {
        if self.sp >= STACK_SIZE {
            return Err(anyhow!("stack overflow"));
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> Object {
        let obj = self.stack[self.sp - 1].clone();
        self.stack[self.sp - 1] = Object::Null;
        self.sp -= 1;
        obj
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::compiler::Compiler;
    use crate::{lexer::Lexer, object::object::Object, parser::Parser};

    use super::*;

    struct VmTestCase {
        input: String,
        expected: Vec<Object>,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for VmTestCase { input, expected } in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut compiler = Compiler::new();
            compiler.compile_program(program);

            let mut vm = Vm::new(compiler.bytecode());
            let _ = vm.run();

            // assert!(false);

            let stack = vm
                .stack
                .iter()
                .filter(|x| x != &&Object::Null)
                .collect::<Vec<_>>();
            assert_eq!(stack.len(), expected.len(), "stack has wrong number of objects got={:?}, want={:?}", stack, expected);

            for (i, obj) in expected.iter().enumerate() {
                assert_eq!(stack[i], obj);
            }
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests: Vec<VmTestCase> = vec![
            VmTestCase {
                input: "1".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: "2".to_string(),
                expected: vec![Object::Integer(2)],
            },
            VmTestCase {
                input: "1 + 2".to_string(),
                expected: vec![Object::Integer(3)],
            },
        ];

        run_vm_tests(tests);
    }
}
