use std::fs::OpenOptions;

use crate::code::code::{Instructions, OpCode};
use crate::compiler::compiler::Bytecode;
use crate::object::object::Object;
use anyhow::{anyhow, Result};

// const STACK_SIZE: usize = 50;
const STACK_SIZE: usize = 2048;

const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);
#[derive(Debug)]
pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    sp: usize,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![Object::Null; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            return &Object::Null;
        }
        &self.stack[self.sp - 1]
    }

    pub fn run(&mut self) -> Result<()> {
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
                Ok(op)
                    if op == OpCode::OpAdd
                        || op == OpCode::OpSub
                        || op == OpCode::OpMult
                        || op == OpCode::OpDiv =>
                {
                    self.execute_binary_operation(op)?;
                }
                Ok(op)
                    if op == OpCode::OpGreaterThan
                        || op == OpCode::OpNotEqual
                        || op == OpCode::OpEqual =>
                {
                    self.execute_comparition(op)?;
                }
                Ok(OpCode::OpTrue) => self.push(TRUE)?,
                Ok(OpCode::OpFalse) => self.push(FALSE)?,
                Ok(OpCode::OpPop) => {
                    self.pop();
                }
                _ => todo!(),
            }
            ip += 1;
        }
        Ok(())
    }

    fn execute_comparition(&mut self, op: OpCode) -> Result<()> {
        let right_obj = self.pop();
        let left_obj = self.pop();

        match (left_obj.clone(), right_obj.clone()) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_integer_comparition(op, left, right)
            }
            (Object::Boolean(left), Object::Boolean(right)) => match op {
                OpCode::OpEqual => self.push(Object::Boolean(left == right)),
                OpCode::OpNotEqual => self.push(Object::Boolean(left != right)),
                _ => Err(anyhow!(
                    "Unknown operator: {:?} ({} {})",
                    op,
                    left_obj.obj_type(),
                    right_obj.obj_type()
                )),
            },
            _ => Err(anyhow!(
                "Left and Right should be both same type, got={} and {}",
                left_obj.obj_type(),
                right_obj.obj_type()
            )),
        }
    }

    fn execute_integer_comparition(&mut self, op: OpCode, left: i64, right: i64) -> Result<()> {
        dbg!(&op);
        dbg!(&left);
        dbg!(&right);
        match op {
            OpCode::OpEqual => self.push(Object::Boolean(left == right)),
            OpCode::OpNotEqual => self.push(Object::Boolean(left != right)),
            OpCode::OpGreaterThan => self.push(Object::Boolean(left > right)),
            _ => Err(anyhow!("Unknown operator: {:?}", op)),
        }
    }

    fn execute_binary_operation(&mut self, op: OpCode) -> Result<()> {
        let right = self.pop();
        let left = self.pop();

        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_binary_integer_operation(op, left, right)?;
            }
            _ => Err(anyhow!("unsupported types for OpAdd"))?,
        }
        Ok(())
    }

    fn execute_binary_integer_operation(
        &mut self,
        op: OpCode,
        left: i64,
        right: i64,
    ) -> Result<()> {
        match op {
            OpCode::OpAdd => self.push(Object::Integer(left + right)),
            OpCode::OpSub => self.push(Object::Integer(left - right)),
            OpCode::OpMult => self.push(Object::Integer(left * right)),
            OpCode::OpDiv => self.push(Object::Integer(left / right)),
            _ => Err(anyhow!("unknown integer operator: {:?}", op)),
        }
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
        // self.stack[self.sp - 1] = Object::Null;
        self.sp -= 1;
        obj
    }

    pub fn last_popped_stack_elem(&self) -> Object {
        self.stack[self.sp].clone()
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

            let stack_element = vm.last_popped_stack_elem();

            assert_eq!(stack_element, expected[0]);

            // let stack = vm
            //     .stack
            //     .iter()
            //     .filter(|x| x != &&Object::Null)
            //     .collect::<Vec<_>>();
            // // assert_eq!(stack.len(), expected.len(), "stack has wrong number of objects got={:?}, want={:?}", stack, expected);

            // for (i, obj) in expected.iter().enumerate() {
            //     assert_eq!(stack[i], obj);
            // }
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
            VmTestCase {
                input: "1 - 2".to_string(),
                expected: vec![Object::Integer(-1)],
            },
            VmTestCase {
                input: "1 * 2".to_string(),
                expected: vec![Object::Integer(2)],
            },
            VmTestCase {
                input: "4 / 2".to_string(),
                expected: vec![Object::Integer(2)],
            },
            VmTestCase {
                input: "50 / 2 * 2 + 10 - 5".to_string(),
                expected: vec![Object::Integer(55)],
            },
            VmTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: vec![Object::Integer(60)],
            },
            VmTestCase {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: vec![Object::Integer(32)],
            },
            VmTestCase {
                input: "5 * 2 + 10".to_string(),
                expected: vec![Object::Integer(20)],
            },
            VmTestCase {
                input: "5 + 2 * 10".to_string(),
                expected: vec![Object::Integer(25)],
            },
            VmTestCase {
                input: "5 * (2 + 10)".to_string(),
                expected: vec![Object::Integer(60)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VmTestCase {
                input: "true".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "false".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 < 2".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "1 > 2".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 < 1".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 > 1".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 == 1".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "1 != 1".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 == 2".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "1 != 2".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "true == true".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "false == false".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "true == false".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "true != false".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "false != true".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "(1 < 2) == true".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "(1 < 2) == false".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "(1 > 2) == true".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "(1 > 2) == false".to_string(),
                expected: vec![Object::Boolean(true)],
            },
        ];

        run_vm_tests(tests);
    }
}
