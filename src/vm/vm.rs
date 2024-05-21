use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::code::code::string;
use crate::code::code::{Instructions, OpCode};
use crate::compiler::compiler::Bytecode;
use crate::object::object::Object;
use anyhow::{anyhow, Result};

use super::frame::Frame;

pub const STACK_SIZE: usize = 50;
// pub const STACK_SIZE: usize = 2048;
// pub const GLOBAL_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 50;
// const GLOBAL_SIZE: usize = 65536;
pub const MAX_FRAMES: usize = 1024;

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
pub const NULL: Object = Object::Null;

// TODO: Fix overflow when using GLOBAL_SIZE = 65536 (16 bits)
#[derive(Debug)]
pub struct Vm {
    constants: Vec<Object>,

    stack: [Object; STACK_SIZE],
    sp: usize,
    globals: Rc<RefCell<[Object; GLOBAL_SIZE]>>,

    frames: Vec<Frame>,
    frames_index: usize,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Self {
        let main_fn = Object::CompiledFunction {
            instructions: bytecode.instructions,
        };
        let main_frame = Frame::new(main_fn);

        let frames = vec![main_frame];

        Self {
            constants: bytecode.constants,

            stack: [NULL; STACK_SIZE],
            sp: 0,
            globals: Rc::new(RefCell::new([NULL; GLOBAL_SIZE])),

            frames,
            frames_index: 1,
        }
    }

    pub fn new_with_global_store(
        bytecode: Bytecode,
        s: Rc<RefCell<[Object; GLOBAL_SIZE]>>,
    ) -> Self {
        Self {
            constants: bytecode.constants,

            stack: [NULL; STACK_SIZE],
            sp: 0,
            globals: s,

            frames: vec![Frame::new(Object::CompiledFunction {
                instructions: bytecode.instructions,
            })],
            frames_index: 1,
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.frames_index - 1]
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frames_index += 1;
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
        self.frames_index -= 1;
    }

    pub fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            return &Object::Null;
        }
        &self.stack[self.sp - 1]
    }

    pub fn run(&mut self) -> Result<()> {
        while self.current_frame().ip == usize::max_value()
            || self.current_frame().ip < self.current_frame().instructions().len() - 1
        {
            // Weird fix, I am using a usize::max_value() to indicate that the frame is new (-1)
            self.current_frame().ip = self.current_frame().ip.wrapping_add(1);

            let ip = self.current_frame().ip;
            let ins = self.current_frame().instructions();
            let op = ins[ip];

            dbg!(&ip);
            dbg!(op);

            match op.try_into() {
                Ok(OpCode::OpConstant) => {
                    let const_index = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

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
                Ok(OpCode::OpBang) => {
                    self.execute_bang_operator()?;
                }
                Ok(OpCode::OpMinus) => {
                    self.execute_minus_operator()?;
                }
                Ok(OpCode::OpJump) => {
                    let pos = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip = (pos - 1) as usize;
                }
                Ok(OpCode::OpJumpNotTruthy) => {
                    let pos = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

                    let condition = self.pop();
                    if !Self::is_truthy(condition) {
                        self.current_frame().ip = (pos - 1) as usize;
                    }
                }
                Ok(OpCode::OpTrue) => self.push(TRUE)?,
                Ok(OpCode::OpFalse) => self.push(FALSE)?,
                Ok(OpCode::OpNull) => self.push(Object::Null)?,
                Ok(OpCode::OpPop) => {
                    self.pop();
                }
                Ok(OpCode::OpSetGlobal) => {
                    let global_index = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

                    let obj = self.pop();
                    let mut globals = self.globals.borrow_mut();
                    globals[global_index as usize] = obj
                }
                Ok(OpCode::OpGetGlobal) => {
                    let global_index = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

                    let obj: Object;
                    {
                        let globals = self.globals.borrow();
                        obj = globals[global_index as usize].clone();
                    }

                    self.push(obj)?;
                }
                Ok(OpCode::OpArray) => {
                    let num_elements = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

                    let array = self.build_array(self.sp - num_elements as usize, self.sp);

                    self.sp -= num_elements as usize;

                    self.push(array)?;
                }
                Ok(OpCode::OpHash) => {
                    let num_elements =
                        u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap()) as usize;
                    self.current_frame().ip += 2;

                    let hash = self.build_hash(self.sp - num_elements, self.sp);

                    self.sp -= num_elements;

                    self.push(Object::Hash(hash))?;
                }
                Ok(OpCode::OpIndex) => {
                    let index = self.pop();
                    let left = self.pop();

                    self.execute_index_expression(left, index)?;
                }
                Ok(OpCode::OpCall) => {
                    let function = self.stack[self.sp - 1].clone();

                    match &function {
                        Object::CompiledFunction { instructions } => {
                            let frame = Frame::new(function);
                            self.push_frame(frame);
                        }
                        _ => return Err(anyhow!("Trying to call a non-function")),
                    };
                }
                Ok(OpCode::OpReturnValue) => {
                    let return_value = self.pop();
                    self.pop_frame();
                    self.pop();

                    self.push(return_value)?;
                }
                Ok(OpCode::OpReturn) => {
                    self.pop_frame();
                    self.pop();

                    self.push(Object::Null)?;
                }
                _ => todo!(),
            }
        }
        Ok(())
    }

    fn execute_index_expression(&mut self, left: Object, index: Object) -> Result<()> {
        let left_type = left.obj_type();
        match (left, index.clone()) {
            (Object::Array(elements), Object::Integer(idx)) => {
                let idx = idx as usize;
                if idx >= elements.len() {
                    self.push(Object::Null)?;
                } else {
                    self.push(elements[idx].clone())?;
                }
            }
            (Object::Hash(hash), idx) => {
                let value = hash.get(&idx);
                if let Some(value) = value {
                    self.push(value.clone())?;
                } else {
                    self.push(Object::Null)?;
                }
            }
            _ => {
                return Err(anyhow!(
                    "index operator not supported: {:?} {:?}",
                    left_type,
                    index
                ))
            }
        }
        Ok(())
    }

    fn build_hash(&self, start_index: usize, end_index: usize) -> HashMap<Object, Object> {
        let mut hash = HashMap::new();

        for i in (start_index..end_index).step_by(2) {
            let key = self.stack[i].clone();
            let value = self.stack[i + 1].clone();

            hash.insert(key, value);
        }

        hash
    }

    fn build_array(&self, start_index: usize, end_index: usize) -> Object {
        let mut elements = vec![];

        for i in start_index..end_index {
            elements.push(self.stack[i].clone());
        }

        Object::Array(elements)
    }

    fn is_truthy(obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean(b) => b,
            _ => true,
        }
    }

    fn execute_minus_operator(&mut self) -> Result<()> {
        let operand = self.pop();
        match operand {
            Object::Integer(operand) => self.push(Object::Integer(-operand)),
            _ => return Err(anyhow!("unsupported type for OpMinus")),
        }
    }

    fn execute_bang_operator(&mut self) -> Result<()> {
        let operand = self.pop();
        match operand {
            Object::Boolean(true) => self.push(FALSE),
            Object::Boolean(false) => self.push(TRUE),
            Object::Null => self.push(TRUE),
            _ => self.push(FALSE),
        }
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
        match op {
            OpCode::OpEqual => self.push(Object::Boolean(left == right)),
            OpCode::OpNotEqual => self.push(Object::Boolean(left != right)),
            OpCode::OpGreaterThan => self.push(Object::Boolean(left > right)),
            _ => Err(anyhow!("Unknown operator: {:?}", op)),
        }
    }

    fn execute_binary_operation(&mut self, op: OpCode) -> Result<()> {
        dbg!("aaaaa");

        let right = self.pop();
        let left = self.pop();

        dbg!(&op, &left, &right);

        match (left, right) {
            (Object::Integer(left), Object::Integer(right)) => {
                self.execute_binary_integer_operation(op, left, right)?;
            }
            (Object::String(left), Object::String(right)) => {
                self.execute_binary_string_operation(op, left, right)?;
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

    fn execute_binary_string_operation(
        &mut self,
        op: OpCode,
        left: String,
        right: String,
    ) -> Result<()> {
        match op {
            OpCode::OpAdd => self.push(Object::String(format!("{}{}", left, right))),
            _ => Err(anyhow!("unknown string operator: {:?}", op)),
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
    use std::collections::HashMap;

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
            let _ = compiler.compile_program(program);

            let mut vm = Vm::new(compiler.bytecode());
            let res = vm.run();

            res.unwrap();

            // assert!(false);

            let stack_element = vm.last_popped_stack_elem();
            assert_eq!(stack_element, expected[0]);
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
            VmTestCase {
                input: "-5".to_string(),
                expected: vec![Object::Integer(-5)],
            },
            VmTestCase {
                input: "-10".to_string(),
                expected: vec![Object::Integer(-10)],
            },
            VmTestCase {
                input: "-50 + 100 + -50".to_string(),
                expected: vec![Object::Integer(0)],
            },
            VmTestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: vec![Object::Integer(50)],
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
            VmTestCase {
                input: "!true".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "!false".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "!5".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "!!true".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "!!false".to_string(),
                expected: vec![Object::Boolean(false)],
            },
            VmTestCase {
                input: "!!5".to_string(),
                expected: vec![Object::Boolean(true)],
            },
            VmTestCase {
                input: "!(if (false) { 5; })".to_string(),
                expected: vec![Object::Boolean(true)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestCase {
                input: "if (true) { 10 }".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "if (true) { 10 } else { 20 }".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "if (false) { 10 } else { 20 }".to_string(),
                expected: vec![Object::Integer(20)],
            },
            VmTestCase {
                input: "if (1) { 10 }".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "if (1 < 2) { 10 }".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: vec![Object::Integer(20)],
            },
            VmTestCase {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "if (false) { 10 }".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "if ((if (false) { 10 })) { 10 } else { 20 }".to_string(),
                expected: vec![Object::Integer(20)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VmTestCase {
                input: "let one = 1; one".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: "let one = 1; let two = 2; one + two".to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "let one = 1; let two = one + one; one + two;".to_string(),
                expected: vec![Object::Integer(3)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestCase {
                input: r#""monkey""#.to_string(),
                expected: vec![Object::String("monkey".to_string())],
            },
            VmTestCase {
                input: r#""mon"+"key""#.to_string(),
                expected: vec![Object::String("monkey".to_string())],
            },
            VmTestCase {
                input: r#""mon"+"key"+"banana""#.to_string(),
                expected: vec![Object::String("monkeybanana".to_string())],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestCase {
                input: "[]".to_string(),
                expected: vec![Object::Array(vec![])],
            },
            VmTestCase {
                input: "[1, 2, 3]".to_string(),
                expected: vec![Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ])],
            },
            VmTestCase {
                input: "[1 + 2, 3 * 4, 5 + 6]".to_string(),
                expected: vec![Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ])],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            VmTestCase {
                input: "{}".to_string(),
                expected: vec![Object::Hash(HashMap::new())],
            },
            VmTestCase {
                input: "{1: 2, 2: 3}".to_string(),
                expected: vec![Object::Hash(HashMap::from([
                    (Object::Integer(1), Object::Integer(2)),
                    (Object::Integer(2), Object::Integer(3)),
                ]))],
            },
            VmTestCase {
                input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}".to_string(),
                expected: vec![Object::Hash(HashMap::from([
                    (Object::Integer(2), Object::Integer(4)),
                    (Object::Integer(6), Object::Integer(16)),
                ]))],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            VmTestCase {
                input: "[1, 2, 3][1]".to_string(),
                expected: vec![Object::Integer(2)],
            },
            VmTestCase {
                input: "[1, 2, 3][0 + 2]".to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "[[1, 1, 1]][0][0]".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: "[][0]".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "[1, 2, 3][99]".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "[1][-1]".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[1]".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: "{1: 1, 2: 2}[2]".to_string(),
                expected: vec![Object::Integer(2)],
            },
            VmTestCase {
                input: "{1: 1}[0]".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "{}[0]".to_string(),
                expected: vec![Object::Null],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_without_arguments() {
        let tests = vec![
            VmTestCase {
                input: "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen();".to_string(),
                expected: vec![Object::Integer(15)],
            },
            VmTestCase {
                input: "let one = fn() { 1; }; let two = fn() { 2; }; one() + two();".to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "let a = fn() { 1; }; let b = fn() { a() + 1; }; let c = fn() { b() + 1; }; c();".to_string(),
                expected: vec![Object::Integer(3)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_with_return_statement() {
        let tests = vec![
            VmTestCase {
                input: "let earlyExit = fn() { return 99; 100; }; earlyExit();".to_string(),
                expected: vec![Object::Integer(99)],
            },
            VmTestCase {
                input: "let earlyExit = fn() { return 99; return 100; }; earlyExit();".to_string(),
                expected: vec![Object::Integer(99)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = vec![
            VmTestCase {
                input: "let noReturn = fn() { }; noReturn();".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn(); }; noReturn(); 1; noReturnTwo();".to_string(),
                expected: vec![Object::Null, Object::Null],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_first_class_functions() {
        let tests = vec![
            VmTestCase {
                input: "let returnsOne = fn() { 1; }; let returnsOneReturner = fn() { returnsOne; }; returnsOneReturner()();".to_string(),
                expected: vec![Object::Integer(1)],
            },
        ];

        run_vm_tests(tests);
    }
}
