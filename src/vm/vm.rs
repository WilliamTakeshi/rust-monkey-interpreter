use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::code::code::OpCode;
use crate::compiler::compiler::Bytecode;
use crate::object::buildin::{get_builtin_functions, BUILTINS};
use crate::object::object::Object;
use anyhow::{anyhow, Result};

use super::frame::Frame;

pub const STACK_SIZE: usize = 50;
// pub const STACK_SIZE: usize = 2048;
// pub const GLOBAL_SIZE: usize = 2048;
pub const GLOBAL_SIZE: usize = 50;
// const GLOBAL_SIZE: usize = 65536;
// pub const MAX_FRAMES: usize = 1024;

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
            num_locals: 0,
            num_params: 0,
        };
        let main_closure = Object::Closure {
            fn_obj: Rc::new(main_fn),
            free: vec![],
        };
        let main_frame = Frame::new(main_closure, 0);

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

            frames: vec![Frame::new(
                Object::CompiledFunction {
                    instructions: bytecode.instructions,
                    num_locals: 0,
                    num_params: 0,
                },
                0,
            )],
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

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        self.frames.pop().unwrap()
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

            match op.try_into() {
                Ok(OpCode::OpConstant) => {
                    let const_index = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    self.current_frame().ip += 2;

                    self.push(self.constants[const_index as usize].clone())?;
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
                    let num_args = ins[ip + 1];
                    self.current_frame().ip += 1;

                    self.execute_call(num_args as usize)?;
                }
                Ok(OpCode::OpReturnValue) => {
                    let return_value = self.pop();
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(return_value)?;
                }
                Ok(OpCode::OpReturn) => {
                    let frame = self.pop_frame();
                    self.sp = frame.base_pointer - 1;

                    self.push(Object::Null)?;
                }
                Ok(OpCode::OpSetLocal) => {
                    let local_index = ins[ip + 1];
                    self.current_frame().ip += 1;

                    let frame = self.current_frame().clone();
                    let obj = self.pop();

                    self.stack[frame.base_pointer + local_index as usize] = obj;
                }
                Ok(OpCode::OpGetLocal) => {
                    let local_index = ins[ip + 1];

                    self.current_frame().ip += 1;

                    let frame = self.current_frame().clone();

                    self.push(self.stack[frame.base_pointer + local_index as usize].clone())?;
                }
                Ok(OpCode::OpGetBuiltin) => {
                    let builtin_index = ins[ip + 1];
                    self.current_frame().ip += 1;

                    let buildin_functions = get_builtin_functions();
                    let definition = buildin_functions.get(BUILTINS[builtin_index as usize]);

                    match definition {
                        Some(Object::Buildin(..)) => {
                            self.push(definition.unwrap().clone())?;
                        }
                        _ => return Err(anyhow!("builtin not found")),
                    }
                }
                Ok(OpCode::OpClosure) => {
                    let const_index = u16::from_be_bytes(ins[ip + 1..=ip + 2].try_into().unwrap());
                    let num_free_var = ins[ip + 3];
                    self.current_frame().ip += 3;

                    self.push_closure(const_index, num_free_var)?;
                }
                Ok(OpCode::OpGetFree) => {
                    let free_index = ins[ip + 1];
                    self.current_frame().ip += 1;

                    let current_closure = self.current_frame().cl.clone();

                    match current_closure {
                        Object::Closure { free, .. } => {
                            let free = free[free_index as usize].clone();

                            self.push(free)?;
                        }
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            }
        }
        Ok(())
    }

    fn push_closure(&mut self, const_index: u16, num_free_var: u8) -> Result<()> {
        let obj = self.constants[const_index as usize].clone();
        match obj {
            Object::CompiledFunction { .. } => {
                let free = self.stack[self.sp - num_free_var as usize..self.sp].to_vec();
                self.sp -= num_free_var as usize;
                self.push(Object::Closure {
                    fn_obj: Rc::new(obj),
                    free,
                })?;
            }
            _ => return Err(anyhow!("expected CompiledFunction, got {:?}", obj)),
        }
        Ok(())
    }

    fn execute_call(&mut self, num_args: usize) -> Result<()> {
        let callee = self.stack[self.sp - 1 - num_args].clone();

        match callee {
            Object::Closure { .. } => {
                self.call_closure(callee, num_args)?;
            }
            Object::Buildin(..) => {
                self.call_buildin(callee, num_args)?;
            }
            _ => return Err(anyhow!("calling non-function and non-buildin")),
        }
        Ok(())
    }

    fn call_buildin(&mut self, callee: Object, num_args: usize) -> Result<()> {
        let mut args = vec![];

        for i in 0..num_args {
            args.push(self.stack[self.sp - num_args + i].clone());
        }

        let result = match callee {
            Object::Buildin(_, num_params, f) => {
                if num_params != num_args as u32 {
                    Object::Err(format!(
                        "wrong number of arguments. want={}, got={}",
                        num_params, num_args
                    ))
                } else {
                    f(args)
                }
            }
            _ => return Err(anyhow!("calling non-buildin")),
        };

        self.sp -= num_args + 1;
        self.push(result)?;

        Ok(())
    }

    fn call_closure(&mut self, callee: Object, num_args: usize) -> Result<()> {
        match &callee {
            Object::Closure { fn_obj, .. } => match &**fn_obj {
                Object::CompiledFunction {
                    num_locals,
                    num_params,
                    ..
                } => {
                    if *num_params != num_args as u16 {
                        return Err(anyhow!(
                            "wrong number of arguments. want={}, got={}",
                            num_params,
                            num_args
                        ));
                    }
                    let frame = Frame::new(callee.clone(), self.sp - num_args);
                    self.push_frame(frame.clone());
                    self.sp = frame.base_pointer + *num_locals as usize;
                }
                _ => return Err(anyhow!("Expected CompiledFunction, got {:?}", fn_obj)),
            },
            _ => return Err(anyhow!("Trying to call a non-function")),
        };

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
            _ => Err(anyhow!("unsupported type for OpMinus")),
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
        let right = self.pop();
        let left = self.pop();

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

    fn run_vm_tests_expect_error(tests: Vec<VmTestCase>) {
        for VmTestCase { input, .. } in tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            let mut compiler = Compiler::new();
            let _ = compiler.compile_program(program);

            let mut vm = Vm::new(compiler.bytecode());
            let res = vm.run();

            assert!(res.is_err());
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
            VmTestCase {
                input: "
                    let returnsOneReturner = fn() {
                        let returnsOne = fn() { 1; };
                        returnsOne;
                    };
                    returnsOneReturner()();".to_string(),
                expected: vec![Object::Integer(1)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            VmTestCase {
                input: "let one = fn() { let one = 1; one }; one();".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input:
                    "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo();"
                        .to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "
                let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                oneAndTwo() + threeAndFour();
                "
                .to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "
                let firstFoobar = fn() { let foobar = 50; foobar; };
                let secondFoobar = fn() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();
                "
                .to_string(),
                expected: vec![Object::Integer(150)],
            },
            VmTestCase {
                input: "
                let globalSeed = 50;
                let minusOne = fn() {
                let num = 1;
                globalSeed - num;
                }
                let minusTwo = fn() {
                let num = 2;
                globalSeed - num;
                }
                minusOne() + minusTwo();
                "
                .to_string(),
                expected: vec![Object::Integer(97)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = vec![
            VmTestCase {
                input: "
                let identity = fn(a) { a; };
                identity(4);"
                    .to_string(),
                expected: vec![Object::Integer(4)],
            },
            VmTestCase {
                input: "
                let sum = fn(a, b) { a + b; };
                sum(1, 2);"
                    .to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "
                let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2);"
                    .to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: "
                let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2) + sum(3, 4);"
                    .to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "
                let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                let outer = fn() {
                    sum(1, 2) + sum(3, 4);
                };
                outer();"
                    .to_string(),
                expected: vec![Object::Integer(10)],
            },
            VmTestCase {
                input: "
                let globalNum = 10;
                let sum = fn(a, b) {
                    let c = a + b;
                    c + globalNum;
                };
                let outer = fn() {
                    sum(1, 2) + sum(3, 4) + globalNum;
                };
                outer() + globalNum;"
                    .to_string(),
                expected: vec![Object::Integer(50)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        let tests = vec![
            VmTestCase {
                input: "fn() { 1; }(1);".to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: "fn(a) { a; }();".to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: "fn(a, b) { a + b; }(1);".to_string(),
                expected: vec![Object::Null],
            },
        ];

        run_vm_tests_expect_error(tests);
    }

    #[test]
    fn test_builtins_functions() {
        let tests = vec![
            VmTestCase {
                input: r#"len("");"#.to_string(),
                expected: vec![Object::Integer(0)],
            },
            VmTestCase {
                input: r#"len("four");"#.to_string(),
                expected: vec![Object::Integer(4)],
            },
            VmTestCase {
                input: r#"len("hello world");"#.to_string(),
                expected: vec![Object::Integer(11)],
            },
            VmTestCase {
                input: r#"len(1);"#.to_string(),
                expected: vec![Object::Err(
                    "argument to 'len' not supported, got INTEGER".to_string(),
                )],
            },
            VmTestCase {
                input: r#"len("", "");"#.to_string(),
                expected: vec![Object::Err(
                    "wrong number of arguments. want=1, got=2".to_string(),
                )],
            },
            VmTestCase {
                input: r#"len([1, 2, 3])"#.to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: r#"len([])"#.to_string(),
                expected: vec![Object::Integer(0)],
            },
            VmTestCase {
                input: r#"puts("hello", "world")"#.to_string(),
                expected: vec![Object::Err(
                    "wrong number of arguments. want=1, got=2".to_string(),
                )],
            },
            VmTestCase {
                input: r#"first([1,2,3])"#.to_string(),
                expected: vec![Object::Integer(1)],
            },
            VmTestCase {
                input: r#"first([])"#.to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: r#"first(1)"#.to_string(),
                expected: vec![Object::Err(
                    "argument to 'first' not supported, got INTEGER".to_string(),
                )],
            },
            VmTestCase {
                input: r#"last(1)"#.to_string(),
                expected: vec![Object::Err(
                    "argument to 'last' not supported, got INTEGER".to_string(),
                )],
            },
            VmTestCase {
                input: r#"last([1,2,3])"#.to_string(),
                expected: vec![Object::Integer(3)],
            },
            VmTestCase {
                input: r#"last([])"#.to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: r#"rest([1,2,3])"#.to_string(),
                expected: vec![Object::Array(vec![Object::Integer(2), Object::Integer(3)])],
            },
            VmTestCase {
                input: r#"rest([])"#.to_string(),
                expected: vec![Object::Null],
            },
            VmTestCase {
                input: r#"push([], 1)"#.to_string(),
                expected: vec![Object::Array(vec![Object::Integer(1)])],
            },
            VmTestCase {
                input: r#"push(1, 1)"#.to_string(),
                expected: vec![Object::Err(
                    "argument to 'push' not supported, got INTEGER".to_string(),
                )],
            },
        ];

        run_vm_tests(tests);
    }
    #[test]
    fn test_closures() {
        let tests = vec![
            VmTestCase {
                input: "
                let newClosure = fn(a) {
                    fn() { a; };
                };
                let closure = newClosure(99);
                closure();"
                    .to_string(),
                expected: vec![Object::Integer(99)],
            },
            VmTestCase {
                input: "
                let newAdder = fn(a, b) {
                    fn(c) { a + b + c };
                };
                let adder = newAdder(1, 2);
                adder(8);"
                    .to_string(),
                expected: vec![Object::Integer(11)],
            },
            VmTestCase {
                input: "
                let newAdder = fn(a, b) {
                    let c = a + b;
                    fn(d) { c + d };
                };
                let adder = newAdder(1, 2);
                adder(8);"
                    .to_string(),
                expected: vec![Object::Integer(11)],
            },
            VmTestCase {
                input: "
                let newAdderOuter = fn(a, b) {
                    let c = a + b;
                    fn(d) {
                        let e = d + c;
                        fn(f) { e + f; };
                    };
                };
                let newAdderInner = newAdderOuter(1, 2)
                let adder = newAdderInner(3);
                adder(8);"
                    .to_string(),
                expected: vec![Object::Integer(14)],
            },
            VmTestCase {
                input: "
                let a = 1;
                let newAdderOuter = fn(b) {
                    fn(c) {
                        fn(d) { a + b + c + d };
                    };
                };
                let newAdderInner = newAdderOuter(2)
                let adder = newAdderInner(3);
                adder(8);"
                    .to_string(),
                expected: vec![Object::Integer(14)],
            },
            VmTestCase {
                input: "
                let newClosure = fn(a, b) {
                    let one = fn() { a; };
                    let two = fn() { b; };
                    fn() { one() + two(); };
                };
                let closure = newClosure(9, 90);
                closure();"
                    .to_string(),
                expected: vec![Object::Integer(99)],
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_recursive_fibonacci() {
        let tests = vec![VmTestCase {
            input: "
                let fibonacci = fn(x) {
                    if (x == 0) {
                        return 0;
                    } else {
                        if (x == 1) {
                        return 1;
                    } else {
                        fibonacci(x - 1) + fibonacci(x - 2);
                    }
                    }
                };
                fibonacci(15);"
                .to_string(),
            expected: vec![Object::Integer(610)],
        }];

        run_vm_tests(tests);
    }
}
