use std::convert::TryFrom;

pub type Instructions = Vec<u8>;

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    OpConstant = 0,
    OpAdd = 1,
    OpPop = 2,
    OpSub = 3,
    OpMult = 4,
    OpDiv = 5,
    OpTrue = 6,
    OpFalse = 7,
    OpEqual = 8,
    OpNotEqual = 9,
    OpGreaterThan = 10,
    OpMinus = 11,
    OpBang = 12,
    OpJumpNotTruthy = 13,
    OpJump = 14,
    OpNull = 15,
    OpGetGlobal = 16,
    OpSetGlobal = 17,
    OpArray = 18,
    OpHash = 19,
    OpIndex = 20,
    OpCall = 21,
    OpReturnValue = 22,
    OpReturn = 23,
    OpGetLocal = 24,
    OpSetLocal = 25,
    OpGetBuiltin = 26,
    OpClosure = 27,
    OpGetFree = 28,
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == OpCode::OpConstant as u8 => Ok(OpCode::OpConstant),
            x if x == OpCode::OpAdd as u8 => Ok(OpCode::OpAdd),
            x if x == OpCode::OpPop as u8 => Ok(OpCode::OpPop),
            x if x == OpCode::OpSub as u8 => Ok(OpCode::OpSub),
            x if x == OpCode::OpMult as u8 => Ok(OpCode::OpMult),
            x if x == OpCode::OpDiv as u8 => Ok(OpCode::OpDiv),
            x if x == OpCode::OpTrue as u8 => Ok(OpCode::OpTrue),
            x if x == OpCode::OpFalse as u8 => Ok(OpCode::OpFalse),
            x if x == OpCode::OpEqual as u8 => Ok(OpCode::OpEqual),
            x if x == OpCode::OpNotEqual as u8 => Ok(OpCode::OpNotEqual),
            x if x == OpCode::OpGreaterThan as u8 => Ok(OpCode::OpGreaterThan),
            x if x == OpCode::OpMinus as u8 => Ok(OpCode::OpMinus),
            x if x == OpCode::OpBang as u8 => Ok(OpCode::OpBang),
            x if x == OpCode::OpJumpNotTruthy as u8 => Ok(OpCode::OpJumpNotTruthy),
            x if x == OpCode::OpJump as u8 => Ok(OpCode::OpJump),
            x if x == OpCode::OpNull as u8 => Ok(OpCode::OpNull),
            x if x == OpCode::OpGetGlobal as u8 => Ok(OpCode::OpGetGlobal),
            x if x == OpCode::OpSetGlobal as u8 => Ok(OpCode::OpSetGlobal),
            x if x == OpCode::OpArray as u8 => Ok(OpCode::OpArray),
            x if x == OpCode::OpHash as u8 => Ok(OpCode::OpHash),
            x if x == OpCode::OpIndex as u8 => Ok(OpCode::OpIndex),
            x if x == OpCode::OpCall as u8 => Ok(OpCode::OpCall),
            x if x == OpCode::OpReturnValue as u8 => Ok(OpCode::OpReturnValue),
            x if x == OpCode::OpReturn as u8 => Ok(OpCode::OpReturn),
            x if x == OpCode::OpGetLocal as u8 => Ok(OpCode::OpGetLocal),
            x if x == OpCode::OpSetLocal as u8 => Ok(OpCode::OpSetLocal),
            x if x == OpCode::OpGetBuiltin as u8 => Ok(OpCode::OpGetBuiltin),
            x if x == OpCode::OpClosure as u8 => Ok(OpCode::OpClosure),
            x if x == OpCode::OpGetFree as u8 => Ok(OpCode::OpGetFree),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub struct Definition {
    name: String,
    operand_widths: Vec<u8>,
}

pub fn string(instructions: Instructions) -> String {
    let mut i = 0;
    let mut out = "".to_string();
    while i < instructions.len() {
        let def = OpCode::try_from(instructions[i]);

        if let Ok(opcode) = def {
            let (operands, read_bytes) =
                read_operands(opcode.to_definition(), instructions[i + 1..].to_vec());
            let fmt_instruction =
                format_instruction(&instructions, opcode.to_definition(), operands);
            out.push_str(format!("{:04} {}\n", i, fmt_instruction).as_str());
            i += 1 + read_bytes;
        } else {
            return format!("ERROR: unknown opcode {}", instructions[i]);
        }
    }
    out
}

pub fn format_instruction(
    instruction: &Instructions,
    def: Definition,
    operands: Vec<u16>,
) -> String {
    let operand_count = def.operand_widths.len();
    if operands.len() != operand_count {
        return format!(
            "ERROR: instruction has wrong number of operands. got={}, want={}",
            instruction.len(),
            operand_count
        );
    }

    match operand_count {
        0 => def.name,
        1 => {
            format!("{} {}", def.name, operands[0])
        }
        2 => {
            format!("{} {} {}", def.name, operands[0], operands[1])
        }
        _ => format!("ERROR: unhandled operand_count for {}", def.name),
    }
}

impl OpCode {
    fn to_definition(&self) -> Definition {
        match *self {
            OpCode::OpConstant => Definition {
                name: String::from("OpConstant"),
                operand_widths: vec![2],
            },
            OpCode::OpAdd => Definition {
                name: String::from("OpAdd"),
                operand_widths: vec![],
            },
            OpCode::OpPop => Definition {
                name: String::from("OpPop"),
                operand_widths: vec![],
            },
            OpCode::OpSub => Definition {
                name: String::from("OpSub"),
                operand_widths: vec![],
            },
            OpCode::OpMult => Definition {
                name: String::from("OpMult"),
                operand_widths: vec![],
            },
            OpCode::OpDiv => Definition {
                name: String::from("OpDiv"),
                operand_widths: vec![],
            },
            OpCode::OpTrue => Definition {
                name: String::from("OpTrue"),
                operand_widths: vec![],
            },
            OpCode::OpFalse => Definition {
                name: String::from("OpFalse"),
                operand_widths: vec![],
            },
            OpCode::OpEqual => Definition {
                name: String::from("OpEqual"),
                operand_widths: vec![],
            },
            OpCode::OpNotEqual => Definition {
                name: String::from("OpNotEqual"),
                operand_widths: vec![],
            },
            OpCode::OpGreaterThan => Definition {
                name: String::from("OpGreaterThan"),
                operand_widths: vec![],
            },
            OpCode::OpMinus => Definition {
                name: String::from("OpMinus"),
                operand_widths: vec![],
            },
            OpCode::OpBang => Definition {
                name: String::from("OpBang"),
                operand_widths: vec![],
            },
            OpCode::OpJumpNotTruthy => Definition {
                name: String::from("OpJumpNotTruthy"),
                operand_widths: vec![2],
            },
            OpCode::OpJump => Definition {
                name: String::from("OpJump"),
                operand_widths: vec![2],
            },
            OpCode::OpNull => Definition {
                name: String::from("OpNull"),
                operand_widths: vec![],
            },
            OpCode::OpSetGlobal => Definition {
                name: String::from("OpSetGlobal"),
                operand_widths: vec![2],
            },
            OpCode::OpGetGlobal => Definition {
                name: String::from("OpGetGlobal"),
                operand_widths: vec![2],
            },
            OpCode::OpArray => Definition {
                name: String::from("OpArray"),
                operand_widths: vec![2],
            },
            OpCode::OpHash => Definition {
                name: String::from("OpHash"),
                operand_widths: vec![2],
            },
            OpCode::OpIndex => Definition {
                name: String::from("OpIndex"),
                operand_widths: vec![],
            },
            OpCode::OpCall => Definition {
                name: String::from("OpCall"),
                operand_widths: vec![1],
            },
            OpCode::OpReturnValue => Definition {
                name: String::from("OpReturnValue"),
                operand_widths: vec![],
            },
            OpCode::OpReturn => Definition {
                name: String::from("OpReturn"),
                operand_widths: vec![],
            },
            OpCode::OpGetLocal => Definition {
                name: String::from("OpGetLocal"),
                operand_widths: vec![1],
            },
            OpCode::OpSetLocal => Definition {
                name: String::from("OpSetLocal"),
                operand_widths: vec![1],
            },
            OpCode::OpGetBuiltin => Definition {
                name: String::from("OpGetBuiltin"),
                operand_widths: vec![1],
            },
            OpCode::OpClosure => Definition {
                name: String::from("OpClosure"),
                operand_widths: vec![2, 1],
            },
            OpCode::OpGetFree => Definition {
                name: String::from("OpGetFree"),
                operand_widths: vec![1],
            },
        }
    }
}

pub fn read_operands(def: Definition, instructions: Instructions) -> (Vec<u16>, usize) {
    let mut operands = vec![];
    let mut offset = 0;

    for width in def.operand_widths {
        match width {
            2 => {
                let operand = u16::from_be_bytes([instructions[offset], instructions[offset + 1]]);
                operands.push(operand);
            }
            1 => {
                let operand = instructions[offset];
                operands.push(operand as u16);
            }
            _ => unimplemented!(),
        }

        offset += width as usize;
    }

    (operands, offset)
}

pub fn make(op: OpCode, operands: Vec<u16>) -> Vec<u8> {
    let def = op.to_definition();
    let mut instruction = vec![op as u8];

    for (idx, operand) in operands.iter().enumerate() {
        let width = def.operand_widths[idx];

        match width {
            2 => {
                instruction.push((operand >> 8) as u8);
                instruction.push(*operand as u8);
            }
            1 => {
                instruction.push(*operand as u8);
            }
            _ => unimplemented!(),
        }
    }

    instruction
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let tests = [
            (
                OpCode::OpConstant,
                vec![65534],
                vec![OpCode::OpConstant as u8, 255, 254],
            ),
            (OpCode::OpAdd, vec![], vec![OpCode::OpAdd as u8]),
            (
                OpCode::OpGetLocal,
                vec![255],
                vec![OpCode::OpGetLocal as u8, 255],
            ),
            (
                OpCode::OpClosure,
                vec![65534, 255],
                vec![OpCode::OpClosure as u8, 255, 254, 255],
            ),
        ];

        for (op, operands, expected) in tests {
            let instructions = make(op, operands);

            assert_eq!(instructions.len(), expected.len());

            for i in 0..instructions.len() {
                assert_eq!(instructions[i], expected[i])
            }
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = [
            (OpCode::OpConstant, vec![1], 2),
            (OpCode::OpConstant, vec![65535], 2),
            (OpCode::OpGetLocal, vec![255], 1),
            (OpCode::OpClosure, vec![65534, 255], 3),
        ];

        for (op, operands, bytes_read) in tests {
            let def = op.to_definition();
            let instruction = make(op, operands.clone());

            let (operands_read, n) = read_operands(def, instruction[1..].to_vec());

            assert_eq!(n, bytes_read);
            assert_eq!(operands, operands_read);
        }
    }

    #[test]
    fn test_instructions_string() {
        let tests: Vec<(Vec<Instructions>, String)> = vec![
            (
                vec![
                    make(OpCode::OpConstant, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![65535]),
                ],
                "0000 OpConstant 1
0003 OpConstant 2
0006 OpConstant 65535\n"
                    .to_string(),
            ),
            (
                vec![
                    make(OpCode::OpAdd, vec![]),
                    make(OpCode::OpGetLocal, vec![1]),
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![65535]),
                    make(OpCode::OpClosure, vec![65535, 255]),
                ],
                "0000 OpAdd
0001 OpGetLocal 1
0003 OpConstant 2
0006 OpConstant 65535
0009 OpClosure 65535 255\n"
                    .to_string(),
            ),
        ];

        for (instructions, expected) in tests {
            let mut concated = vec![];

            for inst in instructions {
                concated.extend(inst);
            }

            assert_eq!(string(concated), expected);
        }
    }
}
