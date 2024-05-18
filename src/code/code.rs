use std::convert::TryFrom;

pub type Instructions = Vec<u8>;

pub enum OpCode {
    OpConstant = 0,
    OpAdd = 1,
}

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
        // 2 => {
        //     format!("{} {} {}", def.name, operands[0], operands[1])
        // }
        _ => format!("ERROR: unhandled operand_count for {}", def.name),
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == OpCode::OpConstant as u8 => Ok(OpCode::OpConstant),
            x if x == OpCode::OpAdd as u8 => Ok(OpCode::OpAdd),
            _ => Err(()),
        }
    }
}

impl OpCode {
    fn to_definition(&self) -> Definition {
        match self {
            &OpCode::OpConstant => Definition {
                name: String::from("OpConstant"),
                operand_widths: vec![2],
            },
            &OpCode::OpAdd => Definition {
                name: String::from("OpAdd"),
                operand_widths: vec![],
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
                    make(OpCode::OpConstant, vec![2]),
                    make(OpCode::OpConstant, vec![65535]),
                ],
                "0000 OpAdd
0001 OpConstant 2
0004 OpConstant 65535\n"
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