use crate::ast::ast::Expression;

use super::ast::{Program, Statement};

pub type ModifyFn<'a> = &'a dyn Fn(Expression) -> Expression;

pub fn modify_program(program: Program, func: ModifyFn) -> Program {
    modify_statements(program, func)
}

pub fn modify_statements(statements: Vec<Statement>, func: ModifyFn) -> Vec<Statement> {
    let mut new_statements = vec![];
    for statement in statements {
        new_statements.push(modify_statement(statement, func))
    }

    new_statements
}

pub fn modify_statement(statement: Statement, func: ModifyFn) -> Statement {
    match statement {
        Statement::ExpressionStmt(expr) => Statement::ExpressionStmt(modify_expr(expr, func)),
        Statement::Let(ident, expr) => Statement::Let(ident, modify_expr(expr, func)),
        Statement::Return(expr) => Statement::Return(modify_expr(expr, func)),
    }
}

pub fn modify_exprs(exprs: Vec<Expression>, func: ModifyFn) -> Vec<Expression> {
    let mut new_exprs = vec![];

    for expr in exprs {
        new_exprs.push(modify_expr(expr, func));
    }

    new_exprs
}

pub fn modify_expr(expr: Expression, func: ModifyFn) -> Expression {
    match expr {
        Expression::IntLiteral(_) => func(expr),
        Expression::Boolean(_) => func(expr),
        Expression::StringLiteral(_) => func(expr),
        Expression::InfixExpr(op, left, right) => Expression::InfixExpr(
            op,
            Box::from(modify_expr(*left, func)),
            Box::from(modify_expr(*right, func)),
        ),
        Expression::PrefixExpr(op, right) => {
            Expression::PrefixExpr(op, Box::from(modify_expr(*right, func)))
        }
        Expression::IndexExpr(left, index) => Expression::IndexExpr(
            Box::from(modify_expr(*left, func)),
            Box::from(modify_expr(*index, func)),
        ),
        Expression::IfExpr(condition, consequence, alternative) => {
            let condition = modify_expr(*condition, func);
            let consequence = modify_statements(consequence, func);
            let alternative = modify_statements(alternative, func);

            Expression::IfExpr(Box::from(condition), consequence, alternative)
        }
        Expression::FnLiteral(parameters, body) => Expression::FnLiteral(
            modify_exprs(parameters, func),
            modify_statements(body, func),
        ),
        Expression::ArrayLiteral(exprs) => Expression::ArrayLiteral(modify_exprs(exprs, func)),
        Expression::HashLiteral(pairs) => {
            let mut new_pairs = vec![];

            for (key, val) in pairs {
                new_pairs.push((modify_expr(key, func), modify_expr(val, func)));
            }

            Expression::HashLiteral(new_pairs)
        }

        _ => func(expr),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::ast::{Infix, Prefix};

    use super::*;

    #[test]
    fn test_modify() {
        let one = { || Expression::IntLiteral(1) };
        let two = { || Expression::IntLiteral(2) };

        let turn_one_into_two = |expr| {
            if let Expression::IntLiteral(num) = expr {
                if num != 1 {
                    return expr;
                }

                return Expression::IntLiteral(2);
            }
            expr
        };

        let tests = vec![
            (one(), two()),
            (
                Expression::InfixExpr(Infix::Plus, Box::from(one()), Box::from(two())),
                Expression::InfixExpr(Infix::Plus, Box::from(two()), Box::from(two())),
            ),
            (
                Expression::InfixExpr(Infix::Plus, Box::from(two()), Box::from(one())),
                Expression::InfixExpr(Infix::Plus, Box::from(two()), Box::from(two())),
            ),
            (
                Expression::PrefixExpr(Prefix::Minus, Box::from(one())),
                Expression::PrefixExpr(Prefix::Minus, Box::from(two())),
            ),
            (
                Expression::IndexExpr(Box::from(one()), Box::from(one())),
                Expression::IndexExpr(Box::from(two()), Box::from(two())),
            ),
            (
                Expression::IfExpr(
                    Box::from(one()),
                    vec![Statement::ExpressionStmt(one())],
                    vec![Statement::ExpressionStmt(one())],
                ),
                Expression::IfExpr(
                    Box::from(two()),
                    vec![Statement::ExpressionStmt(two())],
                    vec![Statement::ExpressionStmt(two())],
                ),
            ),
            (
                Expression::IndexExpr(Box::from(one()), Box::from(one())),
                Expression::IndexExpr(Box::from(two()), Box::from(two())),
            ),
            (
                Expression::FnLiteral(
                    vec![Expression::Ident(String::from("value"))],
                    vec![Statement::ExpressionStmt(one())],
                ),
                Expression::FnLiteral(
                    vec![Expression::Ident(String::from("value"))],
                    vec![Statement::ExpressionStmt(two())],
                ),
            ),
            (
                Expression::ArrayLiteral(vec![one(), one()]),
                Expression::ArrayLiteral(vec![two(), two()]),
            ),
            (
                Expression::HashLiteral(vec![(one(), one())]),
                Expression::HashLiteral(vec![(two(), two())]),
            ),
        ];

        let tests_program: Vec<(Program, Program)> = vec![
            (
                vec![Statement::ExpressionStmt(one())],
                vec![Statement::ExpressionStmt(two())],
            ),
            (
                vec![Statement::Return(one())],
                vec![Statement::Return(two())],
            ),
            (
                vec![Statement::Let(String::from("value"), one())],
                vec![Statement::Let(String::from("value"), two())],
            ),
        ];

        for (input, expected) in tests {
            let modified = modify_expr(input, &turn_one_into_two);
            assert_eq!(modified, expected);
        }

        for (input, expected) in tests_program {
            let modified = modify_program(input, &turn_one_into_two);
            assert_eq!(modified, expected);
        }
    }
}
