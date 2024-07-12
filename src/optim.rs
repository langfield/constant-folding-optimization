// A constant-folding optimization

use crate::ast::*;

pub fn eval(op: Operator, m: u8, n: u8) -> u8 {
    match op {
        Operator::Add => m + n,
        Operator::Subtract => m - n,
        Operator::Multiply => m * n,
        Operator::Divide=> m / n,
    }
}

pub fn fold_val(v: Value) -> Value {
    match v {
        Value::Expression(e) =>
            match fold_expr(*e) {
                Expression::Value(v1) => *v1,
                e1 => Value::Expression(Box::new(e1))
            }
        v1 => v1
    }
}

pub fn fold_expr(e: Expression) -> Expression {
    match e {
        Expression::Value(v) => Expression::Value(Box::new(fold_val(*v))),
        Expression::Binary {left: v, operator: op, right: expr} =>
            match (fold_val(v), fold_expr(*expr)) {
                (Value::Integer(m), Expression::Value(v1)) =>
                    match *v1 {
                        Value::Integer(n) => Expression::Value(Box::new(Value::Integer(eval(op,m,n)))),
                        _ => Expression::Binary {left: Value::Integer(m), operator: op, right: Box::new(Expression::Value(v1))}
                    },
                (v1, expr1) => Expression::Binary {left: v1, operator: op, right: Box::new(expr1)}
            }
    }
}

pub fn fold_stmt(s: Statement) -> Statement {
    match s {
        Statement::Assign {variable: v, expression: e} => Statement::Assign {variable: v, expression: fold_expr(e)}
    }
}

pub fn fold(p: Program) -> Program {
    Program {statements: p.statements.into_iter().map(fold_stmt).collect(), ..p}
}
