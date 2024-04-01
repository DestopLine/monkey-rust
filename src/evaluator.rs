use crate::{ast, object::{Object, Error}};

//  TODO: Implement these as actual sigletons
const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

macro_rules! new_error {
    ($($arg:tt)*) => {{
        let res = Error { message: std::fmt::format(std::format_args!($($arg)*)) };
        res
    }};
}

fn bool_native_to_obj(x: bool) -> Object {
    if x {
        TRUE
    } else {
        FALSE
    }
}

pub fn eval(node: ast::Node) -> Result<Object, Error> {
    match node {
        ast::Node::Program(program) => eval_program(program),
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ExpressionStatement(expr_stmt) => {
                eval(ast::Node::Expression(expr_stmt.expression))
            }
            ast::Statement::BlockStatement(block) => eval_block_statement(block),
            ast::Statement::Return(ret) => {
                let val = eval(ast::Node::Expression(ret.return_value))?;
                Ok(Object::ReturnValue(Box::new(val)))
            }
            _ => unimplemented!(),
        },
        ast::Node::Expression(expr) => match expr {
            ast::Expression::IntegerLiteral(lit) => Ok(Object::Integer(lit.value)),
            ast::Expression::Boolean(lit) => Ok(bool_native_to_obj(lit.value)),
            ast::Expression::PrefixExpression(expr) => {
                let right = eval(ast::Node::Expression(*expr.right))?;
                eval_prefix_expression(expr.operator, right)
            }
            ast::Expression::InfixExpression(expr) => {
                let left = eval(ast::Node::Expression(*expr.left))?;
                let right = eval(ast::Node::Expression(*expr.right))?;
                eval_infix_expression(expr.operator, left, right)
            }
            ast::Expression::IfExpression(expr) => {
                let condition = eval(ast::Node::Expression(*expr.condition))?;

                if is_truthy(condition) {
                    eval(ast::Node::Statement(ast::Statement::BlockStatement(
                        expr.consequence,
                    )))
                } else if let Some(alt) = expr.alternative {
                    eval(ast::Node::Statement(ast::Statement::BlockStatement(alt)))
                } else {
                    Ok(NULL)
                }
            }
            _ => unimplemented!(),
        },
    }
}

fn eval_program(program: ast::Program) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in program.statements {
        result = eval(ast::Node::Statement(stmt))?;

        if let Object::ReturnValue(ret_val) = result {
            result = *ret_val;
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: ast::BlockStatement) -> Result<Object, Error> {
    let mut result = Object::Null;

    for stmt in block.statements {
        result = eval(ast::Node::Statement(stmt))?;

        if let Object::ReturnValue(_) = result {
            break;
        }
    }

    Ok(result)
}

fn eval_prefix_expression(operator: String, right: Object) -> Result<Object, Error> {
    match operator.as_str() {
        "!" => Ok(match right {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        }),
        "-" => match right {
            Object::Integer(n) => Ok(Object::Integer(-n)),
            _ => Err(new_error!("Unknown operator: -{right:?}")),
        },
        _ => Err(new_error!("Unknown operator: {operator}{right:?}")),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Result<Object, Error> {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (l, r) => {
            if std::mem::discriminant(&l) != std::mem::discriminant(&r) {
                return Err(new_error!("Type mismatch: {l:?} {operator} {r:?}"))
            }
            match operator.as_str() {
                "==" => Ok(bool_native_to_obj(l == r)),
                "!=" => Ok(bool_native_to_obj(l != r)),
                _ => Err(new_error!("Unknown operator: {l:?} {operator} {r:?}")),
            }
        },
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Result<Object, Error> {
    match operator.as_str() {
        "+" => Ok(Object::Integer(left + right)),
        "-" => Ok(Object::Integer(left - right)),
        "*" => Ok(Object::Integer(left * right)),
        "/" => Ok(Object::Integer(left / right)),
        "<" => Ok(bool_native_to_obj(left < right)),
        ">" => Ok(bool_native_to_obj(left > right)),
        "==" => Ok(bool_native_to_obj(left == right)),
        "!=" => Ok(bool_native_to_obj(left != right)),
        _ => Err(new_error!("Unknown operator: {left} {operator} {right}")),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_integer_expression() {
        let tests = [
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 -10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: String) -> Result<Object, Error> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(ast::Node::Program(program))
    }

    fn test_integer_object(obj: Object, expected: i64) {
        assert_eq!(obj, Object::Integer(expected));
    }

    #[test]
    fn eval_boolean_expression() {
        let tests = [
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_boolean_object(evaluated, expected);
        }
    }

    fn test_boolean_object(obj: Object, expected: bool) {
        assert_eq!(obj, Object::Boolean(expected));
    }

    #[test]
    fn bang_operator() {
        let tests = [
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_boolean_object(evaluated, expected);
        }
    }

    enum Value {
        Int(i64),
        // Bool(bool),
        Null,
    }

    #[test]
    fn if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Value::Int(10)),
            ("if (false) { 10 }", Value::Null),
            ("if (1) { 10 }", Value::Int(10)),
            ("if (1 < 2) { 10 }", Value::Int(10)),
            ("if (1 > 2) { 10 }", Value::Null),
            ("if (1 > 2) { 10 } else { 20 }", Value::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Value::Int(10)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            match expected {
                Value::Int(i) => test_integer_object(evaluated, i),
                Value::Null => test_null_object(evaluated),
            }
        }
    }

    fn test_null_object(obj: Object) {
        assert_eq!(obj, NULL);
    }

    #[test]
    fn return_statement() {
        let tests = [
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn error_handling() {
        let tests = [
            ("5 + true;", "Type mismatch: Integer(5) + Boolean(true)"),
            ("5 + true; 5;", "Type mismatch: Integer(5) + Boolean(true)"),
            ("-true", "Unknown operator: -Boolean(true)"),
            ("true + false;", "Unknown operator: Boolean(true) + Boolean(false)"),
            ("5; true + false; 5", "Unknown operator: Boolean(true) + Boolean(false)"),
            (
                "if (10 > 1) { true + false; }",
                "Unknown operator: Boolean(true) + Boolean(false)",
            ),
            (
                "
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }
                ",
                "Unknown operator: Boolean(true) + Boolean(false)",
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string());

            let Err(Error { message }) = evaluated else {
                panic!("Expected Error, got {evaluated:?} instead");
            };

            assert_eq!(message, expected.to_string());
        }
    }
}
