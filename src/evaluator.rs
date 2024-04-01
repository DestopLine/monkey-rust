use crate::{ast, object::Object};

//  TODO: Implement these as actual sigletons
const NULL: Object = Object::Null;
const TRUE: Object = Object::Boolean(true);
const FALSE: Object = Object::Boolean(false);

fn bool_native_to_obj(x: bool) -> Object {
    if x {
        TRUE
    } else {
        FALSE
    }
}

pub fn eval(node: ast::Node) -> Option<Object> {
    match node {
        ast::Node::Program(program) => Some(eval_program(program)),
        ast::Node::Statement(stmt) => match stmt {
            ast::Statement::ExpressionStatement(expr_stmt) => {
                eval(ast::Node::Expression(expr_stmt.expression))
            }
            ast::Statement::BlockStatement(block) => Some(eval_block_statement(block)),
            ast::Statement::Return(ret) => {
                let val = eval(ast::Node::Expression(ret.return_value))?;
                Some(Object::ReturnValue(Box::new(val)))
            }
            _ => None,
        },
        ast::Node::Expression(expr) => match expr {
            ast::Expression::IntegerLiteral(lit) => Some(Object::Integer(lit.value)),
            ast::Expression::Boolean(lit) => Some(bool_native_to_obj(lit.value)),
            ast::Expression::PrefixExpression(expr) => {
                let right = eval(ast::Node::Expression(*expr.right))?;
                Some(eval_prefix_expression(expr.operator, right))
            }
            ast::Expression::InfixExpression(expr) => {
                let left = eval(ast::Node::Expression(*expr.left))?;
                let right = eval(ast::Node::Expression(*expr.right))?;
                Some(eval_infix_expression(expr.operator, left, right))
            }
            ast::Expression::IfExpression(expr) => {
                let condition = eval(ast::Node::Expression(*expr.condition)).unwrap();

                if is_truthy(condition) {
                    eval(ast::Node::Statement(ast::Statement::BlockStatement(
                        expr.consequence,
                    )))
                } else if let Some(alt) = expr.alternative {
                    eval(ast::Node::Statement(ast::Statement::BlockStatement(alt)))
                } else {
                    Some(NULL)
                }
            }
            _ => None,
        },
    }
}

fn eval_program(program: ast::Program) -> Object {
    let mut result = None;

    for stmt in program.statements {
        result = eval(ast::Node::Statement(stmt));
        if let Some(Object::ReturnValue(ret_val)) = result {
            result = Some(*ret_val);
            break;
        }
    }

    result.unwrap_or(NULL)
}

fn eval_block_statement(block: ast::BlockStatement) -> Object {
    let mut result = None;

    for stmt in block.statements {
        result = eval(ast::Node::Statement(stmt));
        if let Some(Object::ReturnValue(_)) = result {
            break;
        }
    }

    result.unwrap_or(NULL)
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => match right {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        },
        "-" => match right {
            Object::Integer(n) => Object::Integer(-n),
            _ => NULL,
        },
        _ => NULL,
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (l, r) => match operator.as_str() {
            "==" => bool_native_to_obj(l == r),
            "!=" => bool_native_to_obj(l != r),
            _ => NULL,
        },
    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => bool_native_to_obj(left < right),
        ">" => bool_native_to_obj(left > right),
        "==" => bool_native_to_obj(left == right),
        "!=" => bool_native_to_obj(left != right),
        _ => NULL,
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
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }

    fn test_eval(input: String) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(ast::Node::Program(program)).unwrap()
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
            let evaluated = test_eval(input.to_string());
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
            let evaluated = test_eval(input.to_string());
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
            let evaluated = test_eval(input.to_string());
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
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }
}
