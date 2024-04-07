use std::{cell::RefCell, collections::HashMap, rc::Rc, str::FromStr};

use crate::{
    ast,
    environment::{Env, Environment},
    object::{self, new_error, Error, Function, Object},
};

pub fn eval(program: &ast::Program, env: &Env) -> Result<Rc<Object>, Error> {
    let mut result = env.borrow().get_singleton(None);

    for stmt in &program.statements {
        result = eval_statement(stmt, env)?;

        if let Object::ReturnValue(ret_val) = &*result {
            result = Rc::clone(ret_val);
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &ast::BlockStatement, env: &Env) -> Result<Rc<Object>, Error> {
    let mut result = env.borrow().get_singleton(None);

    for stmt in &block.statements {
        result = eval_statement(&stmt, env)?;

        if let Object::ReturnValue(_) = *result {
            break;
        }
    }

    Ok(result)
}

fn eval_statement(statement: &ast::Statement, env: &Env) -> Result<Rc<Object>, Error> {
    match statement {
        ast::Statement::ExpressionStatement(expr_stmt) => {
            eval_expression(&expr_stmt.expression, env)
        }
        ast::Statement::BlockStatement(block) => eval_block_statement(block, env),
        ast::Statement::Return(ret) => {
            let val = eval_expression(&ret.return_value, env)?;
            Ok(Rc::new(Object::ReturnValue(val)))
        }
        ast::Statement::Let(let_stmt) => {
            let val = eval_expression(&let_stmt.value, env)?;
            env.borrow_mut().set(let_stmt.name.value.clone(), val);
            Ok(env.borrow().get_singleton(None))
        }
    }
}

fn eval_expression(expression: &ast::Expression, env: &Env) -> Result<Rc<Object>, Error> {
    match expression {
        ast::Expression::IntegerLiteral(lit) => Ok(Rc::new(Object::Integer(lit.value))),
        ast::Expression::Boolean(lit) => Ok(env.borrow().get_singleton(Some(lit.value))),
        ast::Expression::StringLiteral(lit) => Ok(Rc::new(Object::String(lit.value.clone()))),
        ast::Expression::PrefixExpression(expr) => {
            let right = eval_expression(&*expr.right, env)?;
            eval_prefix_expression(&expr.operator, &right, env)
        }
        ast::Expression::InfixExpression(expr) => {
            let left = eval_expression(&*expr.left, env)?;
            let right = eval_expression(&*expr.right, env)?;
            eval_infix_expression(&expr.operator, &left, &right, env)
        }
        ast::Expression::IfExpression(expr) => {
            let condition = eval_expression(&*expr.condition, env)?;

            if is_truthy(condition) {
                eval_block_statement(&expr.consequence, env)
            } else if let Some(alt) = &expr.alternative {
                eval_block_statement(&alt, env)
            } else {
                Ok(env.borrow().get_singleton(None))
            }
        }
        ast::Expression::Identifier(ident) => match env.borrow().get(&ident.value) {
            Some(v) => Ok(v),
            None => match object::BuiltinFn::from_str(&ident.value) {
                Ok(func) => Ok(Rc::new(Object::Builtin(func))),
                Err(_) => Err(new_error!("Identifier not found: {}", ident.value)),
            },
        },
        ast::Expression::FunctionLiteral(lit) => Ok(Rc::new(Object::Function(object::Function {
            parameters: lit.parameters.clone(),
            body: lit.body.clone(),
            env: Rc::clone(env),
        }))),
        ast::Expression::CallExpression(call) => {
            let evaluated_call = eval_expression(&*call.function, env)?;
            let args = eval_expressions(&call.arguments, env)?;
            apply_function(&evaluated_call, args, env)
        }
        ast::Expression::ArrayLiteral(arr) => Ok(Rc::new(Object::Array(eval_expressions(
            &arr.elements,
            env,
        )?))),
        ast::Expression::IndexExpression(idx) => {
            let left = eval_expression(&idx.left, env)?;
            let index = eval_expression(&idx.index, env)?;
            match (&*left, &*index) {
                (Object::Array(array), Object::Integer(index)) => Ok(Object::filter_option(array.get(*index as usize), env)),
                (Object::Hash(object::HashObj(map)), _) => {
                    if !index.is_hashable() {
                        Err(new_error!("Unusable as hash key: {index}"))
                    } else {
                        Ok(Object::filter_option(map.get(&index), env))
                    }
                },
                _ => Err(new_error!("Index operator not supported: {left}")),
            }
            
        }
        ast::Expression::HashLiteral(hash) => eval_hash_literal(hash, env),
        _ => unimplemented!(),
    }
}

fn apply_function(obj: &Rc<Object>, args: Vec<Rc<Object>>, env: &Env) -> Result<Rc<Object>, Error> {
    match &**obj {
        Object::Function(func) => {
            let extended_env = extend_function_env(&func, args);
            let evaluated = eval_block_statement(&func.body, &extended_env)?;

            if let Object::ReturnValue(val) = &*evaluated {
                Ok(Rc::clone(val))
            } else {
                Ok(evaluated)
            }
        }
        Object::Builtin(builtin) => Ok(Object::filter_singleton((builtin.func)(args)?, env)),
        o @ _ => Err(new_error!("Not a function: {o}")),
    }
}

fn extend_function_env(func: &Function, args: Vec<Rc<Object>>) -> Env {
    let mut env = Environment::enclosed_by(Rc::clone(&func.env));

    for (param, arg) in func.parameters.iter().zip(args.iter()) {
        env.set(param.value.clone(), arg.clone());
    }

    Rc::new(RefCell::new(env))
}

fn eval_expressions(exprs: &Vec<ast::Expression>, env: &Env) -> Result<Vec<Rc<Object>>, Error> {
    let mut result = Vec::new();

    for expr in exprs {
        let evaluated = eval_expression(&expr, env)?;
        result.push(evaluated)
    }

    Ok(result)
}

fn eval_prefix_expression(
    operator: &String,
    right: &Rc<Object>,
    env: &Env,
) -> Result<Rc<Object>, Error> {
    match operator.as_str() {
        "!" => Ok(env.borrow().get_singleton(match **right {
            Object::Boolean(boolean) => Some(!boolean),
            Object::Null => None,
            _ => Some(false),
        })),
        "-" => match **right {
            Object::Integer(n) => Ok(Rc::new(Object::Integer(-n))),
            _ => Err(new_error!("Unknown operator: -{right}")),
        },
        _ => Err(new_error!("Unknown operator: {operator}{right}")),
    }
}

fn eval_infix_expression(
    operator: &String,
    left: &Rc<Object>,
    right: &Rc<Object>,
    env: &Env,
) -> Result<Rc<Object>, Error> {
    match (&**left, &**right) {
        (Object::Integer(l), Object::Integer(r)) => {
            eval_integer_infix_expression(operator, *l, *r, env)
        }
        (Object::String(l), Object::String(r)) => match operator.as_str() {
            "+" => Ok(Rc::new(Object::String(format!("{l}{r}")))),
            "==" => Ok(env.borrow().get_singleton(Some(l == r))),
            "!=" => Ok(env.borrow().get_singleton(Some(l != r))),
            _ => Err(new_error!("Unknown operator: {left} {operator} {right}")),
        },
        (l, r) => {
            if std::mem::discriminant(l) != std::mem::discriminant(r) {
                return Err(new_error!("Type mismatch: {l} {operator} {r}"));
            }
            match operator.as_str() {
                "==" => Ok(env.borrow().get_singleton(Some(l == r))),
                "!=" => Ok(env.borrow().get_singleton(Some(l != r))),
                _ => Err(new_error!("Unknown operator: {l} {operator} {r}")),
            }
        }
    }
}

fn eval_integer_infix_expression(
    operator: &String,
    left: i64,
    right: i64,
    env: &Env,
) -> Result<Rc<Object>, Error> {
    match operator.as_str() {
        "+" => Ok(Rc::new(Object::Integer(left + right))),
        "-" => Ok(Rc::new(Object::Integer(left - right))),
        "*" => Ok(Rc::new(Object::Integer(left * right))),
        "/" => Ok(Rc::new(Object::Integer(left / right))),
        "<" => Ok(env.borrow().get_singleton(Some(left < right))),
        ">" => Ok(env.borrow().get_singleton(Some(left > right))),
        "==" => Ok(env.borrow().get_singleton(Some(left == right))),
        "!=" => Ok(env.borrow().get_singleton(Some(left != right))),
        _ => Err(new_error!("Unknown operator: {left} {operator} {right}")),
    }
}

fn eval_hash_literal(hash: &ast::HashLiteral, env: &Env) -> Result<Rc<Object>, Error> {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in &hash.pairs {
        let key = eval_expression(&key_node, env)?;

        if !key.is_hashable() {
            return Err(new_error!("Unusable as hash key: {key}"));
        }

        let value = eval_expression(&value_node, env)?;

        pairs.insert(key, value);
    }

    Ok(Rc::new(Object::Hash(object::HashObj(pairs))))
}

fn is_truthy(obj: Rc<Object>) -> bool {
    match *obj {
        Object::Null => false,
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{ast::MonkeyNode, lexer::Lexer, parser::Parser};

    use self::object::HashObj;

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

    fn test_eval(input: String) -> Result<Rc<Object>, Error> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        eval(&program, &Rc::new(RefCell::new(Environment::new())))
    }

    fn test_integer_object(obj: Rc<Object>, expected: i64) {
        assert_eq!(*obj, Object::Integer(expected));
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

    fn test_boolean_object(obj: Rc<Object>, expected: bool) {
        assert_eq!(*obj, Object::Boolean(expected));
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

    fn test_null_object(obj: Rc<Object>) {
        assert_eq!(*obj, Object::Null);
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
            ("5 + true;", "Type mismatch: 5 + true"),
            ("5 + true; 5;", "Type mismatch: 5 + true"),
            ("-true", "Unknown operator: -true"),
            (
                "true + false;",
                "Unknown operator: true + false",
            ),
            (
                "5; true + false; 5",
                "Unknown operator: true + false",
            ),
            (
                "if (10 > 1) { true + false; }",
                "Unknown operator: true + false",
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
                "Unknown operator: true + false",
            ),
            ("foobar", "Identifier not found: foobar"),
            (
                r#" "Hello" - "World" "#,
                r#"Unknown operator: "Hello" - "World""#,
            ),
            (
                r#"{"name": "Monkey"}[fn(x) { x }]"#,
                "Unusable as hash key: fn(x)",
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

    #[test]
    fn let_statement() {
        let tests = [
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn function_object() {
        let input = "fn(x) { x + 2; };";

        let evaluated = test_eval(input.to_string()).unwrap();
        let Object::Function(func) = &*evaluated else {
            panic!("Expected Function, got {evaluated:?}");
        };

        assert_eq!(func.parameters.len(), 1);
        assert_eq!(func.parameters[0].string(), "x".to_string());
        assert_eq!(func.body.string(), "(x + 2)".to_string());
    }

    #[test]
    fn function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn closures() {
        let input = String::from(
            "
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
",
        );
        let evaluated = test_eval(input).unwrap();
        test_integer_object(evaluated, 4);
    }

    #[test]
    #[ignore = "too much noise from the `impl Drop`s"]
    fn garbage_collector() {
        //  NOTE: Use --ignored --show-output to test this one
        let input = String::from(
            "
let foo = fn(x) {
    let bar = 69;
    if (x < 2) {
        foo(x + 1)
    }
}
foo(0);
",
        );

        // impl Drop for Object {
        //     fn drop(&mut self) {
        //         println!("Dropped {self:?}")
        //     }
        // }
        //
        // impl Drop for Environment {
        //     fn drop(&mut self) {
        //         println!("Dropped Env")
        //     }
        // }

        test_eval(input).unwrap();
        println!("Done!");
    }

    #[test]
    fn string_literal() {
        let input = r#" "Hello World!" "#.to_string();

        let evaluated = test_eval(input).unwrap();

        assert_eq!(*evaluated, Object::String(String::from("Hello World!")));
    }

    #[test]
    fn string_concatenation() {
        let input = r#" "Hello" + " " + "World!" "#.to_string();

        let evaluated = test_eval(input).unwrap();

        assert_eq!(*evaluated, Object::String(String::from("Hello World!")));
    }

    #[test]
    fn builtin_functions() {
        let tests = [
            (r#"len("")"#, Ok(0)),
            (r#"len("four")"#, Ok(4)),
            (r#"len("hello world")"#, Ok(11)),
            (
                r#"len(1)"#,
                Err("Argument to `len` not supported, got Integer(1)"),
            ),
            (
                r#"len("one", "two")"#,
                Err("Wrong number of arguments: got 2, expected 1"),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string());

            match (evaluated, expected) {
                (Ok(obj), Ok(i)) => test_integer_object(obj, i),
                (Err(Error { message }), Err(exp)) => assert_eq!(message, exp),
                (Err(err), Ok(ok)) => panic!("Expected {ok}, got {err:?} instead"),
                (Ok(ok), Err(err)) => panic!("Expected {err:?}, got {ok:?} instead"),
            }
        }
    }

    #[test]
    fn array_literals() {
        let input = "[1, 2 * 2, 3 + 3]".to_string();

        let evaluated = test_eval(input).unwrap();

        let Object::Array(elements) = &*evaluated else {
            panic!("Expected Array, got {evaluated:?} instead");
        };

        assert_eq!(elements.len(), 3);
        test_integer_object(elements[0].clone(), 1);
        test_integer_object(elements[1].clone(), 4);
        test_integer_object(elements[2].clone(), 6);
    }

    #[test]
    fn array_index_expressions() {
        let tests = [
            ("[1, 2, 3][0]", Some(1)),
            ("[1, 2, 3][1]", Some(2)),
            ("[1, 2, 3][2]", Some(3)),
            ("let i = 0; [1][i];", Some(1)),
            ("[1, 2, 3][1 + 1];", Some(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Some(2),
            ),
            ("[1, 2, 3][3]", None),
            ("[1, 2, 3][-1]", None),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            match expected {
                Some(v) => test_integer_object(evaluated, v),
                None => test_null_object(evaluated),
            }
        }
    }

    #[test]
    fn hash_literals() {
        let input = r#" let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#.to_string();

        let evaluated = test_eval(input).unwrap();
        let Object::Hash(HashObj(result)) = &*evaluated else {
            panic!("Expected Hash, got {evaluated:?} instead");
        };

        let expected = HashMap::from([
            (Object::String(String::from("one")), 1),
            (Object::String(String::from("two")), 2),
            (Object::String(String::from("three")), 3),
            (Object::Integer(4), 4),
            (Object::Boolean(true), 5),
            (Object::Boolean(false), 6),
        ]);

        assert_eq!(result.len(), expected.len());

        for (expected_key, expected_value) in expected {
            let Some(pair) = result.get(&expected_key) else {
                panic!("Hash has wrong number of pairs, got {}", result.len());
            };
            test_integer_object(pair.clone(), expected_value);
        }
    }

    #[test]
    fn hash_index_expressions() {
        let tests = [
            (
                r#"{"foo": 5}["foo"]"#,
                Some(5),
            ),
            (
                r#"{"foo": 5}["bar"]"#,
                None,
            ),
            (
                r#"let key = "foo"; {"foo": 5}[key]"#,
                Some(5),
            ),
            (
                r#"{}["foo"]"#,
                None,
            ),
            (
                r#"{5: 5}[5]"#,
                Some(5),
            ),
            (
                r#"{true: 5}[true]"#,
                Some(5),
            ),
            (
                r#"{false: 5}[false]"#,
                Some(5),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input.to_string()).unwrap();
            match expected {
                Some(n) => test_integer_object(evaluated, n),
                None => test_null_object(evaluated)
            }
        }
    }
}
