use crate::*;

use piston_meta::{Convert, Range};

fn parse_rule(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res: Option<Expr> = None;
    let mut args: Vec<Expr> = vec![];
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = parse_expr("res", convert, ignored) {
            convert.update(range);
            res = Some(v);
        } else if let Ok((range, v)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            args.push(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let res = res.ok_or(())?;
    Ok((convert.subtract(start), Expr::Rule(Box::new(res), args)))
}

fn parse_inner(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let arg = arg.ok_or(())?;
    Ok((convert.subtract(start), Expr::Inner(Box::new(arg))))
}

fn parse_app(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Arc<String>> = None;
    let mut arg: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("f") {
            convert.update(range);
            f = Some(v);
        } else if let Ok((range, v)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let f = f.ok_or(())?;
    let arg = arg.ok_or(())?;
    Ok((convert.subtract(start), Expr::App(
        Box::new(Expr::Sym(f)),
        Box::new(arg)
    )))
}

fn parse_ava(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut avatar: Option<Arc<String>> = None;
    let mut core: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("avatar") {
            convert.update(range);
            avatar = Some(v);
        } else if let Ok((range, v)) = parse_expr("core", convert, ignored) {
            convert.update(range);
            core = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let avatar = avatar.ok_or(())?;
    let core = core.ok_or(())?;
    Ok((convert.subtract(start), Expr::Ava(
        Box::new(Expr::Sym(avatar)),
        Box::new(core)
    )))
}

fn parse_uniq(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(Expr::UniqAva(Box::new(v)));
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let arg = arg.ok_or(())?;
    Ok((convert.subtract(start), arg))
}

fn parse_sym(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut val: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("val") {
            convert.update(range);
            val = Some(Expr::Sym(v));
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let val = val.ok_or(())?;
    Ok((convert.subtract(start), val))
}

fn parse_has(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Expr> = None;
    let mut arg: Option<Expr> = None;
    let mut res: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("f", convert, ignored) {
            convert.update(range);
            f = Some(val);
        } else if let Ok((range, val)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(val);
        } else if let Ok((range, v)) = parse_expr("res", convert, ignored) {
            convert.update(range);
            res = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let f = f.ok_or(())?;
    let arg = arg.ok_or(())?;
    let res = res.ok_or(())?;
    Ok((convert.subtract(start), Expr::Has(
        Box::new(Expr::App(Box::new(f), Box::new(arg))),
        Box::new(res)
    )))
}

fn parse_eq(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Expr> = None;
    let mut arg: Option<Expr> = None;
    let mut res: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("f", convert, ignored) {
            convert.update(range);
            f = Some(val);
        } else if let Ok((range, val)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(val);
        } else if let Ok((range, v)) = parse_expr("res", convert, ignored) {
            convert.update(range);
            res = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let f = f.ok_or(())?;
    let arg = arg.ok_or(())?;
    let res = res.ok_or(())?;
    Ok((convert.subtract(start), Expr::Eq(
        Box::new(Expr::App(Box::new(f), Box::new(arg))),
        Box::new(res)
    )))
}

fn parse_role_of(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr> = None;
    let mut role: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg = Some(val);
        } else if let Ok((range, v)) = parse_expr("role", convert, ignored) {
            convert.update(range);
            role = Some(v);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let arg = arg.ok_or(())?;
    let role = role.ok_or(())?;
    Ok((convert.subtract(start), Expr::RoleOf(
        Box::new(arg),
        Box::new(role)
    )))
}

fn parse_amb_role(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr> = None;
    let mut b1: Option<Expr> = None;
    let mut b2: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("a", convert, ignored) {
            convert.update(range);
            a = Some(val);
        } else if let Ok((range, val)) = parse_expr("b1", convert, ignored) {
            convert.update(range);
            b1 = Some(val);
        } else if let Ok((range, val)) = parse_expr("b2", convert, ignored) {
            convert.update(range);
            b2 = Some(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let a = a.ok_or(())?;
    let b1 = b1.ok_or(())?;
    let b2 = b2.ok_or(())?;
    Ok((convert.subtract(start), ambiguous_role(a, b1, b2)))
}

fn parse_amb_rel(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr> = None;
    let mut b1: Option<Expr> = None;
    let mut b2: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("a", convert, ignored) {
            convert.update(range);
            a = Some(val);
        } else if let Ok((range, val)) = parse_expr("b1", convert, ignored) {
            convert.update(range);
            b1 = Some(val);
        } else if let Ok((range, val)) = parse_expr("b2", convert, ignored) {
            convert.update(range);
            b2 = Some(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let a = a.ok_or(())?;
    let b1 = b1.ok_or(())?;
    let b2 = b2.ok_or(())?;
    Ok((convert.subtract(start), ambiguous_rel(a, b1, b2)))
}

fn parse_rel(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr> = None;
    let mut b: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("a", convert, ignored) {
            convert.update(range);
            a = Some(val);
        } else if let Ok((range, val)) = parse_expr("b", convert, ignored) {
            convert.update(range);
            b = Some(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let a = a.ok_or(())?;
    let b = b.ok_or(())?;
    Ok((convert.subtract(start), Expr::Rel(
        Box::new(a),
        Box::new(b)
    )))
}

fn parse_expr(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut expr: Option<Expr> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_role_of("role_of", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_rel("rel", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_sym("sym", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_uniq("uniq", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_ava("ava", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_inner("inner", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_app("app", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_eq("eq", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_has("has", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_amb_rel("amb_rel", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_amb_role("amb_role", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else if let Ok((range, val)) = parse_rule("rule", convert, ignored) {
            convert.update(range);
            expr = Some(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let expr = expr.ok_or(())?;
    Ok((convert.subtract(start), expr))
}

fn parse_data(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Vec<Expr>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res = vec![];
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("expr", convert, ignored) {
            convert.update(range);
            res.push(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    Ok((convert.subtract(start), res))
}

/// Parses a string.
pub fn parse_str(data: &str) -> Result<Vec<Expr>, String> {
    use piston_meta::{parse_errstr, syntax_errstr};

    let syntax_src = include_str!("../assets/syntax.txt");
    let syntax = syntax_errstr(syntax_src)?;

    let mut meta_data = vec![];
    parse_errstr(&syntax, &data, &mut meta_data)?;

    // piston_meta::json::print(&meta_data);

    let convert = Convert::new(&meta_data);
    let mut ignored = vec![];
    match parse_data("data", convert, &mut ignored) {
        Err(()) => Err("Could not convert meta data".into()),
        Ok((_, expr)) => Ok(expr),
    }
}

/// Parses a source file.
pub fn parse(source: &str) -> Result<Vec<Expr>, String> {
    use std::fs::File;
    use std::io::Read;

    let mut data_file = File::open(source).map_err(|err|
        format!("Could not open `{}`, {}", source, err))?;
    let mut data = String::new();
    data_file.read_to_string(&mut data).unwrap();

    parse_str(&data)
}
