use crate::*;

use piston_meta::{Convert, Range};
use std::path::Path;

fn parse_rule<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res: Option<Expr<T>> = None;
    let mut args: Vec<Expr<T>> = vec![];
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

fn parse_inner<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr<T>> = None;
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

fn parse_app<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Expr<T>> = None;
    let mut arg: Vec<Expr<T>> = vec![];
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("f") {
            convert.update(range);
            if T::is_var(&v) {
                f = Some(Expr::Var(v));
            } else {
                f = Some(Expr::Sym(v.into()));
            }
        } else if let Ok((range, v)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            arg.push(v);
        } else if let Ok((range, v)) = convert.meta_bool("tail") {
            convert.update(range);
            if v {arg.push(Tail)};
        } else if let Ok((range, v)) = convert.meta_string("tail_sym") {
            convert.update(range);
            arg.push(TailVar(v.into()));
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let mut expr = f.ok_or(())?;
    for a in &arg {
        expr = app(expr, a.clone());
    }

    Ok((convert.subtract(start), expr))
}

fn parse_ava<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut avatar: Option<Expr<T>> = None;
    let mut core: Option<Expr<T>> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("avatar") {
            convert.update(range);
            if T::is_var(&v) {
                avatar = Some(Expr::Var(v));
            } else {
                avatar = Some(Expr::Sym(v.into()));
            }
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
        Box::new(avatar),
        Box::new(core)
    )))
}

fn parse_uniq<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr<T>> = None;
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

/// Parses symbol or variable.
///
/// Converts to variable automatically when starting with upper case.
fn parse_sym_or_var<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut val: Option<Expr<T>> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, v)) = convert.meta_string("val") {
            convert.update(range);
            if T::is_var(&v) {
                val = Some(Expr::Var(v));
            } else {
                val = Some(Expr::Sym(v.into()));
            }
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let val = val.ok_or(())?;
    Ok((convert.subtract(start), val))
}

fn parse_has<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Expr<T>> = None;
    let mut arg: Option<Expr<T>> = None;
    let mut res: Option<Expr<T>> = None;
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

fn parse_eq<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut f: Option<Expr<T>> = None;
    let mut arg: Option<Expr<T>> = None;
    let mut res: Option<Expr<T>> = None;
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

fn parse_neq<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut left: Option<Expr<T>> = None;
    let mut right: Option<Expr<T>> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("left", convert, ignored) {
            convert.update(range);
            left = Some(val);
        } else if let Ok((range, val)) = parse_expr("right", convert, ignored) {
            convert.update(range);
            right = Some(val);
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let left = left.ok_or(())?;
    let right = right.ok_or(())?;
    Ok((convert.subtract(start), Expr::Neq(
        Box::new(left),
        Box::new(right)
    )))
}

fn parse_role_of<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut arg: Option<Expr<T>> = None;
    let mut role: Option<Expr<T>> = None;
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

fn parse_amb_role<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr<T>> = None;
    let mut b1: Option<Expr<T>> = None;
    let mut b2: Option<Expr<T>> = None;
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

fn parse_amb_rel<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr<T>> = None;
    let mut b1: Option<Expr<T>> = None;
    let mut b2: Option<Expr<T>> = None;
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

fn parse_rel<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut a: Option<Expr<T>> = None;
    let mut b: Option<Expr<T>> = None;
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

fn parse_expr<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>
) -> Result<(Range, Expr<T>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut expr: Option<Expr<T>> = None;
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
        } else if let Ok((range, val)) = parse_sym_or_var("sym", convert, ignored) {
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
        } else if let Ok((range, val)) = parse_neq("neq", convert, ignored) {
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
        } else if let Ok((range, val)) = convert.meta_bool("amb") {
            convert.update(range);
            expr = Some(Ambiguity(val));
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    let expr = expr.ok_or(())?;
    Ok((convert.subtract(start), expr))
}

fn parse_data<T: Symbol>(
    node: &str,
    mut convert: Convert,
    ignored: &mut Vec<Range>,
    parent: &Path
) -> Result<(Range, Vec<Expr<T>>), ()> {
    let start = convert;
    let start_range = convert.start_node(node)?;
    convert.update(start_range);

    let mut res: Vec<Expr<T>> = vec![];
    let mut eval: Option<T> = None;
    let mut role: Option<Expr<T>> = None;
    loop {
        if let Ok(range) = convert.end_node(node) {
            convert.update(range);
            break;
        } else if let Ok((range, val)) = parse_expr("expr", convert, ignored) {
            convert.update(range);
            let val = if let Some(eval) = eval.as_ref() {
                let top = true;
                val.eval_lift(eval, top)
            } else {
                val
            };
            res.push(val);
        } else if let Ok((range, val)) = convert.meta_string("eval") {
            convert.update(range);
            eval = Some(val.into());
        } else if let Ok((range, val)) = convert.meta_bool("no_eval") {
            convert.update(range);
            if val {eval = None};
        } else if let Ok((range, val)) = convert.meta_string("import") {
            convert.update(range);
            match parse(parent.join(&**val)) {
                Ok(facts) => res.extend(facts),
                Err(err) => println!("ERROR:\n{}", err),
            }
        } else if let Ok((range, val)) = parse_expr("role", convert, ignored) {
            convert.update(range);
            role = Some(val);
        } else if let Ok((range, val)) = parse_expr("arg", convert, ignored) {
            convert.update(range);
            if let Some(role) = role.as_ref() {
                res.push(RoleOf(Box::new(val), Box::new(role.clone().into())));
            }
        } else {
            let range = convert.ignore();
            convert.update(range);
            ignored.push(range);
        }
    }

    Ok((convert.subtract(start), res))
}

/// Parses a string.
pub fn parse_str<T: Symbol>(
    data: &str,
    parent: &Path
) -> Result<Vec<Expr<T>>, String> {
    use piston_meta::{parse_errstr, syntax_errstr};

    let syntax_src = include_str!("../assets/syntax.txt");
    let syntax = syntax_errstr(syntax_src)?;

    let mut meta_data = vec![];
    parse_errstr(&syntax, &data, &mut meta_data)?;

    // piston_meta::json::print(&meta_data);

    let convert = Convert::new(&meta_data);
    let mut ignored = vec![];
    match parse_data("data", convert, &mut ignored, parent) {
        Err(()) => Err("Could not convert meta data".into()),
        Ok((_, expr)) => Ok(expr),
    }
}

/// Helps specifying the parse data type.
pub type ParseData<T = Arc<String>> = Vec<Expr<T>>;
/// Helps specifying the type of the parsing result.
pub type ParseResult<T = Arc<String>> = Result<ParseData<T>, String>;

/// Parses a source file.
pub fn parse<P, T>(source: P) -> ParseResult<T>
    where P: AsRef<Path>, T: Symbol
{
    use std::fs::File;
    use std::io::Read;

    let source = source.as_ref();
    let mut data_file = File::open(source).map_err(|err|
        format!("Could not open `{:?}`, {}", source, err))?;
    let mut data = String::new();
    let parent = if let Some(dir) = source.parent() {
        dir
    } else {
        return Err("Could not get parent directory of file".into());
    };
    data_file.read_to_string(&mut data).unwrap();
    parse_str(&data, &parent)
}
