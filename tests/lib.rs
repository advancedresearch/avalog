extern crate avalog;

fn check(facts: &'static str, goals: &'static str) {
    use avalog::{solve_with_accelerator, parse, infer, Accelerator};

    let facts = parse(facts).unwrap();
    let goals = parse(goals).unwrap();
    assert!(solve_with_accelerator(
        &facts,
        &goals,
        None,
        &[],
        &[],
        infer,
        &mut Accelerator::new()
    ).1.is_ok());
}

fn fail(facts: &'static str, goals: &'static str) {
    use avalog::{solve_with_accelerator, parse, infer, Accelerator};

    let facts = parse(facts).unwrap();
    let goals = parse(goals).unwrap();
    assert!(solve_with_accelerator(
        &facts,
        &goals,
        None,
        &[],
        &[],
        infer,
        &mut Accelerator::new()
    ).1.is_err());
}

#[test]
fn capital() {
    check("source/capital.txt", "source/capital-0.txt");
}

#[test]
fn category() {
    check("source/category.txt", "source/category-0.txt");
}

#[test]
fn chu_space() {
    check("source/chu_space.txt", "source/no_amb.txt");
}

#[test]
fn assocativity() {
    check("source/associativity.txt", "source/associativity-0.txt");
}

#[test]
fn bool_alg() {
    check("source/bool_alg.txt", "source/bool_alg-0.txt");
}

#[test]
fn squares() {
    check("source/squares.txt", "source/squares-0.txt");
    check("source/squares2.txt", "source/squares2-0.txt");
    check("source/squares3.txt", "source/squares3-0.txt");
    check("source/squares4.txt", "source/squares4-0.txt");
}

#[test]
fn convert_unique_has_into_eq() {
    check("source/convert_has_into_eq.txt", "source/convert_has_into_eq-0.txt");
    fail("source/convert_has_into_eq2.txt", "source/convert_has_into_eq2-0.txt");
    check("source/convert_has_into_eq3.txt", "source/convert_has_into_eq3-0.txt");
    check("source/convert_has_into_eq4.txt", "source/convert_has_into_eq4-0.txt");
    check("source/convert_has_into_eq5.txt", "source/convert_has_into_eq5-0.txt");
    fail("source/convert_has_into_eq6.txt", "source/convert_has_into_eq6-0.txt");
    check("source/convert_has_into_eq6.txt", "source/convert_has_into_eq6-1.txt");
}

#[test]
fn convert_eq_into_has() {
    check("source/convert_eq_into_has.txt", "source/convert_eq_into_has-0.txt");
}

#[test]
fn copy() {
    check("source/copy.txt", "source/copy-0.txt");
}

#[test]
fn role_lift() {
    check("source/role_lift_app.txt", "source/role_lift_app-0.txt");
    check("source/role_lift_ava.txt", "source/role_lift_ava-0.txt");
    check("source/role_lift_inner.txt", "source/role_lift_inner-0.txt");
    check("source/role_lift_eq.txt", "source/role_lift_eq-0.txt");
}
