extern crate avalog;

fn check(facts: &'static str, goals: &'static str) {
    use avalog::{solve, parse, infer};

    let facts = parse(facts).unwrap();
    let goals = parse(goals).unwrap();
    assert!(solve(
        &facts,
        &goals,
        None,
        &[],
        &[],
        infer,
    ).is_ok());
}

#[test]
fn capital() {
    check("source/capital.txt", "source/capital-0.txt");
}

#[test]
fn category() {
    check("source/category.txt", "source/category-0.txt");
}
