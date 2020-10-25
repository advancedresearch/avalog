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
