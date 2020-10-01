use avalog::*;

fn main() {
    let start = match parse("source/grandparent.txt") {
        Ok(x) => x,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    let goal = match parse("source/grandparent-0.txt") {
        Ok(x) => x,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };
    let order_constraints = vec![];

    // println!("TEST {:?}", goal);

    let res = solve_and_reduce(
        &start,
        &goal,
        &[],
        &order_constraints,
        infer,
    );
    if res.is_ok() {
        println!("OK");
    } else {
        println!("ERROR");
    }
    match res {
        Ok(ref res) | Err(ref res) => {
            for r in res {
                println!("{}", r);
            }
        }
    }
}
