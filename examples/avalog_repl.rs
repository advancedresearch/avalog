use avalog::*;

fn main() {
    println!("=== Avalog 0.2 ===");
    println!("Type `help` for more information.");

    let mut facts = vec![];
    loop {
        use std::io::{self, Write};

        print!("> ");
        let mut input = String::new();
        io::stdout().flush().unwrap();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {}
            Err(_) => {
                println!("ERROR: Could not read input");
                continue;
            }
        };

        match input.trim() {
            "bye" => break,
            "help" => {print_help(); continue}
            x => {
                if x.starts_with("prove ") {
                    match parse_str(x[6..].trim()) {
                        Ok(goals) => {
                            prove(&goals, &facts);
                            continue;
                        }
                        Err(err) => {
                            println!("ERROR:\n{}", err);
                            continue;
                        }
                    }
                }
            }
        }

        match parse_str(&input) {
            Ok(new_facts) => {
                facts.extend(new_facts);
            }
            Err(err) => {
                println!("ERROR:\n{}", err);
                continue;
            }
        };

    }
}

fn prove(goals: &[Expr], facts: &[Expr]) {
    let order_constraints = vec![];

    let res = solve_and_reduce(
        &facts,
        &goals,
        &[],
        &order_constraints,
        infer,
    );
    match res {
        Ok(ref res) | Err(ref res) => {
            let mut in_start = true;
            for r in res {
                if in_start {
                    let mut found = false;
                    for q in facts {
                        if q == r {
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        in_start = false;
                        print!("----------------------------------------");
                        println!("----------------------------------------");
                    }
                }
                println!("{}", r);
            }
        }
    }

    println!("");
    if res.is_ok() {
        println!("OK");
    } else {
        println!("ERROR");
    }
}

fn print_help() {print!("{}", include_str!("../assets/help.txt"))}
