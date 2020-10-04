use avalog::*;

fn main() {
    println!("=== Avalog 0.2 ===");
    println!("Type `help` for more information.");

    let ref parent = match std::env::current_dir() {
        Ok(x) => x,
        Err(_) => {
            eprintln!("Could not get working directory");
            return;
        }
    };
    let mut facts = vec![];
    let mut settings = ProveSettings {
        hide_facts: false,
        hide_rules: false,
    };
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
            "clear" => {facts.clear(); continue}
            "hide facts" => {
                settings.hide_facts = true;
                continue
            }
            "hide rules" => {
                settings.hide_rules = true;
                continue
            }
            "hide all" => {
                settings.hide_facts = true;
                settings.hide_rules = true;
                continue
            }
            "show facts" => {
                settings.hide_facts = false;
                continue
            }
            "show rules" => {
                settings.hide_rules = false;
                continue
            }
            "show all" => {
                settings.hide_facts = false;
                settings.hide_rules = false;
                continue
            }
            "help" => {print_help(); continue}
            "help hide" => {print_help_hide(); continue}
            "help pairs" => {print_help_pairs(); continue}
            "help roles" => {print_help_roles(); continue}
            "help avatars" => {print_help_avatars(); continue}
            "help application" => {print_help_application(); continue}
            x => {
                if x.starts_with("prove ") {
                    match parse_str(x[6..].trim(), &parent) {
                        Ok(goals) => {
                            prove(&goals, &facts, &settings);
                            continue;
                        }
                        Err(err) => {
                            println!("ERROR:\n{}", err);
                            continue;
                        }
                    }
                } else if x.starts_with("echo ") {
                    match parse_str(x[5..].trim(), parent) {
                        Ok(facts) => {
                            println!("{:#?}", facts);
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

        match parse_str(&input, parent) {
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

pub struct ProveSettings {
    pub hide_facts: bool,
    pub hide_rules: bool,
}

fn prove(goals: &[Expr], facts: &[Expr], settings: &ProveSettings) {
    let order_constraints = vec![];

    let res = solve_and_reduce(
        &facts,
        &goals,
        None,
        &[],
        &order_constraints,
        infer,
    );
    let mut count_hidden_facts = 0;
    let mut count_hidden_rules = 0;
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
                        if count_hidden_facts > 0 {
                            println!("<---oo-o--< {} hidden facts >--o-oo--->", count_hidden_facts);
                        }
                        if count_hidden_rules > 0 {
                            println!("<---oo-o--< {} hidden rules >--o-oo--->", count_hidden_rules);
                        }
                        print!("----------------------------------------");
                        println!("----------------------------------------");
                    }
                }
                let rule = if let Expr::Rule(_, _) = r {true} else {false};
                let hide = in_start && (!rule && settings.hide_facts ||
                    rule && settings.hide_rules);
                if !hide {
                    println!("{}", r);
                } else {
                    if !rule {count_hidden_facts += 1};
                    if rule {count_hidden_rules += 1};
                }
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
fn print_help_hide() {print!("{}", include_str!("../assets/help-hide.txt"))}
fn print_help_pairs() {print!("{}", include_str!("../assets/help-pairs.txt"))}
fn print_help_roles() {print!("{}", include_str!("../assets/help-roles.txt"))}
fn print_help_avatars() {print!("{}", include_str!("../assets/help-avatars.txt"))}
fn print_help_application() {print!("{}", include_str!("../assets/help-application.txt"))}
