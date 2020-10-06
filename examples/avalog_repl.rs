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
        max_size: Some(600),
        reduce: true,
    };
    let mut last_import: Option<String> = None;
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
            "no maxsize" => {
                settings.max_size = None;
                continue
            }
            "reload" => {
                if let Some(v) = last_import.as_ref() {
                    input = v.into();
                } else {
                    println!("ERROR: No last import");
                    continue;
                }
            }
            "help" => {print_help(); continue}
            "help hide" => {print_help_hide(); continue}
            "help pairs" => {print_help_pairs(); continue}
            "help roles" => {print_help_roles(); continue}
            "help avatars" => {print_help_avatars(); continue}
            "help application" => {print_help_application(); continue}
            "help rules" => {print_help_rules(); continue}
            x => {
                if x.starts_with("prove ") {
                    match parse_str(x[6..].trim(), &parent) {
                        Ok(goals) => {
                            settings.reduce = true;
                            prove(&goals, &facts, &settings);
                            continue;
                        }
                        Err(err) => {
                            println!("ERROR:\n{}", err);
                            continue;
                        }
                    }
                } else if x.starts_with("provenr ") {
                    match parse_str(x[8..].trim(), &parent) {
                        Ok(goals) => {
                            settings.reduce = false;
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
                } else if x.starts_with("maxsize ") {
                    match x[8..].trim().parse::<usize>() {
                        Ok(n) => settings.max_size = Some(n),
                        Err(_) => eprintln!("ERROR: Could not parse number"),
                    };
                    continue;
                } else if x.starts_with("import ") {
                    last_import = Some(input.clone());
                    // Don't continue since import is intrinsic.
                }
            }
        }

        match parse_str(&input, parent) {
            Ok(new_facts) => {
                facts.extend(new_facts);
                facts.sort();
                facts.dedup();
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
    pub max_size: Option<usize>,
    pub reduce: bool,
}

fn prove(goals: &[Expr], facts: &[Expr], settings: &ProveSettings) {
    use std::time::SystemTime;
    let order_constraints = vec![];

    let start_time = SystemTime::now();
    let f = if settings.reduce {solve_and_reduce} else {solve};
    let res = f(
        &facts,
        &goals,
        settings.max_size,
        &[],
        &order_constraints,
        infer,
    );
    let end_time = SystemTime::now();
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
        if let Some(m) = settings.max_size {
            let n = match res {Ok(x) | Err(x) => x.len()};
            if n == m {
                println!("Maximum limit reached.");
                println!("Use `maxsize <number>` or `no maxsize` to set limit.");
                println!("Current maximum number of facts and rules: {}", m);
            }
        }
    }
    match end_time.duration_since(start_time) {
        Ok(d) => println!("Proof search took {} milliseconds", d.as_millis()),
        Err(_) => {}
    }
}

fn print_help() {print!("{}", include_str!("../assets/help.txt"))}
fn print_help_hide() {print!("{}", include_str!("../assets/help-hide.txt"))}
fn print_help_pairs() {print!("{}", include_str!("../assets/help-pairs.txt"))}
fn print_help_roles() {print!("{}", include_str!("../assets/help-roles.txt"))}
fn print_help_avatars() {print!("{}", include_str!("../assets/help-avatars.txt"))}
fn print_help_application() {print!("{}", include_str!("../assets/help-application.txt"))}
fn print_help_rules() {print!("{}", include_str!("../assets/help-rules.txt"))}
