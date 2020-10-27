use std::time::SystemTime;
use std::path::PathBuf;

use avalog::*;

fn main() {
    println!("=== Avalog 0.4 ===");
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
        last_import: None,
        graph_file: None,
        graph_layout: None,
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
            "no maxsize" => {
                settings.max_size = None;
                continue
            }
            "reload" => {
                if let Some(v) = settings.last_import.as_ref() {
                    input = v.into();
                } else {
                    println!("ERROR: No last import");
                    continue;
                }
            }
            "graph" => {
                export_graph(&facts, &settings, parent);
                continue;
            }
            "no graphf" => {
                settings.graph_file = None;
                continue;
            }
            "help" => {print_help(); continue}
            "help hide" => {print_help_hide(); continue}
            "help pairs" => {print_help_pairs(); continue}
            "help roles" => {print_help_roles(); continue}
            "help avatars" => {print_help_avatars(); continue}
            "help application" => {print_help_application(); continue}
            "help rules" => {print_help_rules(); continue}
            "help equality" => {print_help_equality(); continue}
            "help inequality" => {print_help_inequality(); continue}
            x => {
                if x.starts_with("?") {
                    match parse_str(x[1..].trim(), &parent) {
                        Ok(pat) => {
                            search_pat(&pat, &facts, &settings);
                            continue;
                        }
                        Err(err) => {
                            println!("ERROR:\n{}", err);
                            continue;
                        }
                    }
                } else if x.starts_with("prove ") {
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
                    settings.last_import = Some(input.clone());
                    // Don't continue since import is intrinsic.
                } else if x.starts_with("graphf ") {
                    // Set GraphViz file name.
                    if let Some(txt) = json_str(x[7..].trim()) {
                        settings.graph_file = Some(parent.join(&txt));
                    }
                    continue;
                } else if x.starts_with("graphl ") {
                    if let Some(txt) = json_str(x[7..].trim()) {
                        settings.graph_layout = Some(txt);
                    }
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

// Parses a JSON string.
fn json_str(txt: &str) -> Option<String> {
    use read_token::ReadToken;
    let r = ReadToken::new(txt, 0);
    if let Some(range) = r.string() {
        if let Ok(txt) = r.parse_string(range.length) {
            Some(txt)
        } else {
            println!("ERROR:\nCould not parse string");
            None
        }
    } else {
        println!("ERROR:\nExpected string");
        None
    }
}

pub struct ProveSettings {
    pub hide_facts: bool,
    pub hide_rules: bool,
    pub max_size: Option<usize>,
    pub reduce: bool,
    pub last_import: Option<String>,
    pub graph_file: Option<PathBuf>,
    pub graph_layout: Option<String>,
}

fn conclusion(
    res: &Result<Vec<Expr>, Vec<Expr>>,
    start_time: SystemTime,
    end_time: SystemTime,
    settings: &ProveSettings,
) {
    println!("");
    if res.is_ok() {
        println!("OK");
    } else {
        println!("ERROR");
        if let Some(m) = settings.max_size {
            let n = match res {Ok(x) | Err(x) => x.len()};
            if n >= m {
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

fn export_graph(facts: &[Expr], settings: &ProveSettings, parent: &PathBuf) {
    use std::fmt::Write;
    use std::collections::{HashMap, HashSet};
    use std::fs::File;

    let start_time = SystemTime::now();
    let res = search(
        &facts,
        |e| if let Expr::Rel(_, _) = e {Some(e.clone())}
            else if let Expr::RoleOf(_, _) = e {Some(e.clone())}
            else {None},
        settings.max_size,
        &[],
        &[],
        infer
    );
    let end_time = SystemTime::now();
    let n: usize;
    let mut nodes: HashSet<Expr> = HashSet::new();
    let mut roles: HashMap<Expr, Expr> = HashMap::new();
    match res {
        Ok(ref res) | Err(ref res) => {
            for e in res {
                if let Expr::Rel(a, b) = e {
                    if !nodes.contains(a) {nodes.insert((**a).clone());}
                    if !nodes.contains(b) {nodes.insert((**b).clone());}
                } else if let Expr::RoleOf(a, b) = e {
                    if !roles.contains_key(a) {
                        roles.insert((**a).clone(), (**b).clone());
                    }
                }
            }
            n = res.len()
        }
    }
    conclusion(&res, start_time, end_time, settings);
    println!("{} results found", n);

    let file = if let Some(file) = &settings.graph_file {
        parent.join(file)
    } else if let Some(last_import) = &settings.last_import {
        if let Some(txt) = json_str(last_import[7..].trim()) {
            parent.join(txt + ".dot")
        } else {
            return
        }
    } else {
        parent.join("tmp.dot")
    };
    println!("Exporting to `{}`", file.clone().into_os_string().into_string().unwrap());

    let layout: &str = settings.graph_layout.as_ref().map(|s| &**s).unwrap_or("neato");
    let node_color = "#ffffffa0";
    let edge_color = "black";
    let mut s = String::new();
    writeln!(&mut s, "digraph G {{").unwrap();
    writeln!(&mut s, "  layout={}; edge[penwidth=1,color=\"{}\"]", layout, edge_color).unwrap();
    writeln!(&mut s, "  node[regular=true,style=filled,fillcolor=\"{}\"]", node_color).unwrap();
    match res {
        Ok(ref res) | Err(ref res) => {
            for e in res {
                if let Expr::Rel(a, b) = e {
                    if roles.contains_key(b) {
                        writeln!(&mut s, "  \"{}\" -> \"{}\"[label=\"{}\"];", a, b,
                                 roles.get(b).unwrap()).unwrap();
                    } else {
                        writeln!(&mut s, "  \"{}\" -> \"{}\";", a, b).unwrap();
                    }
                }
            }
        }
    }
    writeln!(&mut s, "}}").unwrap();

    match File::create(file) {
        Ok(mut f) => {
            use std::io::Write;
            if write!(&mut f, "{}", s).is_err() {
                println!("ERROR:\nWhen exporting graph to file");
            }
        }
        Err(_) => {
            println!("ERROR:\nCould not create file");
            return;
        }
    };
}

fn search_pat(pat: &[Expr], facts: &[Expr], settings: &ProveSettings) {
    if pat.len() == 0 {return};

    let pat = &pat[0];
    let start_time = SystemTime::now();
    let res = search(
        &facts,
        |e| {
            let mut vs = vec![];
            let mut tail = vec![];
            if bind(pat, e, &mut vs, &mut tail) {
                Some(e.clone())
            } else {
                None
            }
        },
        settings.max_size,
        &[],
        &[],
        infer
    );
    let end_time = SystemTime::now();
    let n: usize;
    match res {
        Ok(ref res) | Err(ref res) => {
            for r in res {
                println!("{}", r);
            }
            n = res.len();
        }
    }
    conclusion(&res, start_time, end_time, settings);
    println!("{} results found", n);
}

fn prove(goals: &[Expr], facts: &[Expr], settings: &ProveSettings) {
    let start_time = SystemTime::now();
    let f = if settings.reduce {solve_and_reduce} else {solve};
    let res = f(
        &facts,
        &goals,
        settings.max_size,
        &[],
        &[],
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

    conclusion(&res, start_time, end_time, settings);
}

fn print_help() {print!("{}", include_str!("../assets/help.txt"))}
fn print_help_hide() {print!("{}", include_str!("../assets/help-hide.txt"))}
fn print_help_pairs() {print!("{}", include_str!("../assets/help-pairs.txt"))}
fn print_help_roles() {print!("{}", include_str!("../assets/help-roles.txt"))}
fn print_help_avatars() {print!("{}", include_str!("../assets/help-avatars.txt"))}
fn print_help_application() {print!("{}", include_str!("../assets/help-application.txt"))}
fn print_help_rules() {print!("{}", include_str!("../assets/help-rules.txt"))}
fn print_help_equality() {print!("{}", include_str!("../assets/help-equality.txt"))}
fn print_help_inequality() {print!("{}", include_str!("../assets/help-inequality.txt"))}
