use std::path::Path;

use snekbirb;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: snekbirb <level>");
        return;
    }
    let path = Path::new(&args[1]);
    let maybe_answer = snekbirb::solve(&path);
    if let Some(answer) = maybe_answer {
        println!("winning moves: {}", answer);
    } else {
        println!("no solution found :(");
    }
}
