use std::path::Path;

use snekbirb;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 && args.len() != 3 {
        println!("Usage: snekbirb <level> [move string]");
        return;
    }
    let path = Path::new(&args[1]);
    if args.len() == 2 {
        let maybe_answer = snekbirb::solve(&path);
        if let Some(answer) = maybe_answer {
            println!("winning moves: {}", answer);
        } else {
            println!("no solution found :(");
        }
    } else {
        let moves = &args[2];
        snekbirb::apply_moves(path, moves);
    }
}
