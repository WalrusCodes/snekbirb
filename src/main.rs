use std::path::Path;

use snekbirb;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 || args.len() > 4 {
        println!("Usage: snekbirb <level> [<move string> --animate]]");
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
        let results = snekbirb::apply_moves(path, moves);
        let animate = args.len() > 3 && args[3] == "--animate";
        for (mv, grid) in results {
            if animate {
                // clear screen, move to "home" position
                println!("\x1b[2J\x1b[H{}\n{}", mv, grid);
                std::thread::sleep(std::time::Duration::from_millis(250));
            } else {
                println!("\n{}\n{}", mv, grid);
            }
        }
    }
}
