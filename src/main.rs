use std::path::Path;

use snekbirb;

fn main() {
    let path = Path::new("levels/4.txt");
    let maybe_answer = snekbirb::solve(&path);
    if let Some(answer) = maybe_answer {
        println!("winning moves: {}", answer);
    } else {
        println!("no solution found :(");
    }
}
