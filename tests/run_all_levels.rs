use walkdir::{DirEntry, WalkDir};

use snekbirb;

/// Returns true iff the directory entry is a file ending with ".txt".
fn is_level_file(entry: &DirEntry) -> bool {
    entry.file_type().is_file() && entry.file_name().to_str().unwrap().ends_with(".txt")
}

/// Runs the solver on all levels (".txt" files under "levels/") and ensures that each has a
/// solution.
#[test]
fn run_all_levels() {
    let walker = WalkDir::new("examples/").into_iter();
    for entry in walker {
        let entry = entry.unwrap();
        if !is_level_file(&entry) {
            continue;
        }
        let path = entry.path();
        println!("Running level: {}", path.to_str().unwrap());
        snekbirb::solve(path).expect(&format!("'{:?}' has a solution", &entry));
    }
}
