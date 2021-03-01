// ........
// ..21..E.
// .######.
//
// // E: exit
// // #: ground
// // 1-9: snek

// One thing on a grid tile.
enum Tile {
    // Empty space, we can move through this.
    Empty,
    // Level exit - we win if snek head gets here.
    Exit,
    // We can be supported by this.
    Ground,
    // A segment of the snek, where 1 is head, and rest of the segments are incrementally higher
    // numbers.
    Snek(u8),
}

struct State {
    rows: Vec<Vec<Tile>>,
}

impl State {
    /// Takes in all lines, parses them, builds state.
    fn parse(text: &str) -> State {
        let rows = text
            .lines()
            .filter(|line| !(line.is_empty() || line.starts_with("//")))
            .map(State::parse_row)
            .collect();
        State { rows }
    }

    /// Parses a single line of level input, returns a row of tiles.
    fn parse_row(line: &str) -> Vec<Tile> {
        line.chars()
            .map(|c| match c {
                '.' => Tile::Empty,
                'E' => Tile::Exit,
                '#' => Tile::Ground,
                '1'..='9' => Tile::Snek(c.to_digit(10).unwrap() as u8),
                _ => {
                    panic!("invalid input: {}", line);
                }
            })
            .collect()
    }

    fn format_row(row: &[Tile]) -> String {
        row.iter()
            .map(|tile| match tile {
                Tile::Empty => '.',
                Tile::Exit => 'E',
                Tile::Ground => '#',
                Tile::Snek(x) => std::char::from_digit(*x as u32, 10).unwrap(),
            })
            .chain(std::iter::once('\n'))
            .collect()
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for row in self.rows.iter() {
            f.write_str(&State::format_row(row))?;
            // write!(f, "{}\n", State::format_row(row))?;
        }
        Ok(())
    }
}

fn main() {
    let lines = std::fs::read_to_string("levels/1.txt").unwrap();
    let state = State::parse(&lines);
    println!("initial state:\n{}", &state);
}
