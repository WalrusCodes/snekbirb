// One thing on a grid tile.
#[derive(Debug, Clone)]
enum Tile {
    // Empty space, we can move through this.
    Empty,
    // Level exit - we win if snek head gets here.
    Exit,
    // We can be supported by this.
    Ground,
    // A segment of the snek, where 1 is head, and rest of the segments are incrementally higher
    // numbers.
    Snek(usize),
}

enum Direction {
    Left,
    Right,
    Up,
    Down,
}

/// Position in the grid (row, col).
#[derive(Debug, Clone)]
struct Pos(usize, usize);

impl Pos {
    fn apply(&self, dir: &Direction) -> Pos {
        let x = match dir {
            Direction::Left => (self.0, self.1 - 1),
            Direction::Right => (self.0, self.1 + 1),
            Direction::Up => (self.0 - 1, self.1),
            Direction::Down => (self.0 + 1, self.1),
        };
        Pos(x.0, x.1)
    }
}

#[derive(Debug, Clone)]
struct State {
    rows: Vec<Vec<Tile>>,
    snek_count: usize,
}

impl State {
    /// Takes in all lines, parses them, builds state.
    fn parse(text: &str) -> State {
        let rows = text
            .lines()
            .filter(|line| !(line.is_empty() || line.starts_with("//")))
            .map(State::parse_row)
            .collect();
        // TODO: support multiple sneks.
        let snek_count = 1;
        State { rows, snek_count }
    }

    /// Parses a single line of level input, returns a row of tiles.
    fn parse_row(line: &str) -> Vec<Tile> {
        line.chars()
            .map(|c| match c {
                '.' => Tile::Empty,
                'E' => Tile::Exit,
                '#' => Tile::Ground,
                '0'..='9' => Tile::Snek(c.to_digit(10).unwrap() as usize),
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

    /// Returns the tile in given position.
    fn get(&self, pos: &Pos) -> Tile {
        self.rows[pos.0][pos.1].clone()
    }

    /// Sets the tile in given position.
    fn set(&mut self, pos: &Pos, tile: &Tile) {
        self.rows[pos.0][pos.1] = tile.clone();
    }

    /// Finds positions of snek segments.
    ///
    /// Returned vector contains positions of snek segments, with head being in position 0.
    // TODO: add support for more than one snek.
    fn find_snek(&self) -> Vec<Pos> {
        let mut snek = Vec::<Pos>::new();

        // Scan through the tiles, look for Tile::Sneks, put them in the right position.
        for (row_idx, row) in self.rows.iter().enumerate() {
            for (col_idx, tile) in row.iter().enumerate() {
                if let Tile::Snek(snek_num) = tile {
                    if (*snek_num + 1) >= snek.len() {
                        snek.resize(snek_num + 1, Pos(99999, 99999));
                    }
                    snek[*snek_num] = Pos(row_idx, col_idx);
                }
            }
        }
        snek
    }

    /// Returns true if any segment of the snek is sitting on something solid (just Ground for
    /// now).
    //
    // TODO: pass in which snek.
    fn is_snek_supported(&self, snek: &[Pos]) -> bool {
        for pos in snek.iter() {
            let tile_under = self.get(&pos.apply(&Direction::Down));
            if let Tile::Ground = tile_under {
                return true;
            }
            // TODO: fruit support snek too.
        }
        false
    }

    /// Removes given snek from the state, decrements snek_count.
    fn remove_snek(&mut self, snek: &[Pos]) {
        for pos in snek.iter() {
            self.set(pos, &Tile::Empty);
        }
        self.snek_count -= 1;
    }

    /// Takes some state, applies one movement for one snek, builds new state.
    //
    // TODO: add support for more than one snek.
    fn do_move(&self, dir: Direction) -> Option<State> {
        let snek = self.find_snek();
        let head = snek[0].clone();

        // step 1: check if new tile is free or exit
        let new_tile_pos = head.apply(&dir);
        let new_tile = self.get(&new_tile_pos);

        match new_tile {
            Tile::Ground => {
                return None;
            }
            Tile::Snek(_) => {
                // TODO: try pushing if running into a different snek.
                return None;
            }
            // TODO: only exit if all froot eaten.
            // TODO: support stepping on exit tile without exiting if not all froot eaten.
            Tile::Exit => {
                println!("victory!");
                let mut state = self.clone();
                state.remove_snek(&snek);
                return Some(state);
            }
            Tile::Empty => {}
        };

        // step 2: 2 -> 1 -> [new tile]
        let mut state = self.clone();
        // Set new head.
        state.set(&new_tile_pos, &Tile::Snek(0));
        // Move each segment but the last.
        for idx in 0..(snek.len() - 1) {
            state.set(&snek[idx], &Tile::Snek(idx + 1));
        }
        // Set last segment to empty.
        state.set(&snek.last().unwrap(), &Tile::Empty);

        // step 3: apply gravity
        loop {
            let mut snek = state.find_snek();
            if state.is_snek_supported(&snek) {
                break;
            }
            // sort the snek by rows so that we don't overwrite ourselves as we move the segments
            // down.
            snek.sort_unstable_by_key(|pos| pos.0);
            // snek segments should be sorted by row, smallest (higher rows first). we want to
            // iterate from highest numbered rows first.
            for pos in snek.iter().rev() {
                // make sure we are not falling into the exit. we know that falling into exit
                // head-first is a victory, but we are not sure what the real game does if you fall
                // into the exit not-head-first.
                let pos_below = pos.apply(&Direction::Down);

                if let Tile::Exit = state.get(&pos_below) {
                    println!("victory by falling into exit!");
                    state.remove_snek(&snek);
                    return Some(state);
                }
                state.set(&pos_below, &state.get(pos));
                state.set(pos, &Tile::Empty);
            }
        }
        Some(state)
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

    // moving right should result in a new state.
    // let state_r = state.do_move(Direction::Right).unwrap();
    // println!("resulting state:\n{}", &state_r);

    // // moving up should result in a new state.
    let state_u = state.do_move(Direction::Up).unwrap();
    println!("resulting state:\n{}", &state_u);

    // moving up, then right should result in snek falling.
    let state_ur = state_u.do_move(Direction::Right).unwrap();
    println!("resulting state:\n{}", &state_ur);
    let state_urr = state_ur.do_move(Direction::Right).unwrap();
    println!("resulting state:\n{}", &state_urr);
    let state_urrr = state_urr.do_move(Direction::Right).unwrap();
    println!("resulting state:\n{}", &state_urrr);
    let state_urrrr = state_urrr.do_move(Direction::Right).unwrap();
    println!("resulting state:\n{}", &state_urrrr);
    dbg!(&state_urrrr.snek_count);

    // confirm that we can't move down from the starting state.
    // assert!(state.do_move(Direction::Down).is_none());
}
