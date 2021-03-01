use std::collections::VecDeque;

// One thing on a grid tile.
#[derive(Debug, Clone)]
enum Tile {
    // Empty space, we can move through this.
    Empty,
    // Level exit - we win if snek head gets here.
    Exit,
    // We can be supported by this.
    Ground,
    // Froot can be eaten to elongate snek.
    Fruit,
    // A segment of the snek, where 1 is head, and rest of the segments are incrementally higher
    // numbers.
    Snek(usize),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
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

    /// Checks if given direction can be applied without going out of bounds.
    fn can_apply(&self, dir: &Direction, rows: usize, cols: usize) -> bool {
        match dir {
            Direction::Left => self.1 != 0,
            Direction::Right => self.1 < (cols - 1),
            Direction::Up => self.0 != 0,
            Direction::Down => self.0 < (rows - 1),
        }
    }

    fn maybe_apply(&self, dir: &Direction, rows: usize, cols: usize) -> Option<Pos> {
        if self.can_apply(dir, rows, cols) {
            Some(self.apply(dir))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    rows: Vec<Vec<Tile>>,
    // For convenience, the number of sneks left on the grid.
    snek_count: usize,
    // Moves made so far.
    moves: Vec<Direction>,
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
        State {
            rows,
            snek_count,
            moves: Vec::new(),
        }
    }

    /// Parses a single line of level input, returns a row of tiles.
    fn parse_row(line: &str) -> Vec<Tile> {
        line.chars()
            .map(|c| match c {
                '.' => Tile::Empty,
                'E' => Tile::Exit,
                'F' => Tile::Fruit,
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
                Tile::Fruit => 'F',
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
            if let Some(tile_under_pos) = self.maybe_apply_pos(pos, Direction::Down) {
                match self.get(&tile_under_pos) {
                    Tile::Ground | Tile::Fruit => {
                        return true;
                    }
                    // TODO: support being supported by other sneks.
                    _ => {}
                }
            }
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

    /// Applies gravity - makes sneks fall down until they are supported at least on one segment.
    ///
    /// Returns true iff snek is within bounds.
    fn apply_gravity(&mut self) -> bool {
        // TODO: support multiple sneks.
        loop {
            let mut snek = self.find_snek();
            if self.is_snek_supported(&snek) {
                return true;
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
                if let Some(pos_below) = self.maybe_apply_pos(pos, Direction::Down) {
                    if let Tile::Exit = self.get(&pos_below) {
                        println!("victory by falling into exit!");
                        self.remove_snek(&snek);
                        return true;
                    }
                    self.set(&pos_below, &self.get(pos));
                    self.set(pos, &Tile::Empty);
                } else {
                    // We tried to fall out of bounds.
                    return false;
                }
            }
        }
    }

    /// Applies direction to given position if able, based on level boundaries.
    fn maybe_apply_pos(&self, pos: &Pos, dir: Direction) -> Option<Pos> {
        return pos.maybe_apply(&dir, self.rows.len(), self.rows.first().unwrap().len());
    }

    /// Takes some state, applies one movement for one snek, builds new state.
    //
    // TODO: add support for more than one snek.
    fn do_move(&self, dir: Direction) -> Option<State> {
        let snek = self.find_snek();
        let head = snek[0].clone();

        // step 1: check if new tile is free or exit

        // check if direction is valid based on current position and level boundaries
        let new_tile_pos = match self.maybe_apply_pos(&head, dir) {
            Some(pos) => pos,
            None => return None,
        };
        let new_tile = self.get(&new_tile_pos);

        let mut state = self.clone();
        state.moves.push(dir);

        match new_tile {
            Tile::Ground => {
                return None;
            }
            Tile::Snek(_) => {
                // TODO: try pushing if running into a different snek.
                return None;
            }
            Tile::Fruit => {
                // step 2: 2 -> 1 -> 0
                state.set(&new_tile_pos, &Tile::Snek(0));
                // increment the number on each snek segment by one.
                for (idx, pos) in snek.iter().enumerate() {
                    state.set(pos, &Tile::Snek(idx + 1));
                }
            }
            // TODO: only exit if all froot eaten.
            // TODO: support stepping on exit tile without exiting if not all froot eaten.
            Tile::Exit => {
                state.remove_snek(&snek);
                return Some(state);
            }
            Tile::Empty => {
                // step 2: 2 -> 1 -> [new tile]
                // Set new head.
                state.set(&new_tile_pos, &Tile::Snek(0));
                // Move each segment but the last.
                for idx in 0..(snek.len() - 1) {
                    state.set(&snek[idx], &Tile::Snek(idx + 1));
                }
                // Set last segment to empty.
                state.set(&snek.last().unwrap(), &Tile::Empty);
            }
        };

        // step 3: apply gravity. if this returns false, snek was trying to fall off the level
        // (with at least one segment being outside the bounds).
        if !state.apply_gravity() {
            None
        } else {
            Some(state)
        }
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "sneks: {}\n", self.snek_count)?;
        for row in self.rows.iter() {
            f.write_str(&State::format_row(row))?;
        }
        Ok(())
    }
}

struct SearchState {
    /// States that we still need to examine.
    queue: VecDeque<State>,
}

impl SearchState {
    /// Parses the level file, creates initial queue of the first state.
    fn load_level(filename: &str) -> SearchState {
        let lines = std::fs::read_to_string(filename).unwrap();
        let state = State::parse(&lines);
        let mut queue = VecDeque::new();
        queue.push_back(state);
        SearchState { queue }
    }

    /// Pulls the front state off of the queue, tries out all possible movements, pushes new viable
    /// states to the queue.
    ///
    /// Returns winning state if found, otherwise returns None.
    fn process_one_state(&mut self) -> Option<State> {
        let current = self.queue.pop_front().unwrap();
        // try all possible directions:
        let dirs_to_try = [
            Direction::Left,
            Direction::Right,
            Direction::Up,
            Direction::Down,
        ];
        for dir in dirs_to_try.iter() {
            if let Some(new_state) = current.do_move(*dir) {
                if new_state.snek_count == 0 {
                    return Some(new_state);
                }
                self.queue.push_back(new_state);
            }
        }
        // Did not get to victory yet.
        None
    }

    /// Keeps processing items from the queue until victory or out of new states.
    fn run(&mut self) -> Option<State> {
        while !self.queue.is_empty() {
            if self.queue.len() > 100_000 {
                panic!("too many states in queue");
            }

            if let Some(winning_state) = self.process_one_state() {
                println!("victory!!!");
                println!("{}", winning_state);
                return Some(winning_state);
            }
        }
        None
    }
}

fn main() {
    let mut s = SearchState::load_level("levels/2.txt");
    println!("initial state:\n{}", &s.queue.front().unwrap());

    let maybe_state = s.run();
    dbg!(&maybe_state);
}
