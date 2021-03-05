use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
    iter::once,
    path::Path,
};

// One thing on a grid tile.
#[derive(Debug, Clone, PartialEq, Hash)]
enum Tile {
    // Empty space, we can move through this.
    Empty,
    // Level exit - we win if snek head gets here. We use this enum for parsing, but then replace
    // it with Empty in the state so that a snek can occupy the exit space before exit is open.
    Exit,
    // We can be supported by this.
    Ground,
    // Froot can be eaten to elongate snek.
    Fruit,
    // Spiky boi - stepping on it, you ded.
    Spike,
    // A segment of the snek, where 0 is head, and rest of the segments are incrementally higher
    // numbers. A gap in numbers denotes start of a different snek. We only use RawSnek when
    // loading the level.
    RawSnek(usize),
    // A segment of the snek. The number describes which Snek it is (0: first snek, 1: second
    // snek, etc.).
    SomeSnek(usize),
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    Left,
    Right,
    Up,
    Down,
}

#[derive(PartialEq)]
enum PushResult {
    Moved,
    DidNotMove,
    WouldDie,
}

/// Position in the grid (row, col).
#[derive(Debug, Clone, PartialEq, Copy, Hash)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Move(usize, Direction);
type Moves = Vec<Move>;

#[derive(Debug, Clone)]
struct State {
    rows: Vec<Vec<Tile>>,
    // Location of the exit tile.
    exit_pos: Pos,
    // For convenience, the number of fruit left on the grid.
    fruit_count: usize,
    // Sneks on a grid: each outer vector describes a snek, inner vector describes positions of the
    // segments, head first. When snek is gone, an empty vector remains.
    sneks: Vec<Vec<Pos>>,
    // For convenience, the number of sneks left on the grid.
    snek_count: usize,
    // Moves made so far.
    moves: Moves,
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.rows == other.rows && self.sneks == other.sneks
    }
}

impl Hash for State {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        for row in self.rows.iter() {
            Hash::hash_slice(row, hasher);
        }
        for snek in self.sneks.iter() {
            Hash::hash_slice(snek, hasher);
        }
    }
}

impl Eq for State {}

impl State {
    /// Finds all tiles that match given predicatve.
    fn find_tiles_by_predicate<F>(&self, fun: F) -> Vec<Pos>
    where
        F: Fn(&Tile) -> bool,
    {
        let mut out = Vec::new();
        for (row_idx, row) in self.rows.iter().enumerate() {
            for (col_idx, tile) in row.iter().enumerate() {
                if fun(tile) {
                    out.push(Pos(row_idx, col_idx));
                }
            }
        }
        out
    }

    /// Finds all tiles of given type.
    fn find_tiles(&self, tile_cmp: &Tile) -> Vec<Pos> {
        self.find_tiles_by_predicate(|t| t == tile_cmp)
    }

    /// Takes in all lines, parses them, builds state.
    fn parse(text: &str) -> State {
        let rows = text
            .lines()
            .filter(|line| !(line.is_empty() || line.starts_with("//")))
            .map(State::parse_row)
            .collect();
        let mut tmp = State {
            rows,
            exit_pos: Pos(0, 0), // placeholder
            sneks: Vec::new(),
            fruit_count: 0, // placeholder
            snek_count: 0,  // placeholder
            moves: Vec::new(),
        };
        let sneks = tmp.find_sneks();
        // Replace RawSneks with SomeSneks which describe just which snek it is.
        for (idx, segments) in sneks.iter().enumerate() {
            for pos in segments.iter() {
                tmp.set(pos, &Tile::SomeSnek(idx));
            }
        }
        tmp.sneks = sneks;

        tmp.snek_count = tmp.sneks.len();
        tmp.fruit_count = tmp.find_tiles(&Tile::Fruit).len();
        let mut exit_tiles = tmp.find_tiles(&Tile::Exit);
        assert_eq!(exit_tiles.len(), 1);
        tmp.exit_pos = exit_tiles.pop().unwrap();
        tmp.set(&tmp.exit_pos.clone(), &Tile::Empty);
        tmp
    }

    fn from_base36(c: char) -> usize {
        c.to_digit(36).unwrap() as usize
    }

    /// Parses a single line of level input, returns a row of tiles.
    fn parse_row(line: &str) -> Vec<Tile> {
        line.chars()
            .map(|c| match c {
                '.' => Tile::Empty,
                'E' => Tile::Exit,
                'F' => Tile::Fruit,
                '#' => Tile::Ground,
                '*' => Tile::Spike,
                '0'..='9' | 'a'..='z' => Tile::RawSnek(State::from_base36(c)),
                _ => {
                    panic!("invalid input: {}", line);
                }
            })
            .collect()
    }

    fn format_tile(&self, pos: &Pos) -> char {
        if *pos == self.exit_pos {
            return 'E';
        }
        let tile = self.get(pos);
        match tile {
            Tile::Empty => '.',
            Tile::Exit => 'E',
            Tile::Fruit => 'F',
            Tile::Ground => '#',
            Tile::Spike => '*',
            Tile::RawSnek(x) => std::char::from_digit(x as u32, 36).unwrap(),
            Tile::SomeSnek(x) => std::char::from_digit(x as u32, 36).unwrap(),
        }
    }

    fn format_row(&self, row_idx: usize) -> String {
        (0..self.rows[0].len())
            .map(|col_idx| self.format_tile(&Pos(row_idx, col_idx)))
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

    /*
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
    */

    /// Finds positions of all snek segments.
    ///
    /// Returned vector contains vectors, which contain positions of snek segments, with head being
    /// in position 0.
    fn find_sneks(&self) -> Vec<Vec<Pos>> {
        // 012   <-- snek #0
        // 456   <-- snek #1

        let snek_poses = self.find_tiles_by_predicate(|t| matches!(t, Tile::RawSnek(_)));

        // A mapping of all snek indices to their positions.
        let mut snek_indices: HashMap<usize, Pos> = HashMap::new();
        for pos in snek_poses.iter() {
            if let Tile::RawSnek(idx) = self.get(pos) {
                assert!(snek_indices.insert(idx, pos.clone()).is_none());
            }
        }

        // Sort the indices. After this, we have a vector that looks like this: [0, 1, 2, 4, 5, 6].
        let mut sorted_snek_indices: Vec<usize> = snek_indices.keys().cloned().collect();
        sorted_snek_indices.sort();

        // Walk through the vector, split into ranges based on gaps.
        let mut out: Vec<Vec<Pos>> = Vec::new();
        let mut last_idx = 0;
        let mut first = true;
        for idx in sorted_snek_indices.iter() {
            // 012456
            //   ^
            //   last_idx = 2  idx = 4  -> gap
            //   last_idx = 1  idx = 2  -> no gap
            if first || ((last_idx + 1) < *idx) {
                out.push(Vec::new());
            }
            out.last_mut()
                .unwrap()
                .push(snek_indices.get(idx).unwrap().clone());
            first = false;
            last_idx = *idx;
        }
        out
    }

    /*
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
    */

    /// Removes given snek from the state, decrements snek_count.
    fn remove_snek(&mut self, snek_idx: usize) {
        let snek_poses = self.sneks[snek_idx].clone();
        for pos in snek_poses.iter() {
            self.set(&pos, &Tile::Empty);
        }
        self.sneks[snek_idx].clear();
        self.snek_count -= 1;
    }

    /// Applies gravity - makes sneks fall down until they are supported at least on one segment.
    ///
    /// Returns true iff sneks are within bounds & didn't die.
    fn apply_gravity(&mut self) -> bool {
        // keep trying to push each object down until nothing moves.
        let mut again = true;
        while again {
            again = false;
            for idx in 0..self.sneks.len() {
                if self.sneks[idx].is_empty() {
                    continue;
                }
                let pos = self.sneks[idx].first().unwrap().clone();
                match self.push(None, &pos, Direction::Down) {
                    PushResult::Moved => {
                        again = true;
                    }
                    PushResult::DidNotMove => {}
                    PushResult::WouldDie => {
                        return false;
                    }
                }
            }
        }
        true
        /*
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
                    if let Tile::Spike = self.get(&pos_below) {
                        // we are falling on a spike, so this is a dead-end
                        return false;
                    }
                    if pos_below == self.exit_pos && self.fruit_count == 0 {
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
        */
    }

    /// Applies direction to given position if able, based on level boundaries.
    fn maybe_apply_pos(&self, pos: &Pos, dir: Direction) -> Option<Pos> {
        return pos.maybe_apply(&dir, self.rows.len(), self.rows.first().unwrap().len());
    }

    fn get_object_id(tile: &Tile) -> usize {
        match tile {
            Tile::SomeSnek(idx) => *idx,
            _ => panic!("not the right tile: {:?}", tile),
        }
    }

    /// Tries pushing the new tile in given direction, transitively pushing everything
    /// that can move.
    ///
    /// If maybe_snek_idx is passed in, that's the snek that's causing the pushing and cannot be
    /// moved.
    fn push(
        &mut self,
        maybe_snek_idx: Option<usize>,
        tile_pos: &Pos,
        dir: Direction,
    ) -> PushResult {
        // Objects that are already moving.
        let mut moving: HashSet<usize> = HashSet::new();
        // Objects that we need to test.
        let mut queue: VecDeque<usize> = VecDeque::new();

        let obj_idx = State::get_object_id(&self.get(tile_pos));
        queue.push_back(obj_idx);
        moving.insert(obj_idx);

        while !queue.is_empty() {
            let idx = queue.pop_front().unwrap();
            let mut has_solid_rest = false;
            let mut has_spike = false;
            for pos in self.sneks[idx].iter() {
                if let Some(new_pos) = self.maybe_apply_pos(pos, dir) {
                    match self.get(&new_pos) {
                        Tile::Empty => (),
                        Tile::Spike => {
                            has_spike = true;
                        }
                        Tile::Fruit | Tile::Ground => {
                            has_solid_rest = true;
                        }
                        Tile::SomeSnek(other_idx) => {
                            if maybe_snek_idx.is_some() && maybe_snek_idx.unwrap() == other_idx {
                                // trying to push ourselves -> bad
                                return PushResult::DidNotMove;
                            }
                            if !moving.contains(&other_idx) {
                                // trying to push something that we don't know if it moves yet
                                queue.push_back(other_idx);
                                moving.insert(other_idx);
                            }
                        }
                        Tile::Exit | Tile::RawSnek(_) => panic!("shouldn't be seeing these"),
                    }
                } else {
                    // out of bounds
                    return PushResult::WouldDie;
                }
            }
            if has_solid_rest {
                return PushResult::DidNotMove;
            } else if has_spike {
                return PushResult::WouldDie;
            }
        }

        // If we are here, it means the original object that was being pushed can move, possibly
        // also moving other objects. We perform the move here, checking for exit as well.

        // dbg!("moving: ", &moving);
        // println!("{}", &self);

        let mut new_rows = self.rows.clone();
        let mut new_sneks = self.sneks.clone();
        // Clear out the objects being moved.
        for obj_idx in moving.iter() {
            for pos in self.sneks[*obj_idx].iter() {
                new_rows[pos.0][pos.1] = Tile::Empty;
            }
        }
        // Now reapply with the movement.
        for obj_idx in moving.iter() {
            new_sneks[*obj_idx].clear();
            for pos in self.sneks[*obj_idx].iter() {
                let new_pos = pos.apply(&dir);
                new_rows[new_pos.0][new_pos.1] = self.rows[pos.0][pos.1].clone();
                new_sneks[*obj_idx].push(new_pos);
            }
        }
        self.rows = new_rows;
        self.sneks = new_sneks;

        // Check if any snek moved into exit.
        if self.fruit_count == 0 {
            for obj_idx in moving.iter() {
                if let Some(snek_head) = self.sneks[*obj_idx].first() {
                    if *snek_head == self.exit_pos {
                        self.remove_snek(*obj_idx);
                    }
                }
            }
        }

        // dbg!("after move");
        // println!("{}", &self);

        PushResult::Moved
    }

    /// Takes some state, applies one movement for one snek, builds new state.
    fn do_move(&self, snek_idx: usize, dir: Direction) -> Option<State> {
        let snek = &self.sneks[snek_idx];
        let head = snek[0].clone();

        // step 1: check if new tile is free or exit

        // check if direction is valid based on current position and level boundaries
        let new_tile_pos = match self.maybe_apply_pos(&head, dir) {
            Some(pos) => pos,
            None => return None,
        };
        let new_tile = self.get(&new_tile_pos);

        let mut state = self.clone();
        state.moves.push(Move(snek_idx, dir));

        match new_tile {
            Tile::Ground => {
                return None;
            }
            Tile::Spike => {
                return None;
            }
            Tile::Fruit => {
                // fruit om nom nom nom
                state.set(&new_tile_pos, &Tile::SomeSnek(snek_idx));
                state.sneks[snek_idx].insert(0, new_tile_pos);
                state.fruit_count -= 1;
            }
            Tile::Exit | Tile::RawSnek(_) => {
                panic!("shouldn't happen");
            }
            Tile::Empty | Tile::SomeSnek(_) => {
                // If we stepping on SomeSnek, it means we are pushing. See if we can push.
                if let Tile::SomeSnek(_) = new_tile {
                    if state.push(Some(snek_idx), &new_tile_pos, dir) != PushResult::Moved {
                        return None;
                    }
                }

                // Check if we are about to step on the exit tile while it is open.
                if new_tile_pos == self.exit_pos && self.fruit_count == 0 {
                    state.remove_snek(snek_idx);
                } else {
                    // Insert the new head, remove last segment.
                    let new_snek = &mut state.sneks[snek_idx];
                    new_snek.insert(0, new_tile_pos); // note: inefficient!
                    new_snek.pop();
                    // Set last segment to empty.
                    state.set(&new_tile_pos, &Tile::SomeSnek(snek_idx));
                    state.set(&snek.last().unwrap(), &Tile::Empty);
                }
            }
        };

        // step 3: apply gravity. if this returns false, snek was trying to fall off the level
        // (with at least one segment being outside the bounds).
        if !state.apply_gravity() {
            // println!("gravity says no");
            None
        } else {
            // println!("gravity says ok");
            Some(state)
        }
    }

    /// Formats the moves that we took as a string.
    fn format_moves(&self) -> String {
        let mut out: Vec<char> = Vec::new();
        let mut prev_snek_idx = 999;
        for Move(snek_idx, dir) in self.moves.iter() {
            if *snek_idx != prev_snek_idx {
                if prev_snek_idx != 999 {
                    out.push(' ');
                }
                out.push(std::char::from_digit(*snek_idx as u32, 10).unwrap());
                out.push(':');
                prev_snek_idx = *snek_idx;
            }
            out.push(match dir {
                Direction::Left => 'L',
                Direction::Right => 'R',
                Direction::Up => 'U',
                Direction::Down => 'D',
            });
        }
        out.iter().collect()
    }
}

impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "sneks: {} froot count: {} exit: {:?}\n",
            self.snek_count, self.fruit_count, self.exit_pos
        )?;
        for row_idx in 0..self.rows.len() {
            f.write_str(&self.format_row(row_idx))?;
        }
        Ok(())
    }
}

struct SearchState {
    /// States that we still need to examine.
    queue: VecDeque<State>,
    /// States that we have seen.
    seen: HashSet<State>,
    /// Mapping from moves to state.
    moves: HashMap<Moves, State>,
}

impl SearchState {
    /// Parses the level file, creates initial queue of the first state.
    fn load_level(file: &Path) -> SearchState {
        let lines = std::fs::read_to_string(file).unwrap();
        let state = State::parse(&lines);
        let mut queue = VecDeque::new();
        queue.push_back(state.clone());
        let mut seen = HashSet::new();
        seen.insert(state);
        SearchState {
            queue,
            seen,
            moves: HashMap::new(),
        }
    }

    /// Pulls the front state off of the queue, tries out all possible movements, pushes new viable
    /// states to the queue.
    ///
    /// Returns winning state if found, otherwise returns None.
    fn process_one_state(&mut self) -> Option<State> {
        let dirs_to_try = [
            Direction::Left,
            Direction::Right,
            Direction::Up,
            Direction::Down,
        ];

        let current = self.queue.pop_front().unwrap();

        // First try the previous snek so that the final sequence of moves alternates between the
        // sneks less, if possible.
        let snek_idx_iter = 0..current.sneks.len();
        let sneks_to_try: Vec<usize> = if let Some(&Move(last_idx, _)) = current.moves.last() {
            once(last_idx)
                .chain(snek_idx_iter.filter(|x| *x != last_idx))
                .collect()
        } else {
            snek_idx_iter.collect()
        };

        // for each snek, try all possible directions:
        for snek_idx in sneks_to_try.into_iter() {
            if current.sneks[snek_idx].is_empty() {
                continue;
            }
            for dir in dirs_to_try.iter() {
                // println!(
                //     "{} trying state {} {:?}",
                //     current.format_moves(),
                //     snek_idx,
                //     dir
                // );

                if let Some(new_state) = current.do_move(snek_idx, *dir) {
                    // If the new state results in victory, return it immediately.
                    if new_state.snek_count == 0 {
                        return Some(new_state);
                    }

                    // Check if we have seen the new state before, if so, discard it.
                    if self.seen.contains(&new_state) {
                        continue;
                    }
                    // println!(
                    //     "  {} added state {} {:?}",
                    //     current.format_moves(),
                    //     snek_idx,
                    //     dir
                    // );
                    self.moves
                        .insert(new_state.moves.clone(), new_state.clone());
                    self.seen.insert(new_state.clone());
                    self.queue.push_back(new_state);
                }
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
                // println!("victory!!!");
                // println!("{}", winning_state);
                return Some(winning_state);
            }
        }
        None
    }
}

/// Loads a level from given path, tries to solve it, and if successful, returns the winning moves.
pub fn solve(file: &Path) -> Option<String> {
    let mut s = SearchState::load_level(file);
    println!("initial state:\n{}", &s.queue.front().unwrap());

    if let Some(winning_state) = s.run() {
        for m in 1..winning_state.moves.len() {
            let moves: Moves = winning_state.moves[0..m].iter().cloned().collect();
            // println!("moves: {:?}", &moves);
            println!("{}", s.moves.get(&moves).unwrap());
        }
        // println!("winning moves: {}", winning_state.format_moves());
        Some(winning_state.format_moves())
    } else {
        None
    }
}
