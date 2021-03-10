use std::{
    collections::{HashMap, VecDeque},
    path::Path,
    time::{Duration, Instant},
};

use lru::LruCache;

mod alloc;
mod moves;
mod pos;
mod tile;

use alloc::CountingAlloc;
use moves::{Move, Moves};
use pos::{Direction, Pos};
use tile::{Grid, Tile};

#[global_allocator]
static ALLOCATOR: CountingAlloc = CountingAlloc::new();

#[derive(Debug, Clone)]
struct State {
    // Grid of tiles.
    grid: Grid,
    // Moves made so far.
    moves: Moves,
}

impl State {
    fn from_file(file: &Path) -> State {
        let lines = std::fs::read_to_string(file).unwrap();
        State {
            grid: Grid::parse(&lines),
            moves: Moves::new(),
        }
    }
}

struct SearchState {
    start: Grid,
    /// States that we still need to examine.
    queue: VecDeque<Moves>,
    /// States that we have seen.
    // seen: HashSet<Grid>,
    moves_to_grids: LruCache<Moves, Grid>,
    /// States we have seen. This is basically a HashSet<Grid>, but to save space, we are storing a
    /// hash of the grid, and in case there is a collision, we redo the moves to verify.
    seen: HashMap<u64, Vec<Moves>>,
}

impl SearchState {
    /// Parses the level file, creates initial queue of the first state.
    fn load_level(file: &Path) -> SearchState {
        let start = Grid::parse(&std::fs::read_to_string(file).unwrap());

        let mut queue = VecDeque::new();
        queue.push_back(Moves::new());

        let mut moves_to_grids = LruCache::new(100_000);
        moves_to_grids.put(Moves::new(), start.clone());

        let mut seen = HashMap::new();
        seen.insert(start.hash_value(), vec![Moves::new()]);
        SearchState {
            start,
            queue,
            moves_to_grids,
            seen,
        }
    }

    fn get_from_lru_or_rebuild(&mut self, moves: &Moves) -> Grid {
        if let Some(grid) = self.moves_to_grids.get(moves) {
            return grid.clone();
        }
        let mut grid = self.start.clone();
        for m in moves {
            grid = grid.do_move(*m).unwrap();
        }
        grid
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
        let snek_idx_iter = 0..self.start.snek_count();
        let sneks_to_try: Vec<u8> = if let Some(Move(last_idx, _)) = current.last() {
            std::iter::once(last_idx)
                .chain(snek_idx_iter.filter(|x| *x != last_idx))
                .collect()
        } else {
            snek_idx_iter.collect()
        };

        let grid = self.get_from_lru_or_rebuild(&current);

        // for each snek, try all possible directions:
        for snek_idx in sneks_to_try.into_iter() {
            if grid.objects[snek_idx as usize].is_empty() {
                continue;
            }
            for dir in dirs_to_try.iter() {
                let new_move = Move::new(snek_idx, *dir);
                // println!(
                //     "{} trying state {} {:?}",
                //     current.format_moves(),
                //     snek_idx,
                //     dir
                // );

                if let Some(new_grid) = grid.do_move(new_move) {
                    let is_victory = new_grid.snek_count == 0;
                    let new_moves = current.clone_and_add(new_move);
                    let h = new_grid.hash_value();
                    if self.seen.contains_key(&h) {
                        // XXX: rebuild (if not in LRU) and check keys.
                        continue;
                    }
                    self.seen.insert(h, vec![new_moves.clone()]);

                    self.moves_to_grids.put(new_moves.clone(), new_grid.clone());

                    // If the new state results in victory, return it immediately.
                    if is_victory {
                        let new_state = State {
                            grid: new_grid.clone(),
                            moves: new_moves,
                        };
                        return Some(new_state);
                    }
                    // println!(
                    //     "  {} added state {} {:?}",
                    //     current.format_moves(),
                    //     snek_idx,
                    //     dir
                    // );
                    self.queue.push_back(new_moves);
                }
            }
        }
        // Did not get to victory yet.
        None
    }

    /// Keeps processing items from the queue until victory or out of new states.
    fn run(&mut self) -> Option<State> {
        let mut next_print = Instant::now();
        while !self.queue.is_empty() {
            // if self.queue.len() > 10_000_000 {
            //     panic!("too many states in queue");
            // }
            if Instant::now() > next_print {
                println!(
                    "in queue: {} seen: {} mem usage current: {} max: {}",
                    self.queue.len(),
                    self.seen.len(),
                    ALLOCATOR.get_current(),
                    ALLOCATOR.get_max()
                );
                next_print = Instant::now() + Duration::from_secs(2);
            }

            if let Some(winning_state) = self.process_one_state() {
                return Some(winning_state);
            }
        }
        None
    }
}

/// Loads a level from given path, tries to solve it, and if successful, returns the winning moves.
pub fn solve(file: &Path) -> Option<String> {
    println!("size of tile: {}", std::mem::size_of::<Tile>());
    println!("size of state: {}", std::mem::size_of::<State>());
    println!("initial memory usage: {}", ALLOCATOR.get_current());
    let mut s = SearchState::load_level(file);
    println!(
        "memory usage after initial state load: {}",
        ALLOCATOR.get_current()
    );
    println!("initial state:\n{}", &s.start.format_grid());

    let result = s.run();

    println!("total states: {}", s.seen.len());
    println!(
        "ending memory usage: {} max: {}",
        ALLOCATOR.get_current(),
        ALLOCATOR.get_max()
    );

    if let Some(winning_state) = result {
        Some(winning_state.moves.format())
    } else {
        None
    }
}

/// Loads a level from given path, then applies given moves, returning a vector of formatted
/// moves and states.
pub fn apply_moves(file: &Path, moves: &str) -> Vec<(String, String)> {
    let state = State::from_file(file);
    let moves = Moves::parse(moves);
    let mut grid = state.grid.clone();
    let mut out = Vec::new();

    for m in moves {
        if let Some(new_grid) = grid.do_move(m) {
            out.push((format!("{:?}", m), new_grid.format_grid()));
            grid = new_grid;
        } else {
            panic!("failed at move {:?}", m);
        }
    }
    out
}
