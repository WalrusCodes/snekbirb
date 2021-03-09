use std::{
    collections::{HashSet, VecDeque},
    path::Path,
    time::{Duration, Instant},
};

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
    /// States that we still need to examine.
    queue: VecDeque<State>,
    /// States that we have seen.
    seen: HashSet<Grid>,
}

impl SearchState {
    /// Parses the level file, creates initial queue of the first state.
    fn load_level(file: &Path) -> SearchState {
        let state = State::from_file(file);
        let mut queue = VecDeque::new();
        queue.push_back(state.clone());
        let mut seen = HashSet::new();
        seen.insert(state.grid);
        SearchState { queue, seen }
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
        let snek_idx_iter = 0..current.grid.snek_count();
        let sneks_to_try: Vec<u8> = if let Some(Move(last_idx, _)) = current.moves.last() {
            std::iter::once(last_idx)
                .chain(snek_idx_iter.filter(|x| *x != last_idx))
                .collect()
        } else {
            snek_idx_iter.collect()
        };

        // for each snek, try all possible directions:
        for snek_idx in sneks_to_try.into_iter() {
            if current.grid.objects[snek_idx as usize].is_empty() {
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

                if let Some(grid) = current.grid.do_move(new_move) {
                    // Check if we have seen the new state before, if so, discard it.
                    if self.seen.contains(&grid) {
                        continue;
                    }
                    self.seen.insert(grid.clone());

                    let is_victory = grid.snek_count == 0;
                    let new_state = State {
                        grid,
                        moves: current.moves.clone_and_add(new_move),
                    };
                    // If the new state results in victory, return it immediately.
                    if is_victory {
                        return Some(new_state);
                    }
                    // println!(
                    //     "  {} added state {} {:?}",
                    //     current.format_moves(),
                    //     snek_idx,
                    //     dir
                    // );
                    self.queue.push_back(new_state);
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
            if self.queue.len() > 10_000_000 {
                panic!("too many states in queue");
            }
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
    println!(
        "initial state:\n{}",
        &s.queue.front().unwrap().grid.format_grid()
    );

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
            // println!("{:?}:", m);
            // println!("{}", new_grid.format_grid());
            out.push((format!("{:?}", m), new_grid.format_grid()));
            grid = new_grid;
        } else {
            panic!("failed at move {:?}", m);
        }
    }
    out
}
