use std::{
    collections::{hash_map::DefaultHasher, HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
    rc::Rc,
};

use super::{Direction, Move, Pos};

#[derive(PartialEq)]
enum PushResult {
    Moved,
    DidNotMove,
    WouldDie,
}

// One thing on a grid tile.
#[derive(Debug, Clone, PartialEq)]
pub enum Tile {
    // Empty space, we can move through this.
    Empty,
    // Level exit - we win if snek head gets here. We use this enum for parsing, but then replace
    // it with Empty in the state so that a snek can occupy the exit space before exit is open.
    // Exit,
    // We can be supported by this.
    Ground,
    // Froot can be eaten to elongate snek.
    Fruit,
    // Spiky boi - stepping on it, you ded.
    Spike,
    // A segment of the snek. The number describes the object index. The positions of the snake
    // segments can be found in "objects" field in state with matching index.
    Snek(u8),
    // A block. The number describes the object index, in the same namespace as snek segments. The
    // positions for block segments can be found in the "objects" field.
    Block(u8),
    // TODO: add teleports.
}

impl Tile {
    fn get_object_id(&self) -> u8 {
        match self {
            Tile::Snek(idx) => *idx,
            Tile::Block(idx) => *idx,
            _ => panic!("not the right tile: {:?}", self),
        }
    }
}

impl Hash for Tile {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(match self {
            Tile::Empty => 0,
            Tile::Ground => 1,
            Tile::Fruit => 2,
            Tile::Spike => 0,
            Tile::Snek(x) => x.wrapping_add(3),
            Tile::Block(x) => x.wrapping_add(3 + 36),
        });
    }
}

fn from_base36(c: char) -> u8 {
    c.to_digit(36).unwrap() as u8
}

// Splits [0, 1, 2, 4, 5, 7] into [[0, 1, 2], [4, 5], [7]].
fn split_into_ranges<'a, I>(values: I) -> Vec<Vec<u8>>
where
    I: Iterator<Item = &'a u8>,
{
    let mut sorted_values: Vec<u8> = values.cloned().collect();
    sorted_values.sort();
    let mut out = Vec::new();
    let mut last_idx = 0;
    let mut first = true;
    for idx in sorted_values.iter() {
        if first || ((last_idx + 1) < *idx) {
            out.push(Vec::new());
        }
        out.last_mut().unwrap().push(*idx);
        first = false;
        last_idx = *idx;
    }
    out
}

/// Finds positions of all snek segments, denoted by 0..9a..z, with gaps in the ranges separating
/// the sneks.
///
/// Returned vector contains vectors, which contain positions of snek segments, with head being
/// in position 0.
fn find_snek_positions(chars: &[Vec<char>]) -> Vec<Vec<Pos>> {
    // 012   <-- snek #0
    // 456   <-- snek #1

    // Build a mapping of base 36 digits to positions.
    let mut snek_indices: HashMap<u8, Pos> = HashMap::new();
    for row_idx in 0..chars.len() {
        for col_idx in 0..chars[0].len() {
            let c = chars[row_idx][col_idx];
            let num = match c {
                '0'..='9' | 'a'..='z' => from_base36(c),
                _ => {
                    continue;
                }
            };
            snek_indices.insert(num, Pos(row_idx as u8, col_idx as u8));
        }
    }

    // Build a sorted list of separate ranges, e.g., [[0, 1], [3, 4, 5]].
    let indice_ranges = split_into_ranges(snek_indices.keys());

    // Build a list of positions for each snek.
    let mut out: Vec<Vec<Pos>> = Vec::new();
    for input_indices in indice_ranges {
        out.push(Vec::new());
        let out_snek = out.last_mut().unwrap();
        for input_idx in input_indices.iter() {
            out_snek.push(snek_indices[input_idx]);
        }
    }
    out
}

/// Data that doesn't change after the level is loaded. We share one instance of this between Grid
/// entries.
#[derive(Debug)]
struct StaticData {
    col_count: usize,
    row_count: usize,

    // Position of the exit tile.
    exit_pos: Pos,

    // Mapping from teleport position to the other side.
    teleports: HashMap<Pos, Pos>,

    // Position that the first block appears in "objects", same as starting "snek_count".
    first_block_idx: usize,
}

#[derive(Debug, Clone)]
pub struct Grid {
    tiles: Vec<Tile>, // len = col_count * row_count
    // Data that doesn't change as grid changes, so we just keep a pointer to it.
    s: Rc<StaticData>,
    // Positions of sneks (with indices 0..first_block_idx, and blocks from first_block_idx..).
    pub objects: Vec<Vec<Pos>>,
    fruit_count: usize,
    pub snek_count: usize,
}

impl Grid {
    pub fn parse(text: &str) -> Grid {
        // First, split the text up into a 2D array of characters.
        let chars: Vec<Vec<char>> = text
            .lines()
            .filter(|line| !(line.is_empty() || line.starts_with("//")))
            .map(|line| line.chars().collect::<Vec<char>>())
            .collect();
        let row_count = chars.len();
        let col_count = chars[0].len();
        let mut fruit_count = 0;
        let mut exit_pos: Option<Pos> = None;
        let mut teleports = HashMap::new();

        // Temporary structure for tracking teleport positions as we construct them.
        let mut teleport_char_to_pos: HashMap<char, Pos> = HashMap::new();

        // Then, build a list of snek positions. The index of each lists will be the object id. We will
        // add block objects to this list later.
        let mut objects = find_snek_positions(&chars);
        let snek_count = objects.len();
        let first_block_idx = snek_count;

        let mut block_indices: HashMap<char, usize> = HashMap::new();

        // Build the tiles grid.
        let mut tiles: Vec<Tile> = Vec::with_capacity(row_count * col_count);
        tiles.resize(row_count * col_count, Tile::Empty);

        for (snek_idx, snek_positions) in objects.iter().enumerate() {
            for pos in snek_positions {
                tiles[pos.0 as usize * col_count + pos.1 as usize] = Tile::Snek(snek_idx as u8);
            }
        }

        for (row_idx, row) in chars.iter().enumerate() {
            for (col_idx, c) in row.iter().enumerate() {
                let pos = Pos(row_idx as u8, col_idx as u8);
                let tile = match c {
                    '0'..='9' | 'a'..='z' => {
                        // Skip the sneks, we already assigned them separately.
                        continue;
                    }
                    'U'..='Z' => {
                        if !block_indices.contains_key(c) {
                            block_indices.insert(*c, objects.len());
                            objects.push(Vec::new());
                        }
                        let block_idx = *block_indices.get(c).unwrap();
                        objects[block_idx].push(pos);
                        Tile::Block(block_idx as u8)
                    }
                    '.' => Tile::Empty,
                    '#' => Tile::Ground,
                    '*' => Tile::Spike,
                    'F' => {
                        fruit_count += 1;
                        Tile::Fruit
                    }
                    'E' => {
                        assert!(exit_pos.is_none());
                        exit_pos = Some(Pos(row_idx as u8, col_idx as u8));
                        Tile::Empty
                    }
                    '@' => {
                        if let Some(pos2) = teleport_char_to_pos.remove(c) {
                            teleports.insert(pos, pos2);
                            teleports.insert(pos2, pos);
                        } else {
                            teleport_char_to_pos.insert(*c, pos);
                        }
                        Tile::Empty
                    }
                    _ => {
                        panic!("invalid input character: {}", c);
                    }
                };
                tiles[row_idx * col_count + col_idx] = tile;
            }
        }

        if !teleport_char_to_pos.is_empty() {
            panic!("only one half of teleport found");
        }

        Grid {
            tiles,
            s: Rc::new(StaticData {
                col_count,
                row_count,
                exit_pos: exit_pos.expect("no exit"),
                teleports,
                first_block_idx,
            }),
            objects,
            fruit_count,
            snek_count,
        }
    }

    fn grid_idx(&self, pos: Pos) -> usize {
        (pos.0 as usize) * self.s.col_count + (pos.1 as usize)
    }

    /// Returns the tile in given position.
    fn get(&self, pos: Pos) -> Tile {
        self.tiles[self.grid_idx(pos)].clone()
    }

    /// Sets the tile in given position.
    fn set(&mut self, pos: Pos, tile: Tile) {
        let idx = self.grid_idx(pos);
        self.tiles[idx] = tile;
    }

    /// Applies direction to given position if able, based on level boundaries.
    fn maybe_apply_pos(&self, pos: Pos, dir: Direction) -> Option<Pos> {
        pos.maybe_apply(dir, self.s.row_count as u8, self.s.col_count as u8)
    }

    /// Removes given snek from the state, decrements snek_count.
    fn remove_snek(&mut self, snek_idx: u8) {
        assert!(!self.objects[snek_idx as usize].is_empty());
        let snek_positions = self.objects[snek_idx as usize].clone();
        for pos in snek_positions {
            self.set(pos, Tile::Empty);
        }
        self.objects[snek_idx as usize].clear();
        self.snek_count -= 1;
    }

    /// Tries pushing given tile in given direction, transitively pushing everything that can move.
    /// Only sneks or blocks can be pushed.
    ///
    /// If maybe_snek_idx is passed in, that's the snek that's causing the pushing and cannot be
    /// moved itself.
    fn push_object(
        &mut self,
        maybe_snek_idx: Option<u8>,
        tile_pos: Pos,
        dir: Direction,
    ) -> PushResult {
        // Objects that are already moving.
        let mut moving: HashSet<u8> = HashSet::new();
        // Objects that we need to test.
        let mut queue: VecDeque<u8> = VecDeque::new();

        let obj_idx = self.get(tile_pos).get_object_id();
        queue.push_back(obj_idx);
        moving.insert(obj_idx);

        let mut has_spike_or_out_of_bounds = false;
        while !queue.is_empty() {
            let idx = queue.pop_front().unwrap();
            for pos in self.objects[idx as usize].iter() {
                if let Some(new_pos) = self.maybe_apply_pos(*pos, dir) {
                    match self.get(new_pos) {
                        Tile::Empty => (),
                        Tile::Spike => {
                            if idx < (self.s.first_block_idx as u8) {
                                // sneks can get poked
                                has_spike_or_out_of_bounds = true;
                            } else {
                                // blocks can rest of spikes
                                return PushResult::DidNotMove;
                            }
                        }
                        Tile::Fruit | Tile::Ground => {
                            return PushResult::DidNotMove;
                        }
                        Tile::Snek(other_idx) | Tile::Block(other_idx) => {
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
                    }
                } else {
                    has_spike_or_out_of_bounds = true;
                }
            }
        }
        if has_spike_or_out_of_bounds {
            return PushResult::WouldDie;
        }

        // If we are here, it means the original object that was being pushed can move, possibly
        // also moving other objects. We perform the move here, checking for exit as well.

        // dbg!("moving: ", &moving);
        // println!("{}", &self);

        let mut new_tiles = self.tiles.clone();
        let mut new_objects = self.objects.clone();
        // Clear out the objects being moved.
        for obj_idx in moving.iter() {
            for pos in self.objects[*obj_idx as usize].iter() {
                new_tiles[self.grid_idx(*pos)] = Tile::Empty;
            }
        }
        // Now reapply with the movement.
        for obj_idx in moving.iter() {
            new_objects[*obj_idx as usize].clear();
            for pos in self.objects[*obj_idx as usize].iter() {
                let new_pos = pos.apply(dir);
                new_tiles[self.grid_idx(new_pos)] = self.tiles[self.grid_idx(*pos)].clone();
                new_objects[*obj_idx as usize].push(new_pos);
            }
        }

        let mut maybe_teleport: Vec<Pos> = Vec::new();
        for (&tlp_pos, _) in self.s.teleports.iter() {
            let tile = &new_tiles[self.grid_idx(tlp_pos)];
            match tile {
                Tile::Snek(_) | Tile::Block(_) => {
                    if self.get(tlp_pos) != *tile {
                        maybe_teleport.push(tlp_pos);
                    }
                }
                _ => {}
            }
        }

        self.tiles = new_tiles;
        self.objects = new_objects;

        for pos in maybe_teleport {
            self.apply_teleport(pos);
        }

        // Check if any snek moved into exit.
        if self.fruit_count == 0 {
            for obj_idx in moving {
                if let Some(&obj_head) = self.objects[obj_idx as usize].first() {
                    if let Tile::Snek(_) = self.get(obj_head) {
                        if obj_head == self.s.exit_pos {
                            self.remove_snek(obj_idx);
                        }
                    }
                }
            }
        }

        // dbg!("after move");
        // println!("{}", &self);

        PushResult::Moved
    }

    /// Applies gravity - makes sneks fall down until they are supported at least on one segment.
    ///
    /// Returns true iff sneks are within bounds & didn't die.
    fn apply_gravity(&mut self) -> bool {
        // keep trying to push each object down until nothing moves.
        let mut again = true;
        while again {
            again = false;
            for idx in 0..self.objects.len() {
                if self.objects[idx].is_empty() {
                    continue;
                }
                let pos = *self.objects[idx].first().unwrap();
                match self.push_object(None, pos, Direction::Down) {
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
    }

    fn apply_teleport(&mut self, from: Pos) {
        let obj = self.get(from);
        let id = obj.get_object_id();
        let to = self.s.teleports.get(&from).unwrap();
        let mut new_positions: Vec<Pos> = Vec::new();
        for pos in &self.objects[id as usize] {
            let dy = (to.0 as i8) - (from.0 as i8);
            let dx = (to.1 as i8) - (from.1 as i8);
            if let Some(new_pos) =
                pos.maybe_add(self.s.row_count as u8, self.s.col_count as u8, dy, dx)
            {
                if self.get(new_pos) != Tile::Empty {
                    return;
                }

                new_positions.push(new_pos);
            } else {
                return;
            }
        }
        for pos in self.objects[id as usize].clone() {
            self.set(pos, Tile::Empty);
        }
        for pos in &new_positions {
            self.set(*pos, obj.clone());
        }
        self.objects[id as usize] = new_positions;
    }

    /// Tries applying movement for given snek. If it succeeds, returns the new Grid.
    pub fn do_move(&self, Move(snek_idx, dir): Move) -> Option<Grid> {
        let snek = &self.objects[snek_idx as usize];
        let head = snek[0].clone();

        // step 1: check if new tile is free or exit

        // check if direction is valid based on current position and level boundaries
        let new_tile_pos = match self.maybe_apply_pos(head, dir) {
            Some(pos) => pos,
            None => return None,
        };
        let new_tile = self.get(new_tile_pos);

        let mut grid;

        match new_tile {
            Tile::Ground => {
                return None;
            }
            Tile::Spike => {
                return None;
            }
            Tile::Fruit => {
                grid = self.clone();
                // fruit om nom nom nom
                grid.set(new_tile_pos, Tile::Snek(snek_idx));
                grid.objects[snek_idx as usize].insert(0, new_tile_pos);
                grid.fruit_count -= 1;
            }
            Tile::Empty | Tile::Snek(_) | Tile::Block(_) => {
                grid = self.clone();

                // If we stepping on snek or block, it means we are pushing. See if we can push.
                // This will also fail out if we try to push ourselves.
                if new_tile != Tile::Empty {
                    if grid.push_object(Some(snek_idx), new_tile_pos, dir) != PushResult::Moved {
                        return None;
                    }
                }

                // Check if we are about to step on the exit tile while it is open.
                if new_tile_pos == self.s.exit_pos && self.fruit_count == 0 {
                    grid.remove_snek(snek_idx);
                } else {
                    // Insert the new head, remove last segment.
                    let new_snek = &mut grid.objects[snek_idx as usize];
                    new_snek.insert(0, new_tile_pos); // note: inefficient!
                    new_snek.pop();
                    // Set new segment to snek, last segment to empty.
                    grid.set(new_tile_pos, Tile::Snek(snek_idx));
                    grid.set(*snek.last().unwrap(), Tile::Empty);
                }
            }
        };

        // If we moved to a teleport spot and we weren't on it yet, see if we can teleport.
        if self.s.teleports.contains_key(&new_tile_pos) {
            if new_tile != grid.get(new_tile_pos) {
                grid.apply_teleport(new_tile_pos);
            }
        }

        // step 3: apply gravity. if this returns false, snek was trying to fall off the level
        // (with at least one segment being outside the bounds), or fall on a spike.
        if !grid.apply_gravity() {
            None
        } else {
            Some(grid)
        }
    }

    pub fn snek_count(&self) -> u8 {
        self.snek_count as u8
    }

    fn format_tile(&self, pos: Pos) -> char {
        if pos == self.s.exit_pos {
            return 'E';
        }
        let tile = self.get(pos);
        if tile == Tile::Empty && self.s.teleports.contains_key(&pos) {
            return '@';
        }
        match tile {
            Tile::Empty => '.',
            Tile::Fruit => 'F',
            Tile::Ground => '#',
            Tile::Spike => '*',
            // 0-9a-z
            Tile::Snek(x) => std::char::from_digit(x as u32, 36).unwrap(),
            // UVWXYZ
            Tile::Block(x) => ('U' as u8 + x - self.s.first_block_idx as u8) as char,
        }
    }

    fn format_row(&self, row_idx: u8) -> String {
        (0..self.s.col_count)
            .map(|col_idx| self.format_tile(Pos(row_idx, col_idx as u8)))
            .chain(std::iter::once('\n'))
            .collect()
    }

    pub fn format_grid(&self) -> String {
        (0..self.s.row_count)
            .map(|row_idx| self.format_row(row_idx as u8))
            .collect()
    }

    pub fn hash_value(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl PartialEq for Grid {
    fn eq(&self, other: &Self) -> bool {
        return self.tiles == other.tiles && self.objects == other.objects;
    }
}

impl Eq for Grid {}

impl Hash for Grid {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        Hash::hash_slice(&self.tiles, hasher);
        for obj in self.objects.iter() {
            Hash::hash_slice(obj, hasher);
        }
    }
}
