#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

/// Position in the grid (row, col).
#[derive(Debug, Clone, PartialEq, Copy, Hash)]
pub struct Pos(pub u8, pub u8);

impl Pos {
    pub fn apply(&self, dir: Direction) -> Pos {
        let x = match dir {
            Direction::Left => (self.0, self.1 - 1),
            Direction::Right => (self.0, self.1 + 1),
            Direction::Up => (self.0 - 1, self.1),
            Direction::Down => (self.0 + 1, self.1),
        };
        Pos(x.0, x.1)
    }

    /// Checks if given direction can be applied without going out of bounds.
    fn can_apply(&self, dir: Direction, rows: u8, cols: u8) -> bool {
        match dir {
            Direction::Left => self.1 != 0,
            Direction::Right => self.1 < (cols - 1),
            Direction::Up => self.0 != 0,
            Direction::Down => self.0 < (rows - 1),
        }
    }

    pub fn maybe_apply(&self, dir: Direction, rows: u8, cols: u8) -> Option<Pos> {
        if self.can_apply(dir, rows, cols) {
            Some(self.apply(dir))
        } else {
            None
        }
    }
}
