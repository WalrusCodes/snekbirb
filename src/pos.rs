#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

/// Position in the grid (row, col).
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
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

    pub fn maybe_add(&self, rows: u8, cols: u8, dy: i8, dx: i8) -> Option<Pos> {
        let new_y = self.0 as i8 + dy;
        let new_x = self.1 as i8 + dx;
        if new_y >= 0 && (new_y as u8) < rows && new_x >= 0 && (new_x as u8) < cols {
            Some(Pos(new_y as u8, new_x as u8))
        } else {
            None
        }
    }
}
