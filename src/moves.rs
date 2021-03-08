use super::Direction;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Move(pub u8, pub Direction);
#[derive(Debug, Clone)]
pub struct Moves(Vec<Move>);

impl Move {
    pub fn new(snek_idx: u8, dir: Direction) -> Move {
        Move(snek_idx, dir)
    }
}

impl Moves {
    pub fn new() -> Moves {
        Moves(Vec::new())
    }

    pub fn clone_and_add(&self, new_move: Move) -> Moves {
        let mut out = self.0.clone();
        out.push(new_move);
        Moves(out)
    }

    /// Formats the moves that we took as a string.
    pub fn format(&self) -> String {
        let mut out: Vec<char> = Vec::new();
        let mut prev_snek_idx = 255;
        for Move(snek_idx, dir) in self.0.iter() {
            if *snek_idx != prev_snek_idx {
                if prev_snek_idx != 255 {
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

    /// Parses a string into a vector of moves.
    ///
    /// Input format matches what we output for solutions, i.e.,
    /// "<snake num>:<one or more directions> [...]".
    pub fn parse(s: &str) -> Moves {
        let mut snek_idx = 255u8;
        let mut out = Vec::new();
        for ch in s.chars() {
            match ch {
                '0'..='9' => {
                    snek_idx = ch.to_digit(10).unwrap() as u8;
                }
                'U' => out.push(Move(snek_idx, Direction::Up)),
                'D' => out.push(Move(snek_idx, Direction::Down)),
                'L' => out.push(Move(snek_idx, Direction::Left)),
                'R' => out.push(Move(snek_idx, Direction::Right)),
                ' ' | ':' => {}
                _ => {
                    panic!("invalid char: {}", ch);
                }
            }
        }
        Moves(out)
    }

    pub fn last(&self) -> Option<Move> {
        self.0.last().cloned()
    }
}

impl IntoIterator for Moves {
    type Item = Move;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
