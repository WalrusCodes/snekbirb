use std::alloc::{GlobalAlloc, Layout, System};

use std::sync::atomic::{
    AtomicUsize,
    Ordering::{Relaxed, SeqCst},
};

pub struct CountingAlloc {
    bytes: AtomicUsize,
    max: AtomicUsize,
}

impl CountingAlloc {
    pub const fn new() -> CountingAlloc {
        CountingAlloc {
            bytes: AtomicUsize::new(0),
            max: AtomicUsize::new(0),
        }
    }

    pub fn get_current(&self) -> usize {
        self.bytes.load(Relaxed)
    }

    pub fn get_max(&self) -> usize {
        self.max.load(Relaxed)
    }
}

unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ret = System.alloc(layout);
        if !ret.is_null() {
            let previous = self.bytes.fetch_add(layout.size(), SeqCst);
            // Technically we are saving the max of previous value, but this saves us from an extra
            // load and the difference generally would be negligible.
            self.max.fetch_max(previous, Relaxed);
        }
        return ret;
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
        self.bytes.fetch_sub(layout.size(), SeqCst);
    }
}

// #[global_allocator]
// static A: Counter = Counter;

// fn main() {
//     println!("allocated bytes before main: {}", ALLOCATED.load(SeqCst));
// }
