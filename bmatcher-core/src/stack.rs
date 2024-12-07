use alloc::vec::Vec;

pub trait Stack<T> {
    fn new() -> Self;

    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn reserve(&mut self, _size: usize) {}
    fn truncate(&mut self, size: usize);

    fn push_value(&mut self, value: T) -> bool;
    fn pop_value(&mut self) -> Option<T>;

    fn stack_mut(&mut self) -> &mut [T];
}

pub struct StaticStack<const N: usize, T> {
    stack: [T; N],
    length: usize,
}

impl<const N: usize, T: Default + Copy> Stack<T> for StaticStack<N, T> {
    fn new() -> Self {
        Self {
            stack: [Default::default(); N],
            length: 0,
        }
    }

    fn len(&self) -> usize {
        self.length
    }

    fn stack_mut(&mut self) -> &mut [T] {
        &mut self.stack[0..self.length]
    }

    fn truncate(&mut self, size: usize) {
        self.length = size;
        assert!(size <= self.stack.len());
    }

    fn push_value(&mut self, value: T) -> bool {
        if self.length == self.stack.len() {
            return false;
        }

        self.stack[self.length] = value;
        self.length += 1;
        true
    }

    fn pop_value(&mut self) -> Option<T> {
        if self.length > 0 {
            self.length -= 1;
            let value = self.stack[self.length];
            Some(value)
        } else {
            None
        }
    }
}

pub struct HeapStack<T> {
    stack: Vec<T>,
}

impl<T> Stack<T> for HeapStack<T> {
    fn new() -> Self {
        Self { stack: Vec::new() }
    }

    fn len(&self) -> usize {
        self.stack.len()
    }

    fn stack_mut(&mut self) -> &mut [T] {
        &mut self.stack
    }

    fn reserve(&mut self, size: usize) {
        self.stack.reserve(size);
    }

    fn truncate(&mut self, size: usize) {
        self.stack.truncate(size);
    }

    fn push_value(&mut self, value: T) -> bool {
        self.stack.push(value);
        true
    }

    fn pop_value(&mut self) -> Option<T> {
        self.stack.pop()
    }
}
