use std::sync::atomic::{AtomicUsize, Ordering};

// Repr C to move the 3 metadata words first as a optimization, the mutex can be pretty big
// since parking_lot is able to store T without boxing.
// Would be nice to have &'static str be only one word, but &&str would be worse to use
#[repr(C)]
pub struct Mutex<T> {
    locking_thread: AtomicUsize,
    inner: parking_lot::Mutex<T>,
}

pub struct MutexGuard<'a, T: 'a> {
    mutex: &'a Mutex<T>,
    guard: parking_lot::MutexGuard<'a, T>,
}

unsafe impl<T> Sync for Mutex<T> {}

impl<T> Mutex<T> {
    pub fn new(value: T) -> Mutex<T> {
        Mutex {
            inner: parking_lot::Mutex::new(value),
            locking_thread: AtomicUsize::new(0),
        }
    }

    pub fn try_lock<'s>(&'s self) -> Result<MutexGuard<'s, T>, ()> {
        let self_thread_id;
        self_thread_id = thread_id::get();
        if self.locking_thread.load(Ordering::Relaxed) == self_thread_id {
            return Err(());
        }
        let guard = self.inner.lock();
        self.locking_thread.store(self_thread_id, Ordering::Relaxed);
        Ok(MutexGuard {
            mutex: self,
            guard,
        })
    }

    pub fn lock<'s>(&'s self) -> MutexGuard<'s, T> {
        MutexGuard {
            mutex: self,
            guard: self.lock_inner(),
        }
    }

    fn lock_inner<'s>(&'s self) -> parking_lot::MutexGuard<'s, T> {
        let self_thread_id;
        self_thread_id = thread_id::get();
        if self.locking_thread.load(Ordering::Relaxed) == self_thread_id {
            panic!("Thread {} tried to lock a mutex recursively", self_thread_id);
        }
        let guard = self.inner.lock();
        self.locking_thread.store(self_thread_id, Ordering::Relaxed);
        guard
    }
}

impl<T: Default> Default for Mutex<T> {
    fn default() -> Self {
        Mutex {
            inner: Default::default(),
            locking_thread: AtomicUsize::new(0),
        }
    }
}

impl<'a, T: 'a> Drop for MutexGuard<'a, T> {
    fn drop(&mut self) {
        self.mutex.locking_thread.store(0, Ordering::Relaxed);
    }
}

impl<'a, T: 'a> std::ops::Deref for MutexGuard<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.guard
    }
}

impl<'a, T: 'a> std::ops::DerefMut for MutexGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.guard
    }
}
