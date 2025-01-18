use crate::{AcpiError, AcpiResult};
use core::{
    alloc::{Allocator, Layout},
    ptr::NonNull,
};

/// Thin wrapper around a regular slice, taking a reference to an allocator for automatic
/// deallocation when the slice is dropped out of scope.
#[derive(Debug)]
pub struct ManagedSlice<T, A>
where
    A: Allocator,
{
    inner: *mut T,
    len: usize,
    allocator: A,
}

unsafe impl<T: Send, A: Allocator> Send for ManagedSlice<T, A> {}
unsafe impl<T: Sync, A: Allocator> Sync for ManagedSlice<T, A> {}

impl<T, A> ManagedSlice<T, A>
where
    A: Allocator,
{
    /// Attempt to allocate a new `ManagedSlice` that holds `len` `T`s.
    pub fn new_in(len: usize, allocator: A) -> AcpiResult<Self> {
        let layout = Layout::array::<T>(len).map_err(|_| AcpiError::AllocError)?;
        match allocator.allocate(layout) {
            Ok(mut ptr) => Ok(ManagedSlice { inner: unsafe { ptr.as_mut().as_mut_ptr().cast() }, len, allocator }),
            Err(_) => Err(AcpiError::AllocError),
        }
    }
}

#[cfg(feature = "alloc")]
impl<T> ManagedSlice<T, alloc::alloc::Global> {
    pub fn new(len: usize) -> AcpiResult<Self> {
        Self::new_in(len, alloc::alloc::Global)
    }
}

impl<T, A> Drop for ManagedSlice<T, A>
where
    A: Allocator,
{
    fn drop(&mut self) {
        unsafe {
            let layout = Layout::array::<T>(self.len).unwrap();
            self.allocator.deallocate(NonNull::new_unchecked(self.inner as *mut u8), layout);
        }
    }
}

impl<T, A> core::ops::Deref for ManagedSlice<T, A>
where
    A: Allocator,
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::slice::from_raw_parts(self.inner, self.len) }
    }
}

impl<T, A> core::ops::DerefMut for ManagedSlice<T, A>
where
    A: Allocator,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { core::slice::from_raw_parts_mut(self.inner, self.len) }
    }
}

impl<T: Clone, A: Allocator + Clone> Clone for ManagedSlice<T, A> {
    fn clone(&self) -> Self {
        let mut new_managed_slice = ManagedSlice::new_in(self.len(), self.allocator.clone()).unwrap();
        new_managed_slice.clone_from_slice(self);
        new_managed_slice
    }
}
