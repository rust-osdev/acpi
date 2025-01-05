use crate::{AcpiError, AcpiResult};
use core::{
    alloc::{Allocator, Layout},
    mem,
    ptr::NonNull,
};

/// Thin wrapper around a regular slice, taking a reference to an allocator for automatic
/// deallocation when the slice is dropped out of scope.
#[derive(Debug)]
pub struct ManagedSlice<'a, T, A>
where
    A: Allocator,
{
    slice: &'a mut [T],
    allocator: A,
}

impl<T, A> ManagedSlice<'_, T, A>
where
    A: Allocator,
{
    /// Attempt to allocate a new `ManagedSlice` that holds `len` `T`s.
    pub fn new_in(len: usize, allocator: A) -> AcpiResult<Self> {
        let layout = Layout::array::<T>(len).map_err(|_| AcpiError::AllocError)?;
        match allocator.allocate(layout) {
            Ok(mut ptr) => {
                let slice = unsafe { core::slice::from_raw_parts_mut(ptr.as_mut().as_mut_ptr().cast(), len) };
                Ok(ManagedSlice { slice, allocator })
            }
            Err(_) => Err(AcpiError::AllocError),
        }
    }
}

#[cfg(feature = "alloc")]
impl<T> ManagedSlice<'_, T, alloc::alloc::Global> {
    pub fn new(len: usize) -> AcpiResult<Self> {
        Self::new_in(len, alloc::alloc::Global)
    }
}

impl<T, A> Drop for ManagedSlice<'_, T, A>
where
    A: Allocator,
{
    fn drop(&mut self) {
        unsafe {
            let slice_ptr = NonNull::new_unchecked(self.slice.as_ptr().cast_mut().cast::<u8>());
            let slice_layout =
                Layout::from_size_align_unchecked(mem::size_of_val(self.slice), mem::align_of_val(self.slice));
            self.allocator.deallocate(slice_ptr, slice_layout);
        }
    }
}

impl<T, A> core::ops::Deref for ManagedSlice<'_, T, A>
where
    A: Allocator,
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.slice
    }
}

impl<T, A> core::ops::DerefMut for ManagedSlice<'_, T, A>
where
    A: Allocator,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.slice
    }
}

impl<T: Clone, A: Allocator + Clone> Clone for ManagedSlice<'_, T, A> {
    fn clone(&self) -> Self {
        let mut new_managed_slice = ManagedSlice::new_in(self.len(), self.allocator.clone()).unwrap();
        new_managed_slice.clone_from_slice(self);
        new_managed_slice
    }
}
