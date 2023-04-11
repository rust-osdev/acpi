use core::{alloc, mem};

/// Thin wrapper around a regular slice, taking a reference to an allocator for automatic
/// deallocation when the slice is dropped out of scope.
#[derive(Debug)]
pub struct ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    slice: &'a mut [T],
    allocator: A,
}

impl<'a, T, A> ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    /// Attempts to allocate a new `&mut [T]` in the given allocator.
    pub fn new_in(len: usize, allocator: A) -> crate::AcpiResult<Self> {
        // Safety: Type automatically deallocated memory on `Drop` and;
        //         Constructed slice is from valid, aligned, allocated memory.
        unsafe {
            allocator
                .allocate(alloc::Layout::array::<T>(len).map_err(|_| crate::AcpiError::AllocError)?)
                .map(|mut ptr| core::slice::from_raw_parts_mut(ptr.as_mut().as_mut_ptr().cast(), len))
                .map(|slice| Self { slice, allocator })
                .map_err(|_| crate::AcpiError::AllocError)
        }
    }
}

impl<'a, T, A> Drop for ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    fn drop(&mut self) {
        // Safety: Slice is required by function to point to non-null memory.
        let slice_ptr = unsafe { core::ptr::NonNull::new_unchecked(self.slice.as_ptr().cast_mut().cast::<u8>()) };
        // Safety: Slice is constructed from a valid layout.
        let slice_layout = unsafe {
            alloc::Layout::from_size_align_unchecked(mem::size_of_val(self.slice), mem::align_of_val(self.slice))
        };

        // Safety: Caller is required to provide a slice allocated with the provided allocator.
        unsafe { self.allocator.deallocate(slice_ptr, slice_layout) };
    }
}

impl<'a, T, A> core::ops::Deref for ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.slice
    }
}

impl<'a, T, A> core::ops::DerefMut for ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.slice
    }
}
