use core::{alloc, mem};

pub struct ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    slice: &'a mut [T],
    allocator: &'a A,
}

impl<'a, T, A> ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    pub fn new_in(len: usize, allocator: &'a A) -> crate::AcpiResult<Self> {
        // SAFETY: Struct layouts are required to be valid.
        let layout =
            unsafe { alloc::Layout::from_size_align_unchecked(mem::size_of::<T>() * len, mem::align_of::<T>()) };

        unsafe { allocator.allocate(layout).map(|ptr| ptr.as_uninit_slice_mut().align_to_mut::<T>().1) }
            .map(|slice| Self { slice, allocator })
            .map_err(|_| crate::AcpiError::AllocError)
    }
}

impl<'a, T, A> Drop for ManagedSlice<'a, T, A>
where
    A: alloc::Allocator,
{
    fn drop(&mut self) {
        // SAFETY: Slice is required by function to point to non-null memory.
        let slice_ptr = unsafe { core::ptr::NonNull::new_unchecked(self.slice.as_ptr().cast_mut().cast::<u8>()) };
        // SAFETY: Slice is constructed from a valid layout.
        let slice_layout = unsafe {
            alloc::Layout::from_size_align_unchecked(mem::size_of_val(self.slice), mem::align_of_val(self.slice))
        };

        // SAFETY: Caller is required to provide a slice allocated with the provided allocator.
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
