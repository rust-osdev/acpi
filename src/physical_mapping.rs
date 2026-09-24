use crate::Handler;
use core::{
    fmt,
    ops::{Deref, DerefMut},
    pin::Pin,
    ptr::NonNull,
};

/// Describes a physical mapping.
///
/// The region mapped must be at least `size_of::<T>()` bytes, but may be bigger.
#[derive(Debug)]
pub struct RawPhysicalMapping<T: ?Sized> {
    /// The physical address of the mapped structure. The actual mapping may start at a lower address
    /// if the requested physical address is not well-aligned.
    physical_start: usize,
    /// The virtual address of the mapped structure. It must be a valid, non-null pointer to the
    /// start of the requested structure. The actual virtual mapping may start at a lower address
    /// if the requested address is not well-aligned.
    virtual_start: NonNull<T>,
    /// The size of the requested region, in bytes. Can be equal or larger to `size_of::<T>()`. If a
    /// larger region has been mapped, this should still be the requested size.
    region_length: usize,
    /// The total size of the produced mapping. This may be the same as `region_length`, or larger to
    /// meet requirements of the mapping implementation.
    mapped_length: usize,
}

impl<T: ?Sized> RawPhysicalMapping<T> {
    /// Construct a new RawPhysicalMapping.
    ///
    /// # Safety
    ///
    /// `virtual_start` must be unique amongst all `RawPhysicalMappings` that are created,
    /// and `virtual_start + sizeof<T>` must not alias other `RawPhysicalMappings`.
    ///
    /// If this `RawPhysicalMapping` will ever be unmapped, then all arguments provided must be
    /// consistent to allow that unmapping to take place.
    ///
    /// If this `RawPhysicalMapping` will be used as part of a `MappedGas`, then `virtual_start`
    /// must not point to memory within the Rust allocation system (as described in
    /// [`core::ptr::write_volatile`])
    pub unsafe fn new(
        physical_start: usize,
        virtual_start: NonNull<T>,
        region_length: usize,
        mapped_length: usize,
    ) -> Self {
        Self { physical_start, virtual_start, region_length, mapped_length }
    }

    pub fn get_physical_start(&self) -> usize {
        self.physical_start
    }

    /// Get the underlying virtual pointer for this mapping
    ///
    /// # Safety
    ///
    /// The returned pointer must not be used to create duplicate references to the same address,
    /// or any other action that would violate the Rust aliasing rules. Always try to access the
    /// stored data by dereferencing a PhysicalMapping.
    pub unsafe fn get_virtual_start(&self) -> NonNull<T> {
        self.virtual_start
    }

    pub fn get_region_length(&self) -> usize {
        self.region_length
    }

    pub fn get_mapped_length(&self) -> usize {
        self.mapped_length
    }
}

impl<T: ?Sized> Clone for RawPhysicalMapping<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for RawPhysicalMapping<T> {}

/// Describes a physical mapping created by [`Handler::map_physical_region`] and unmapped by
/// [`Handler::unmap_physical_region`]. The region mapped must be at least `size_of::<T>()`
/// bytes, but may be bigger.
pub struct PhysicalMapping<H, T>
where
    H: Handler,
{
    raw: RawPhysicalMapping<T>,

    // See the notes adjacent to the `unsafe impl Sync` line if you are planning to add new methods
    // that access this member.
    /// The [`Handler`] that was used to produce the mapping. When this mapping is dropped, this
    /// handler will be used to unmap the region.
    handler: H,
}

impl<H, T> PhysicalMapping<H, T>
where
    H: Handler,
{
    /// Creates a new physical mapping from the given handler.
    ///
    /// # Safety
    ///
    /// - `physical_address` must point to a valid `T` in physical memory.
    /// - `size` must be at least `size_of::<T>()`.
    pub unsafe fn new(physical_address: usize, size: usize, handler: H) -> PhysicalMapping<H, T> {
        let raw = unsafe { handler.map_physical_region(physical_address, size) };
        PhysicalMapping { raw, handler }
    }

    /// Creates a new physical mapping from a previously constructed [`RawPhysicalMapping<T>`].
    ///
    /// # Safety
    ///
    /// This must not be used to create duplicate physical mappings for the same
    /// [`RawPhysicalMapping`].
    ///
    /// It is unlikely that this function will be useful outside of tests.
    pub unsafe fn new_unchecked(raw: RawPhysicalMapping<T>, handler: H) -> PhysicalMapping<H, T> {
        PhysicalMapping { raw, handler }
    }

    /// Get a pinned reference to the inner `T`. This is generally only useful if `T` is `!Unpin`,
    /// otherwise the mapping can simply be dereferenced to access the inner type.
    pub fn get(&self) -> Pin<&T> {
        unsafe { Pin::new_unchecked(self.raw.virtual_start.as_ref()) }
    }

    pub fn get_raw(&self) -> RawPhysicalMapping<T> {
        self.raw
    }
}

impl<H, T> fmt::Debug for PhysicalMapping<H, T>
where
    H: Handler,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PhysicalMapping")
            .field("physical_start", &self.raw.physical_start)
            .field("virtual_start", &self.raw.virtual_start)
            .field("region_length", &self.raw.region_length)
            .field("mapped_length", &self.raw.mapped_length)
            .field("handler", &())
            .finish()
    }
}

unsafe impl<H: Handler + Send, T: Send> Send for PhysicalMapping<H, T> {}

// Safety: H need not be sync as PhysicalMapping owns a copy of Handler, and only ever uses it in
// the context of `&mut self` - therefore, usage of the Handler is implicitly synchronised.
//
// HOWEVER: If `handler` is ever called in a PhysicalMapping member that takes only `&self`, this
// will become incorrect, and `H: Handler` below should become `H: Handler + Sync`.
unsafe impl<H: Handler, T: Sync> Sync for PhysicalMapping<H, T> {}

impl<H, T> Deref for PhysicalMapping<H, T>
where
    T: Unpin,
    H: Handler,
{
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.raw.virtual_start.as_ref() }
    }
}

impl<H, T> DerefMut for PhysicalMapping<H, T>
where
    T: Unpin,
    H: Handler,
{
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.raw.virtual_start.as_mut() }
    }
}

impl<H, T> Drop for PhysicalMapping<H, T>
where
    H: Handler,
{
    fn drop(&mut self) {
        unsafe {
            self.handler.unmap_physical_region(self.raw);
        }
    }
}
