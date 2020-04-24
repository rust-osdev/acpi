use core::{ops::Deref, ptr::NonNull};

/// Describes a physical mapping created by `AcpiHandler::map_physical_region` and unmapped by
/// `AcpiHandler::unmap_physical_region`. The region mapped must be at least `size_of::<T>()`
/// bytes, but may be bigger.
pub struct PhysicalMapping<T> {
    pub physical_start: usize,
    pub virtual_start: NonNull<T>,
    pub region_length: usize, // Can be equal or larger than size_of::<T>()
    pub mapped_length: usize, // Differs from `region_length` if padding is added for alignment
}

impl<T> Deref for PhysicalMapping<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.virtual_start.as_ref() }
    }
}

/// An implementation of this trait must be provided to allow `acpi` to access platform-specific
/// functionality, such as mapping regions of physical memory. You are free to implement these
/// however you please, as long as they conform to the documentation of each function.
pub trait AcpiHandler {
    /// Given a starting physical address and a size, map a region of physical memory that contains
    /// a `T` (but may be bigger than `size_of::<T>()`). The address doesn't have to be
    /// page-aligned, so the implementation may have to add padding to either end. The given
    /// size must be greater or equal to the size of a `T`. The virtual address the memory is
    /// mapped to does not matter, as long as it is accessible from `acpi`.
    unsafe fn map_physical_region<T>(&mut self, physical_address: usize, size: usize) -> PhysicalMapping<T>;

    /// Unmap the given physical mapping. Safe because we consume the mapping, and so it can't be
    /// used after being passed to this function.
    fn unmap_physical_region<T>(&mut self, region: PhysicalMapping<T>);
}
