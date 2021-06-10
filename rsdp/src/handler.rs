use core::{ops::Deref, ptr::NonNull};

/// Describes a physical mapping created by `AcpiHandler::map_physical_region` and unmapped by
/// `AcpiHandler::unmap_physical_region`. The region mapped must be at least `size_of::<T>()`
/// bytes, but may be bigger.
pub struct PhysicalMapping<H, T>
where
    H: AcpiHandler,
{
    pub physical_start: usize,
    pub virtual_start: NonNull<T>,
    pub region_length: usize, // Can be equal or larger than size_of::<T>()
    pub mapped_length: usize, // Differs from `region_length` if padding is added for alignment
    pub handler: H,
}

unsafe impl<H: AcpiHandler + Send, T: Send> Send for PhysicalMapping<H, T> {}
unsafe impl<H: AcpiHandler + Sync, T: Sync> Sync for PhysicalMapping<H, T> {}

impl<H, T> Deref for PhysicalMapping<H, T>
where
    H: AcpiHandler,
{
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { self.virtual_start.as_ref() }
    }
}

impl<H, T> Drop for PhysicalMapping<H, T>
where
    H: AcpiHandler,
{
    fn drop(&mut self) {
        self.handler.unmap_physical_region(self)
    }
}

/// An implementation of this trait must be provided to allow `acpi` to access platform-specific
/// functionality, such as mapping regions of physical memory. You are free to implement these
/// however you please, as long as they conform to the documentation of each function. The handler is stored in
/// every `PhysicalMapping` so it's able to unmap itself when dropped, so this type needs to be something you can
/// clone/move about freely (e.g. a reference, wrapper over `Rc`, marker struct, etc.).
pub trait AcpiHandler: Clone + Sized {
    /// Given a physical address and a size, map a region of physical memory that contains `T` (note: the passed
    /// size may be larger than `size_of::<T>()`). The address is not neccessarily page-aligned, so the
    /// implementation may need to map more than `size` bytes. The virtual address the region is mapped to does not
    /// matter, as long as it is accessible to `acpi`.
    unsafe fn map_physical_region<T>(&self, physical_address: usize, size: usize) -> PhysicalMapping<Self, T>;

    /// Unmap the given physical mapping. This is called when a `PhysicalMapping` is dropped.
    fn unmap_physical_region<T>(&self, region: &PhysicalMapping<Self, T>);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_send_sync() {
        // verify that PhysicalMapping implements Send and Sync
        fn test_send_sync<T: Send + Sync>() {}
        fn caller<H: AcpiHandler + Send + Sync, T: Send + Sync>() {
            test_send_sync::<PhysicalMapping<H, T>>();
        }
    }
}
