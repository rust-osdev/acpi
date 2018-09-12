use core::marker::PhantomData;
use core::ops::{Index, IndexMut};
use memory::paging::entry::*;
use memory::paging::ENTRY_COUNT;
use memory::FrameAllocator;

pub const P4: *mut Table<Level4> = 0xffffffff_fffff000 as *mut _;

pub struct Table<L: TableLevel> {
    entries: [Entry; ENTRY_COUNT],
    level: PhantomData<L>,
}

impl<L> Table<L>
where
    L: TableLevel,
{
    pub fn zero(&mut self) {
        for entry in self.entries.iter_mut() {
            entry.set_unused();
        }
    }
}

/*
 * Addresses are expected to be canonical (bits 48-63 must be the same as bit 47), otherwise the
 * CPU will #GP when we ask it to translate it.
 */
fn make_address_canonical(address: usize) -> usize {
    let sign_extension = 0o177777_000_000_000_000_0000 * ((address >> 47) & 0b1);
    (address & ((1 << 48) - 1)) | sign_extension
}

impl<L> Table<L>
where
    L: HierarchicalLevel,
{
    fn next_table_address(&self, index: usize) -> Option<usize> {
        let entry_flags = self[index].flags();
        if entry_flags.contains(EntryFlags::PRESENT) && !entry_flags.contains(EntryFlags::HUGE_PAGE)
        {
            let table_address = self as *const _ as usize;
            Some(make_address_canonical((table_address << 9) | (index << 12)))
        } else {
            None
        }
    }

    pub fn next_table(&self, index: usize) -> Option<&Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &*(address as *const _) })
    }

    pub fn next_table_mut(&mut self, index: usize) -> Option<&mut Table<L::NextLevel>> {
        self.next_table_address(index)
            .map(|address| unsafe { &mut *(address as *mut _) })
    }

    pub fn next_table_create<A>(
        &mut self,
        index: usize,
        allocator: &mut A,
    ) -> &mut Table<L::NextLevel>
    where
        A: FrameAllocator,
    {
        if self.next_table(index).is_none() {
            assert!(
                !self.entries[index].flags().contains(EntryFlags::HUGE_PAGE),
                "mapping code does not support huge pages"
            );
            let frame = allocator.allocate_frame().expect("no frames available");
            self.entries[index].set(frame, EntryFlags::PRESENT | EntryFlags::WRITABLE);
            self.next_table_mut(index).unwrap().zero();
        }
        self.next_table_mut(index).unwrap()
    }
}

impl<L> Index<usize> for Table<L>
where
    L: TableLevel,
{
    type Output = Entry;

    fn index(&self, index: usize) -> &Entry {
        &self.entries[index]
    }
}

impl<L> IndexMut<usize> for Table<L>
where
    L: TableLevel,
{
    fn index_mut(&mut self, index: usize) -> &mut Entry {
        &mut self.entries[index]
    }
}

pub trait TableLevel {}

pub enum Level4 {}
#[allow(dead_code)]
pub enum Level3 {}
#[allow(dead_code)]
pub enum Level2 {}
pub enum Level1 {}

impl TableLevel for Level4 {}
impl TableLevel for Level3 {}
impl TableLevel for Level2 {}
impl TableLevel for Level1 {}

pub trait HierarchicalLevel: TableLevel {
    type NextLevel: TableLevel;
}

impl HierarchicalLevel for Level4 {
    type NextLevel = Level3;
}

impl HierarchicalLevel for Level3 {
    type NextLevel = Level2;
}

impl HierarchicalLevel for Level2 {
    type NextLevel = Level1;
}
