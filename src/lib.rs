#![no_std]

extern crate alloc;

use alloc::sync::Arc;
use dynamic_cast::{SupportsInterfaces, impl_supports_interfaces};

pub use basic_oop_macro::class;

#[doc(hidden)]
pub use alloc::sync::Arc as std_sync_Arc;
#[doc(hidden)]
pub use core::mem::transmute as std_mem_transmute;

pub type Vtable = *const *const ();

pub struct Obj {
    vtable: Vtable,
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(usize)]
pub enum ObjMethods {
    Count = 0usize,
}

impl Obj {
    pub fn new() -> Arc<dyn TObj> {
        Arc::new(unsafe { Self::new_raw(OBJ_VTABLE.as_ptr()) })
    }

    pub unsafe fn new_raw(vtable: Vtable) -> Self {
        Obj { vtable }
    }
}

pub unsafe trait TObj: SupportsInterfaces {
    fn vtable(&self) -> Vtable;
}

impl_supports_interfaces!(Obj: TObj);

unsafe impl TObj for Obj {
    fn vtable(&self) -> Vtable { self.vtable }
}

#[repr(C)]
pub struct VtableJoin<const A: usize, const B: usize> {
    pub a: [*const (); A],
    pub b: [*const (); B],
}

#[macro_export]
macro_rules! obj_vtable {
    (
        $base_methods_count:ident,
        $self_methods_count:ident,
        $all_methods_count:expr,
        $vtable_name:ident
    ) => { };
}

pub struct ObjVtable(pub [*const (); ObjMethods::Count as usize]);

impl ObjVtable {
    pub const fn new() -> Self {
        ObjVtable([])
    }
}

const OBJ_VTABLE: [*const (); ObjMethods::Count as usize] = ObjVtable::new().0;
