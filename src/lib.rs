#![no_std]

extern crate alloc;

use alloc::sync::Arc;
use dynamic_cast::{SupportsInterfaces, impl_supports_interfaces};
use macro_magic::export_tokens_no_emit;

#[doc(hidden)]
pub use macro_magic;

pub use basic_oop_macro::class_unsafe;

#[doc(hidden)]
pub use alloc::sync::Arc as alloc_sync_Arc;
#[doc(hidden)]
pub use core::mem::transmute as core_mem_transmute;
#[doc(hidden)]
pub use dynamic_cast::SupportsInterfaces as dynamic_cast_SupportsInterfaces;
#[doc(hidden)]
pub use dynamic_cast::impl_supports_interfaces as dynamic_cast_impl_supports_interfaces;
#[doc(hidden)]
pub use dynamic_cast::dyn_cast_arc as dynamic_cast_dyn_cast_arc;

#[export_tokens_no_emit]
struct inherited_from_Obj { __class__: ::basic_oop::Obj }

pub type Vtable = *const *const ();

pub struct Obj {
    vtable: Vtable,
}

impl Obj {
    pub fn new() -> Arc<dyn TObj> {
        Arc::new(unsafe { Self::new_raw(OBJ_VTABLE.as_ptr()) })
    }

    pub unsafe fn new_raw(vtable: Vtable) -> Self {
        Obj { vtable }
    }

    pub fn vtable(&self) -> Vtable { self.vtable }
}

pub trait TObj: SupportsInterfaces {
    fn obj(&self) -> &Obj;
}

impl_supports_interfaces!(Obj: TObj);

impl TObj for Obj {
    fn obj(&self) -> &Obj { self }
}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
#[repr(usize)]
pub enum ObjMethods {
    MethodsCount = 0usize,
}

pub struct ObjVtable(pub [*const (); ObjMethods::MethodsCount as usize]);

impl ObjVtable {
    pub const fn new() -> Self {
        ObjVtable([])
    }
}

const OBJ_VTABLE: [*const (); ObjMethods::MethodsCount as usize] = ObjVtable::new().0;

#[repr(C)]
pub struct VtableJoin<const A: usize, const B: usize> {
    pub a: [*const (); A],
    pub b: [*const (); B],
}
