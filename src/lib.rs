#![no_std]

extern crate alloc;

use alloc::rc::Rc;
use alloc::sync::Arc;
use dynamic_cast::{SupportsInterfaces, impl_supports_interfaces};
use macro_magic::export_tokens_no_emit;

#[doc(hidden)]
pub use macro_magic;

pub use basic_oop_macro::class_unsafe;

pub use basic_oop_macro::class_sync_unsafe;

#[doc(hidden)]
pub use alloc::rc::Rc as alloc_rc_Rc;
#[doc(hidden)]
pub use alloc::sync::Arc as alloc_sync_Arc;
#[doc(hidden)]
pub use core::mem::transmute as core_mem_transmute;
#[doc(hidden)]
pub use dynamic_cast::SupportsInterfaces as dynamic_cast_SupportsInterfaces;
#[doc(hidden)]
pub use dynamic_cast::impl_supports_interfaces as dynamic_cast_impl_supports_interfaces;
#[doc(hidden)]
pub use dynamic_cast::dyn_cast_rc as dynamic_cast_dyn_cast_rc;
#[doc(hidden)]
pub use dynamic_cast::dyn_cast_arc as dynamic_cast_dyn_cast_arc;

#[export_tokens_no_emit]
struct inherited_from_Obj { __class__: ::basic_oop::Obj }

pub type Vtable = *const *const ();

pub struct Obj {
    vtable: Vtable,
}

impl Obj {
    pub fn new() -> Rc<dyn TObj> {
        Rc::new(unsafe { Self::new_raw(OBJ_VTABLE.as_ptr()) })
    }

    pub fn new_sync() -> Arc<dyn TObj> {
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
pub enum ObjVirtMethods {
    VirtMethodsCount = 0usize,
}

pub struct ObjVtable(pub [*const (); ObjVirtMethods::VirtMethodsCount as usize]);

impl ObjVtable {
    pub const fn new() -> Self {
        ObjVtable([])
    }
}

const OBJ_VTABLE: [*const (); ObjVirtMethods::VirtMethodsCount as usize] = ObjVtable::new().0;

#[repr(C)]
pub struct VtableJoin<const A: usize, const B: usize> {
    pub a: [*const (); A],
    pub b: [*const (); B],
}
