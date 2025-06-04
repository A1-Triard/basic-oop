#![feature(macro_metavar_expr_concat)]

#![deny(warnings)]
#![doc(test(attr(deny(warnings))))]
#![doc(test(attr(allow(dead_code))))]
#![doc(test(attr(allow(unused_variables))))]
#![allow(clippy::needless_doctest_main)]

//! The crate provides basic tools for writing object-oriented code in Rust.
//! Very basic: no multiply inheritance, no interfaces
//! (but this can be added in future in a limited form), no incapsulation.
//! All classes in hierarchy should have distinct names even if they are located in different modules.
//!
//! # How to define a class
//!
//! First you need to _import_ the parent class. All classes should be derived from some parent
//! class. The only class not having any parent is `Obj` defined in `basic_oop::obj` module. It contains
//! no fields or methods. Lets import it:
//!
//! ```ignore
//! import! { pub test_class:
//!     use [obj basic_oop::obj];
//! }
//! ```
//!
//! Here `test_class` correspond to our new class name.
//! (Although it is possible to use different names in the import section and in the class definition,
//! doing so is not recommended to avoid confusion among users of the class.)
//!
//! The pub keyword means that our class will be public. In case of a private class, it should be omitted.
//!
//! All types that we plan to use in the method signatures of our class
//! should also be imported as unique names. For example:
//!
//! ```ignore
//! import! { pub test_class:
//!     use [obj basic_oop::obj];
//!     use std::rc::Rc;
//!     use std::rc::Weak as rc_Weak;
//! }
//! ```
//! 
//! Now we can start to define our class using [`class_unsafe`] macro.
//!
//! Classes defined with the [`class_unsafe`] macro are intended for use either with the [`Rc`](alloc::rc::Rc)
//! smart pointer or with the [`Arc`](alloc::sync::Arc) smart pointer.
//! The [`Obj`](obj::Obj) class is the only class indifferent to the type of smart
//! pointer.
//!
//! Suppose we don't need reference counter atomicity. Then our class
//! definition will be the next:
//!
//! ```ignore
//! #[class_unsafe(inherits_Obj)]
//! pub struct TestClass { }
//! ```
//!
//! Each class should have two constructors: one for creating this particular class
//! and one for calling it from the constructor of the inheritor:
//!
//! ```ignore
//! impl TestClass {
//!     pub fn new() -> Rc<dyn TTestClass> {
//!         Rc::new(unsafe { Self::new_raw(TEST_CLASS_VTABLE.as_ptr()) })
//!     }
//!
//!     pub unsafe fn new_raw(vtable: Vtable) -> Self {
//!         TestClass { obj: unsafe { Obj::new_raw(vtable) } }
//!     }
//! }
//! ```
//!
//! # Fields
//!
//! To add a field to class, we just write it in ordinar way:
//!
//! ```ignore
//! #[class_unsafe(inherits_Obj)]
//! pub struct TestClass {
//!     field: RefCell<Rc<String>>,
//! }
//!
//! impl TestClass {
//!     pub fn new(field: Rc<String>) -> Rc<dyn TTestClass> {
//!         Rc::new(unsafe { Self::new_raw(field, TEST_CLASS_VTABLE.as_ptr()) })
//!     }
//!
//!     pub unsafe fn new_raw(field: Rc<String>, vtable: Vtable) -> Self {
//!         TestClass {
//!             obj: unsafe { Obj::new_raw(vtable) },
//!             field: RefCell::new(field),
//!         }
//!     }
//! }
//! ```
//!
//! # Non-virtual methods
//!
//! To add a method, it is needed to specify a fictive field with `#[non_virt]` attribute and
//! function type:
//!
//! ```ignore
//! #[class_unsafe(inherits_Obj)]
//! pub struct TestClass {
//!     ...
//!     #[non_virt]
//!     get_field: fn() -> Rc<String>,
//! }
//! ```
//!
//! Then `TestClassExt` extension trait will be generated contained appropriate function calling
//! `TestClass::get_field_impl`. We must provide this implementing function:
//!
//! ```ignore
//! impl TestClass {
//!     fn get_field_impl(this: &Rc<dyn TTestClass>) -> Rc<String> {
//!         this.test_class().field.borrow().clone()
//!     }
//! }
//! ```
//!
//! # Virtual methods
//!
//! Adding a virtual method is no different from adding a non-virtual method only this time
//! we use `virt`:
//!
//! ```ignore
//! #[class_unsafe(inherits_Obj)]
//! pub struct TestClass {
//!     ...
//!     #[virt]
//!     set_field: fn(value: Rc<String>),
//! }
//!
//! impl TestClass {
//!     fn set_field_impl(this: &Rc<dyn TTestClass>, value: Rc<String>) {
//!         *this.test_class().field.borrow_mut() = value;
//!     }
//! }
//! ```
//!
//! # Derived class. Method overriding.
//!
//! Lets import our class and derive another one from it:
//!
//! ```ignore
//! import! { pub derived_class:
//!     use [test_class crate::test_class];
//! }
//!
//! #[class_unsafe(inherits_TestClass)]
//! pub struct DerivedClass {
//! }
//! ```
//!
//! Now we wants to override `set_field`, how we do it? Simple:
//!
//! ```ignore
//! #[class_unsafe(inherits_TestClass)]
//! pub struct DerivedClass {
//!     #[over]
//!     set_field: (),
//! }
//!
//! impl DerivedClass {
//!     pub fn set_field_impl(this: &Rc<dyn TTestClass>, value: Rc<String>) {
//!         let value = /* coerce value */;
//!         TestClass::set_field_impl(this, value);
//!     }
//! }
//! ```
//!
//! The type of the overridden function is already known from the base class definition,
//! so there is no need to re-write it, which is why the type of the phony field is specified as `()`.
//!
//! # Using the class.

#![no_std]

extern crate alloc;

#[doc=include_str!("../README.md")]
type _DocTestReadme = ();

#[doc(hidden)]
pub use macro_magic;

/// Generates class and appropriate helper types and traits.
///
/// Usage:
///
/// ```ignore
/// #[class_unsafe(inherits_ParentClass)]
/// struct Class {
///     field: FieldType,
///     #[non_virt]
///     non_virtual_method: fn(args: ArgsType) -> ResultType,
///     #[virt]
///     virtual_method: fn(args: ArgsType) -> ResultType,
///     #[over]
///     parent_virtual_method: (),
/// }
///
/// impl Class {
///     fn non_virtual_method_impl(this: Rc<dyn TClass>, args: ArgsType) -> ResultType {
///         ...
///     }
///
///     fn virtual_method_impl(this: Rc<dyn TClass>, args: ArgsType) -> ResultType {
///         ...
///     }
///
///     fn parent_virtual_method_impl(this: Rc<dyn TParentClass>, args: ArgsType) -> ResultType {
///         let base_result = ParentClass::parent_virtual_method_impl(this, args);
///         ...
///     }
/// }
/// ```
///
/// For a more detailed overview, please refer to the crate documentation.
///
/// # Safety
///
/// This macro may produce unsound code if the argument to the macro differs
/// from `inherits_Obj`/`inherits_Obj_sync` imported from the [`obj`] module
/// and another `inherits_...` generated by this macro for a direct or indirect inheritor of `Obj`.
///
/// In other words, it is safe to use this macro as long as you
/// don't try to manually create something that this macro will accept as a valid argument.
pub use basic_oop_macro::class_unsafe;

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

/// The pointer to the table containing pointers to class virtual functions.
///
/// Use [`class_unsafe`] macro to generate `Vtable`.
pub type Vtable = *const *const ();

#[doc(hidden)]
#[repr(C)]
pub struct VtableJoin<const A: usize, const B: usize> {
    pub a: [*const (); A],
    pub b: [*const (); B],
}

/// Imports base class into the current scope so that it can be inherited from.
///
/// The macro accepts input in the following form:
///
/// ```ignore
/// $vis:vis $class:ident :
/// use $([$base:ident $path:path])+ ;
/// $( $(#[$attr:meta])* use $($custom_use:tt)+ ; )*
/// ```
///
/// See module documentation for explanation how to use it.
#[macro_export]
macro_rules! import {
    (
        $vis:vis $class:ident :
        use $([$base:ident $($path:tt)*])+ ;
        $($custom_use:tt)*
    ) => {
        $vis mod ${concat($class, _types)} {
            $(
                #[allow(unused_imports)]
                pub use $($path)*::*;
                #[allow(unused_imports)]
                pub use $($path)*:: ${concat($base, _types)} ::*;
            )+
            $crate::import_impl! { @split [] [$($custom_use)*] }
        }
        use ${concat($class, _types)} ::*;
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! import_impl {
    (
        @split [$($t:tt)*] [ ; $($custom_use:tt)*]
    ) => {
        $crate::import_impl! { @use [$($t)* ; ] }
        $crate::import_impl! { @split [] [$($custom_use)*] }
    };
    (
        @split [$($t:tt)*] [$x:tt $($custom_use:tt)*]
    ) => {
        $crate::import_impl! { @split [$($t)* $x] [$($custom_use)*] }
    };
    (
        @split [] []
    ) => {
    };
    (
        @split [$($t:tt)+] []
    ) => {
        $crate::import_impl! { @use [$($t)+] }
    };
    (
        @use [$(#[$attr:meta])* use $($list:tt)*]
    ) => {
        $(#[$attr])*
        pub use $($list)*
    };
}

pub mod obj {
    use alloc::rc::Rc;
    use alloc::sync::Arc;
    use dynamic_cast::{SupportsInterfaces, impl_supports_interfaces};
    use macro_magic::export_tokens_no_emit;
    use crate::Vtable;

    pub mod obj_types { }

    #[export_tokens_no_emit]
    #[non_sync]
    struct inherits_Obj { __class__: Obj }

    #[export_tokens_no_emit]
    #[sync]
    struct inherits_Obj_sync { __class__: Obj }

    /// Base class, contains no fields or methods.
    ///
    /// Use [`import`] and [`class_unsafe`](crate::class_unsafe) macros
    /// to define a class inherited from `Obj`:
    ///
    /// ```ignore
    /// #[class_unsafe(inherits_Obj)]
    /// struct Class { }
    /// ```
    ///
    /// for [`Rc`]-based class, or
    ///
    /// ```ignore
    /// #[class_unsafe(inherits_Obj_sync)]
    /// struct Class { }
    /// ```
    ///
    /// for [`Arc`]-based one.
    #[derive(Debug, Clone)]
    pub struct Obj {
        vtable: Vtable,
    }

    unsafe impl Send for Obj { }

    unsafe impl Sync for Obj { }

    impl Obj {
        /// Creates new `Obj` class instance, wrapped in [`Rc`] smart pointer.
        ///
        /// A rarely used function, since it creates `Obj` itself, not one of its inheritors.
        #[allow(clippy::new_ret_no_self)]
        pub fn new() -> Rc<dyn TObj> {
            Rc::new(unsafe { Self::new_raw(OBJ_VTABLE.as_ptr()) })
        }

        /// Creates new `Obj` class instance, wrapped in [`Arc`] smart pointer.
        ///
        /// A rarely used function, since it creates `Obj` itself, not one of its inheritors.
        pub fn new_sync() -> Arc<dyn TObj> {
            Arc::new(unsafe { Self::new_raw(OBJ_VTABLE.as_ptr()) })
        }

        /// Creates new `Obj`.
        ///
        /// Intended to be called from inheritors constructors to initialize a base type field.
        ///
        /// # Safety
        ///
        /// Calling this function is safe iff vtable is empty or
        /// generated using the [`class_unsafe`](crate::class_unsafe) macro on a
        /// direct or indirect `Obj` inheritor.
        pub unsafe fn new_raw(vtable: Vtable) -> Self {
            Obj { vtable }
        }

        /// Returns vtable, passed to the constructor.
        pub fn vtable(&self) -> Vtable { self.vtable }
    }

    /// Represents [`Obj`] or any of its inheritors.
    ///
    /// Usually obtained by using
    /// [`dyn_cast_rc`](dynamic_cast::dyn_cast_rc)/[`dyn_cast_arc`](dynamic_cast::dyn_cast_arc)
    /// on a derived trait.
    pub trait TObj: SupportsInterfaces {
        fn obj(&self) -> &Obj;
    }

    impl_supports_interfaces!(Obj: TObj);

    impl TObj for Obj {
        fn obj(&self) -> &Obj { self }
    }

    /// [`Obj`] virtual methods list.
    ///
    /// Used by the [`class_unsafe`](crate::class_unsafe) macro, not intended for direct use in code.
    #[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
    #[repr(usize)]
    pub enum ObjVirtMethods {
        VirtMethodsCount = 0usize,
    }

    /// [`Obj`] virtual methods table.
    ///
    /// Used by the [`class_unsafe`](crate::class_unsafe) macro, not intended for direct use in code.
    pub struct ObjVtable(pub [*const (); ObjVirtMethods::VirtMethodsCount as usize]);

    impl ObjVtable {
        /// Creates [`Obj`] virtual methods table.
        ///
        /// Used by the [`class_unsafe`](crate::class_unsafe) macro, not intended for direct use in code.
        #[allow(clippy::new_without_default)]
        pub const fn new() -> Self {
            ObjVtable([])
        }
    }

    const OBJ_VTABLE: [*const (); ObjVirtMethods::VirtMethodsCount as usize] = ObjVtable::new().0;
}
