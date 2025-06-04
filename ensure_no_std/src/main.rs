#![feature(macro_metavar_expr_concat)]

#![deny(warnings)]

#![no_std]
#![no_main]

extern crate alloc;

#[cfg(windows)]
#[link(name="msvcrt")]
extern { }

mod no_std {
    use composable_allocators::{AsGlobal, System};
    use core::panic::PanicInfo;
    use exit_no_std::exit;

    #[global_allocator]
    static ALLOCATOR: AsGlobal<System> = AsGlobal(System);

    #[panic_handler]
    fn panic(_info: &PanicInfo) -> ! {
        exit(99)
    }

    #[no_mangle]
    extern "C" fn rust_eh_personality() { }
}

use alloc::rc::Rc;
use basic_oop::{class_unsafe, import, Vtable};
use core::cell::Cell;
use core::ffi::{c_char, c_int};

import! { test_class:
    use [obj basic_oop::obj];
}

#[class_unsafe(inherits_Obj)]
struct TestClass {
    field: Cell<u8>,
    #[virt]
    inc_field: fn(),
}

impl TestClass {
    fn new(field: u8) -> Rc<dyn TTestClass> {
        Rc::new(unsafe { Self::new_raw(field, TEST_CLASS_VTABLE.as_ptr()) })
    }

    unsafe fn new_raw(field: u8, vtable: Vtable) -> Self {
        TestClass {
            obj: unsafe { Obj::new_raw(vtable) },
            field: Cell::new(field)
        }
    }

    fn inc_field_impl(this: &Rc<dyn TTestClass>) {
        this.test_class().field.update(|x| x + 1);
    }
}

#[no_mangle]
extern "C" fn main(_argc: c_int, _argv: *const *const c_char) -> c_int {
    let class = TestClass::new(5);
    class.inc_field();
    assert_eq!(class.test_class().field.get(), 6);
    0
}
