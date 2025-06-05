![maintenance: experimental](https://img.shields.io/badge/maintenance-experimental-blue.svg)

# basic-oop

Simple OOP for Rust.

```rust
#![feature(macro_metavar_expr_concat)]

mod test_class {
    use basic_oop::{import, class_unsafe, Vtable};
    use std::cell::RefCell;

    import! { pub test_class:
        use [obj basic_oop::obj];
        use std::rc::Rc;
        #[allow(unused_imports)]
        use std::rc::Weak as rc_Weak;
    }

    #[class_unsafe(inherits_Obj)]
    pub struct TestClass {
        field: RefCell<Rc<String>>,
        #[non_virt]
        get_field: fn() -> Rc<String>,
        #[virt]
        set_field: fn(value: Rc<String>),
    }

    impl TestClass {
        pub fn new(field: Rc<String>) -> Rc<dyn IsTestClass> {
            Rc::new(unsafe { Self::new_raw(field, TEST_CLASS_VTABLE.as_ptr()) })
        }

        pub unsafe fn new_raw(field: Rc<String>, vtable: Vtable) -> Self {
            TestClass {
                obj: unsafe { Obj::new_raw(vtable) },
                field: RefCell::new(field),
            }
        }

        pub fn get_field_impl(this: &Rc<dyn IsTestClass>) -> Rc<String> {
            this.test_class().field.borrow().clone()
        }

        pub fn set_field_impl(this: &Rc<dyn IsTestClass>, value: Rc<String>) {
            *this.test_class().field.borrow_mut() = value;
        }
    }
}

mod derived_class {
    use basic_oop::{import, class_unsafe, Vtable};
    use dynamic_cast::dyn_cast_rc;

    import! { pub derived_class:
        use [test_class crate::test_class];
    }

    #[class_unsafe(inherits_TestClass)]
    pub struct DerivedClass {
        #[virt]
        coerce_field: fn(value: Rc<String>) -> Rc<String>,
        #[over]
        set_field: (),
    }

    impl DerivedClass {
        pub fn new(field: Rc<String>) -> Rc<dyn IsDerivedClass> {
            Rc::new(unsafe { Self::new_raw(field, DERIVED_CLASS_VTABLE.as_ptr()) })
        }

        pub unsafe fn new_raw(field: Rc<String>, vtable: Vtable) -> Self {
            DerivedClass {
                test_class: unsafe { TestClass::new_raw(field, vtable) },
            }
        }

        pub fn coerce_field_impl(_this: &Rc<dyn IsDerivedClass>, value: Rc<String>) -> Rc<String> {
            Rc::new(value.as_ref().clone() + " coerced")
        }

        pub fn set_field_impl(this: &Rc<dyn IsTestClass>, value: Rc<String>) {
            let value = {
                let this: Rc<dyn IsDerivedClass> = dyn_cast_rc(this.clone()).unwrap();
                this.coerce_field(value)
            };
            TestClass::set_field_impl(this, value);
        }
    }
}

use derived_class::DerivedClass;
use dynamic_cast::dyn_cast_rc;
use std::rc::Rc;
use test_class::{IsTestClass, TestClassExt};

fn main() {
    let class = DerivedClass::new(Rc::new("initial".to_string()));
    let base_class: Rc<dyn IsTestClass> = dyn_cast_rc(class).unwrap();
    assert_eq!(base_class.get_field().as_ref(), "initial");
    base_class.set_field(Rc::new("changed".to_string()));
    assert_eq!(base_class.get_field().as_ref(), "changed coerced");
}
```
