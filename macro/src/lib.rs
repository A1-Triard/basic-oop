extern crate proc_macro;

use anycase::{to_pascal, to_snake, to_screaming_snake};
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use macro_magic::import_tokens_attr;
use proc_macro2::{TokenStream, Span};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use quote::{quote, TokenStreamExt, ToTokens};
use syn::{parse_macro_input, parse_quote, ItemStruct, Path, Type, Fields, Meta, Attribute, Visibility, Ident};
use syn::{Field, FieldMutability, TypePath, Token, PathSegment, PathArguments, LitInt, TypeBareFn, Expr};
use syn::{BareFnArg, Generics, Signature, Pat, PatType, PatIdent, FnArg, ImplItemFn, Stmt, ExprPath};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

struct Base {
    ty: Path,
    methods: Vec<(Ident, TypeBareFn)>,
}

fn parse_base_types(inherited_from: ItemStruct) -> Result<Vec<Base>, Diagnostic> {
    let Fields::Named(fields) = inherited_from.fields else {
        return Err(inherited_from.fields.span().error("invalid base class"));
    };
    let mut res = Vec::new();
    let mut base = None;
    for field in fields.named {
        if field.ident.as_ref().unwrap().to_string() == "__class__" {
            if let Some(base) = base.take() {
                res.push(base);
            }
            let Type::Path(type_path) = field.ty else {
                return Err(field.ty.span().error("invalid base class"));
            };
            if type_path.path.segments.len() < 2 {
                return Err(type_path.span().error("invalid base class"));
            }
            base = Some(Base {
                ty: type_path.path,
                methods: Vec::new()
            });
        } else {
            let name = field.ident.as_ref().unwrap().clone();
            let Type::BareFn(type_fn) = field.ty else {
                return Err(field.ty.span().error("invalid base class"));
            };
            let Some(base) = base.as_mut() else {
                return Err(type_fn.span().error("invalid base class"));
            };
            base.methods.push((name, type_fn));
        }
    }
    if let Some(base) = base.take() {
        res.push(base);
    }
    Ok(res)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum FieldKind {
    Method,
    Override,
    Data,
}

fn parse_field_meta(meta: &Meta) -> FieldKind {
    match meta {
        Meta::Path(path) if
               path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments[0].arguments.is_none()
        => match path.segments[0].ident.to_string().as_ref() {
            "virt" => FieldKind::Method,
            "over" => FieldKind::Override,
            _ => FieldKind::Data,
        },
        _ => FieldKind::Data,
    }
}

fn parse_field_attrs(attrs: &[Attribute]) -> Result<FieldKind, Diagnostic> {
    let mut kind = FieldKind::Data;
    for attr in attrs {
        match parse_field_meta(&attr.meta) {
            FieldKind::Data => { },
            FieldKind::Method => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'virt'/'over' attributes can be specified for a field")
                    );
                }
                kind = FieldKind::Method;
            },
            FieldKind::Override => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'virt'/'over' attributes can be specified for a field")
                    );
                }
                kind = FieldKind::Override;
            },
        }
    }
    Ok(kind)
}

struct Class {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    mod_: Path,
    fields: Vec<Field>,
    methods: Vec<(Ident, TypeBareFn)>,
    overrides: Vec<Ident>,
}

impl Class {
    fn parse(descr: ItemStruct) -> Result<Class, Diagnostic> {
        if descr.generics.lt_token.is_some() || descr.generics.where_clause.is_some() {
            return Err(descr.generics.span().error("basic-oop does not support generics"));
        }
        let mut mod_ = None;
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        let mut overrides = Vec::new();
        let Fields::Named(descr_fields) = descr.fields else {
            return Err(descr.fields.span().error("class should be described as struct with named fields"));
        };
        for (is_first, field) in descr_fields.named.iter().identify_first() {
            if is_first {
                if field.ident.as_ref().unwrap().to_string() != "__mod__" {
                    return Err(
                         field.ident.as_ref().unwrap().span()
                        .error("first field should be '__mod__' with full path to class")
                    );
                }
                let Type::Path(type_path) = &field.ty else {
                    return Err(
                         field.ty.span()
                        .error("first field should be '__mod__' with full path to class")
                    );
                };
                if type_path.qself.is_some() {
                    return Err(
                         type_path.span()
                        .error("first field should be '__mod__' with full path to class")
                    );
                }
                if type_path.path.leading_colon.is_none() || type_path.path.segments.is_empty() {
                    return Err(
                         type_path.span()
                        .error("first field should be '__mod__' with full path to class starting with leading colon")
                    );
                }
                mod_ = Some(type_path.path.clone());
            } else {
                match parse_field_attrs(&field.attrs)? {
                    FieldKind::Data => fields.push(field.clone()),
                    FieldKind::Method => {
                        let name = field.ident.clone().unwrap();
                        if name.to_string() == "__class__" {
                            return Err(name.span().error("this name is reserved"));
                        }
                        let Type::BareFn(type_fn) = &field.ty else {
                            return Err(field.ty.span().error("invalid virtual method type"));
                        };
                        if
                               type_fn.lifetimes.is_some()
                            || type_fn.unsafety.is_some()
                            || type_fn.abi.is_some()
                            || type_fn.variadic.is_some()
                            || type_fn.inputs.iter().any(|x| !x.attrs.is_empty())
                        {
                            return Err(field.ty.span().error("invalid virtual method type"));
                        }
                        if let Some(arg) = type_fn.inputs.iter().find(|x| x.name.is_none()) {
                            return Err(arg.span().error("argument name required"));
                        }
                        methods.push((name, type_fn.clone()));
                    },
                    FieldKind::Override => {
                        let name = field.ident.clone().unwrap();
                        let Type::Tuple(type_tuple) = &field.ty else {
                            return Err(field.ty.span().error("invalid override method type"));
                        };
                        if !type_tuple.elems.is_empty() {
                            return Err(field.ty.span().error("invalid override method type"));
                        }
                        overrides.push(name);
                    },
                }
            }
        }
        let Some(mod_) = mod_ else {
            return Err(descr_fields.span().error("at least '__mod__' field should be specified"));
        };
        if mod_.leading_colon.is_none() {
            return Err(
                 mod_.span()
                .error("first field should be '__mod__' with full path to class starting with leading colon")
            );
        }
        Ok(Class {
            attrs: descr.attrs,
            vis: descr.vis,
            name: descr.ident,
            mod_,
            fields,
            methods,
            overrides,
        })
    }
}

fn build_inherited_from(
    base_types: &[Base],
    class_name: &Ident,
    class_mod: &Path,
    methods: &[(Ident, TypeBareFn)]
) -> ItemStruct {
    let name = Ident::new(&("inherited_from_".to_string() + &class_name.to_string()), Span::call_site());
    let mut struct_: ItemStruct = parse_quote! {
        #[::basic_oop::macro_magic::export_tokens_no_emit]
        struct #name {
            __class__: #class_mod::#class_name
        }
    };
    let Fields::Named(fields) = &mut struct_.fields else { panic!() };
    for (method_name, method_ty) in methods {
        fields.named.push(Field {
            attrs: Vec::new(),
            vis: Visibility::Inherited,
            mutability: FieldMutability::None,
            ident: Some(method_name.clone()),
            colon_token: Some(<Token![:]>::default()),
            ty: Type::BareFn(method_ty.clone()),
        });
    }
    for base_type in base_types {
        fields.named.push(Field {
            attrs: Vec::new(),
            vis: Visibility::Inherited,
            mutability: FieldMutability::None,
            ident: Some(Ident::new("__class__", Span::call_site())),
            colon_token: Some(<Token![:]>::default()),
            ty: Type::Path(TypePath {
                qself: None,
                path: base_type.ty.clone()
            }),
        });
        for (method_name, method_ty) in &base_type.methods {
            fields.named.push(Field {
                attrs: Vec::new(),
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: Some(method_name.clone()),
                colon_token: Some(<Token![:]>::default()),
                ty: Type::BareFn(method_ty.clone()),
            });
        }
    }
    struct_
}

fn sanitize_base_type(mut base_type: Path, mod_: &Path) -> Path {
    if base_type.segments[0].ident.to_string() == mod_.segments[0].ident.to_string() {
        // base type in same crate
        base_type.leading_colon = None;
        base_type.segments[0].ident = Ident::new("crate", Span::call_site());
    }
    base_type
}

fn patch_path(path: &mut Path, f: impl FnOnce(String) -> String) {
    let ident = f(path.segments.last().unwrap().ident.to_string());
    path.segments.last_mut().unwrap().ident = Ident::new(&ident, Span::call_site());
}

fn build_attrs(attrs: &[Attribute]) -> TokenStream {
    let mut tokens = TokenStream::new();
    tokens.append_all(attrs);
    tokens
}

fn build_struct(
    base_types: &[Base],
    attrs: &[Attribute],
    vis: &Visibility,
    name: &Ident,
    mod_: &Path,
    fields: &[Field]
) -> ItemStruct {
    let base_type = sanitize_base_type(base_types[0].ty.clone(), mod_);
    let base_field = Ident::new(&to_snake(base_type.segments.last().unwrap().ident.to_string()), Span::call_site());
    let attrs = build_attrs(attrs);
    let mut struct_: ItemStruct = parse_quote! {
        #attrs
        #vis struct #name {
            #base_field: #base_type
        }
    };
    let Fields::Named(struct_fields) = &mut struct_.fields else { panic!() };
    for field in fields {
        struct_fields.named.push(field.clone());
    }
    struct_
}

fn build_trait(base_types: &[Base], vis: &Visibility, class_name: &Ident, mod_: &Path) -> TokenStream {
    let base_type = sanitize_base_type(base_types[0].ty.clone(), mod_);
    let base_field = Ident::new(
        &to_snake(base_type.segments.last().unwrap().ident.to_string()),
        Span::call_site()
    );
    let mut base_trait = sanitize_base_type(base_types[0].ty.clone(), mod_);
    patch_path(&mut base_trait, |x| "T".to_string() + &x);
    let trait_name = Ident::new(&("T".to_string() + &class_name.to_string()), Span::call_site());
    let method_name = Ident::new(&to_snake(class_name.to_string()), Span::call_site());
    let mut trait_ = quote! {
        #vis trait #trait_name: #base_trait {
            fn #method_name(&self) -> &#class_name;
        }

        impl #trait_name for #class_name {
            fn #method_name(&self) -> &#class_name { self }
        }

        impl #base_trait for #class_name {
            fn #base_field(&self) -> &#base_type { &self.#base_field }
        }
    };
    for base_base_type in base_types.iter().skip(1) {
        let method_name = Ident::new(
            &to_snake(base_base_type.ty.segments.last().unwrap().ident.to_string()),
            Span::call_site()
        );
        let mut base_base_trait = sanitize_base_type(base_base_type.ty.clone(), mod_);
        patch_path(&mut base_base_trait, |x| "T".to_string() + &x);
        let base_base_type_ty = &base_base_type.ty;
        trait_.extend(quote! {
            impl #base_base_trait for #class_name {
                fn #method_name(&self) -> &#base_base_type_ty {
                    #base_base_trait::#method_name(&self.#base_field)
                }
            }
        });
    }
    let mut traits_list: Punctuated<Path, Token![,]> = Punctuated::new();
    let mut trait_path = Path {
        leading_colon: None,
        segments: Punctuated::new()
    };
    trait_path.segments.push(PathSegment {
        ident: trait_name,
        arguments: PathArguments::None
    });
    traits_list.push(trait_path);
    for base_type in base_types {
        let mut base_trait = sanitize_base_type(base_type.ty.clone(), mod_);
        patch_path(&mut base_trait, |x| "T".to_string() + &x);
        traits_list.push(base_trait);
    }
    trait_.extend(quote! {
        ::basic_oop::dynamic_cast_impl_supports_interfaces!(#class_name: #traits_list);
    });
    trait_
}

fn build_methods_enum(
    base_type: &Path,
    vis: &Visibility,
    class_name: &Ident,
    mod_: &Path,
    methods: &[(Ident, TypeBareFn)]
) -> TokenStream {
    let mut base_methods_enum = sanitize_base_type(base_type.clone(), mod_);
    patch_path(&mut base_methods_enum, |x| x + "Methods");
    let methods_enum = Ident::new(&(class_name.to_string() + "Methods"), Span::call_site());
    let mut values = TokenStream::new();
    values.append_terminated(
        methods.iter().enumerate().map(|(i, (method_name, _method_ty))| {
            let name = Ident::new(&to_pascal(method_name.to_string()), Span::call_site());
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            quote! {
                #name = (#base_methods_enum::MethodsCount as usize) + #index
            }
        }),
        <Token![,]>::default()
    );
    let count = methods.len();
    quote! {
        #[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd, Hash)]
        #[repr(usize)]
        #vis enum #methods_enum {
            #values
            MethodsCount = (#base_methods_enum::MethodsCount as usize) + #count
        }
    }
}

fn build_consts_for_vtable(class_name: &Ident, class_mod: &Path, base_types: &[Base]) -> TokenStream {
    let enum_name = Ident::new(&(class_name.to_string() + "Methods"), Span::call_site());
    let mut tokens = TokenStream::new();
    for base_type in base_types {
        let mut base_methods_enum = sanitize_base_type(base_type.ty.clone(), class_mod);
        patch_path(&mut base_methods_enum, |x| x + "Methods");
        let base_const_name = Ident::new(
            &(
                to_screaming_snake(
                    class_name.to_string() + &base_type.ty.segments.last().unwrap().ident.to_string()
                ) + "_METHODS_COUNT"
            ),
            Span::call_site()
        );
        let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
        tokens.extend(quote! {
            const #base_const_name: usize = #base_methods_enum::MethodsCount as usize;
            const #complement_const_name: usize = (#enum_name::MethodsCount as usize) - #base_const_name;
        });
    }
    tokens
}

fn actual_base_method_ty(mut ty: TypeBareFn, base_type: &Path, class_mod: &Path) -> TypeBareFn {
    let mut base_trait = sanitize_base_type(base_type.clone(), class_mod);
    patch_path(&mut base_trait, |x| "T".to_string() + &x);
    let this_arg = BareFnArg {
        attrs: Vec::new(),
        name: Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &::basic_oop::alloc_sync_Arc<dyn #base_trait> },
    };
    ty.inputs.insert(0, this_arg);
    ty
}

fn actual_method_ty(mut ty: TypeBareFn, class_name: &Ident) -> TypeBareFn {
    let trait_name = Ident::new(&("T".to_string() + &class_name.to_string()), Span::call_site());
    let this_arg = BareFnArg {
        attrs: Vec::new(),
        name: Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &::basic_oop::alloc_sync_Arc<dyn #trait_name> },
    };
    ty.inputs.insert(0, this_arg);
    ty
}

fn fn_ty_without_idents(mut ty: TypeBareFn) -> TypeBareFn {
    for arg in &mut ty.inputs {
        arg.name = None;
    }
    ty
}

fn bare_fn_arg_to_fn_arg(a: &BareFnArg) -> FnArg {
    let Some((name, colon_token)) = &a.name else { panic!() }; 
    FnArg::Typed(PatType {
        attrs: Vec::new(),
        pat: Box::new(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: name.clone(),
            subpat: None
        })),
        colon_token: colon_token.clone(),
        ty: Box::new(a.ty.clone()),
    })
}

fn fn_signature(ty: &TypeBareFn, name: Ident) -> Signature {
    Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: ty.fn_token.clone(),
        ident: name,
        generics: Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        },
        paren_token: ty.paren_token.clone(),
        inputs: ty.inputs.iter().map(bare_fn_arg_to_fn_arg).collect(),
        variadic: None,
        output: ty.output.clone(),
    }
}

fn build_vtable(
    base_types: &[Base],
    class_name: &Ident,
    class_mod: &Path,
    vis: &Visibility,
    methods: &[(Ident, TypeBareFn)],
    overrides: &[Ident],
) -> TokenStream {
    let vtable_name = Ident::new(&(class_name.to_string() + "Vtable"), Span::call_site());
    let methods_enum_name = Ident::new(&(class_name.to_string() + "Methods"), Span::call_site());
    let struct_ = quote! {
        #vis struct #vtable_name(pub [*const (); #methods_enum_name::MethodsCount as usize]);
    };
    let mut methods_impl = TokenStream::new();
    methods_impl.append_separated(methods.iter().map(|(m, _)| {
        let impl_name = Ident::new(&(m.to_string() + "_impl"), Span::call_site());
        quote! { #class_name::#impl_name as *const () }
    }), <Token![,]>::default());
    let mut base_vtable = sanitize_base_type(base_types[0].ty.clone(), class_mod);
    patch_path(&mut base_vtable, |x| x + "Vtable");
    let base_vtable_new: Expr = parse_quote! { #base_vtable::new() };
    let mut base_vtable_with_overrides = base_vtable_new;
    for override_name in overrides {
        let impl_name = Ident::new(&(override_name.to_string() + "_impl"), Span::call_site());
        base_vtable_with_overrides = parse_quote! {
            #base_vtable_with_overrides.#override_name(#class_name::#impl_name)
        };
    }
    let base_const_name = Ident::new(
        &(
            to_screaming_snake(
                class_name.to_string() + &base_types[0].ty.segments.last().unwrap().ident.to_string()
            ) + "_METHODS_COUNT"
        ),
        Span::call_site()
    );
    let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
    let mut base_methods = TokenStream::new();
    for base_type in base_types {
        let mut base_vtable = sanitize_base_type(base_type.ty.clone(), class_mod);
        patch_path(&mut base_vtable, |x| x + "Vtable");
        let base_const_name = Ident::new(
            &(
                to_screaming_snake(
                    class_name.to_string() + &base_type.ty.segments.last().unwrap().ident.to_string()
                ) + "_METHODS_COUNT"
            ),
            Span::call_site()
        );
        let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
        for (base_method, base_method_ty) in &base_type.methods {
            let ty = actual_base_method_ty(base_method_ty.clone(), &base_type.ty, class_mod);
            let ty_without_idents = fn_ty_without_idents(ty);
            base_methods.extend(quote! {
                pub const fn #base_method(
                    self,
                    f: #ty_without_idents
                ) -> Self {
                    let vtable = unsafe { ::basic_oop::core_mem_transmute::<
                        [*const (); #methods_enum_name::MethodsCount as usize],
                        ::basic_oop::VtableJoin<#base_const_name, #complement_const_name>
                    >(self.0) };
                    let vtable: ::basic_oop::VtableJoin<
                        #base_const_name,
                        #complement_const_name
                    > = ::basic_oop::VtableJoin {
                        a: #base_vtable(vtable.a).#base_method(f).0,
                        b: vtable.b
                    };
                    #vtable_name(unsafe { ::basic_oop::core_mem_transmute::<
                        ::basic_oop::VtableJoin<#base_const_name, #complement_const_name>,
                        [*const (); #methods_enum_name::MethodsCount as usize]
                    >(vtable) })
                }
            });
        }
    }
    let mut methods_tokens = TokenStream::new();
    for (method_index, (method_name, method_ty)) in methods.iter().enumerate() {
        let ty = actual_method_ty(method_ty.clone(), class_name);
        let ty_without_idents = fn_ty_without_idents(ty);
        let mut list: Punctuated<Expr, Token![,]> = Punctuated::new();
        for i in 0 .. method_index {
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            list.push(parse_quote! { vtable.b[#index] });
        }
        list.push(parse_quote! { f as *const () });
        for i in method_index + 1 .. methods.len() {
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            list.push(parse_quote! { vtable.b[#index] });
        }
        methods_tokens.extend(quote! {
            pub const fn #method_name(
                self,
                f: #ty_without_idents
            ) -> Self {
                let vtable = unsafe { ::basic_oop::core_mem_transmute::<
                    [*const (); #methods_enum_name::MethodsCount as usize],
                    ::basic_oop::VtableJoin<#base_const_name, #complement_const_name>
                >(self.0) };
                let vtable: ::basic_oop::VtableJoin<
                    #base_const_name,
                    #complement_const_name
                > = ::basic_oop::VtableJoin {
                    a: vtable.a,
                    b: [#list]
                };
                #vtable_name(unsafe { ::basic_oop::core_mem_transmute::<
                    ::basic_oop::VtableJoin<#base_const_name, #complement_const_name>,
                    [*const (); #methods_enum_name::MethodsCount as usize]
                >(vtable) })
            }
        });
    }
    quote! {
        #struct_

        impl #vtable_name {
            pub const fn new() -> Self {
                let vtable: ::basic_oop::VtableJoin<
                    #base_const_name,
                    #complement_const_name
                > = ::basic_oop::VtableJoin {
                    a: #base_vtable_with_overrides.0,
                    b: [#methods_impl]
                };
                #vtable_name(unsafe { ::basic_oop::core_mem_transmute::<
                    ::basic_oop::VtableJoin<#base_const_name, #complement_const_name>,
                    [*const (); #methods_enum_name::MethodsCount as usize]
                >(vtable) })
            }

            #base_methods
            #methods_tokens
        }
    }
}

fn build_methods(
    base_types: &[Base],
    vis: &Visibility,
    class_name: &Ident,
    class_mod: &Path,
    methods: &[(Ident, TypeBareFn)]
) -> TokenStream {
    let methods_enum_name = Ident::new(&(class_name.to_string() + "Methods"), Span::call_site());
    let mut methods_tokens = TokenStream::new();
    for base_type in base_types {
        let base_type_ty = sanitize_base_type(base_type.ty.clone(), class_mod);
        let mut base_trait = base_type_ty.clone();
        patch_path(&mut base_trait, |x| "T".to_string() + &x);
        for (method_name, method_ty) in &base_type.methods {
            let ty = actual_method_ty(method_ty.clone(), class_name);
            let signature = fn_signature(&ty, method_name.clone());
            let mut item: ImplItemFn = parse_quote! {
                #vis #signature {
                    let this: ::basic_oop::alloc_sync_Arc<dyn #base_trait>
                        = ::basic_oop::dynamic_cast_dyn_cast_arc(this.clone()).unwrap();
                    #base_type_ty::#method_name(&this)
                }
            };
            let Stmt::Expr(Expr::Call(call), _) = item.block.stmts.last_mut().unwrap() else { panic!() };
            for arg in ty.inputs.iter().skip(1) {
                let mut segments = Punctuated::new();
                segments.push(PathSegment {
                    ident: arg.name.clone().unwrap().0,
                    arguments: PathArguments::None,
                });
                call.args.push(Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: Path { leading_colon: None, segments },
                }));
            }
            item.to_tokens(&mut methods_tokens);
        }
    }
    for (method_name, method_ty) in methods {
        let ty = actual_method_ty(method_ty.clone(), class_name);
        let signature = fn_signature(&ty, method_name.clone());
        let ty_without_idents = fn_ty_without_idents(ty.clone());
        let name = Ident::new(&to_pascal(method_name.to_string()), Span::call_site());
        let mut item: ImplItemFn = parse_quote! {
            #vis #signature {
                let vtable = this.vtable();
                let method = unsafe { ::basic_oop::core_mem_transmute::<*const (), #ty_without_idents>(
                    *vtable.add(#methods_enum_name::#name as usize)
                ) };
                method()
            }
        };
        let Stmt::Expr(Expr::Call(call), _) = item.block.stmts.last_mut().unwrap() else { panic!() };
        for arg in ty.inputs {
            let mut segments = Punctuated::new();
            segments.push(PathSegment {
                ident: arg.name.unwrap().0,
                arguments: PathArguments::None,
            });
            call.args.push(Expr::Path(ExprPath {
                attrs: Vec::new(),
                qself: None,
                path: Path { leading_colon: None, segments },
            }));
        }
        item.to_tokens(&mut methods_tokens);
    }
    quote! {
        impl #class_name {
            #methods_tokens
        }
    }
}

fn build(inherited_from: ItemStruct, class: ItemStruct) -> Result<TokenStream, Diagnostic> {
    let base_types = parse_base_types(inherited_from)?;
    let class = Class::parse(class)?;
    let new_inherited_from = build_inherited_from(&base_types, &class.name, &class.mod_, &class.methods);
    let struct_ = build_struct(&base_types, &class.attrs, &class.vis, &class.name, &class.mod_, &class.fields);
    let trait_ = build_trait(&base_types, &class.vis, &class.name, &class.mod_);
    let methods_enum = build_methods_enum(&base_types[0].ty, &class.vis, &class.name, &class.mod_, &class.methods);
    let consts_for_vtable = build_consts_for_vtable(&class.name, &class.mod_, &base_types);
    let vtable = build_vtable(&base_types, &class.name, &class.mod_, &class.vis, &class.methods, &class.overrides);
    let methods = build_methods(&base_types, &class.vis, &class.name, &class.mod_, &class.methods);
    Ok(quote! {
        #new_inherited_from
        #struct_
        #trait_
        #methods_enum
        #consts_for_vtable
        #vtable
        #methods
    })
}

#[import_tokens_attr(::basic_oop::macro_magic)]
#[proc_macro_attribute]
pub fn class_unsafe(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inherited_from = parse_macro_input!(attr as ItemStruct);
    let class = parse_macro_input!(input as ItemStruct);
    match build(inherited_from, class) {
        Ok(tokens) => tokens.into(),
        Err(diag) => diag.emit_as_expr_tokens().into(),
    }
}
