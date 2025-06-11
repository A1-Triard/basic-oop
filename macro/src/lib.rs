#![deny(warnings)]

extern crate proc_macro;

use anycase::{to_pascal, to_snake, to_screaming_snake};
use indoc::{formatdoc, indoc};
use macro_magic::import_tokens_attr;
use proc_macro2::{TokenStream, Span};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use quote::{quote, TokenStreamExt, ToTokens};
use syn::{parse_macro_input, parse_quote, ItemStruct, Path, Type, Fields, Meta, Attribute, Visibility, Ident};
use syn::{Field, FieldMutability, TypePath, Token, PathSegment, PathArguments, LitInt, TypeBareFn, Expr};
use syn::{BareFnArg, Generics, Signature, Pat, PatType, PatIdent, FnArg, ImplItemFn, Stmt, ExprPath};
use syn::{Receiver, TypeReference, AttrStyle};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Bracket;

fn parse_base_field_meta(meta: &Meta) -> Option<bool> {
    match meta {
        Meta::Path(path) if
               path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments[0].arguments.is_none()
        => match path.segments[0].ident.to_string().as_ref() {
            "non_virt" => Some(false),
            "virt" => Some(true),
            _ => None
        },
        _ => None
    }
}

fn parse_base_field_attrs(attrs: &[Attribute]) -> Option<bool> {
    for attr in attrs {
        if let Some(virt) = parse_base_field_meta(&attr.meta) {
            return Some(virt);
        }
    }
    None
}

fn parse_base_meta(meta: &Meta) -> Option<bool> {
    match meta {
        Meta::Path(path) if
               path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments[0].arguments.is_none()
        => match path.segments[0].ident.to_string().as_ref() {
            "non_sync" => Some(false),
            "sync" => Some(true),
            _ => None
        },
        _ => None
    }
}

fn parse_base_sync(inherits: &ItemStruct) -> Option<bool> {
    for attr in &inherits.attrs {
        if let Some(is_sync) = parse_base_meta(&attr.meta) {
            return Some(is_sync);
        }
    }
    None
}

struct Base {
    ty: Path,
    non_virt_methods: Vec<(Ident, TypeBareFn, Vec<Attribute>)>,
    virt_methods: Vec<(Ident, TypeBareFn, Vec<Attribute>)>,
}

fn parse_base_types(inherits: ItemStruct) -> Result<Vec<Base>, Diagnostic> {
    let Fields::Named(fields) = inherits.fields else {
        return Err(inherits.fields.span().error("invalid base class"));
    };
    let mut res = Vec::new();
    let mut base = None;
    for field in fields.named {
        if *field.ident.as_ref().unwrap() == "__class__" {
            if let Some(base) = base.take() {
                res.push(base);
            }
            let Type::Path(type_path) = field.ty else {
                return Err(field.ty.span().error("invalid base class"));
            };
            if type_path.path.segments.len() != 1 {
                return Err(type_path.span().error("invalid base class"));
            }
            base = Some(Base {
                ty: type_path.path,
                non_virt_methods: Vec::new(),
                virt_methods: Vec::new()
            });
        } else {
            let name = field.ident.as_ref().unwrap().clone();
            let Type::BareFn(type_fn) = &field.ty else {
                return Err(field.ty.span().error("invalid base class"));
            };
            let Some(base) = base.as_mut() else {
                return Err(type_fn.span().error("invalid base class"));
            };
            let Some(virt) = parse_base_field_attrs(&field.attrs) else {
                return Err(field.span().error("invalid base class"))
            };
            let attrs = field.attrs.into_iter().filter(|x| parse_base_field_meta(&x.meta).is_none()).collect();
            if virt {
                base.virt_methods.push((name, type_fn.clone(), attrs));
            } else {
                base.non_virt_methods.push((name, type_fn.clone(), attrs));
            }
        }
    }
    if let Some(base) = base.take() {
        res.push(base);
    }
    Ok(res)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
enum FieldKind {
    NonVirtMethod,
    VirtMethod,
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
            "non_virt" => FieldKind::NonVirtMethod,
            "virt" => FieldKind::VirtMethod,
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
            FieldKind::NonVirtMethod => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'non_virt'/'virt'/'over' attributes can be specified for a field")
                    );
                }
                kind = FieldKind::NonVirtMethod;
            },
            FieldKind::VirtMethod => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'non_virt'/'virt'/'over' attributes can be specified for a field")
                    );
                }
                kind = FieldKind::VirtMethod;
            },
            FieldKind::Override => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'non_virt'/'virt'/'over' attributes can be specified for a field")
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
    fields: Vec<Field>,
    non_virt_methods: Vec<(Ident, TypeBareFn, Vec<Attribute>)>,
    virt_methods: Vec<(Ident, TypeBareFn, Vec<Attribute>)>,
    overrides: Vec<Ident>,
}

impl Class {
    fn parse(descr: ItemStruct) -> Result<Class, Diagnostic> {
        if descr.generics.lt_token.is_some() || descr.generics.where_clause.is_some() {
            return Err(descr.generics.span().error("basic-oop does not support generics"));
        }
        let mut fields = Vec::new();
        let mut non_virt_methods = Vec::new();
        let mut virt_methods = Vec::new();
        let mut overrides = Vec::new();
        let Fields::Named(descr_fields) = descr.fields else {
            return Err(descr.fields.span().error("class should be described as struct with named fields"));
        };
        for field in descr_fields.named.iter() {
            match parse_field_attrs(&field.attrs)? {
                FieldKind::Data => fields.push(field.clone()),
                FieldKind::NonVirtMethod => {
                    let name = field.ident.clone().unwrap();
                    if name == "__class__" {
                        return Err(name.span().error("this name is reserved"));
                    }
                    let Type::BareFn(type_fn) = &field.ty else {
                        return Err(field.ty.span().error("invalid non-virtual method type"));
                    };
                    if
                           type_fn.unsafety.is_some()
                        || type_fn.abi.is_some()
                        || type_fn.variadic.is_some()
                        || type_fn.inputs.iter().any(|x| !x.attrs.is_empty())
                    {
                        return Err(field.ty.span().error("invalid non-virtual method type"));
                    }
                    if let Some(arg) = type_fn.inputs.iter().find(|x| x.name.is_none()) {
                        return Err(arg.span().error("argument name required"));
                    }
                    let attrs = field.attrs.iter()
                        .filter(|x| parse_field_meta(&x.meta) == FieldKind::Data)
                        .cloned().collect();
                    non_virt_methods.push((name, type_fn.clone(), attrs));
                },
                FieldKind::VirtMethod => {
                    let name = field.ident.clone().unwrap();
                    if name == "__class__" {
                        return Err(name.span().error("this name is reserved"));
                    }
                    let Type::BareFn(type_fn) = &field.ty else {
                        return Err(field.ty.span().error("invalid virtual method type"));
                    };
                    if
                           type_fn.unsafety.is_some()
                        || type_fn.abi.is_some()
                        || type_fn.variadic.is_some()
                        || type_fn.inputs.iter().any(|x| !x.attrs.is_empty())
                    {
                        return Err(field.ty.span().error("invalid virtual method type"));
                    }
                    if let Some(arg) = type_fn.inputs.iter().find(|x| x.name.is_none()) {
                        return Err(arg.span().error("argument name required"));
                    }
                    let attrs = field.attrs.iter()
                        .filter(|x| parse_field_meta(&x.meta) == FieldKind::Data)
                        .cloned().collect();
                    virt_methods.push((name, type_fn.clone(), attrs));
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
        Ok(Class {
            attrs: descr.attrs,
            vis: descr.vis,
            name: descr.ident,
            fields,
            non_virt_methods,
            virt_methods,
            overrides,
        })
    }
}

fn build_inherits(
    base_types: &[Base],
    class_name: &Ident,
    sync: bool,
    non_virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)],
    virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)]
) -> ItemStruct {
    let name = Ident::new(&("inherits_".to_string() + &class_name.to_string()), Span::call_site());
    let sync_name = Ident::new(if sync { "sync" } else { "non_sync" }, Span::call_site());
    let mut struct_: ItemStruct = parse_quote! {
        #[::basic_oop::macro_magic::export_tokens_no_emit]
        #[#sync_name]
        struct #name {
            __class__: #class_name
        }
    };
    let Fields::Named(fields) = &mut struct_.fields else { panic!() };
    for (method_name, method_ty, method_attrs) in non_virt_methods {
        let mut attrs = method_attrs.clone();
        let mut segments = Punctuated::new();
        segments.push(PathSegment {
            ident: Ident::new("non_virt", Span::call_site()),
            arguments: PathArguments::None
        });
        attrs.push(Attribute {
            pound_token: <Token![#]>::default(),
            style: AttrStyle::Outer,
            bracket_token: Bracket::default(),
            meta: Meta::Path(Path { leading_colon: None, segments }),
        });
        fields.named.push(Field {
            attrs,
            vis: Visibility::Inherited,
            mutability: FieldMutability::None,
            ident: Some(method_name.clone()),
            colon_token: Some(<Token![:]>::default()),
            ty: Type::BareFn(method_ty.clone()),
        });
    }
    for (method_name, method_ty, method_attrs) in virt_methods {
        let mut attrs = method_attrs.clone();
        let mut segments = Punctuated::new();
        segments.push(PathSegment {
            ident: Ident::new("virt", Span::call_site()),
            arguments: PathArguments::None
        });
        attrs.push(Attribute {
            pound_token: <Token![#]>::default(),
            style: AttrStyle::Outer,
            bracket_token: Bracket::default(),
            meta: Meta::Path(Path { leading_colon: None, segments }),
        });
        fields.named.push(Field {
            attrs,
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
        for (method_name, method_ty, method_attrs) in &base_type.non_virt_methods {
            let mut attrs = method_attrs.clone();
            let mut segments = Punctuated::new();
            segments.push(PathSegment {
                ident: Ident::new("non_virt", Span::call_site()),
                arguments: PathArguments::None
            });
            attrs.push(Attribute {
                pound_token: <Token![#]>::default(),
                style: AttrStyle::Outer,
                bracket_token: Bracket::default(),
                meta: Meta::Path(Path { leading_colon: None, segments }),
            });
            fields.named.push(Field {
                attrs,
                vis: Visibility::Inherited,
                mutability: FieldMutability::None,
                ident: Some(method_name.clone()),
                colon_token: Some(<Token![:]>::default()),
                ty: Type::BareFn(method_ty.clone()),
            });
        }
        for (method_name, method_ty, method_attrs) in &base_type.virt_methods {
            let mut attrs = method_attrs.clone();
            let mut segments = Punctuated::new();
            segments.push(PathSegment {
                ident: Ident::new("virt", Span::call_site()),
                arguments: PathArguments::None
            });
            attrs.push(Attribute {
                pound_token: <Token![#]>::default(),
                style: AttrStyle::Outer,
                bracket_token: Bracket::default(),
                meta: Meta::Path(Path { leading_colon: None, segments }),
            });
            fields.named.push(Field {
                attrs,
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
    fields: &[Field]
) -> ItemStruct {
    let base_type = base_types[0].ty.clone();
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

fn build_trait(base_types: &[Base], vis: &Visibility, class_name: &Ident, sync: bool) -> TokenStream {
    let doc_name = class_name.to_string();
    let rc = if sync { "arc" } else { "rc" };
    let doc = formatdoc!("
        Represents [`{doc_name}`] or any of its inheritors.

        Use `dynamic_cast_{rc}` from `dynamic_cast` crate to convert to or from this trait.
    ");
    let method_doc = indoc!("
        Returns reference to inner data.
    ");
    let into_method_doc = indoc!("
        Converts to inner data.
    ");
    let base_type = base_types[0].ty.clone();
    let base_field = Ident::new(
        &to_snake(base_type.segments.last().unwrap().ident.to_string()),
        Span::call_site()
    );
    let into_base = Ident::new(&("into_".to_string() + &base_field.to_string()), Span::call_site());
    let mut base_trait = base_types[0].ty.clone();
    patch_path(&mut base_trait, |x| "Is".to_string() + &x);
    let trait_name = Ident::new(&("Is".to_string() + &class_name.to_string()), Span::call_site());
    let method_name = Ident::new(&to_snake(class_name.to_string()), Span::call_site());
    let into_method_name = Ident::new(&("into_".to_string() + &to_snake(class_name.to_string())), Span::call_site());
    let mut trait_ = quote! {
        #[doc=#doc]
        #vis trait #trait_name: #base_trait {
            #[doc=#method_doc]
            fn #method_name(&self) -> &#class_name;

            #[doc=#into_method_doc]
            fn #into_method_name(self) -> #class_name where Self: Sized;
        }

        impl #trait_name for #class_name {
            fn #method_name(&self) -> &#class_name { self }

            fn #into_method_name(self) -> #class_name { self }
        }

        impl #base_trait for #class_name {
            fn #base_field(&self) -> &#base_type { &self.#base_field }

            fn #into_base(self) -> #base_type { self.#base_field }
        }
    };
    for base_base_type in base_types.iter().skip(1) {
        let method_name = Ident::new(
            &to_snake(base_base_type.ty.segments.last().unwrap().ident.to_string()),
            Span::call_site()
        );
        let into_name = Ident::new(&("into_".to_string() + &method_name.to_string()), Span::call_site());
        let mut base_base_trait = base_base_type.ty.clone();
        patch_path(&mut base_base_trait, |x| "Is".to_string() + &x);
        let base_base_type_ty = &base_base_type.ty;
        trait_.extend(quote! {
            impl #base_base_trait for #class_name {
                fn #method_name(&self) -> &#base_base_type_ty {
                    #base_base_trait::#method_name(&self.#base_field)
                }

                fn #into_name(self) -> #base_base_type_ty {
                    #base_base_trait::#into_name(self.#base_field)
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
        let mut base_trait = base_type.ty.clone();
        patch_path(&mut base_trait, |x| "Is".to_string() + &x);
        traits_list.push(base_trait);
    }
    trait_.extend(quote! {
        ::basic_oop::dynamic_cast_impl_supports_interfaces!(#class_name: #traits_list);
    });
    trait_
}

fn build_virt_methods_enum(
    base_type: &Path,
    vis: &Visibility,
    class_name: &Ident,
    virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)]
) -> TokenStream {
    let doc_name = class_name.to_string();
    let doc = formatdoc!("
        [`{doc_name}`] virtual methods list.

        Used by the [`class_unsafe`](::basic_oop::class_unsafe) macro, not intended for direct use in code.
    ");
    let mut base_methods_enum = base_type.clone();
    patch_path(&mut base_methods_enum, |x| x + "VirtMethods");
    let methods_enum = Ident::new(&(class_name.to_string() + "VirtMethods"), Span::call_site());
    let mut values = TokenStream::new();
    values.append_terminated(
        virt_methods.iter().enumerate().map(|(i, (method_name, _method_ty, _))| {
            let name = Ident::new(&to_pascal(method_name.to_string()), Span::call_site());
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            quote! {
                #name = (#base_methods_enum::VirtMethodsCount as usize) + #index
            }
        }),
        <Token![,]>::default()
    );
    let count = virt_methods.len();
    quote! {
        #[doc=#doc]
        #[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd, Hash)]
        #[repr(usize)]
        #vis enum #methods_enum {
            #values
            VirtMethodsCount = (#base_methods_enum::VirtMethodsCount as usize) + #count
        }
    }
}

fn build_consts_for_vtable(class_name: &Ident, base_types: &[Base]) -> TokenStream {
    let enum_name = Ident::new(&(class_name.to_string() + "VirtMethods"), Span::call_site());
    let mut tokens = TokenStream::new();
    for base_type in base_types {
        let mut base_methods_enum = base_type.ty.clone();
        patch_path(&mut base_methods_enum, |x| x + "VirtMethods");
        let base_const_name = Ident::new(
            &(
                to_screaming_snake(
                    class_name.to_string() + &base_type.ty.segments.last().unwrap().ident.to_string()
                ) + "_VIRT_METHODS_COUNT"
            ),
            Span::call_site()
        );
        let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
        tokens.extend(quote! {
            const #base_const_name: usize = #base_methods_enum::VirtMethodsCount as usize;
            const #complement_const_name: usize = (#enum_name::VirtMethodsCount as usize) - #base_const_name;
        });
    }
    tokens
}

fn actual_base_method_ty(mut ty: TypeBareFn, base_type: &Path, sync: bool) -> TypeBareFn {
    let rc = if sync {
        quote! { ::basic_oop::alloc_sync_Arc }
    } else {
        quote! { ::basic_oop::alloc_rc_Rc }
    };
    let mut base_trait = base_type.clone();
    patch_path(&mut base_trait, |x| "Is".to_string() + &x);
    let this_arg = BareFnArg {
        attrs: Vec::new(),
        name: Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &#rc<dyn #base_trait> },
    };
    ty.inputs.insert(0, this_arg);
    ty
}

fn actual_method_ty(mut ty: TypeBareFn, class_name: &Ident, sync: bool) -> TypeBareFn {
    let rc = if sync {
        quote! { ::basic_oop::alloc_sync_Arc }
    } else {
        quote! { ::basic_oop::alloc_rc_Rc }
    };
    let trait_name = Ident::new(&("Is".to_string() + &class_name.to_string()), Span::call_site());
    let this_arg = BareFnArg {
        attrs: Vec::new(),
        name: Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &#rc<dyn #trait_name> },
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
        colon_token: *colon_token,
        ty: Box::new(a.ty.clone()),
    })
}

fn method_signature(ty: &TypeBareFn, name: Ident) -> Signature {
    let generics = if let Some(lifetimes) = &ty.lifetimes {
        Generics {
            lt_token: Some(lifetimes.lt_token),
            params: lifetimes.lifetimes.clone(),
            gt_token: Some(lifetimes.gt_token),
            where_clause: None,
        }
    } else {
        Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        }
    };
    let mut s = Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: ty.fn_token,
        ident: name,
        generics,
        paren_token: ty.paren_token,
        inputs: Punctuated::new(),
        variadic: None,
        output: ty.output.clone(),
    };
    let mut segments = Punctuated::new();
    segments.push(PathSegment {
        ident: Ident::new("Self", Span::call_site()),
        arguments: PathArguments::None
    });
    s.inputs.push(FnArg::Receiver(Receiver {
        attrs: Vec::new(),
        reference: Some((<Token![&]>::default(), None)),
        mutability: None,
        self_token: <Token![self]>::default(),
        colon_token: None,
        ty: Box::new(Type::Reference(TypeReference {
            and_token: <Token![&]>::default(),
            lifetime: None,
            mutability: None,
            elem: Box::new(Type::Path(TypePath {
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments
                }
            }))
        }))
    }));
    for arg in ty.inputs.iter().skip(1) {
        s.inputs.push(bare_fn_arg_to_fn_arg(arg));
    }
    s
}

fn impl_method_signature(ty: &TypeBareFn, name: Ident) -> Signature {
    let generics = if let Some(lifetimes) = &ty.lifetimes {
        Generics {
            lt_token: Some(lifetimes.lt_token),
            params: lifetimes.lifetimes.clone(),
            gt_token: Some(lifetimes.gt_token),
            where_clause: None,
        }
    } else {
        Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        }
    };
    let mut s = Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: ty.fn_token,
        ident: name,
        generics,
        paren_token: ty.paren_token,
        inputs: Punctuated::new(),
        variadic: None,
        output: ty.output.clone(),
    };
    for arg in ty.inputs.iter() {
        s.inputs.push(bare_fn_arg_to_fn_arg(arg));
    }
    s
}

fn build_vtable(
    base_types: &[Base],
    class_name: &Ident,
    sync: bool,
    vis: &Visibility,
    virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)],
    overrides: &[Ident],
) -> TokenStream {
    let doc_name = class_name.to_string();
    let doc = formatdoc!("
        [`{doc_name}`] virtual methods table.

        Used by the [`class_unsafe`](::basic_oop::class_unsafe) macro, not intended for direct use in code.
    ");
    let vtable_name = Ident::new(&(class_name.to_string() + "Vtable"), Span::call_site());
    let methods_enum_name = Ident::new(&(class_name.to_string() + "VirtMethods"), Span::call_site());
    let struct_ = quote! {
        #[doc=#doc]
        #vis struct #vtable_name(pub [*const (); #methods_enum_name::VirtMethodsCount as usize]);
    };
    let mut methods_impl = TokenStream::new();
    methods_impl.append_separated(virt_methods.iter().map(|(m, ty, _)| {
        let ty = actual_method_ty(ty.clone(), class_name, sync);
        let ty_without_idents = fn_ty_without_idents(ty);
        let impl_name = Ident::new(&(m.to_string() + "_impl"), Span::call_site());
        quote! { { let f: #ty_without_idents = #class_name::#impl_name; f as *const () } }
    }), <Token![,]>::default());
    let mut base_vtable = base_types[0].ty.clone();
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
            ) + "_VIRT_METHODS_COUNT"
        ),
        Span::call_site()
    );
    let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
    let mut base_methods = TokenStream::new();
    for base_type in base_types {
        let mut base_vtable = base_type.ty.clone();
        patch_path(&mut base_vtable, |x| x + "Vtable");
        let base_const_name = Ident::new(
            &(
                to_screaming_snake(
                    class_name.to_string() + &base_type.ty.segments.last().unwrap().ident.to_string()
                ) + "_VIRT_METHODS_COUNT"
            ),
            Span::call_site()
        );
        let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
        for (base_method, base_method_ty, _) in &base_type.virt_methods {
            let ty = actual_base_method_ty(base_method_ty.clone(), &base_type.ty, sync);
            let ty_without_idents = fn_ty_without_idents(ty);
            base_methods.extend(quote! {
                pub const fn #base_method(
                    self,
                    f: #ty_without_idents
                ) -> Self {
                    let vtable = unsafe { ::basic_oop::core_mem_transmute::<
                        [*const (); #methods_enum_name::VirtMethodsCount as usize],
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
                        [*const (); #methods_enum_name::VirtMethodsCount as usize]
                    >(vtable) })
                }
            });
        }
    }
    let mut methods_tokens = TokenStream::new();
    for (method_index, (method_name, method_ty, _)) in virt_methods.iter().enumerate() {
        let ty = actual_method_ty(method_ty.clone(), class_name, sync);
        let ty_without_idents = fn_ty_without_idents(ty);
        let mut list: Punctuated<Expr, Token![,]> = Punctuated::new();
        for i in 0 .. method_index {
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            list.push(parse_quote! { vtable.b[#index] });
        }
        list.push(parse_quote! { f as *const () });
        for i in method_index + 1 .. virt_methods.len() {
            let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
            list.push(parse_quote! { vtable.b[#index] });
        }
        methods_tokens.extend(quote! {
            pub const fn #method_name(
                self,
                f: #ty_without_idents
            ) -> Self {
                let vtable = unsafe { ::basic_oop::core_mem_transmute::<
                    [*const (); #methods_enum_name::VirtMethodsCount as usize],
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
                    [*const (); #methods_enum_name::VirtMethodsCount as usize]
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
                    [*const (); #methods_enum_name::VirtMethodsCount as usize]
                >(vtable) })
            }

            #base_methods
            #methods_tokens
        }
    }
}

fn build_methods(
    base_types: &[Base],
    class_name: &Ident,
    sync: bool,
    non_virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)],
    virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)]
) -> TokenStream {
    if 
           !base_types.iter().any(|x| x.non_virt_methods.iter().chain(x.virt_methods.iter()).next().is_some())
        && non_virt_methods.is_empty()
        && virt_methods.is_empty()
    {
        return TokenStream::new();
    }
    let rc = if sync {
        quote! { ::basic_oop::alloc_sync_Arc }
    } else {
        quote! { ::basic_oop::alloc_rc_Rc }
    };
    let methods_enum_name = Ident::new(&(class_name.to_string() + "VirtMethods"), Span::call_site());
    let mut methods_tokens = TokenStream::new();
    for base_type in base_types {
        let base_type_ty = base_type.ty.clone();
        let mut base_trait = base_type_ty.clone();
        patch_path(&mut base_trait, |x| "Is".to_string() + &x);
        let mut base_trait_ext = base_type_ty.clone();
        patch_path(&mut base_trait_ext, |x| x + "Ext");
        for (method_name, method_ty, _) in base_type.non_virt_methods.iter().chain(base_type.virt_methods.iter()) {
            let ty = actual_method_ty(method_ty.clone(), class_name, sync);
            let signature = method_signature(&ty, method_name.clone());
            let mut item: ImplItemFn = parse_quote! {
                #signature {
                    let this: #rc<dyn #base_trait> = self.clone();
                    #base_trait_ext::#method_name(&this)
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
    for (method_name, method_ty, _) in non_virt_methods {
        let ty = actual_method_ty(method_ty.clone(), class_name, sync);
        let signature = method_signature(&ty, method_name.clone());
        let name = Ident::new(&(method_name.to_string() + "_impl"), Span::call_site());
        let mut item: ImplItemFn = parse_quote! {
            #signature {
                #class_name::#name(self)
            }
        };
        let Stmt::Expr(Expr::Call(call), _) = item.block.stmts.last_mut().unwrap() else { panic!() };
        for arg in ty.inputs.into_iter().skip(1) {
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
    for (method_name, method_ty, _) in virt_methods {
        let ty = actual_method_ty(method_ty.clone(), class_name, sync);
        let signature = method_signature(&ty, method_name.clone());
        let ty_without_idents = fn_ty_without_idents(ty.clone());
        let name = Ident::new(&to_pascal(method_name.to_string()), Span::call_site());
        let mut item: ImplItemFn = if sync {
            parse_quote! {
                #signature {
                    let vtable = ::basic_oop::obj_sync::IsObjSync::obj_sync(self.as_ref()).vtable();
                    let method = unsafe { ::basic_oop::core_mem_transmute::<*const (), #ty_without_idents>(
                        *vtable.add(#methods_enum_name::#name as usize)
                    ) };
                    method(self)
                }
            }
        } else {
            parse_quote! {
                #signature {
                    let vtable = ::basic_oop::obj::IsObj::obj(self.as_ref()).vtable();
                    let method = unsafe { ::basic_oop::core_mem_transmute::<*const (), #ty_without_idents>(
                        *vtable.add(#methods_enum_name::#name as usize)
                    ) };
                    method(self)
                }
            }
        };
        let Stmt::Expr(Expr::Call(call), _) = item.block.stmts.last_mut().unwrap() else { panic!() };
        for arg in ty.inputs.into_iter().skip(1) {
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
    let trait_name = Ident::new(&(class_name.to_string() + "Ext"), Span::call_site());
    let t = Ident::new(&("Is".to_string() + &class_name.to_string()), Span::call_site());
    quote! {
        impl #trait_name for #rc<dyn #t> {
            #methods_tokens
        }
    }
}

fn build_vtable_const(class_name: &Ident) -> TokenStream {
    let methods_enum_name = Ident::new(&(class_name.to_string() + "VirtMethods"), Span::call_site());
    let vtable_name = Ident::new(&(class_name.to_string() + "Vtable"), Span::call_site());
    let const_name = Ident::new(&to_screaming_snake(vtable_name.to_string()), Span::call_site());
    quote! {
        const #const_name: [*const (); #methods_enum_name::VirtMethodsCount as usize] = #vtable_name::new().0;
    }
}

fn build_call_trait(
    base_types: &[Base],
    vis: &Visibility,
    class_name: &Ident,
    sync: bool,
    non_virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)],
    virt_methods: &[(Ident, TypeBareFn, Vec<Attribute>)]
) -> TokenStream {
    if 
           !base_types.iter().any(|x| x.non_virt_methods.iter().chain(x.virt_methods.iter()).next().is_some())
        && non_virt_methods.is_empty()
        && virt_methods.is_empty()
    {
        return TokenStream::new();
    }
    let doc_name = class_name.to_string();
    let rc = if sync { "Arc" } else { "Rc" };
    let doc = formatdoc!("
        [`{doc_name}`] methods extension trait.

        Implemented by the `{rc}<{doc_name}>` type for convenient method calling syntax.
    ");
    let mut methods_tokens = TokenStream::new();
    for base_type in base_types {
        for
            (method_name, method_ty, method_attrs)
        in
            base_type.non_virt_methods.iter().chain(base_type.virt_methods.iter())
        {
            for attr in method_attrs {
                attr.to_tokens(&mut methods_tokens);
            }
            let ty = actual_method_ty(method_ty.clone(), class_name, sync);
            let signature = method_signature(&ty, method_name.clone());
            signature.to_tokens(&mut methods_tokens);
            <Token![;]>::default().to_tokens(&mut methods_tokens);
        }
    }
    for (method_name, method_ty, method_attrs) in non_virt_methods.iter().chain(virt_methods.iter()) {
        for attr in method_attrs {
            attr.to_tokens(&mut methods_tokens);
        }
        let ty = actual_method_ty(method_ty.clone(), class_name, sync);
        let signature = method_signature(&ty, method_name.clone());
        signature.to_tokens(&mut methods_tokens);
        <Token![;]>::default().to_tokens(&mut methods_tokens);
    }
    let trait_name = Ident::new(&(class_name.to_string() + "Ext"), Span::call_site());
    quote! {
        #[doc=#doc]
        #vis trait #trait_name {
            #methods_tokens
        }
    }
}

fn build_not_overrided_virt_methods(
    base_types: &[Base],
    vis: &Visibility,
    class_name: &Ident,
    sync: bool,
    overrides: &[Ident],
) -> TokenStream {
    if !base_types.iter().any(|x|
        x.virt_methods.iter().any(|m| !overrides.iter().any(|x| *x == m.0.to_string().as_str()))
    ) {
        return TokenStream::new();
    }
    let mut methods_tokens = TokenStream::new();
    let base_type_ty = &base_types[0].ty;
    for base_type in base_types {
        for (method_name, method_ty, _) in &base_type.virt_methods {
            if overrides.iter().any(|x| *x == method_name.to_string().as_str()) { continue; }
            let impl_method_name = Ident::new(&(method_name.to_string() + "_impl"), Span::call_site());
            let ty = actual_method_ty(method_ty.clone(), &base_type.ty.segments.last().unwrap().ident, sync);
            let signature = impl_method_signature(&ty, impl_method_name.clone());
            let mut item: ImplItemFn = parse_quote! {
                #vis #signature {
                    #base_type_ty::#impl_method_name(this)
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
    quote! {
        impl #class_name {
            #methods_tokens
        }
    }
}

fn build(inherits: ItemStruct, class: ItemStruct) -> Result<TokenStream, Diagnostic> {
    let Some(sync) = parse_base_sync(&inherits) else {
        return Err(inherits.span().error("Invalid base class"));
    };
    let base_types = parse_base_types(inherits)?;
    let class = Class::parse(class)?;
    let new_inherits = build_inherits(
        &base_types, &class.name, sync, &class.non_virt_methods, &class.virt_methods
    );
    let struct_ = build_struct(&base_types, &class.attrs, &class.vis, &class.name, &class.fields);
    let trait_ = build_trait(&base_types, &class.vis, &class.name, sync);
    let methods_enum = build_virt_methods_enum(
        &base_types[0].ty, &class.vis, &class.name, &class.virt_methods
    );
    let consts_for_vtable = build_consts_for_vtable(&class.name, &base_types);
    let vtable = build_vtable(
        &base_types, &class.name, sync, &class.vis, &class.virt_methods, &class.overrides
    );
    let vtable_const = build_vtable_const(&class.name);
    let call_trait = build_call_trait(
        &base_types, &class.vis, &class.name, sync, &class.non_virt_methods, &class.virt_methods
    );
    let methods = build_methods(
        &base_types, &class.name, sync, &class.non_virt_methods, &class.virt_methods
    );
    let not_overrided_methods = build_not_overrided_virt_methods(
        &base_types, &class.vis, &class.name, sync, &class.overrides
    );
    Ok(quote! {
        #new_inherits
        #struct_
        #trait_
        #methods_enum
        #consts_for_vtable
        #vtable
        #vtable_const
        #call_trait
        #methods
        #not_overrided_methods
    })
}

#[import_tokens_attr(::basic_oop::macro_magic)]
#[proc_macro_attribute]
pub fn class_unsafe(attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let inherits = parse_macro_input!(attr as ItemStruct);
    let class = parse_macro_input!(input as ItemStruct);
    match build(inherits, class) {
        Ok(tokens) => tokens.into(),
        Err(diag) => diag.emit_as_expr_tokens().into(),
    }
}
