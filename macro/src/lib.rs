/*
extern crate proc_macro;

use anycase::{to_snake, to_pascal, to_screaming_snake};
use itertools::Itertools;
use proc_macro2::{TokenStream, Span};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{self, Attribute, Visibility, Ident, Type, Token, Path, braced, parse_macro_input, Meta, LitInt};
use syn::{TypeBareFn, Signature, Generics, Block, ImplItemFn, parse_quote};
use syn::{BareFnArg, FnArg, PatType, Pat, PatIdent, Expr, PathArguments, Stmt, PathSegment, ExprPath};
use syn::parse::{ParseStream, Parse};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

#[derive(Clone)]
struct Field {
    attrs: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    ty: Type,
}

impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = Attribute::parse_outer(input)?;
        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;
        Ok(Field { attrs, vis, name, ty })
    }
}

struct Class {
    attrs: Vec<Attribute>,
    vis: Visibility,
    path: Path,
    base: Path,
    fields: Punctuated<Field, Token![,]>,
}

impl Parse for Class {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = Attribute::parse_outer(input)?;
        let vis: Visibility = input.parse()?;
        let path: Path = input.parse()?;
        input.parse::<Token![->]>()?;
        let base: Path = input.parse()?;
        let content;
        braced!(content in input);
        let fields = content.parse_terminated(Field::parse, Token![,])?;
        Ok(Class { attrs, vis, path, base, fields })
    }
}

fn is_virt_meta(meta: &Meta) -> bool {
    match meta {
        Meta::Path(path) => {
               path.leading_colon.is_none()
            && path.segments.len() == 1
            && path.segments[0].arguments.is_none()
            && path.segments[0].ident.to_string() == "virt"
        },
        _ => false
    }
}

fn actual_method_ty(decl_ty: Type, this_trait: &Ident) -> Result<TypeBareFn, Diagnostic> {
    let Type::BareFn(mut bare_fn) = decl_ty else { return Err(decl_ty.span().error("invalid method type")); };
    if
           bare_fn.lifetimes.is_some()
        || bare_fn.unsafety.is_some()
        || bare_fn.abi.is_some()
        || bare_fn.variadic.is_some()
    {
        return Err(bare_fn.span().error("invalid method type"));
    }
    let this_arg = BareFnArg {
        attrs: Vec::new(),
        name: Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &::basic_oop::alloc_sync_Arc<dyn #this_trait> },
    };
    bare_fn.inputs.insert(0, this_arg);
    Ok(bare_fn)
}

fn fn_ty_without_idents(mut ty: TypeBareFn) -> TypeBareFn {
    for arg in &mut ty.inputs {
        arg.name = None;
    }
    ty
}

fn bare_fn_arg_to_fn_arg(a: &BareFnArg) -> Result<FnArg, Diagnostic> {
    let Some((name, colon_token)) = &a.name else { return Err(a.span().error("argument name required")); }; 
    Ok(FnArg::Typed(PatType {
        attrs: a.attrs.clone(),
        pat: Box::new(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: name.clone(),
            subpat: None
        })),
        colon_token: colon_token.clone(),
        ty: Box::new(a.ty.clone()),
    }))
}

fn fn_signature(ty: &TypeBareFn, name: Ident) -> Result<Signature, Diagnostic> {
    Ok(Signature {
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
        inputs: ty.inputs.iter().map(bare_fn_arg_to_fn_arg).process_results(|i| i.collect())?,
        variadic: None,
        output: ty.output.clone(),
    })
}

impl Class {
    fn path(&self, f: impl FnOnce(String) -> String) -> Result<TokenStream, Diagnostic> {
        if
               self.path.leading_colon.is_some()
            || self.path.segments.len() < 2
            || self.path.segments[0].ident.to_string() != "crate"
        {
            return Err(self.path.span().error("full path starting with 'crate' required"));
        }
        let mut segments: Punctuated<PathSegment, Token![::]> = Punctuated::new();
        for segment in self.path.segments.iter() {
            segments.push(segment.clone());
        }
        let path_name = segments.last_mut().unwrap();
        if !path_name.arguments.is_none() {
            return Err(self.path.span().error("expected type name"));
        }
        path_name.ident = Ident::new(&f(path_name.ident.to_string()), Span::call_site());
        Ok(quote! { #segments })
    }

    fn path_dollar(&self, f: impl FnOnce(String) -> String) -> Result<TokenStream, Diagnostic> {
        if
               self.path.leading_colon.is_some()
            || self.path.segments.len() < 2
            || self.path.segments[0].ident.to_string() != "crate"
        {
            return Err(self.path.span().error("full path starting with 'crate' required"));
        }
        let mut segments: Punctuated<PathSegment, Token![::]> = Punctuated::new();
        for segment in self.path.segments.iter().skip(1) {
            segments.push(segment.clone());
        }
        let path_name = segments.last_mut().unwrap();
        if !path_name.arguments.is_none() {
            return Err(self.path.span().error("expected type name"));
        }
        path_name.ident = Ident::new(&f(path_name.ident.to_string()), Span::call_site());
        Ok(quote! { $crate :: #segments })
    }

    fn base_path(&self, f: impl FnOnce(String) -> String) -> Result<Path, Diagnostic> {
        let mut base = self.base.clone();
        if base.leading_colon.is_none() || base.segments.len() < 2 {
            return Err(base.span().error("full path with leading colon required"));
        }
        let base_name = base.segments.last_mut().unwrap();
        if !base_name.arguments.is_none() {
            return Err(base.span().error("expected base type name"));
        }
        base_name.ident = Ident::new(&f(base_name.ident.to_string()), Span::call_site());
        Ok(base)
    }

    fn name(&self, f: impl FnOnce(String) -> String) -> Result<Ident, Diagnostic> {
        if
               self.path.leading_colon.is_some()
            || self.path.segments.len() < 2
            || self.path.segments[0].ident.to_string() != "crate"
        {
            return Err(self.path.span().error("full path starting with 'crate' required"));
        }
        let path_name = self.path.segments.last().unwrap();
        if !path_name.arguments.is_none() {
            return Err(self.path.span().error("expected type name"));
        }
        Ok(Ident::new(&f(path_name.ident.to_string()), Span::call_site()))
    }

    fn base_name(&self, f: impl FnOnce(String) -> String) -> Result<Ident, Diagnostic> {
        if self.base.leading_colon.is_none() || self.base.segments.len() < 2 {
            return Err(self.base.span().error("full path with leading colon required"));
        }
        let base_name = self.base.segments.last().unwrap();
        if !base_name.arguments.is_none() {
            return Err(self.base.span().error("expected base type name"));
        }
        Ok(Ident::new(&f(base_name.ident.to_string()), Span::call_site()))
    }

    fn global_name(&self, f: impl FnOnce(String) -> String) -> Result<Ident, Diagnostic> {
        if
               self.path.leading_colon.is_some()
            || self.path.segments.len() < 2
            || self.path.segments[0].ident.to_string() != "crate"
        {
            return Err(self.path.span().error("full path starting with 'crate' required"));
        }
        let path_name = self.path.segments.last().unwrap();
        if !path_name.arguments.is_none() {
            return Err(self.path.span().error("expected type name"));
        }
        let global = self.path.segments.iter().skip(1).take(self.path.segments.len() - 2).map(|s| {
            if !s.arguments.is_none() { return Err(s.span().error("invalid path")); }
            Ok(s.ident.to_string() + "_")
        }).process_results(|mut i| i.join(""))? + &f(path_name.ident.to_string());
        Ok(Ident::new(&global, Span::call_site()))
    }

    fn base_global_path(&self, f: impl FnOnce(String) -> String) -> Result<Path, Diagnostic> {
        if self.base.leading_colon.is_none() || self.base.segments.len() < 2 {
            return Err(self.base.span().error("full path with leading colon required"));
        }
        let prefix = self.base.segments.iter().skip(1).take(self.base.segments.len() - 2).map(|s| {
            if !s.arguments.is_none() { return Err(s.span().error("invalid path")); }
            Ok(s.ident.to_string() + "_")
        }).process_results(|mut i| i.join(""))?;
        let mut path = Path {
            leading_colon: self.base.leading_colon.clone(),
            segments: Punctuated::new()
        };
        path.segments.push(self.base.segments.first().unwrap().clone());
        path.segments.push(PathSegment {
            ident: Ident::new(&(prefix + &f(self.base.segments.last().unwrap().ident.to_string())), Span::call_site()),
            arguments: PathArguments::None
        });
        Ok(path)
    }

    fn actual_fields(&self) -> Vec<Field> {
        self.fields.iter().filter(|f| !f.attrs.iter().any(|a| is_virt_meta(&a.meta))).cloned().collect()
    }

    fn methods(&self) -> Result<Vec<Field>, Diagnostic> {
        self.fields.iter().filter_map(|f| {
            let Some((i, _)) = f.attrs.iter().enumerate().find(|(_, a)| is_virt_meta(&a.meta)) else {
                return None;
            };
            if f.attrs.len() != 1 {
                return Some(Err(f.attrs[if i == 0 { 1 } else { 0 }].span().error("unexpected extra attribute")));
            }
            Some(Ok(f))
        }).process_results(|i| i.cloned().collect())
    }

    fn make_struct(&self) -> Result<TokenStream, Diagnostic> {
        let mut fields_tokens = TokenStream::new();
        let base_field = self.base_name(to_snake)?;
        let base_ty = &self.base;
        fields_tokens.extend(quote! {
            #base_field: #base_ty,
        });
        let fields = self.actual_fields();
        for field in fields {
            let mut attrs_tokens = TokenStream::new();
            for attr in field.attrs {
                attr.to_tokens(&mut attrs_tokens);
            }
            let vis = field.vis;
            let name = field.name;
            let ty = field.ty;
            fields_tokens.extend(quote! {
                #attrs_tokens
                #vis #name: #ty,
            });
        }
        let mut attrs_tokens = TokenStream::new();
        for attr in &self.attrs {
            attr.to_tokens(&mut attrs_tokens);
        }
        let vis = &self.vis;
        let name = self.name(|x| x)?;
        Ok(quote! {
            #attrs_tokens
            #vis struct #name { #fields_tokens }
        })
    }

    fn make_methods_enum(&self) -> Result<TokenStream, Diagnostic> {
        let methods = self.methods()?;
        let base = self.base_path(|x| x + "Methods")?;
        let mut values = TokenStream::new();
        values.append_terminated(
            methods.iter().enumerate().map(|(i, virt_method)| {
                let name = Ident::new(&to_pascal(virt_method.name.to_string()), Span::call_site());
                let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
                quote! {
                    #name = (#base::Count as usize) + #index
                }
            }),
            Token![,](Span::call_site())
        );
        let vis = &self.vis;
        let name = self.name(|x| x + "Methods")?;
        let count = methods.len();
        Ok(quote! {
            #[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd, Hash)]
            #[repr(usize)]
            #vis enum #name {
                #values
                Count = (#base::Count as usize) + #count
            }
        })
    }

    fn make_methods(&self) -> Result<TokenStream, Diagnostic> {
        let vis = &self.vis;
        let methods = self.methods()?;
        let name = self.name(|x| x)?;
        let trait_name = self.name(|x| "T".to_string() + &x)?;
        let methods_enum_name = self.name(|x| x + "Methods")?;
        let mut methods_tokens = TokenStream::new();
        methods.into_iter().map(|m| -> Result<ImplItemFn, Diagnostic> {
            let name = m.name;
            let enum_name = Ident::new(&to_pascal(name.to_string()), Span::call_site());
            let ty = actual_method_ty(m.ty, &trait_name)?;
            let signature = fn_signature(&ty, name.clone())?;
            let ty_without_idents = fn_ty_without_idents(ty.clone());
            let mut block: Block = parse_quote! {
                {
                    let vtable = ::basic_oop::TObj::obj(this.as_ref()).vtable();
                    let method = unsafe { ::basic_oop::core_mem_transmute::<*const (), #ty_without_idents>(
                        *vtable.add(#methods_enum_name::#enum_name as usize)
                    ) };
                    method()
                }
            };
            let Stmt::Expr(Expr::Call(method_call), _) = block.stmts.last_mut().unwrap() else { panic!() };
            for arg in ty.inputs {
                let mut segments = Punctuated::new();
                segments.push(PathSegment {
                    ident: arg.name.unwrap().0,
                    arguments: PathArguments::None,
                });
                method_call.args.push(Expr::Path(ExprPath {
                    attrs: Vec::new(),
                    qself: None,
                    path: Path { leading_colon: None, segments },
                }));
            }
            Ok(ImplItemFn {
                attrs: Vec::new(),
                vis: vis.clone(),
                defaultness: None,
                sig: signature,
                block,
            })
        }).process_results(|i| methods_tokens.append_all(i))?;
        Ok(quote! {
            impl #name {
                #methods_tokens
            }
        })
    }

    fn make_trait(&self) -> Result<TokenStream, Diagnostic> {
        let name = self.name(|x| x)?;
        let trait_name = self.name(|x| "T".to_string() + &x)?;
        let fn_name = self.name(to_snake)?;
        let vis = &self.vis;
        let base = &self.base;
        let base_trait = self.base_path(|x| "T".to_string() + &x)?;
        let base_field = self.base_name(to_snake)?;
        let impl_t_base_base = self.base_global_path(|x| "impl_t_".to_string() + &to_snake(x) + "_base")?;
        Ok(quote! {
            #vis trait #trait_name: #base_trait {
                fn #fn_name(&self) -> &#name;
            }

            impl #trait_name for #name {
                fn #fn_name(&self) -> &#name { self }
            }

            impl #base_trait for #name {
                fn obj(&self) -> &#base { &self.#base_field }
            }

            #impl_t_base_base!(#name);
        })
    }

    fn impl_supports_interfaces(&self) -> Result<TokenStream, Diagnostic> {
        let name = self.name(|x| x)?;
        let trait_path = self.path_dollar(|x| "T".to_string() + &x)?;
        let macro_name = self.global_name(|x| "impl_supports_interfaces_for_".to_string() + &to_snake(x))?;
        //let macro_name_self_ref = self.path(|x| "impl_supports_interfaces_for_".to_string() + &to_snake(x))?;
        let base_macro_path = self.base_global_path(|x| "impl_supports_interfaces_for_".to_string() + &to_snake(x))?;
        Ok(quote! {
            unsafe impl ::basic_oop::dynamic_cast_SupportsInterfaces for #name {
                fn get_interface_metadata(
                    &self,
                    dyn_interface_id: ::basic_oop::core_any_TypeId,
                ) -> ::basic_oop::core_option_Option<::basic_oop::dynamic_cast_BoxedInterfaceMetadata> {
                    let this = self;
                    crate::#macro_name!(dyn_interface_id, this);
                    ::basic_oop::core_option_Option::None
                }
            }

            #[macro_export]
            macro_rules! #macro_name {
                ($dyn_interface_id:ident, $this:ident) => {
                    #base_macro_path!($dyn_interface_id, $this);
                    if
                        let ::basic_oop::core_option_Option::Some(metadata)
                            = ::basic_oop::dynamic_cast_try_get_interface_metadata_for::<dyn #trait_path>($dyn_interface_id, $this)
                    {
                        return ::basic_oop::core_option_Option::Some(metadata);
                    }
                };
            }
        })
    }

    *
    fn make_consts_for_vtable(&self) -> Result<TokenStream, Diagnostic> {
        let name = self.name()?;
        let enum_name = Ident::new(&(name.to_string() + "Methods"), Span::call_site());
        let mut tokens = TokenStream::new();
        for base in &self.base {
            let base_enum = base_path(base.clone(), |x| x + "Methods")?;
            let base_const_name = base_path(base.clone(), |x| to_screaming_snake(x) + "_COUNT")?
                .segments.last().unwrap().ident.clone();
            let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
            tokens.extend(quote! {
                const #base_const_name: usize = #base_enum::Count as usize;
                const #complement_const_name: usize = (#enum_name::Count as usize) - #base_const_name;
            })
        }
        Ok(tokens)
    }

    fn make_vtable(&self) -> Result<TokenStream, Diagnostic> {
        let base_vtable_name = self.base_ty(|x| x + "Vtable")?;
        let base_const_name = self.base_ty(|x| to_screaming_snake(x) + "_COUNT")?
            .segments.last().unwrap().ident.clone();
        let self_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
        let vis = &self.vis;
        let name = self.name()?;
        let trait_name = Ident::new(&("T".to_string() + &name.to_string()), Span::call_site());
        let methods_enum_name = Ident::new(&(name.to_string() + "Methods"), Span::call_site());
        let vtable_name = Ident::new(&(name.to_string() + "Vtable"), Span::call_site());
        let mut methods_impl = TokenStream::new();
        let methods = self.methods()?;
        methods_impl.append_separated(methods.iter().map(|m| {
            let impl_name = Ident::new(&(m.name.to_string() + "_impl"), Span::call_site());
            quote! {
                #name::#impl_name as *const ()
            }
        }), <Token![,]>::default());
        let mut macro_calls = TokenStream::new();
        process_results(self.base.iter().map(|b| -> Result<TokenStream, Diagnostic> {
            let macro_name = base_path(b.clone(), |x| to_snake(x) + "_vtable")?;
            let base_const_name = base_path(b.clone(), |x| to_screaming_snake(x) + "_COUNT")?
                .segments.last().unwrap().ident.clone();
            let complement_const_name = Ident::new(&(base_const_name.to_string() + "_COMPL"), Span::call_site());
            Ok(quote! {
                #macro_name!(
                    #base_const_name,
                    #complement_const_name,
                    #methods_enum_name::Count as usize,
                    #vtable_name
                )
            })
        }), |i| macro_calls.append_terminated(i, <Token![;]>::default()))?;
        let mut overrides = TokenStream::new();
        process_results(methods.iter().enumerate().map(|(i, m)| -> Result<TokenStream, Diagnostic> {
            let mut list = TokenStream::new();
            list.append_separated((0 .. i).into_iter().map(|j| {
                let index = LitInt::new(&(j.to_string() + "usize"), Span::call_site());
                quote! { vtable.b[#index] }
            }), <Token![,]>::default());
            list.extend(quote! { f as *const () });
            list.append_separated((i + 1 .. methods.len()).into_iter().map(|j| {
                let index = LitInt::new(&(j.to_string() + "usize"), Span::call_site());
                quote! { vtable.b[#index] }
            }), <Token![,]>::default());
            let name = &m.name;
            let ty = actual_method_ty(m.ty.clone(), &trait_name)?;
            Ok(quote! {
                pub const fn #name(
                    self,
                    f: #ty
                ) -> Self {
                    let vtable = unsafe { ::basic_oop::std_mem_transmute::<
                        [*const (); #methods_enum_name::Count as usize],
                        ::basic_oop::VtableJoin<#base_const_name, #self_const_name>
                    >(self.0) };
                    let vtable: ::basic_oop::VtableJoin<
                        #base_const_name,
                        #self_const_name
                    > = ::basic_oop::VtableJoin {
                        a: vtable.a,
                        b: [#list]
                    };
                    #vtable_name(unsafe { ::basic_oop::std_mem_transmute::<
                        ::basic_oop::VtableJoin<#base_const_name, #self_const_name>,
                        [*const (); #methods_enum_name::Count as usize]
                    >(vtable) })
                }
            })
        }), |i| overrides.append_all(i))?;
        Ok(quote! {
            #vis struct #vtable_name(pub [*const (); #methods_enum_name::Count as usize]);

            impl #vtable_name {
                pub const fn new() -> Self {
                    let vtable: ::basic_oop::VtableJoin<
                        #base_const_name,
                        #self_const_name
                    > = ::basic_oop::VtableJoin {
                        a: #base_vtable_name::new().0,
                        b: [#methods_impl]
                    };
                    #vtable_name(unsafe { ::basic_oop::std_mem_transmute::<
                        ::basic_oop::VtableJoin<#base_const_name, #self_const_name>,
                        [*const (); #methods_enum_name::Count as usize]
                    >(vtable) })
                }

                #macro_calls

                #overrides
            }
        })
    }
*

    fn make_class(&self) -> Result<TokenStream, Diagnostic> {
        let the_struct = self.make_struct()?;
        let methods_enum = self.make_methods_enum()?;
        let methods = self.make_methods()?;
        let the_trait = self.make_trait()?;
        let the_impl_supports_interfaces = self.impl_supports_interfaces()?;
        //let consts_for_vtable = self.make_consts_for_vtable()?;
        //let vtable = self.make_vtable()?;
        Ok(quote! {
            #the_struct
            #methods_enum
            #methods
            #the_trait
            #the_impl_supports_interfaces
        })
    }
}

#[proc_macro]
pub fn class(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let c = parse_macro_input!(input as Class);
    match c.make_class() {
        Ok(tokens) => tokens.into(),
        Err(diag) => diag.emit_as_expr_tokens().into(),
    }
}
*/

extern crate proc_macro;

use anycase::{to_pascal, to_snake};
use iter_identify_first_last::IteratorIdentifyFirstLastExt;
use itertools::Itertools;
use macro_magic::import_tokens_attr;
use proc_macro2::{TokenStream, Span};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use quote::{quote, TokenStreamExt};
use syn::{parse_macro_input, parse_quote, ItemStruct, Path, Type, Fields, Meta, Attribute, Visibility, Ident};
use syn::{Field, FieldMutability, TypePath, Token, PathSegment, PathArguments, LitInt};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

fn parse_base_types(inherited_from: ItemStruct) -> Result<Vec<Path>, Diagnostic> {
    let Fields::Unnamed(fields) = inherited_from.fields else {
        return Err(inherited_from.fields.span().error("invalid base class"));
    };
    fields.unnamed.into_iter().map(|field| {
        let Type::Path(type_path) = field.ty else {
            return Err(field.ty.span().error("invalid base class"));
        };
        if type_path.path.segments.len() < 2 {
            return Err(type_path.span().error("invalid base class"));
        }
        Ok(type_path.path)
    }).process_results(|i| i.collect())
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
            "override" => FieldKind::Override,
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
                        .error("only one of 'virt'/'override' attributes can be specified for a field")
                    );
                }
                kind = FieldKind::Method;
            },
            FieldKind::Override => {
                if kind != FieldKind::Data {
                    return Err(
                         attr.span()
                        .error("only one of 'virt'/'override' attributes can be specified for a field")
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
    methods: Vec<Field>,
    overrides: Vec<Field>,
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
                    FieldKind::Method => methods.push(field.clone()),
                    FieldKind::Override => overrides.push(field.clone()),
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

fn build_inherited_from(base_types: &[Path], class_name: &Ident, class_mod: &Path) -> ItemStruct {
    let name = Ident::new(&("inherited_from_".to_string() + &class_name.to_string()), Span::call_site());
    let mut struct_: ItemStruct = parse_quote! {
        #[::basic_oop::macro_magic::export_tokens_no_emit]
        struct #name (
            #class_mod::#class_name
        );
    };
    let Fields::Unnamed(fields) = &mut struct_.fields else { panic!() };
    for base_type in base_types {
        fields.unnamed.push(Field {
            attrs: Vec::new(),
            vis: Visibility::Inherited,
            mutability: FieldMutability::None,
            ident: None,
            colon_token: None,
            ty: Type::Path(TypePath {
                qself: None,
                path: base_type.clone()
            }),
        });
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
    base_types: &[Path],
    attrs: &[Attribute],
    vis: &Visibility,
    name: &Ident,
    mod_: &Path,
    fields: &[Field]
) -> ItemStruct {
    let base_type = sanitize_base_type(base_types[0].clone(), mod_);
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

fn build_trait(base_types: &[Path], vis: &Visibility, class_name: &Ident, mod_: &Path) -> TokenStream {
    let base_type = sanitize_base_type(base_types[0].clone(), mod_);
    let base_field = Ident::new(
        &to_snake(base_type.segments.last().unwrap().ident.to_string()),
        Span::call_site()
    );
    let mut base_trait = sanitize_base_type(base_types[0].clone(), mod_);
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
            &to_snake(base_base_type.segments.last().unwrap().ident.to_string()),
            Span::call_site()
        );
        let mut base_base_trait = sanitize_base_type(base_base_type.clone(), mod_);
        patch_path(&mut base_base_trait, |x| "T".to_string() + &x);
        trait_.extend(quote! {
            impl #base_base_trait for #class_name {
                fn #method_name(&self) -> &#base_base_type {
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
        let mut base_trait = sanitize_base_type(base_type.clone(), mod_);
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
    methods: &[Field]
) -> TokenStream {
    let mut base_methods_enum = sanitize_base_type(base_type.clone(), mod_);
    patch_path(&mut base_methods_enum, |x| x + "Methods");
    let methods_enum = Ident::new(&(class_name.to_string() + "Methods"), Span::call_site());
    let mut values = TokenStream::new();
    values.append_terminated(
        methods.iter().enumerate().map(|(i, method)| {
            let name = Ident::new(&to_pascal(method.ident.as_ref().unwrap().to_string()), Span::call_site());
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

fn build_vtable() -> TokenStream {
}

fn build(inherited_from: ItemStruct, class: ItemStruct) -> Result<TokenStream, Diagnostic> {
    let base_types = parse_base_types(inherited_from)?;
    let class = Class::parse(class)?;
    let new_inherited_from = build_inherited_from(&base_types, &class.name, &class.mod_);
    let struct_ = build_struct(&base_types, &class.attrs, &class.vis, &class.name, &class.mod_, &class.fields);
    let trait_ = build_trait(&base_types, &class.vis, &class.name, &class.mod_);
    let methods_enum = build_methods_enum(&base_types[0], &class.vis, &class.name, &class.mod_, &class.methods);
    Ok(quote! {
        #new_inherited_from
        #struct_
        #trait_
        #methods_enum
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
