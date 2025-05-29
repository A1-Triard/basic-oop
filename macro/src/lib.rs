extern crate proc_macro;

use anycase::{to_snake, to_pascal, to_screaming_snake};
use itertools::process_results;
use proc_macro2::{TokenStream, Delimiter, Group, Span};
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
    attr: Vec<Attribute>,
    vis: Visibility,
    name: Ident,
    ty: Type,
}

impl Parse for Field {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = Attribute::parse_outer(input)?;
        let vis: Visibility = input.parse()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;
        Ok(Field { attr, vis, name, ty })
    }
}

struct Class {
    attr: Vec<Attribute>,
    vis: Visibility,
    path: Path,
    base: Vec<Path>,
    fields: Punctuated<Field, Token![,]>,
}

impl Parse for Class {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attr = Attribute::parse_outer(input)?;
        let vis: Visibility = input.parse()?;
        input.parse::<Token![struct]>()?;
        let path: Path = input.parse()?;
        input.parse::<Token![:]>()?;
        let master_base: Path = input.parse()?;
        let mut base = vec![master_base];
        loop {
            let lookahead = input.lookahead1();
            if lookahead.peek(Token![->]) {
                input.parse::<Token![->]>().unwrap();
                let next_base: Path = input.parse()?;
                base.push(next_base);
            } else {
                break;
            }
        }
        let content;
        braced!(content in input);
        let fields = content.parse_terminated(Field::parse, Token![,])?;
        Ok(Class { attr, vis, path, base, fields })
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
        name:Some((Ident::new("this", Span::call_site()), <Token![:]>::default())),
        ty: parse_quote! { &::basic_oop::std_sync_Arc<dyn #this_trait> },
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
        inputs: process_results(ty.inputs.iter().map(bare_fn_arg_to_fn_arg), |i| i.collect())?,
        variadic: None,
        output: ty.output.clone(),
    })
}

fn base_path(mut base: Path, f: impl FnOnce(String) -> String) -> Result<Path, Diagnostic> {
    let Some(base_ty_name) = base.segments.last_mut() else {
        return Err(base.span().error("expected base type name"));
    };
    if !base_ty_name.arguments.is_none() {
        return Err(base.span().error("expected base type name"));
    }
    base_ty_name.ident = Ident::new(&f(base_ty_name.ident.to_string()), Span::call_site());
    Ok(base)
}

impl Class {
    fn name(&self) -> Result<Ident, Diagnostic> {
        let Some(last_segment) = self.path.segments.last() else {
            return Err(self.path.span().error("class name is mandatory"));
        };
        if !last_segment.arguments.is_none() {
            return Err(last_segment.span().error("unexpected arguments"));
        }
        Ok(last_segment.ident.clone())
    }

    fn actual_fields(&self) -> Vec<Field> {
        self.fields.iter().filter(|f| !f.attr.iter().any(|a| is_virt_meta(&a.meta))).cloned().collect()
    }

    fn virt_methods(&self) -> Result<Vec<Field>, Diagnostic> {
        process_results(self.fields.iter().filter_map(|f| {
            let Some((i, _)) = f.attr.iter().enumerate().find(|(_, a)| is_virt_meta(&a.meta)) else {
                return None;
            };
            if f.attr.len() != 1 {
                return Some(Err(f.attr[if i == 0 { 1 } else { 0 }].span().error("unexpected extra attribute")));
            }
            Some(Ok(f))
        }), |i| i.cloned().collect())
    }

    fn make_struct(&self) -> Result<TokenStream, Diagnostic> {
        let mut fields_tokens = TokenStream::new();
        let Some(base_ty_name) = self.base[0].segments.last() else {
            return Err(self.base[0].span().error("expected base type name"));
        };
        if !base_ty_name.arguments.is_none() {
            return Err(self.base[0].span().error("expected base type name"));
        }
        let base_field = Ident::new(&to_snake(base_ty_name.ident.to_string()), Span::call_site());
        let base_ty = &self.base[0];
        fields_tokens.extend(quote! {
            #base_field: #base_ty,
        });
        let fields = self.actual_fields();
        for field in fields {
            let mut attr_tokens = TokenStream::new();
            for attr in field.attr {
                attr.to_tokens(&mut attr_tokens);
            }
            let vis = field.vis;
            let name = field.name;
            let ty = field.ty;
            fields_tokens.extend(quote! {
                #attr_tokens
                #vis #name: #ty,
            });
        }
        let content = Group::new(Delimiter::Brace, fields_tokens);
        let mut attr_tokens = TokenStream::new();
        for attr in &self.attr {
            attr.to_tokens(&mut attr_tokens);
        }
        let vis = &self.vis;
        let name = self.name()?;
        Ok(quote! {
            #attr_tokens
            #vis struct #name #content
        })
    }

    fn base_ty(&self, f: impl FnOnce(String) -> String) -> Result<Path, Diagnostic> {
        base_path(self.base[0].clone(), f)
    }

    fn make_virt_methods_enum(&self) -> Result<TokenStream, Diagnostic> {
        let virt_methods = self.virt_methods()?;
        let base = self.base_ty(|x| x + "Methods")?;
        let mut values = TokenStream::new();
        values.append_terminated(
            virt_methods.iter().enumerate().map(|(i, virt_method)| {
                let name = Ident::new(&to_pascal(&virt_method.name.to_string()), Span::call_site());
                let index = LitInt::new(&(i.to_string() + "usize"), Span::call_site());
                quote! {
                    #name = (#base::Count as usize) + #index
                }
            }),
            Token![,](Span::call_site())
        );
        let vis = &self.vis;
        let name = Ident::new(&(self.name()?.to_string() + "Methods"), Span::call_site());
        let count = virt_methods.len();
        Ok(quote! {
            #[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd, Hash)]
            #[repr(usize)]
            #vis enum #name {
                #values
                Count = (#base::Count as usize) + #count
            }
        })
    }

    fn make_virt_methods(&self) -> Result<TokenStream, Diagnostic> {
        let vis = &self.vis;
        let virt_methods = self.virt_methods()?;
        let name = self.name()?;
        let trait_name = Ident::new(&("T".to_string() + &name.to_string()), Span::call_site());
        let methods_enum_name = Ident::new(&(name.to_string() + "Methods"), Span::call_site());
        let mut methods_tokens = TokenStream::new();
        process_results(virt_methods.into_iter().map(|m| -> Result<ImplItemFn, Diagnostic> {
            let name = m.name;
            let enum_name = Ident::new(&to_pascal(name.to_string()), Span::call_site());
            let ty = actual_method_ty(m.ty, &trait_name)?;
            let signature = fn_signature(&ty, name.clone())?;
            let ty_without_idents = fn_ty_without_idents(ty.clone());
            let mut block: Block = parse_quote! {
                {
                    let vtable = ::basic_oop::TObj::vtable(this.as_ref());
                    let method = unsafe { ::basic_oop::std_mem_transmute::<*const (), #ty_without_idents>(
                        *vtable.add(#methods_enum_name::#enum_name as usize)
                    ) };
                    method()
                }
            };
            let Stmt::Expr(Expr::Call(method_call), _) = block.stmts.last_mut().unwrap() else { panic!(); };
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
        }), |i| methods_tokens.append_all(i))?;
        Ok(quote! {
            impl #name {
                #methods_tokens
            }
        })
    }

    fn make_trait(&self) -> Result<TokenStream, Diagnostic> {
        let name = self.name()?;
        let trait_name = Ident::new(&("T".to_string() + &name.to_string()), Span::call_site());
        let fn_name = Ident::new(&to_snake(name.to_string()), Span::call_site());
        let vis = &self.vis;
        let base = self.base_ty(|x| "T".to_string() + &x)?;
        Ok(quote! {
            #vis trait #trait_name: #base {
                fn #fn_name(&self) -> &#name;
            }
        })
    }

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
        let virt_methods = self.virt_methods()?;
        methods_impl.append_separated(virt_methods.iter().map(|m| {
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
        process_results(virt_methods.iter().enumerate().map(|(i, m)| -> Result<TokenStream, Diagnostic> {
            let mut list = TokenStream::new();
            list.append_separated((0 .. i).into_iter().map(|j| {
                let index = LitInt::new(&(j.to_string() + "usize"), Span::call_site());
                quote! { vtable.b[#index] }
            }), <Token![,]>::default());
            list.extend(quote! { f as *const () });
            list.append_separated((i + 1 .. virt_methods.len()).into_iter().map(|j| {
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

    fn make_class(&self) -> Result<TokenStream, Diagnostic> {
        let the_struct = self.make_struct()?;
        let methods_enum = self.make_virt_methods_enum()?;
        let virt_methods = self.make_virt_methods()?;
        let the_trait = self.make_trait()?;
        let consts_for_vtable = self.make_consts_for_vtable()?;
        let vtable = self.make_vtable()?;
        Ok(quote! {
            #the_struct
            #methods_enum
            #virt_methods
            #the_trait
            #consts_for_vtable
            #vtable
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
