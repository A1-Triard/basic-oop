extern crate proc_macro;

use itertools::process_results;
use proc_macro2::{TokenStream, Delimiter, Group, Span};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{self, Attribute, Visibility, Ident, Type, Token, Path, braced, parse_macro_input, Meta, LitInt};
use syn::{TypeBareFn, Signature, Generics, Block, ImplItemFn, parse_quote};
use syn::parse::{ParseStream, Parse};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use anycase::{to_snake, to_pascal};

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
        || bare_fn.inputs.is_empty()
        || !bare_fn.inputs[0].attrs.is_empty()
        || bare_fn.inputs[0].name.is_some()
    {
        return Err(bare_fn.span().error("invalid method type"));
    }
    let Type::Path(self_path) = &bare_fn.inputs[0].ty else {
        return Err(bare_fn.span().error("invalid method type"));
    };
    if
           self_path.qself.is_some()
        || self_path.path.leading_colon.is_some()
        || self_path.path.segments.len() != 1
        || !self_path.path.segments[0].arguments.is_none()
        || self_path.path.segments[0].ident.to_string() != "self"
    {
        return Err(bare_fn.span().error("invalid method type"));
    }
    bare_fn.inputs[0].ty = parse_quote! {
        &Arc<dyn #this_trait>
    };
    bare_fn.inputs[0].name = Some((Ident::new("this", Span::call_site()), Token![:](Span::call_site())));
    Ok(bare_fn)
}

fn fn_ty_without_idents(ty: TypeBareFn) -> TypeBareFn {
    for arg in &mut ty.inputs {
        arg.name = None;
    }
    ty
}

fn bare_fn_arg_to_fn_arg(a: &BareFnArg) -> FnArg {

}

fn fn_signature(ty: &TypeBareFn, name: Ident) -> Signature {
    Signature {
        constness: None,
        asyncness: None,
        unsafety: None,
        abi: None,
        fn_token: Token![fn](Span::call_site()),
        ident: name,
        generics: Generics {
            lt_token: None,
            params: Punctuated::new(),
            gt_token: None,
            where_clause: None,
        },
        paren_token: ty.paren_token.clone(),
        inputs: ty.inputs.clone(),
        variadic: None,
        output: ty.output.clone(),
    }
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

    fn make_virt_methods_enum(&self) -> Result<TokenStream, Diagnostic> {
        let virt_methods = self.virt_methods()?;
        let mut base = self.base[0].clone();
        let Some(base_ty_name) = base.segments.last_mut() else {
            return Err(self.base[0].span().error("expected base type name"));
        };
        if !base_ty_name.arguments.is_none() {
            return Err(self.base[0].span().error("expected base type name"));
        }
        base_ty_name.ident = Ident::new(&(base_ty_name.ident.to_string() + "Methods"), Span::call_site());
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
        let virt_methods = self.virt_methods()?;
        let name = self.name()?.to_string();
        let trait_name = Ident::new(&("T" + name), Span::call_site());
        let methods_enum_name = Ident::new(&(name + "Methods"), Span::call_site());
        let mut methods_tokens = TokenStream::new();
        process_results(virt_methods.into_iter().map(|m| {
            let name = m.name;
            let enum_name = Ident::new(&to_pascal(name.to_string()), Span::call_site());
            let ty = actual_method_ty(m.ty, &trait_name)?;
            let signature = fn_signature(&ty, name.clone());
            let ty_without_idents = fn_ty_without_idents(ty);
            let block = Block::parse(quote! {
                {
                    let vtable = basic_oop::TObj::vtable(this.as_ref());
                    let method = unsafe { basic_oop::std_mem_transmute::<*const (), #ty_without_idents>(
                        *vtable.add(#methods_enum_name::#enum_name as usize)
                    ) };
                    method();
                }
            });
            //block.stmts.push(Stmt::Expr(Expr::Call(
            Ok(ImplItemFn {
                attrs: Vec::new(),
                vis: Visibility::Public(Token![pub](Span::call_site())),
                defaultness: None,
                sig: signature,
                block,
            })
        }), |i| methods_tokens.append_all(i));
        Ok(quote! {
            impl #name {
                #methods_tokens
            }
        })
    }

    fn make_class(self) -> Result<TokenStream, Diagnostic> {
        let the_struct = self.make_struct()?;
        let methods_enum = self.make_virt_methods_enum()?;
        Ok(quote! {
            #the_struct
            #methods_enum
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
