//! The `invoke_inline_macro!` macro.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::TokenStream;

use libloading::{Library, Symbol};
use proc_macro_error::abort;
use syn::parse::{Parse, ParseStream};
use syn::{Ident, LitStr};

pub(super) fn invoke_inline_macro(input: TokenStream1) -> TokenStream1 {
    let input: InvokerInput = syn::parse_macro_input!(input);

    let library = Library::new(input.dylib_path.value())
        .unwrap_or_else(|e| abort!(input.dylib_path, "Failed to open library: {}", e));

    match input.macro_type {
        MacroType::Bang(tokens) => {
            let macro_function: Symbol<fn(TokenStream1) -> TokenStream1> =
                unsafe { library_macro(&library, "bang", input.name) };
            macro_function(tokens.into())
        }
        MacroType::Derive(item) => {
            let macro_function: Symbol<fn(TokenStream1) -> TokenStream1> =
                unsafe { library_macro(&library, "derive", input.name) };
            macro_function(item.into())
        }
        MacroType::Attribute(attr, item) => {
            let macro_function: Symbol<fn(TokenStream1, TokenStream1) -> TokenStream1> =
                unsafe { library_macro(&library, "attribute", input.name) };
            macro_function(attr.into(), item.into())
        }
    }
}

struct InvokerInput {
    dylib_path: LitStr,
    name: Ident,
    macro_type: MacroType,
}

impl Parse for InvokerInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            dylib_path: input.parse::<LitStr>()?,
            name: input.parse::<Ident>()?,
            macro_type: input.parse()?,
        })
    }
}

enum MacroType {
    Bang(TokenStream),
    Derive(TokenStream),
    Attribute(TokenStream, TokenStream),
}

impl Parse for MacroType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ty = input.parse::<Ident>()?;

        Ok(match &*ty.to_string() {
            "bang" => Self::Bang(input.parse()?),
            "derive" => Self::Derive(input.parse()?),
            "attribute" => {
                let content;
                syn::parenthesized!(content in input);
                Self::Attribute(content.parse()?, input.parse()?)
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    ty,
                    "Expected `bang`, `derive` or `attribute`",
                ))
            }
        })
    }
}

unsafe fn library_macro<'lib, T>(
    library: &'lib Library,
    macro_type: &str,
    macro_name: Ident,
) -> Symbol<'lib, T> {
    let symbol_name = format!("{}_{}\0", macro_type, macro_name);

    #[allow(unused_unsafe)]
    let symbol = unsafe { library.get::<T>(symbol_name.as_bytes()) };
    symbol.unwrap_or_else(|e| {
        abort!(
            macro_name,
            "Failed to load macro {} from library: {}",
            macro_name,
            e
        )
    })
}
