//! The `inline_proc` attribute macro.

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::{Delimiter, Group, Spacing, Span, TokenStream, TokenTree};
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter, Write};
use std::io::BufReader;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::{env, fs};

use cargo_metadata::diagnostic::{
    Diagnostic as CargoDiagnostic, DiagnosticLevel as CargoLevel, DiagnosticSpan as CargoSpan,
};
use cargo_metadata::Message as CargoMessage;
use once_cell::sync::Lazy;
use proc_macro_error::{abort, abort_call_site, Diagnostic, Level};
use quote::{format_ident, quote, ToTokens};
use serde::de::{self, Deserializer, Unexpected, Visitor};
use serde::Deserialize;
use syn::parse_macro_input;
use syn::{Ident, Item, ItemMacro, ItemMod, MacroDelimiter, Path as RustPath, Visibility};

static TEMP_DIR: Lazy<PathBuf> = Lazy::new(env::temp_dir);
static CRATES_DIR: Lazy<PathBuf> = Lazy::new(|| TEMP_DIR.join("inline-proc-crates"));

pub(super) fn inline_proc(input: TokenStream1) -> TokenStream1 {
    let (mod_name, metadata, content) = parse_mod(parse_macro_input!(input));

    let lib_rs = TokenString::from_tokens(generate_lib_rs(&metadata, &mod_name, content));
    let cargo_toml = generate_cargo_toml(&metadata);

    let crate_root = CRATES_DIR.join(format!("{}-{}", CrateIdentifier, mod_name));
    fs::create_dir_all(&crate_root)
        .unwrap_or_else(|e| abort_call_site!("Failed to create crate root: {}", e));

    let cargo_toml_path = crate_root.join("Cargo.toml");
    let lib_rs_path = crate_root.join("lib.rs");

    fs::write(&cargo_toml_path, &cargo_toml)
        .unwrap_or_else(|e| abort_call_site!("Failed to write Cargo.toml: {}", e));
    fs::write(&lib_rs_path, &lib_rs.tokens)
        .unwrap_or_else(|e| abort_call_site!("Failed to write lib.rs: {}", e));

    let mut cargo = Command::new(&metadata.cargo)
        .arg(if metadata.clippy { "clippy" } else { "check" })
        .arg("--manifest-path")
        .arg(&cargo_toml_path)
        .arg("--message-format=json")
        .arg("--color")
        .arg(if metadata.color { "always" } else { "never" })
        // If running clippy on the outside and clippy inside here Rustup can terminate our
        // process because it thinks there is recursion.
        // Removing this env var prevents this.
        .env_remove("RUST_RECURSION_COUNT")
        .stdout(Stdio::piped())
        .spawn()
        .unwrap_or_else(|e| abort_call_site!("Failed to launch Cargo: {}", e));

    CargoMessage::parse_stream(BufReader::new(cargo.stdout.as_mut().unwrap()))
        .filter_map(|message| {
            message
                .map(|message| match message {
                    CargoMessage::CompilerMessage(message) => Some(message.message),
                    _ => None,
                })
                .transpose()
        })
        .map(|message| match message {
            Ok(message) => cargo_diagnostic_to_diagnostic(message, &lib_rs),
            Err(e) => Diagnostic::new(Level::Error, format!("Failed to read Cargo stdout: {}", e)),
        })
        .for_each(|diagnostic| diagnostic.emit());

    let cargo_exit_code = cargo
        .wait()
        .unwrap_or_else(|e| abort_call_site!("Failed to wait on Cargo check: {}", e));

    proc_macro_error::abort_if_dirty();
    if !cargo_exit_code.success() {
        // An error with Cargo, not rustc
        abort_call_site!("Cargo build failed.");
    }

    Command::new("cargo")
        .arg("build")
        .arg("--manifest-path")
        .arg(&cargo_toml_path)
        // See above `env_remove`
        .env_remove("RUST_RECURSION_COUNT")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .unwrap_or_else(|e| abort_call_site!("Failed to wait on Cargo build: {}", e));

    let mut dylib_path = crate_root.join("target");
    dylib_path.push("debug");
    dylib_path.push(libloading::library_filename("inline_proc_macro"));

    let dylib_path = dylib_path
        .into_os_string()
        .into_string()
        .unwrap_or_else(|path| {
            abort_call_site!(
                "Failed to convert path {} to string",
                PathBuf::from(path).display()
            )
        });

    generate_user_macros(&metadata, &dylib_path).into()
}

struct CrateIdentifier;
impl Display for CrateIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let name = env::var("CARGO_PKG_NAME").unwrap();
        let major_version: u64 = env::var("CARGO_PKG_VERSION_MAJOR")
            .unwrap()
            .parse()
            .unwrap();
        write!(f, "{}-{}", name, major_version)?;
        if major_version == 0 {
            write!(
                f,
                ".{}",
                env::var("CARGO_PKG_VERSION_MINOR")
                    .unwrap()
                    .parse::<u64>()
                    .unwrap()
            )?;
        }
        Ok(())
    }
}

/// Parse the input module. Returns the module name, metadata, and content tokens.
///
// If no features are enabled then it will produce unnecessary warnings
#[cfg_attr(
    not(any(feature = "json", feature = "ron")),
    allow(unreachable_code, unused_variables, clippy::match_single_binding)
)]
fn parse_mod(module: ItemMod) -> (String, Metadata, TokenStream) {
    if !matches!(module.vis, Visibility::Inherited) {
        abort!(
            module.vis,
            "Inline proc module must not have a visibility modifier"
        );
    }

    let (braces, mut module_content) = match module.content {
        Some(content) => content,
        None => abort!(module.semi.unwrap(), "Module must contain content"),
    };

    if module_content.is_empty() {
        abort!(braces.span, "Missing metadata information");
    }

    let (metadata_format, metadata_source) = match module_content.remove(0) {
        Item::Macro(ItemMacro {
            ident: None, mac, ..
        }) if mac.path.segments.first().map_or(false, |seg| {
            seg.ident == "metadata" && seg.arguments.is_empty()
        }) =>
        {
            if mac.path.segments.len() > 2 {
                abort!(mac.path, "Expected two segments");
            }
            let mut segments = mac.path.segments.into_iter();
            let metadata_segment = segments.next().unwrap();
            let format = match segments.next() {
                Some(segment) if segment.arguments.is_empty() => segment.ident,
                Some(segment) => abort!(segment.arguments, "Extra arguments in metadata format"),
                None => abort!(metadata_segment, "Missing metadata format"),
            };

            let (group_span, delimiter) = match mac.delimiter {
                MacroDelimiter::Paren(paren) => (paren.span, Delimiter::Parenthesis),
                MacroDelimiter::Brace(brace) => (brace.span, Delimiter::Brace),
                MacroDelimiter::Bracket(bracket) => (bracket.span, Delimiter::Bracket),
            };
            let mut group = Group::new(delimiter, mac.tokens);
            group.set_span(group_span);

            (format, TokenString::from_token(group))
        }
        _ => abort!(module.mod_token, "Missing metadata information"),
    };

    eprintln!("SOURCE: {}", metadata_source.tokens);

    let metadata: Metadata = match metadata_format {
        #[cfg(feature = "json")]
        format if format == "json" => serde_json::from_str(&metadata_source.tokens)
            .unwrap_or_else(|e| abort!(metadata_source.char_spans[e.column() - 1], e)),
        #[cfg(feature = "ron")]
        format if format == "ron" => ron::from_str(&metadata_source.tokens)
            .unwrap_or_else(|e| abort!(metadata_source.byte_spans[e.position.col], e)),
        format => Diagnostic::spanned(
            format.span(),
            Level::Error,
            format!("Unknown format {}", format),
        )
        .help({
            const SUPPORTED_FORMATS: &[&str] = &[
               #[cfg(feature = "json")]
               "json",
               #[cfg(feature = "ron")]
               "ron",
            ];
            if SUPPORTED_FORMATS.is_empty() {
                "There are no supported formats. Try enabling the `json` or `ron` features of this crate.".to_owned()
            } else {
                format!("Supported formats are: {}", SUPPORTED_FORMATS.join(", "))
            }
        })
        .abort(),
    };

    #[allow(unreachable_code)]
    let content = {
        let mut content = TokenStream::new();
        for item in module_content {
            item.to_tokens(&mut content);
        }
        content
    };

    (module.ident.to_string(), metadata, content)
}

/// Metadata for an inline proc macro.
#[derive(Deserialize)]
struct Metadata {
    #[serde(default = "default_cargo")]
    cargo: PathBuf,
    #[serde(default = "return_true")]
    color: bool,
    #[serde(default)]
    clippy: bool,
    #[serde(default = "default_edition")]
    edition: String,
    dependencies: cargo_toml::DepsSet,
    #[serde(default = "default_inline_proc_path")]
    inline_proc_path: DeserializePath,
    exports: Exports,
}

fn default_cargo() -> PathBuf {
    PathBuf::from(env::var_os("CARGO").unwrap_or_else(|| "cargo".into()))
}
fn default_edition() -> String {
    "2015".to_owned()
}
fn return_true() -> bool {
    true
}
fn default_inline_proc_path() -> DeserializePath {
    DeserializePath(syn::parse2(quote!(::inline_proc)).unwrap())
}

#[derive(Default, Deserialize)]
#[serde(default)]
struct Exports {
    bang_macros: HashMap<DeserializeIdent, Macro>,
    derives: HashMap<DeserializeIdent, Macro>,
    attributes: HashMap<DeserializeIdent, Macro>,
}

impl Exports {
    fn all_macros(&self) -> impl Iterator<Item = (&Ident, &Macro, &'static str)> {
        let add_macro_type = |macro_type| move |(name, mac)| (name, mac, macro_type);

        self.bang_macros
            .iter()
            .map(add_macro_type("bang"))
            .chain(self.derives.iter().map(add_macro_type("derive")))
            .chain(self.attributes.iter().map(add_macro_type("attribute")))
            .map(|(DeserializeIdent(ident), mac, macro_type)| (ident, mac, macro_type))
    }
}

#[derive(Deserialize)]
#[serde(from = "MacroOptions")]
struct Macro {
    function: DeserializePath,
    export: bool,
}

/// Support both `{ function = "function_name", export = true }` and shorthand `"function_name"`.
#[derive(Deserialize)]
#[serde(untagged)]
enum MacroOptions {
    Function(DeserializePath),
    Full {
        function: DeserializePath,
        export: bool,
    },
}

impl From<MacroOptions> for Macro {
    fn from(options: MacroOptions) -> Self {
        match options {
            MacroOptions::Function(function) => Self {
                function,
                export: false,
            },
            MacroOptions::Full { function, export } => Self { function, export },
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
struct DeserializeIdent(Ident);
impl<'de> Deserialize<'de> for DeserializeIdent {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct IdentVisitor;
        impl<'de> Visitor<'de> for IdentVisitor {
            type Value = Ident;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str("an identifier")
            }
            fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
                syn::parse_str(v).map_err(|_| E::invalid_value(Unexpected::Str(v), &self))
            }
        }
        deserializer.deserialize_str(IdentVisitor).map(Self)
    }
}

struct DeserializePath(RustPath);
impl<'de> Deserialize<'de> for DeserializePath {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct PathVisitor;
        impl<'de> Visitor<'de> for PathVisitor {
            type Value = RustPath;
            fn expecting(&self, f: &mut Formatter) -> fmt::Result {
                f.write_str("a Rust path")
            }
            fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
                syn::parse_str(v).map_err(|_| E::invalid_value(Unexpected::Str(v), &self))
            }
        }
        deserializer.deserialize_str(PathVisitor).map(Self)
    }
}

/// A string made from writing out tokens.
#[derive(Default)]
struct TokenString {
    /// The span for each byte of the string.
    byte_spans: Vec<Span>,
    /// The span for each char of the string.
    char_spans: Vec<Span>,
    /// The string itself.
    tokens: String,
    /// Whether the previous token was a punctuation token and joint.
    joint: bool,
}

impl TokenString {
    fn from_tokens(tokens: impl ToTokens) -> Self {
        let mut this = Self::default();
        this.push_tokens(tokens);
        this
    }
    fn from_token(token: impl Into<TokenTree>) -> Self {
        let mut this = Self::default();
        this.push_token(token.into());
        this
    }
    fn push(&mut self, item: impl Display, span: Span) {
        let old_len = self.tokens.len();
        write!(self.tokens, "{}", item).unwrap();
        let written = &self.tokens[old_len..];
        self.byte_spans.extend(written.bytes().map(|_| span));
        self.char_spans.extend(written.chars().map(|_| span));
    }
    fn extend_prev(&mut self, item: impl Display) {
        self.push(item, *self.byte_spans.last().unwrap());
    }

    fn push_tokens(&mut self, tokens: impl ToTokens) {
        for token in tokens.into_token_stream() {
            self.push_token(token);
        }
    }
    fn push_token(&mut self, token: TokenTree) {
        if !self.joint && !self.tokens.is_empty() {
            self.extend_prev(" ");
        }

        match &token {
            TokenTree::Group(group) => {
                let (open, close) = match group.delimiter() {
                    Delimiter::Parenthesis => ("(", ")"),
                    Delimiter::Brace => ("{", "}"),
                    Delimiter::Bracket => ("[", "]"),
                    Delimiter::None => ("", ""),
                };

                self.push(open, group.span_open());
                self.push_tokens(group.stream());
                self.push(close, group.span_close());
            }
            TokenTree::Literal(lit) => {
                let old_len = self.tokens.len();
                write!(self.tokens, "{}", lit).unwrap();
                let written = &self.tokens[old_len..];
                let fallback_span = lit.span();
                self.byte_spans.extend(
                    (0..written.len()).map(|i| lit.subspan(i..=i).unwrap_or(fallback_span)),
                );
                self.char_spans.extend(
                    written
                        .char_indices()
                        .map(|(i, c)| lit.subspan(i..i + c.len_utf8()).unwrap_or(fallback_span)),
                );
            }
            _ => {
                self.push(&token, token.span());
            }
        }

        self.joint = matches!(token, TokenTree::Punct(p) if p.spacing() == Spacing::Joint);
    }
}

fn generate_cargo_toml(metadata: &Metadata) -> String {
    #[derive(serde::Serialize)]
    struct DependenciesWrapper<'a> {
        dependencies: &'a cargo_toml::DepsSet,
    }

    let dependencies = toml::to_string(&DependenciesWrapper {
        dependencies: &metadata.dependencies,
    })
    .unwrap();

    format!(
        "\
package={{name='inline-proc-macro',version='0.0.0',edition='{edition}'}}
lib={{crate-type=['dylib'],path='lib.rs'}}
{dependencies}\
        ",
        edition = metadata.edition,
        dependencies = dependencies,
    )
}

fn generate_lib_rs(metadata: &Metadata, mod_name: &str, code: TokenStream) -> TokenStream {
    let mod_name = Ident::new(mod_name, Span::call_site());

    let mut lib_rs = quote!(
        use proc_macro::TokenStream;
        extern crate proc_macro;
    );

    for (name, mac) in &metadata.exports.bang_macros {
        let function = &mac.function.0;
        let name = format_ident!("bang_{}", name.0);
        lib_rs.extend(quote! {
            #[no_mangle]
            pub fn #name(input: TokenStream) -> TokenStream {
                #mod_name::#function(input)
            }
        });
    }
    for (name, mac) in &metadata.exports.derives {
        let function = &mac.function.0;
        let name = format_ident!("derive_{}", name.0);
        lib_rs.extend(quote! {
            #[no_mangle]
            pub fn #name(item: TokenStream) -> TokenStream {
                #mod_name::#function(item)
            }
        });
    }
    for (name, mac) in &metadata.exports.attributes {
        let function = &mac.function.0;
        let name = format_ident!("attribute_{}", name.0);
        lib_rs.extend(quote! {
            #[no_mangle]
            pub fn #name(attr: TokenStream, item: TokenStream) -> TokenStream {
                #mod_name::#function(attr, item)
            }
        });
    }

    lib_rs.extend(quote! {
        mod #mod_name {
            #code
        }
    });

    lib_rs
}

fn cargo_diagnostic_to_diagnostic(cargo: CargoDiagnostic, source: &TokenString) -> Diagnostic {
    let mut diagnostic = Diagnostic::spanned(
        cargo_spans_to_span(&cargo.spans, source),
        match cargo.level {
            CargoLevel::Ice | CargoLevel::Error => Level::Error,
            _ => Level::Warning,
        },
        cargo.message,
    );

    for child in cargo.children {
        diagnostic = match child.level {
            CargoLevel::Help => Diagnostic::span_help,
            CargoLevel::Note => Diagnostic::span_note,
            _ => Diagnostic::span_error,
        }(
            diagnostic,
            cargo_spans_to_span(&child.spans, source),
            child.message,
        );
    }

    diagnostic
}

fn cargo_spans_to_span(spans: &[CargoSpan], source: &TokenString) -> Span {
    spans
        .iter()
        .find(|span| span.is_primary)
        .or_else(|| spans.first())
        .map(|span| {
            let start = source.byte_spans[span.byte_start as usize];
            start
                .join(source.byte_spans[span.byte_end as usize])
                .unwrap_or(start)
        })
        .unwrap_or_else(Span::call_site)
}

fn generate_user_macros(metadata: &Metadata, dylib_path: &str) -> TokenStream {
    metadata
        .exports
        .all_macros()
        .map(|(name, mac, macro_type)| {
            let macro_type = Ident::new(macro_type, Span::call_site());

            if mac.export {
                let name_inner = format_ident!("{}_inner", name);
                quote! {
                    #[macro_export]
                    #[doc(hidden)]
                    macro_rules! #name_inner {
                        ($inline_proc:path, $($tokens:tt)*) => {
                            $inline_proc!(#dylib_path #name #macro_type $($tokens)*);
                        }
                    }
                }
            } else {
                let inline_proc_path = &metadata.inline_proc_path.0;
                quote! {
                    macro_rules! #name {
                        ($($tokens:tt)*) => {
                            #inline_proc_path::invoke_inline_macro!(#dylib_path #name #macro_type $($tokens)*);
                        }
                    }
                }
            }
        })
        .collect()
}
