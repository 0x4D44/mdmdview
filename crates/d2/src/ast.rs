//! AST node types for D2 diagram language.
//!
//! These types represent the parsed syntax tree of a D2 document.
//! The AST closely mirrors D2's Go AST, adapted for Rust idioms.
//! Key types:
//! - `D2Map`: top-level container of all declarations
//! - `MapNode`: individual declaration (key assignment or comment)
//! - `Key`: shape/edge/property declaration with optional value
//! - `Edge`: connection between two key paths
//! - `KeyPath`: dotted key path (e.g., "server.style.fill")

use crate::graph::Range;

/// Top-level map containing all declarations.
#[derive(Debug, Clone)]
pub struct D2Map {
    pub nodes: Vec<MapNode>,
    pub range: Range,
}

/// A node within a map.
#[derive(Debug, Clone)]
pub enum MapNode {
    Comment(Comment),
    Key(Key),
}

/// A key declaration (shape, edge, or property assignment).
#[derive(Debug, Clone)]
pub struct Key {
    /// Dotted path: "a.b.c"
    pub key: KeyPath,
    /// Connection chain (empty if this is just a shape/property declaration)
    pub edges: Vec<Edge>,
    /// D2 syntax index for edge references: `(a -> b)[0]`
    pub edge_index: Option<usize>,
    /// Property on an edge reference
    pub edge_key: Option<KeyPath>,
    /// Label value (the primary scalar after `:`)
    pub primary: Option<ScalarValue>,
    /// Map, array, or scalar value
    pub value: Option<Value>,
    pub range: Range,
}

/// A string value with source position tracking.
#[derive(Debug, Clone)]
pub struct StringValue {
    /// The resolved string value (after unquoting).
    pub value: String,
    /// Original source text (before unquoting).
    pub raw: String,
    pub range: Range,
}

/// A comment line.
#[derive(Debug, Clone)]
pub struct Comment {
    pub value: String,
    pub range: Range,
}

/// Dotted key path: "server.style.fill"
#[derive(Debug, Clone)]
pub struct KeyPath {
    pub segments: Vec<StringValue>,
}

impl KeyPath {
    /// Join segments into a dotted string.
    pub fn to_string(&self) -> String {
        self.segments
            .iter()
            .map(|s| s.value.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }
}

/// An edge between two keys.
#[derive(Debug, Clone)]
pub struct Edge {
    pub src: KeyPath,
    pub dst: KeyPath,
    /// `<-` (source has arrowhead)
    pub src_arrow: bool,
    /// `->` (destination has arrowhead)
    pub dst_arrow: bool,
    pub range: Range,
}

/// Values that can appear on the right side of `:`.
#[derive(Debug, Clone)]
pub enum Value {
    Scalar(ScalarValue),
    Map(D2Map),
    Array(Vec<Value>),
}

/// Scalar value types.
#[derive(Debug, Clone)]
pub enum ScalarValue {
    Unquoted(String),
    SingleQuoted(String),
    DoubleQuoted(String),
    Number(f64),
    Boolean(bool),
}

impl ScalarValue {
    /// Get the string representation of this scalar.
    pub fn as_str(&self) -> String {
        match self {
            ScalarValue::Unquoted(s)
            | ScalarValue::SingleQuoted(s)
            | ScalarValue::DoubleQuoted(s) => s.clone(),
            ScalarValue::Number(n) => n.to_string(),
            ScalarValue::Boolean(b) => b.to_string(),
        }
    }
}
