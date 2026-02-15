//! Recursive-descent parser for D2 diagram language.
//!
//! Parses D2 source text into an AST (`D2Map`). The parser handles
//! all v1 syntax features: shape declarations, connections, containers,
//! styles, comments, semicolons, and quoted strings.
//!
//! The parser collects multiple errors and continues, returning a
//! partial AST alongside diagnostics. This matches D2's behavior
//! and supports future LSP integration.

use crate::ast::*;
use crate::graph::{Position, Range};

/// A single parse diagnostic (position + message).
#[derive(Debug, thiserror::Error)]
#[error("{range:?}: {message}")]
pub struct ParseError {
    pub message: String,
    pub range: Range,
}

/// Result of parsing: always contains the AST, plus any errors.
pub struct ParseResult {
    pub ast: D2Map,
    pub errors: Vec<ParseError>,
}

/// Parse D2 source text into an AST.
pub fn parse(source: &str) -> ParseResult {
    let mut parser = Parser::new(source);
    let map = parser.parse_map(true);
    ParseResult {
        ast: map,
        errors: parser.errors,
    }
}

// ---------------------------------------------------------------------------
// Parser internals
// ---------------------------------------------------------------------------

struct Parser<'a> {
    /// Source text (retained for future error reporting with source spans).
    #[allow(dead_code)]
    source: &'a str,
    /// Character iterator with byte position tracking.
    chars: Vec<(usize, char)>,
    /// Current position in chars vec.
    pos: usize,
    /// Collected errors.
    errors: Vec<ParseError>,
    /// Current line (0-indexed).
    line: usize,
    /// Current column (0-indexed, byte offset).
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        let chars: Vec<(usize, char)> = source.char_indices().collect();
        Parser {
            source,
            chars,
            pos: 0,
            errors: Vec::new(),
            line: 0,
            col: 0,
        }
    }

    // -- Utility methods --

    fn at_end(&self) -> bool {
        self.pos >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).map(|(_, c)| *c)
    }

    fn peek_ahead(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(&(_, ch)) = self.chars.get(self.pos) {
            self.pos += 1;
            if ch == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += ch.len_utf8();
            }
            Some(ch)
        } else {
            None
        }
    }

    fn current_position(&self) -> Position {
        Position {
            line: self.line,
            column: self.col,
        }
    }

    fn skip_whitespace_no_newline(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn emit_error(&mut self, message: &str, range: Range) {
        self.errors.push(ParseError {
            message: message.to_string(),
            range,
        });
    }

    /// Check if looking at an edge operator: ->, <-, --, <->
    fn looking_at_edge_op(&self) -> bool {
        match self.peek() {
            Some('-') => {
                matches!(self.peek_ahead(1), Some('-') | Some('>'))
            }
            Some('<') => {
                matches!(self.peek_ahead(1), Some('-'))
            }
            _ => false,
        }
    }

    /// Parse an edge operator. Returns (src_arrow, dst_arrow).
    fn parse_edge_op(&mut self) -> Option<(bool, bool)> {
        let c1 = self.peek()?;
        let c2 = self.peek_ahead(1)?;

        match (c1, c2) {
            ('-', '-') => {
                self.advance();
                self.advance();
                Some((false, false))
            }
            ('-', '>') => {
                self.advance();
                self.advance();
                Some((false, true))
            }
            ('<', '-') => {
                self.advance();
                self.advance();
                if self.peek() == Some('>') {
                    self.advance();
                    Some((true, true)) // <->
                } else {
                    Some((true, false)) // <-
                }
            }
            _ => None,
        }
    }

    // -- Core parsing methods --

    /// Parse a top-level or nested map.
    fn parse_map(&mut self, is_file_level: bool) -> D2Map {
        let start = self.current_position();
        let mut nodes = Vec::new();

        loop {
            self.skip_whitespace();

            if self.at_end() {
                break;
            }

            // End of nested map
            if !is_file_level && self.peek() == Some('}') {
                break;
            }

            // Comment
            if self.peek() == Some('#') {
                if let Some(comment) = self.parse_comment() {
                    nodes.push(MapNode::Comment(comment));
                }
                continue;
            }

            // Semicolon separator
            if self.peek() == Some(';') {
                self.advance();
                continue;
            }

            // Newline
            if self.peek() == Some('\n') {
                self.advance();
                continue;
            }

            // Key declaration
            if let Some(key) = self.parse_key() {
                nodes.push(MapNode::Key(key));
            } else {
                // Skip unrecognized character
                let pos = self.current_position();
                let ch = self.advance();
                if let Some(ch) = ch {
                    self.emit_error(
                        &format!("unexpected character: '{}'", ch),
                        Range {
                            start: pos,
                            end: self.current_position(),
                        },
                    );
                }
            }
        }

        let end = self.current_position();
        D2Map {
            nodes,
            range: Range { start, end },
        }
    }

    /// Parse a comment line (# ...).
    fn parse_comment(&mut self) -> Option<Comment> {
        let start = self.current_position();
        self.advance(); // skip '#'

        let mut value = String::new();
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            value.push(ch);
            self.advance();
        }

        Some(Comment {
            value: value.trim().to_string(),
            range: Range {
                start,
                end: self.current_position(),
            },
        })
    }

    /// Parse a key declaration, which may include edges.
    fn parse_key(&mut self) -> Option<Key> {
        let start = self.current_position();

        // Parse the initial key path
        let key = self.parse_key_path()?;

        self.skip_whitespace_no_newline();

        // Check for edge reference index: (a -> b)[0]
        let mut edge_index = None;
        let mut edge_key = None;

        // Check for edge chain
        let mut edges = Vec::new();
        while self.looking_at_edge_op() {
            let edge_start = self.current_position();
            let (src_arrow, dst_arrow) = self.parse_edge_op()?;
            self.skip_whitespace_no_newline();

            let dst = self.parse_key_path()?;
            let edge_end = self.current_position();

            let src = if edges.is_empty() {
                key.clone()
            } else {
                edges.last().map(|e: &Edge| e.dst.clone())?
            };

            edges.push(Edge {
                src,
                dst,
                src_arrow,
                dst_arrow,
                range: Range {
                    start: edge_start,
                    end: edge_end,
                },
            });

            self.skip_whitespace_no_newline();
        }

        // Check for edge reference: (x -> y)[0]
        if self.peek() == Some('[') {
            self.advance();
            let mut num_str = String::new();
            while let Some(ch) = self.peek() {
                if ch == ']' {
                    self.advance();
                    break;
                }
                if ch.is_ascii_digit() {
                    num_str.push(ch);
                    self.advance();
                } else {
                    break;
                }
            }
            edge_index = num_str.parse().ok();

            self.skip_whitespace_no_newline();

            // Check for edge key: (x -> y)[0].style.stroke
            if self.peek() == Some('.') {
                self.advance();
                edge_key = self.parse_key_path();
            }
        }

        // Check for value (after colon or opening brace)
        self.skip_whitespace_no_newline();

        let mut primary = None;
        let mut value = None;

        if self.peek() == Some(':') {
            self.advance();
            self.skip_whitespace_no_newline();

            // Check for map value after colon
            if self.peek() == Some('{') {
                value = Some(self.parse_map_value());
            } else if self.peek() == Some('[') {
                value = Some(self.parse_array_value());
            } else {
                // Parse scalar value (label)
                let scalar = self.parse_scalar_value();
                if let Some(s) = scalar {
                    primary = Some(s.clone());

                    self.skip_whitespace_no_newline();

                    // Check for map after scalar label: `group: "My Group" { ... }`
                    if self.peek() == Some('{') {
                        value = Some(self.parse_map_value());
                    }
                }
            }
        } else if self.peek() == Some('{') {
            value = Some(self.parse_map_value());
        }

        let end = self.current_position();

        Some(Key {
            key,
            edges,
            edge_index,
            edge_key,
            primary,
            value,
            range: Range { start, end },
        })
    }

    /// Parse a dotted key path: "server.style.fill"
    fn parse_key_path(&mut self) -> Option<KeyPath> {
        let mut segments = Vec::new();

        let first = self.parse_key_segment()?;
        segments.push(first);

        // Continue with dot-separated segments
        while self.peek() == Some('.') {
            self.advance();
            if let Some(seg) = self.parse_key_segment() {
                segments.push(seg);
            } else {
                break;
            }
        }

        Some(KeyPath { segments })
    }

    /// Parse a single key segment (quoted or unquoted string).
    fn parse_key_segment(&mut self) -> Option<StringValue> {
        let start = self.current_position();

        match self.peek()? {
            '"' => self.parse_double_quoted_string(),
            '\'' => self.parse_single_quoted_string(),
            '(' => {
                // Edge reference: (a -> b)[0]
                self.advance(); // skip '('
                self.skip_whitespace_no_newline();
                // For now, skip the parenthesized expression and collect as unquoted
                let mut content = String::from("(");
                let mut depth = 1;
                while let Some(ch) = self.peek() {
                    if ch == '(' {
                        depth += 1;
                    }
                    if ch == ')' {
                        depth -= 1;
                        if depth == 0 {
                            content.push(ch);
                            self.advance();
                            break;
                        }
                    }
                    content.push(ch);
                    self.advance();
                }
                Some(StringValue {
                    value: content.clone(),
                    raw: content,
                    range: Range {
                        start,
                        end: self.current_position(),
                    },
                })
            }
            _ => self.parse_unquoted_key_string(),
        }
    }

    /// Parse a double-quoted string.
    fn parse_double_quoted_string(&mut self) -> Option<StringValue> {
        let start = self.current_position();
        self.advance(); // skip opening quote

        let mut value = String::new();
        let mut raw = String::from("\"");

        loop {
            match self.peek() {
                None => {
                    self.emit_error(
                        "unterminated double-quoted string",
                        Range {
                            start,
                            end: self.current_position(),
                        },
                    );
                    break;
                }
                Some('\\') => {
                    raw.push('\\');
                    self.advance();
                    if let Some(esc) = self.peek() {
                        raw.push(esc);
                        self.advance();
                        match esc {
                            'n' => value.push('\n'),
                            't' => value.push('\t'),
                            '\\' => value.push('\\'),
                            '"' => value.push('"'),
                            _ => {
                                value.push('\\');
                                value.push(esc);
                            }
                        }
                    }
                }
                Some('"') => {
                    raw.push('"');
                    self.advance();
                    break;
                }
                Some(ch) => {
                    raw.push(ch);
                    value.push(ch);
                    self.advance();
                }
            }
        }

        Some(StringValue {
            value,
            raw,
            range: Range {
                start,
                end: self.current_position(),
            },
        })
    }

    /// Parse a single-quoted string (no escaping).
    fn parse_single_quoted_string(&mut self) -> Option<StringValue> {
        let start = self.current_position();
        self.advance(); // skip opening quote

        let mut value = String::new();
        let mut raw = String::from("'");

        loop {
            match self.peek() {
                None => {
                    self.emit_error(
                        "unterminated single-quoted string",
                        Range {
                            start,
                            end: self.current_position(),
                        },
                    );
                    break;
                }
                Some('\'') => {
                    raw.push('\'');
                    self.advance();
                    break;
                }
                Some(ch) => {
                    raw.push(ch);
                    value.push(ch);
                    self.advance();
                }
            }
        }

        Some(StringValue {
            value,
            raw,
            range: Range {
                start,
                end: self.current_position(),
            },
        })
    }

    /// Parse an unquoted key string.
    /// D2 allows spaces in unquoted strings. The string terminates at
    /// reserved character sequences: ->, <-, --, <->, :, ., {, }, ;, #, \n
    fn parse_unquoted_key_string(&mut self) -> Option<StringValue> {
        let start = self.current_position();
        let mut value = String::new();

        loop {
            if self.at_end() {
                break;
            }

            let ch = self.peek().unwrap();

            // Terminators
            match ch {
                ':' | '{' | '}' | ';' | '#' | '\n' | '.' | '[' | ']' | '(' | ')' => break,
                '-' => {
                    // Check for edge operators: --, ->
                    if matches!(self.peek_ahead(1), Some('-') | Some('>')) {
                        break;
                    }
                    value.push(ch);
                    self.advance();
                }
                '<' => {
                    // Check for edge operator: <-
                    if self.peek_ahead(1) == Some('-') {
                        break;
                    }
                    value.push(ch);
                    self.advance();
                }
                _ => {
                    value.push(ch);
                    self.advance();
                }
            }
        }

        // Trim trailing whitespace
        let trimmed = value.trim_end().to_string();
        if trimmed.is_empty() {
            return None;
        }

        let raw = trimmed.clone();
        Some(StringValue {
            value: trimmed,
            raw,
            range: Range {
                start,
                end: self.current_position(),
            },
        })
    }

    /// Parse a scalar value (after colon).
    fn parse_scalar_value(&mut self) -> Option<ScalarValue> {
        match self.peek()? {
            '"' => {
                let sv = self.parse_double_quoted_string()?;
                Some(ScalarValue::DoubleQuoted(sv.value))
            }
            '\'' => {
                let sv = self.parse_single_quoted_string()?;
                Some(ScalarValue::SingleQuoted(sv.value))
            }
            _ => {
                // Unquoted value: read until newline, semicolon, #, {, }, ]
                let mut value = String::new();
                loop {
                    if self.at_end() {
                        break;
                    }
                    let ch = self.peek().unwrap();
                    match ch {
                        '\n' | ';' | '#' | '{' | '}' | ']' => break,
                        _ => {
                            value.push(ch);
                            self.advance();
                        }
                    }
                }

                let trimmed = value.trim().to_string();
                if trimmed.is_empty() {
                    return None;
                }

                // Check for boolean
                match trimmed.as_str() {
                    "true" => return Some(ScalarValue::Boolean(true)),
                    "false" => return Some(ScalarValue::Boolean(false)),
                    _ => {}
                }

                // Check for number
                if let Ok(n) = trimmed.parse::<f64>() {
                    if trimmed
                        .chars()
                        .all(|c| c.is_ascii_digit() || c == '.' || c == '-')
                    {
                        return Some(ScalarValue::Number(n));
                    }
                }

                Some(ScalarValue::Unquoted(trimmed))
            }
        }
    }

    /// Parse a map value: { ... }
    fn parse_map_value(&mut self) -> Value {
        self.advance(); // skip '{'
        let map = self.parse_map(false);
        self.skip_whitespace();
        if self.peek() == Some('}') {
            self.advance();
        } else {
            let pos = self.current_position();
            self.emit_error(
                "expected closing '}'",
                Range {
                    start: pos,
                    end: pos,
                },
            );
        }
        Value::Map(map)
    }

    /// Parse an array value: [ ... ]
    fn parse_array_value(&mut self) -> Value {
        self.advance(); // skip '['
        let mut items = Vec::new();

        loop {
            self.skip_whitespace();

            if self.at_end() {
                break;
            }

            if self.peek() == Some(']') {
                self.advance();
                break;
            }

            if self.peek() == Some(';') {
                self.advance();
                continue;
            }

            if let Some(scalar) = self.parse_scalar_value() {
                items.push(Value::Scalar(scalar));
            } else {
                break;
            }
        }

        Value::Array(items)
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_ok(source: &str) -> D2Map {
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
        result.ast
    }

    #[test]
    fn test_empty_source() {
        let ast = parse_ok("");
        assert!(ast.nodes.is_empty());
    }

    #[test]
    fn test_simple_shape() {
        let ast = parse_ok("server");
        assert_eq!(ast.nodes.len(), 1);
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.to_string(), "server");
            assert!(key.edges.is_empty());
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_shape_with_label() {
        let ast = parse_ok("server: \"My Server\"");
        assert_eq!(ast.nodes.len(), 1);
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.to_string(), "server");
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::DoubleQuoted(s)) if s == "My Server"
            ));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_simple_connection() {
        let ast = parse_ok("a -> b");
        assert_eq!(ast.nodes.len(), 1);
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.edges.len(), 1);
            assert_eq!(key.edges[0].src.to_string(), "a");
            assert_eq!(key.edges[0].dst.to_string(), "b");
            assert!(!key.edges[0].src_arrow);
            assert!(key.edges[0].dst_arrow);
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_bidirectional_connection() {
        let ast = parse_ok("a <-> b");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(key.edges[0].src_arrow);
            assert!(key.edges[0].dst_arrow);
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_reverse_connection() {
        let ast = parse_ok("a <- b");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(key.edges[0].src_arrow);
            assert!(!key.edges[0].dst_arrow);
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_undirected_connection() {
        let ast = parse_ok("a -- b");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(!key.edges[0].src_arrow);
            assert!(!key.edges[0].dst_arrow);
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_connection_chain() {
        let ast = parse_ok("a -> b -> c -> d");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.edges.len(), 3);
            assert_eq!(key.edges[0].src.to_string(), "a");
            assert_eq!(key.edges[0].dst.to_string(), "b");
            assert_eq!(key.edges[1].src.to_string(), "b");
            assert_eq!(key.edges[1].dst.to_string(), "c");
            assert_eq!(key.edges[2].src.to_string(), "c");
            assert_eq!(key.edges[2].dst.to_string(), "d");
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_connection_with_label() {
        let ast = parse_ok("a -> b: sends data");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.edges.len(), 1);
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::Unquoted(s)) if s == "sends data"
            ));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_comment() {
        let ast = parse_ok("# this is a comment\nserver");
        assert_eq!(ast.nodes.len(), 2);
        assert!(matches!(&ast.nodes[0], MapNode::Comment(_)));
        assert!(matches!(&ast.nodes[1], MapNode::Key(_)));
    }

    #[test]
    fn test_container() {
        let ast = parse_ok("group: {\n  a\n  b\n}");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.to_string(), "group");
            assert!(matches!(&key.value, Some(Value::Map(m)) if m.nodes.len() == 2));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_container_with_label() {
        let ast = parse_ok("group: \"My Group\" {\n  a\n  b\n}");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.to_string(), "group");
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::DoubleQuoted(s)) if s == "My Group"
            ));
            assert!(matches!(&key.value, Some(Value::Map(_))));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_dotted_key() {
        let ast = parse_ok("server.shape: circle");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.segments.len(), 2);
            assert_eq!(key.key.segments[0].value, "server");
            assert_eq!(key.key.segments[1].value, "shape");
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_style_property() {
        let ast = parse_ok("x.style.fill: \"#ff0000\"");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.segments.len(), 3);
            assert_eq!(key.key.segments[0].value, "x");
            assert_eq!(key.key.segments[1].value, "style");
            assert_eq!(key.key.segments[2].value, "fill");
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_semicolons() {
        let ast = parse_ok("a; b; c");
        let key_count = ast
            .nodes
            .iter()
            .filter(|n| matches!(n, MapNode::Key(_)))
            .count();
        assert_eq!(key_count, 3);
    }

    #[test]
    fn test_multiple_statements() {
        let ast = parse_ok("a\nb\nc -> d");
        let key_count = ast
            .nodes
            .iter()
            .filter(|n| matches!(n, MapNode::Key(_)))
            .count();
        assert_eq!(key_count, 3);
    }

    #[test]
    fn test_unquoted_string_with_spaces() {
        let ast = parse_ok("My Server -> Your Server");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.edges[0].src.to_string(), "My Server");
            assert_eq!(key.edges[0].dst.to_string(), "Your Server");
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_nested_containers() {
        let ast = parse_ok("outer: {\n  inner: {\n    node\n  }\n}");
        if let MapNode::Key(key) = &ast.nodes[0] {
            if let Some(Value::Map(outer_map)) = &key.value {
                assert_eq!(outer_map.nodes.len(), 1);
                if let MapNode::Key(inner_key) = &outer_map.nodes[0] {
                    assert_eq!(inner_key.key.to_string(), "inner");
                    assert!(matches!(&inner_key.value, Some(Value::Map(_))));
                }
            } else {
                panic!("expected map value");
            }
        }
    }

    #[test]
    fn test_direction_keyword() {
        let ast = parse_ok("direction: right");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.to_string(), "direction");
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::Unquoted(s)) if s == "right"
            ));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_boolean_value() {
        let ast = parse_ok("x.style.bold: true");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(matches!(&key.primary, Some(ScalarValue::Boolean(true))));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_number_value() {
        let ast = parse_ok("x.style.stroke-width: 3");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(matches!(&key.primary, Some(ScalarValue::Number(n)) if (*n - 3.0).abs() < 0.001));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_single_quoted_string() {
        let ast = parse_ok("x: 'hello world'");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::SingleQuoted(s)) if s == "hello world"
            ));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_escaped_double_quoted() {
        let ast = parse_ok(r#"x: "hello \"world\"""#);
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert!(matches!(
                &key.primary,
                Some(ScalarValue::DoubleQuoted(s)) if s == r#"hello "world""#
            ));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_dotted_container_syntax() {
        // group.a creates a container "group" with child "a"
        let ast = parse_ok("group.a");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.segments.len(), 2);
            assert_eq!(key.key.segments[0].value, "group");
            assert_eq!(key.key.segments[1].value, "a");
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_width_height() {
        let ast = parse_ok("x.width: 200");
        if let MapNode::Key(key) = &ast.nodes[0] {
            assert_eq!(key.key.segments[1].value, "width");
            assert!(matches!(&key.primary, Some(ScalarValue::Number(n)) if (*n - 200.0).abs() < 0.001));
        } else {
            panic!("expected Key node");
        }
    }

    #[test]
    fn test_complex_diagram() {
        let source = r#"
direction: right

# Services
api: "API Gateway" {
  shape: hexagon
}
db: "PostgreSQL" {
  shape: cylinder
}
cache: "Redis Cache" {
  shape: cylinder
}

api -> db: queries
api -> cache: reads
cache -> db: cache miss
"#;
        let ast = parse_ok(source);
        // Should have: direction, comment, api, db, cache, 3 edges
        let key_count = ast
            .nodes
            .iter()
            .filter(|n| matches!(n, MapNode::Key(_)))
            .count();
        assert!(key_count >= 6, "expected at least 6 key nodes, got {}", key_count);
    }

    // -----------------------------------------------------------------------
    // Error recovery tests
    // -----------------------------------------------------------------------

    /// Parse input and return the full ParseResult (AST + errors).
    fn parse_with_errors(source: &str) -> ParseResult {
        parse(source)
    }

    #[test]
    fn test_stray_closing_brace() {
        // A stray `}` at the file level is not valid syntax. The parser
        // should emit an error for the unexpected `}` but still parse the
        // surrounding declarations `a` and `b`.
        let result = parse_with_errors("a\n}\nb");
        assert!(
            !result.errors.is_empty(),
            "expected errors for stray `}}`, got none"
        );
        // Both `a` and `b` should still be parsed as key nodes.
        let key_count = result
            .ast
            .nodes
            .iter()
            .filter(|n| matches!(n, MapNode::Key(_)))
            .count();
        assert!(
            key_count >= 2,
            "expected at least 2 key nodes after recovery, got {}",
            key_count
        );
    }

    #[test]
    fn test_unterminated_map() {
        // Opening a map with `{` but never closing it should produce an
        // error about the missing `}`.
        let result = parse_with_errors("a: { b");
        assert!(
            !result.errors.is_empty(),
            "expected error for missing `}}`, got none"
        );
        let has_brace_error = result
            .errors
            .iter()
            .any(|e| e.message.contains('}'));
        assert!(
            has_brace_error,
            "expected an error mentioning `}}`, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_edge_missing_destination() {
        // `a -> ` has an edge operator but no destination. The parser
        // should handle this gracefully without panicking.
        let result = parse_with_errors("a -> ");
        // The parser consumed the edge operator but failed to find a
        // destination key. This may result in the entire key being
        // dropped (no nodes) or an error being emitted. Either outcome
        // is acceptable as long as the parser doesn't panic.
        let _ = result.ast.nodes.len();
        let _ = result.errors.len();
    }

    #[test]
    fn test_unterminated_string() {
        // A double-quoted string that is never closed should produce a
        // parse error rather than hanging or panicking.
        let result = parse_with_errors("a: \"unterminated");
        assert!(
            !result.errors.is_empty(),
            "expected error for unterminated string, got none"
        );
        let has_unterminated = result
            .errors
            .iter()
            .any(|e| e.message.contains("unterminated"));
        assert!(
            has_unterminated,
            "expected 'unterminated' in error message, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_multiple_errors_collected() {
        // Several syntax problems in one input. The parser should collect
        // all of them rather than bailing out after the first.
        //
        // Line 1: `}`       — stray closing brace at file level
        // Line 2: `a`       — valid key (should still parse)
        // Line 3: `}`       — another stray closing brace
        // Line 4: `b`       — valid key (should still parse)
        //
        // Each stray `}` is an independent error that the parser can
        // recover from by skipping the unrecognized character.
        let result = parse_with_errors("}\na\n}\nb");
        assert!(
            result.errors.len() >= 2,
            "expected at least 2 errors for multiple syntax problems, got {}: {:?}",
            result.errors.len(),
            result.errors,
        );
        // Despite multiple errors, the parser should still produce some
        // AST nodes — both `a` and `b` should survive recovery.
        let key_count = result
            .ast
            .nodes
            .iter()
            .filter(|n| matches!(n, MapNode::Key(_)))
            .count();
        assert!(
            key_count >= 2,
            "expected at least 2 key nodes after recovery, got {}",
            key_count,
        );
    }
}
