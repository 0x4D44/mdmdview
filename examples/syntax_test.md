# Syntax Highlighting Test

This file tests syntax highlighting and Unicode character rendering.

## Code Block Tests

### Python
```python
def hello_world():
    print("Hello, world!")
    return True
```

### JavaScript  
```javascript
function greet(name) {
    console.log(`Hello, ${name}!`);
    return `Welcome, ${name}`;
}
```

### Rust
```rust
fn main() {
    println!("Hello, world!");
    let arrow = "→";
    println!("Arrow: {}", arrow);
}
```

### SQL
```sql
SELECT name, email 
FROM users 
WHERE active = 1 
ORDER BY created_at DESC;
```

### JSON
```json
{
    "name": "test",
    "version": "1.0.0",
    "arrow": "→",
    "description": "Testing → arrows"
}
```

## Unicode Character Test

Regular text with arrows:
- Left arrow: ← should render as <-
- Right arrow: → should render as ->
- Up arrow: ↑ should render as ^
- Down arrow: ↓ should render as v

In lists:
- Process A → Process B → Process C
- Input ← System → Output
- Data flows: Input → Processing → Output

## Mixed Test

Code with arrows: `input → output` and regular text with arrows: data → processing.

---

*Testing syntax highlighting and Unicode character rendering*
