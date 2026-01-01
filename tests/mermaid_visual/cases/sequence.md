# Mermaid Sequence

```mermaid
sequenceDiagram
    participant Alice
    participant Bob
    Alice->>Bob: Hello
    Bob-->>Alice: Hi
    loop Retry
        Alice->>Bob: Ping
        Bob-->>Alice: Pong
    end
```
