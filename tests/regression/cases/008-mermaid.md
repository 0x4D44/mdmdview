# Mermaid

```mermaid
flowchart TD
    Start --> Check
    Check -->|Yes| Success
    Check -->|No| Retry
    Retry --> Check
```

```mermaid
sequenceDiagram
    participant A as Client
    participant B as Server
    A->>B: Request
    B-->>A: Response
```
