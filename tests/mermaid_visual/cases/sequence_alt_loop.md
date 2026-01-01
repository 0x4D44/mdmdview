# Mermaid Sequence Alt Loop

```mermaid
sequenceDiagram
    participant User
    participant API
    participant DB
    User->>API: Request
    alt cache hit
        API-->>User: 200 OK
    else cache miss
        API->>DB: Query
        loop retry
            DB-->>API: Result
        end
        API-->>User: 200 OK
    end
    Note over User,API: Happy path
```
