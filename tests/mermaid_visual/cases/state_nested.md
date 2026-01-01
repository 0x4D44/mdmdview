# Mermaid State Nested

```mermaid
stateDiagram-v2
    [*] --> Idle
    Idle --> Running: start
    state Running {
        [*] --> Init
        Init --> Active
        Active --> Paused: pause
        Paused --> Active: resume
    }
    Running --> Stopped: stop
    Stopped --> [*]
```
