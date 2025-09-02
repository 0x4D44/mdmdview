# Repository Guidelines

## Project Structure & Modules
- `src/` — Rust sources: `main.rs` (entry), `app.rs` (UI/state), `markdown_renderer.rs` (parsing + rendering), `sample_files.rs` (embedded docs), `lib.rs` (exports).
- `examples/` — Sample `.md` files for manual checks.
- `Cargo.toml` / `Cargo.lock` — crate config; `build.rs` embeds `icon.ico` and Windows resources.
- Tests live inline under `#[cfg(test)]` modules in each file.

## Build, Test, and Dev Commands
- Build: `cargo build --release` — produces `target/release/mdmdview.exe`.
- Run: `cargo run -- [path\\to\\file.md]` — optional file arg.
- Test: `cargo test` or `cargo test -- --nocapture`.
- Lint: `cargo clippy --all-targets -- -D warnings`.
- Format: `cargo fmt --all`.
- CI: GitHub Actions enforces rustfmt/clippy and treats warnings as errors; if build and tests pass, it automatically runs `cargo build --release`.

## Coding Style & Naming
- Use `rustfmt` defaults (4‑space indent, max line width default).
- Naming: `snake_case` functions/modules, `CamelCase` types, `SCREAMING_SNAKE_CASE` consts.
- Errors: prefer `anyhow::Result<T>` for fallible paths; avoid `unwrap()`/`expect()` in non-test code.
- UI: keep egui code responsive; avoid blocking calls in `update()`; preserve keyboard shortcuts and fullscreen behavior.

## Testing Guidelines
- Place unit tests beside code in `mod tests` with clear, behavior‑oriented names (e.g., `test_zoom_functionality`).
- Focus tests on: renderer parsing, font/zoom behavior, file I/O via `tempfile`, and app state transitions (e.g., F11 flag, navigation requests).
- Run all tests locally via `cargo test`; no coverage gate, but add tests when changing parsing or input handling.

## Commit & Pull Requests
- Commits: small, imperative messages. Prefer Conventional Commits when possible, e.g.:
  - `feat(renderer): add language mapping for powershell`
  - `fix(app): prevent deadlock on F11 toggle`
- PRs must include: summary, rationale, screenshots/GIFs for UI changes, test plan (`cargo test`/manual steps), and linked issues. Update `README.md` when user‑visible behavior changes.

## Security & Configuration Tips
- No secrets or network calls; only local file reads and `webbrowser` for links.
- Validate paths; handle unreadable files gracefully.
- Windows resources are set in `build.rs`; keep `icon.ico` path stable when modifying packaging.
