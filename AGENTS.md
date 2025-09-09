# Repository Guidelines

## Project Structure & Module Organization
- `src/` — Rust sources: `main.rs` (entry), `app.rs` (UI/state), `markdown_renderer.rs` (parsing/rendering), `sample_files.rs` (embedded docs), `lib.rs` (exports).
- `examples/` — Sample `.md` files for manual checks.
- `build.rs` — Embeds `icon.ico` and sets Windows resources.
- Tests live inline under `#[cfg(test)] mod tests` within each file.

## Build, Test, and Development Commands
- Build: `cargo build --release` → `target/release/mdmdview.exe`.
- Run: `cargo run -- [path\\to\\file.md]` (file arg optional).
- Test: `cargo test` (use `-- --nocapture` to print output).
- Lint: `cargo clippy --all-targets -- -D warnings` (treat warnings as errors).
- Format: `cargo fmt --all`.
- CI: GitHub Actions enforces `rustfmt`/`clippy`; on green, it runs a release build.

## Coding Style & Naming Conventions
- Use `rustfmt` defaults (4‑space indent, standard line width).
- Naming: `snake_case` functions/modules, `CamelCase` types, `SCREAMING_SNAKE_CASE` consts.
- Errors: prefer `anyhow::Result<T>` for fallible paths; avoid `unwrap()`/`expect()` outside tests.
- UI: keep egui `update()` non‑blocking; preserve keyboard shortcuts and fullscreen behavior.

## Testing Guidelines
- Place unit tests beside code in `mod tests` with behavior‑oriented names (e.g., `test_zoom_functionality`).
- Focus tests on: renderer parsing, font/zoom behavior, file I/O via `tempfile`, and app state transitions (F11 flag, navigation requests).
- Run all tests locally with `cargo test` before opening a PR.

## Commit & Pull Request Guidelines
- Commits: small, imperative; prefer Conventional Commits when possible, e.g.:
  - `feat(renderer): add language mapping for powershell`
  - `fix(app): prevent deadlock on F11 toggle`
- PRs must include: summary, rationale, screenshots/GIFs for UI changes, test plan (`cargo test`/manual steps), and linked issues. Update `README.md` for user‑visible changes.

## Security & Configuration Tips
- No secrets or network calls; only local file reads and `webbrowser` for links.
- Validate paths and handle unreadable files gracefully.
- Windows resources are configured in `build.rs`; keep `icon.ico` path stable when adjusting packaging.
