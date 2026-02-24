// Shell integration utilities for OS-level file operations.
//
// Provides cross-platform functions for interacting with the operating system's
// file manager and clipboard at the file level:
//
// - `clipboard_copy_file`: Places a file path on the system clipboard as a
//   file copy operation (CF_HDROP on Windows, osascript on macOS, xclip on
//   Linux). This allows the user to paste the file into Explorer/Finder/etc.
//
// - `reveal_in_file_manager`: Opens the native file manager (Explorer, Finder,
//   or xdg-open) with the specified file selected/highlighted.
//
// These functions are used by the application's UI to provide context-menu
// actions on the currently open file. Each function uses platform-specific
// implementations gated by `#[cfg(target_os = "...")]` attributes and returns
// `anyhow::Result` for consistent error handling.

use std::path::Path;

/// Place a file on the system clipboard as a file copy (CF_HDROP format on
/// Windows, osascript on macOS, xclip text/uri-list on Linux).
///
/// After calling this, the user can paste the file in their file manager to
/// copy it to a new location.
#[cfg(target_os = "windows")]
pub fn clipboard_copy_file(path: &Path) -> anyhow::Result<()> {
    use clipboard_win::{raw, Clipboard};

    let abs_path = path.canonicalize()?;
    let path_str = abs_path.to_string_lossy();
    // Strip \\?\ prefix — Explorer doesn't handle extended-length paths.
    // Also handle UNC: \\?\UNC\server\share -> \\server\share
    let clean_path = if let Some(rest) = path_str.strip_prefix(r"\\?\UNC\") {
        format!(r"\\{}", rest)
    } else if let Some(rest) = path_str.strip_prefix(r"\\?\") {
        rest.to_string()
    } else {
        path_str.to_string()
    };

    let _clip = Clipboard::new_attempts(10)
        .map_err(|e| anyhow::anyhow!("Failed to open clipboard: {}", e))?;
    raw::set_file_list(&[&clean_path])
        .map_err(|e| anyhow::anyhow!("Failed to set clipboard data: {}", e))?;

    Ok(())
}

/// Place a file on the system clipboard as a file copy (macOS via osascript).
#[cfg(target_os = "macos")]
pub fn clipboard_copy_file(path: &Path) -> anyhow::Result<()> {
    let abs_path = path.canonicalize()?;
    // Escape backslashes and double-quotes for AppleScript string literal
    let escaped = abs_path
        .display()
        .to_string()
        .replace('\\', "\\\\")
        .replace('"', "\\\"");
    let script = format!("set the clipboard to (POSIX file \"{}\")", escaped);
    let output = std::process::Command::new("osascript")
        .arg("-e")
        .arg(&script)
        .output()?;
    if !output.status.success() {
        anyhow::bail!(
            "osascript failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(())
}

/// Place a file on the system clipboard as a file copy (Linux via xclip).
#[cfg(not(any(target_os = "windows", target_os = "macos")))]
pub fn clipboard_copy_file(path: &Path) -> anyhow::Result<()> {
    let abs_path = path.canonicalize()?;
    let uri = format!("file://{}\n", abs_path.display());
    let mut child = std::process::Command::new("xclip")
        .args(["-selection", "clipboard", "-t", "text/uri-list"])
        .stdin(std::process::Stdio::piped())
        .spawn()
        .map_err(|_| anyhow::anyhow!("xclip not found. Install with: sudo apt install xclip"))?;
    if let Some(stdin) = child.stdin.as_mut() {
        use std::io::Write;
        stdin.write_all(uri.as_bytes())?;
    }
    let status = child.wait()?;
    if !status.success() {
        anyhow::bail!("xclip exited with status {}", status);
    }
    Ok(())
}

/// Open the native file manager with the specified file selected/highlighted.
///
/// On Windows this runs `explorer /select,<path>`, on macOS `open -R <path>`,
/// and on Linux `xdg-open <parent-dir>` (since most Linux file managers don't
/// support selecting a specific file).
pub fn reveal_in_file_manager(path: &Path) -> anyhow::Result<()> {
    let abs_path = path.canonicalize()?;

    #[cfg(target_os = "windows")]
    {
        std::process::Command::new("explorer")
            .arg(format!("/select,{}", abs_path.display()))
            .spawn()?;
    }

    #[cfg(target_os = "macos")]
    {
        std::process::Command::new("open")
            .arg("-R")
            .arg(&abs_path)
            .spawn()?;
    }

    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    {
        let parent = abs_path
            .parent()
            .ok_or_else(|| anyhow::anyhow!("Cannot determine parent directory"))?;
        std::process::Command::new("xdg-open")
            .arg(parent)
            .spawn()?;
    }

    Ok(())
}
