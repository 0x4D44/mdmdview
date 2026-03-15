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
// These functions are used by the application's File menu to provide
// shell operations on the currently open file. Each function uses platform-specific
// implementations gated by `#[cfg(target_os = "...")]` attributes and returns
// `anyhow::Result` for consistent error handling.

use std::path::Path;

#[cfg(test)]
thread_local! {
    static FORCED_CLIPBOARD_COPY_ERROR: std::cell::RefCell<bool> =
        const { std::cell::RefCell::new(false) };
    static FORCED_REVEAL_ERROR: std::cell::RefCell<bool> =
        const { std::cell::RefCell::new(false) };
}

#[cfg(test)]
fn take_forced_clipboard_copy_error() -> bool {
    FORCED_CLIPBOARD_COPY_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
fn take_forced_reveal_error() -> bool {
    FORCED_REVEAL_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
pub(crate) fn force_clipboard_copy_error_once() {
    FORCED_CLIPBOARD_COPY_ERROR.with(|flag| {
        *flag.borrow_mut() = true;
    });
}

#[cfg(test)]
pub(crate) fn force_reveal_in_file_manager_error_once() {
    FORCED_REVEAL_ERROR.with(|flag| {
        *flag.borrow_mut() = true;
    });
}

#[cfg(target_os = "windows")]
fn clean_windows_shell_path(abs_path: &Path) -> String {
    let path_str = abs_path.to_string_lossy();
    if let Some(rest) = path_str.strip_prefix(r"\\?\UNC\") {
        format!(r"\\{}", rest)
    } else if let Some(rest) = path_str.strip_prefix(r"\\?\") {
        rest.to_string()
    } else {
        path_str.to_string()
    }
}

#[cfg(target_os = "windows")]
fn windows_reveal_arg(abs_path: &Path) -> String {
    format!("/select,{}", abs_path.display())
}

/// Place a file on the system clipboard as a file copy (CF_HDROP format on
/// Windows, osascript on macOS, xclip text/uri-list on Linux).
///
/// After calling this, the user can paste the file in their file manager to
/// copy it to a new location.
#[cfg(target_os = "windows")]
pub fn clipboard_copy_file(path: &Path) -> anyhow::Result<()> {
    use clipboard_win::{raw, Clipboard};

    #[cfg(test)]
    if take_forced_clipboard_copy_error() {
        anyhow::bail!("forced clipboard error");
    }

    let abs_path = path.canonicalize()?;
    let clean_path = clean_windows_shell_path(&abs_path);
    // Strip \\?\ prefix — Explorer doesn't handle extended-length paths.
    // Also handle UNC: \\?\UNC\server\share -> \\server\share

    let _clip = Clipboard::new_attempts(10)
        .map_err(|e| anyhow::anyhow!("Failed to open clipboard: {}", e))?;
    raw::set_file_list(&[&clean_path])
        .map_err(|e| anyhow::anyhow!("Failed to set clipboard data: {}", e))?;

    Ok(())
}

/// Place a file on the system clipboard as a file copy (macOS via osascript).
#[cfg(target_os = "macos")]
pub fn clipboard_copy_file(path: &Path) -> anyhow::Result<()> {
    #[cfg(test)]
    if take_forced_clipboard_copy_error() {
        anyhow::bail!("forced clipboard error");
    }

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
    #[cfg(test)]
    if take_forced_clipboard_copy_error() {
        anyhow::bail!("forced clipboard error");
    }

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
    #[cfg(test)]
    if take_forced_reveal_error() {
        anyhow::bail!("forced reveal error");
    }

    let abs_path = path.canonicalize()?;

    #[cfg(target_os = "windows")]
    {
        std::process::Command::new("explorer")
            .arg(windows_reveal_arg(&abs_path))
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
        std::process::Command::new("xdg-open").arg(parent).spawn()?;
    }

    Ok(())
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_clipboard_copy_file_nonexistent_path() {
        let path = PathBuf::from("/nonexistent/file.md");
        let result = clipboard_copy_file(&path);
        assert!(result.is_err());
    }

    #[test]
    fn test_reveal_in_file_manager_nonexistent_path() {
        let path = PathBuf::from("/nonexistent/file.md");
        let result = reveal_in_file_manager(&path);
        assert!(result.is_err());
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn test_clean_windows_shell_path_strips_unc_prefix() {
        let path = PathBuf::from(r"\\?\UNC\server\share\file.md");

        assert_eq!(clean_windows_shell_path(&path), r"\\server\share\file.md");
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn test_clean_windows_shell_path_strips_device_prefix() {
        let path = PathBuf::from(r"\\?\C:\Temp\file.md");

        assert_eq!(clean_windows_shell_path(&path), r"C:\Temp\file.md");
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn test_clean_windows_shell_path_keeps_plain_path() {
        let path = PathBuf::from(r"C:\Temp\file.md");

        assert_eq!(clean_windows_shell_path(&path), r"C:\Temp\file.md");
    }

    #[cfg(target_os = "windows")]
    #[test]
    fn test_windows_reveal_arg_formats_select_switch() {
        let path = PathBuf::from(r"C:\Temp\file.md");

        assert_eq!(windows_reveal_arg(&path), r"/select,C:\Temp\file.md");
    }
}
