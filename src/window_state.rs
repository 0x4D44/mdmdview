use std::fs;
use std::io::Write;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy)]
pub struct WindowState {
    pub pos: [f32; 2],
    pub size: [f32; 2],
    pub maximized: bool,
}

/// Application settings that persist across sessions
#[derive(Debug, Clone, Copy)]
pub struct AppSettings {
    pub allow_remote_images: bool,
    pub dark_mode: bool,
}

impl Default for AppSettings {
    fn default() -> Self {
        Self {
            allow_remote_images: false,
            dark_mode: detect_os_dark_mode(),
        }
    }
}

/// Detects the OS-level dark/light theme preference.
/// On Windows, reads `AppsUseLightTheme` from the Personalize registry key.
/// Returns `true` for dark mode, `false` for light mode.
/// Defaults to dark mode if the preference cannot be determined.
#[cfg(all(windows, not(test)))]
fn detect_os_dark_mode() -> bool {
    use winreg::enums::HKEY_CURRENT_USER;
    use winreg::RegKey;
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    let Ok(key) =
        hkcu.open_subkey("Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize")
    else {
        return true; // default to dark if registry key missing
    };
    let value: u32 = key.get_value("AppsUseLightTheme").unwrap_or(0);
    // AppsUseLightTheme: 0 = dark mode, 1 = light mode
    value == 0
}

/// Non-Windows fallback: always default to dark mode.
#[cfg(not(windows))]
fn detect_os_dark_mode() -> bool {
    true
}

fn config_dir() -> Option<PathBuf> {
    #[cfg(target_os = "windows")]
    {
        if let Ok(appdata) = std::env::var("APPDATA") {
            let mut p = PathBuf::from(appdata);
            p.push("MarkdownView");
            return Some(p);
        }
    }

    #[cfg(target_os = "macos")]
    {
        if let Ok(home) = std::env::var("HOME") {
            let mut p = PathBuf::from(home);
            p.push("Library/Application Support/MarkdownView");
            return Some(p);
        }
    }

    // Linux / others: XDG or ~/.config
    if let Ok(xdg) = std::env::var("XDG_CONFIG_HOME") {
        let mut p = PathBuf::from(xdg);
        p.push("mdmdview");
        return Some(p);
    }
    if let Ok(home) = std::env::var("HOME") {
        let mut p = PathBuf::from(home);
        p.push(".config/mdmdview");
        return Some(p);
    }
    None
}

fn state_path() -> Option<PathBuf> {
    config_dir().map(|mut dir| {
        dir.push("window_state.txt");
        dir
    })
}

/// Ensures the config directory exists and creates a file within it.
/// Returns `Ok(None)` when no config directory is available.
fn create_config_file(filename: &str) -> std::io::Result<Option<fs::File>> {
    let Some(mut dir) = config_dir() else {
        return Ok(None);
    };
    if !dir.exists() {
        fs::create_dir_all(&dir)?;
    }
    dir.push(filename);
    fs::File::create(&dir).map(Some)
}

pub fn load_window_state() -> Option<WindowState> {
    #[cfg(windows)]
    {
        if let Some(ws) = load_window_state_registry() {
            return Some(ws);
        }
    }
    let path = state_path()?;
    let s = fs::read_to_string(path).ok()?;
    let parts: Vec<&str> = s.split_whitespace().collect();
    if parts.len() < 5 {
        return None;
    }
    let x = parts[0].parse::<f32>().ok()?;
    let y = parts[1].parse::<f32>().ok()?;
    let w = parts[2].parse::<f32>().ok()?;
    let h = parts[3].parse::<f32>().ok()?;
    let max = matches!(parts[4], "1" | "true" | "True");
    Some(WindowState {
        pos: [x, y],
        size: [w, h],
        maximized: max,
    })
}

pub fn save_window_state(state: &WindowState) -> std::io::Result<()> {
    #[cfg(windows)]
    {
        if let Err(e) = save_window_state_registry(state) {
            eprintln!("Failed to write window state to registry: {e}");
        }
    }
    if let Some(mut f) = create_config_file("window_state.txt")? {
        write_window_state(&mut f, state)?;
    }
    Ok(())
}

fn write_window_state(writer: &mut dyn Write, state: &WindowState) -> std::io::Result<()> {
    #[cfg(test)]
    if take_forced_file_write_error() {
        return Err(std::io::Error::other("forced write error"));
    }
    writeln!(
        writer,
        "{} {} {} {} {}",
        state.pos[0], state.pos[1], state.size[0], state.size[1], state.maximized as u8
    )
}

pub fn sanitize_window_state(ws: WindowState) -> Option<WindowState> {
    if !ws.pos[0].is_finite()
        || !ws.pos[1].is_finite()
        || !ws.size[0].is_finite()
        || !ws.size[1].is_finite()
    {
        return None;
    }

    const MIN_SIZE: f32 = 400.0;
    const MIN_WIDTH: f32 = 600.0;
    const MAX_SIZE: f32 = 10000.0;
    const MAX_POS: f32 = 20000.0;

    Some(WindowState {
        pos: [ws.pos[0].clamp(0.0, MAX_POS), ws.pos[1].clamp(0.0, MAX_POS)],
        size: [
            ws.size[0].clamp(MIN_WIDTH, MAX_SIZE),
            ws.size[1].clamp(MIN_SIZE, MAX_SIZE),
        ],
        maximized: ws.maximized,
    })
}

#[cfg(all(windows, not(test)))]
fn load_window_state_registry() -> Option<WindowState> {
    use winreg::enums::HKEY_CURRENT_USER;
    use winreg::RegKey;
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    let key = hkcu.open_subkey("Software\\MarkdownView").ok()?;
    let x: u32 = key.get_value("PosX").ok()?;
    let y: u32 = key.get_value("PosY").ok()?;
    let w: u32 = key.get_value("Width").ok()?;
    let h: u32 = key.get_value("Height").ok()?;
    let maximized: u32 = key.get_value("Maximized").unwrap_or(0);
    Some(WindowState {
        pos: [x as f32, y as f32],
        size: [w as f32, h as f32],
        maximized: maximized != 0,
    })
}

#[cfg(all(windows, not(test)))]
fn save_window_state_registry(state: &WindowState) -> std::io::Result<()> {
    use winreg::enums::{HKEY_CURRENT_USER, KEY_READ, KEY_WRITE};
    use winreg::RegKey;
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    let (key, _disp) =
        hkcu.create_subkey_with_flags("Software\\MarkdownView", KEY_READ | KEY_WRITE)?;
    let to_u32 = |v: f32| -> u32 {
        if v.is_finite() {
            v.max(0.0).min(u32::MAX as f32).round() as u32
        } else {
            0
        }
    };
    key.set_value("PosX", &to_u32(state.pos[0]))?;
    key.set_value("PosY", &to_u32(state.pos[1]))?;
    key.set_value("Width", &to_u32(state.size[0]))?;
    key.set_value("Height", &to_u32(state.size[1]))?;
    key.set_value("Maximized", &(state.maximized as u32))?;
    Ok(())
}

// --- App Settings persistence ---

fn settings_path() -> Option<PathBuf> {
    config_dir().map(|mut dir| {
        dir.push("settings.txt");
        dir
    })
}

pub fn load_app_settings() -> AppSettings {
    #[cfg(windows)]
    {
        if let Some(settings) = load_app_settings_registry() {
            return settings;
        }
    }
    // Fall back to file-based storage
    let Some(path) = settings_path() else {
        return AppSettings::default();
    };
    let Ok(contents) = fs::read_to_string(path) else {
        return AppSettings::default();
    };
    let mut settings = AppSettings::default();
    for line in contents.lines() {
        if let Some((key, value)) = line.split_once('=') {
            match key.trim() {
                "allow_remote_images" => {
                    settings.allow_remote_images = matches!(value.trim(), "1" | "true");
                }
                "dark_mode" => {
                    settings.dark_mode = matches!(value.trim(), "1" | "true");
                }
                _ => {}
            }
        }
    }
    settings
}

pub fn save_app_settings(settings: &AppSettings) -> std::io::Result<()> {
    #[cfg(windows)]
    {
        if let Err(e) = save_app_settings_registry(settings) {
            eprintln!("Failed to write app settings to registry: {e}");
        }
    }
    if let Some(mut f) = create_config_file("settings.txt")? {
        writeln!(
            f,
            "allow_remote_images={}",
            settings.allow_remote_images as u8
        )?;
        writeln!(f, "dark_mode={}", settings.dark_mode as u8)?;
    }
    Ok(())
}

#[cfg(all(windows, not(test)))]
fn load_app_settings_registry() -> Option<AppSettings> {
    use winreg::enums::HKEY_CURRENT_USER;
    use winreg::RegKey;
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    let key = hkcu.open_subkey("Software\\MarkdownView").ok()?;
    let allow_remote: u32 = key.get_value("AllowRemoteImages").unwrap_or(0);
    let dark_mode = key
        .get_value::<u32, _>("DarkMode")
        .map(|v| v != 0)
        .unwrap_or_else(|_| detect_os_dark_mode());
    Some(AppSettings {
        allow_remote_images: allow_remote != 0,
        dark_mode,
    })
}

#[cfg(all(windows, not(test)))]
fn save_app_settings_registry(settings: &AppSettings) -> std::io::Result<()> {
    use winreg::enums::{HKEY_CURRENT_USER, KEY_READ, KEY_WRITE};
    use winreg::RegKey;
    let hkcu = RegKey::predef(HKEY_CURRENT_USER);
    let (key, _disp) =
        hkcu.create_subkey_with_flags("Software\\MarkdownView", KEY_READ | KEY_WRITE)?;
    key.set_value("AllowRemoteImages", &(settings.allow_remote_images as u32))?;
    key.set_value("DarkMode", &(settings.dark_mode as u32))?;
    Ok(())
}

#[cfg(all(windows, test))]
thread_local! {
    static FORCED_OS_DARK_MODE: std::cell::RefCell<Option<bool>> =
        const { std::cell::RefCell::new(None) };
    static FORCED_SETTINGS_LOAD: std::cell::RefCell<Option<AppSettings>> =
        const { std::cell::RefCell::new(None) };
}

#[cfg(all(windows, test))]
fn detect_os_dark_mode() -> bool {
    FORCED_OS_DARK_MODE.with(|slot| slot.borrow().unwrap_or(true))
}

#[cfg(all(windows, test))]
fn force_os_dark_mode(dark: bool) {
    FORCED_OS_DARK_MODE.with(|slot| {
        *slot.borrow_mut() = Some(dark);
    });
}

#[cfg(all(windows, test))]
fn reset_os_dark_mode() {
    FORCED_OS_DARK_MODE.with(|slot| {
        *slot.borrow_mut() = None;
    });
}

#[cfg(all(windows, test))]
fn load_app_settings_registry() -> Option<AppSettings> {
    FORCED_SETTINGS_LOAD.with(|slot| slot.borrow_mut().take())
}

#[cfg(all(windows, test))]
fn save_app_settings_registry(_settings: &AppSettings) -> std::io::Result<()> {
    Ok(())
}

#[cfg(test)]
thread_local! {
    static FORCED_FILE_WRITE_ERROR: std::cell::RefCell<bool> =
        const { std::cell::RefCell::new(false) };
}

#[cfg(test)]
fn take_forced_file_write_error() -> bool {
    FORCED_FILE_WRITE_ERROR.with(|flag| flag.replace(false))
}

#[cfg(test)]
fn force_file_write_error_once() {
    FORCED_FILE_WRITE_ERROR.with(|flag| {
        *flag.borrow_mut() = true;
    });
}

#[cfg(all(windows, test))]
thread_local! {
    static FORCED_REGISTRY_LOAD: std::cell::RefCell<Option<WindowState>> =
        const { std::cell::RefCell::new(None) };
    static FORCED_REGISTRY_SAVE_ERROR: std::cell::RefCell<bool> =
        const { std::cell::RefCell::new(false) };
}

#[cfg(all(windows, test))]
fn take_forced_registry_load() -> Option<WindowState> {
    FORCED_REGISTRY_LOAD.with(|slot| slot.borrow_mut().take())
}

#[cfg(all(windows, test))]
fn take_forced_registry_save_error() -> bool {
    FORCED_REGISTRY_SAVE_ERROR.with(|flag| flag.replace(false))
}

#[cfg(all(windows, test))]
fn force_registry_load_once(state: WindowState) {
    FORCED_REGISTRY_LOAD.with(|slot| {
        *slot.borrow_mut() = Some(state);
    });
}

#[cfg(all(windows, test))]
fn force_registry_save_error_once() {
    FORCED_REGISTRY_SAVE_ERROR.with(|flag| {
        *flag.borrow_mut() = true;
    });
}

#[cfg(all(windows, test))]
fn load_window_state_registry() -> Option<WindowState> {
    take_forced_registry_load()
}

#[cfg(all(windows, test))]
fn save_window_state_registry(_state: &WindowState) -> std::io::Result<()> {
    if take_forced_registry_save_error() {
        return Err(std::io::Error::other("forced registry error"));
    }
    Ok(())
}

#[cfg(test)]
#[cfg_attr(coverage_nightly, coverage(off))]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};
    use tempfile::TempDir;

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK
            .get_or_init(|| Mutex::new(()))
            .lock()
            .expect("env lock")
    }

    struct EnvGuard {
        key: &'static str,
        original: Option<String>,
    }

    impl EnvGuard {
        fn set(key: &'static str, value: &str) -> Self {
            let original = std::env::var(key).ok();
            std::env::set_var(key, value);
            Self { key, original }
        }

        fn unset(key: &'static str) -> Self {
            let original = std::env::var(key).ok();
            std::env::remove_var(key);
            Self { key, original }
        }
    }

    impl Drop for EnvGuard {
        fn drop(&mut self) {
            if let Some(value) = &self.original {
                std::env::set_var(self.key, value);
            } else {
                std::env::remove_var(self.key);
            }
        }
    }

    /// Helper to set platform-appropriate config env var and return expected config subdir.
    /// On Windows: sets APPDATA, returns "MarkdownView"
    /// On macOS: sets HOME, returns "Library/Application Support/MarkdownView"
    /// On Linux: sets XDG_CONFIG_HOME, returns "mdmdview"
    fn set_config_env(temp_path: &std::path::Path) -> (EnvGuard, PathBuf) {
        #[cfg(target_os = "windows")]
        {
            let guard = EnvGuard::set("APPDATA", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("MarkdownView");
            (guard, subdir)
        }
        #[cfg(target_os = "macos")]
        {
            let guard = EnvGuard::set("HOME", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("Library/Application Support/MarkdownView");
            (guard, subdir)
        }
        #[cfg(not(any(target_os = "windows", target_os = "macos")))]
        {
            let guard = EnvGuard::set("XDG_CONFIG_HOME", temp_path.to_string_lossy().as_ref());
            let subdir = temp_path.join("mdmdview");
            (guard, subdir)
        }
    }

    #[test]
    fn test_sanitize_window_state_clamps_and_rejects_invalid() {
        let invalid = WindowState {
            pos: [f32::NAN, 10.0],
            size: [800.0, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid).is_none());

        let input = WindowState {
            pos: [-50.0, 25000.0],
            size: [100.0, 200.0],
            maximized: true,
        };
        let sanitized = sanitize_window_state(input).expect("expected sanitized state");
        assert_eq!(sanitized.pos[0], 0.0);
        assert_eq!(sanitized.pos[1], 20000.0);
        assert_eq!(sanitized.size[0], 600.0);
        assert_eq!(sanitized.size[1], 400.0);
        assert!(sanitized.maximized);
    }

    #[test]
    fn test_sanitize_window_state_rejects_non_finite_components() {
        let invalid_y = WindowState {
            pos: [10.0, f32::NAN],
            size: [800.0, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid_y).is_none());

        let invalid_w = WindowState {
            pos: [10.0, 20.0],
            size: [f32::NAN, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid_w).is_none());

        let invalid_h = WindowState {
            pos: [10.0, 20.0],
            size: [800.0, f32::NAN],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid_h).is_none());
    }

    #[test]
    fn test_save_and_load_window_state_from_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let state = WindowState {
            pos: [120.0, 80.0],
            size: [1024.0, 768.0],
            maximized: false,
        };
        save_window_state(&state).expect("save");

        let loaded = load_window_state().expect("load");
        assert_eq!(loaded.pos, state.pos);
        assert_eq!(loaded.size, state.size);
        assert_eq!(loaded.maximized, state.maximized);
    }

    #[test]
    fn test_save_window_state_creates_dir_and_writes_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        let _ = fs::remove_dir_all(&config_dir);

        let state = WindowState {
            pos: [10.0, 20.0],
            size: [800.0, 600.0],
            maximized: true,
        };
        save_window_state(&state).expect("save");

        let path = config_dir.join("window_state.txt");
        let contents = fs::read_to_string(&path).expect("read");
        let mut parts = contents.split_whitespace();
        let pos_x: f32 = parts.next().expect("pos x").parse().expect("parse pos x");
        let pos_y: f32 = parts.next().expect("pos y").parse().expect("parse pos y");
        let size_x: f32 = parts.next().expect("size x").parse().expect("parse size x");
        let size_y: f32 = parts.next().expect("size y").parse().expect("parse size y");
        let max_flag = parts.next().expect("max flag");
        assert_eq!(pos_x, state.pos[0]);
        assert_eq!(pos_y, state.pos[1]);
        assert_eq!(size_x, state.size[0]);
        assert_eq!(size_y, state.size[1]);
        assert_eq!(max_flag, "1");
    }

    #[test]
    fn test_write_window_state_error_propagates() {
        struct FailingWriter;

        impl Write for FailingWriter {
            fn write(&mut self, _buf: &[u8]) -> std::io::Result<usize> {
                Err(std::io::Error::other("fail"))
            }

            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        let state = WindowState {
            pos: [1.0, 2.0],
            size: [3.0, 4.0],
            maximized: false,
        };
        let mut writer = FailingWriter;
        let err = write_window_state(&mut writer, &state).expect_err("err");
        assert_eq!(err.kind(), std::io::ErrorKind::Other);
        writer.flush().expect("flush");
    }

    #[test]
    fn test_save_window_state_errors_when_state_path_is_dir() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let state_dir = config_dir.join("window_state.txt");
        fs::create_dir_all(&state_dir).expect("create state dir");

        let state = WindowState {
            pos: [1.0, 2.0],
            size: [3.0, 4.0],
            maximized: false,
        };
        assert!(save_window_state(&state).is_err());
    }

    #[test]
    fn test_save_window_state_forced_write_error_propagates() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        force_file_write_error_once();
        let state = WindowState {
            pos: [1.0, 2.0],
            size: [3.0, 4.0],
            maximized: false,
        };
        let err = save_window_state(&state).expect_err("expected error");
        assert_eq!(err.kind(), std::io::ErrorKind::Other);
    }

    #[test]
    fn test_save_window_state_when_dir_exists() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());
        fs::create_dir_all(&config_dir).expect("create config dir");

        let state = WindowState {
            pos: [5.0, 6.0],
            size: [700.0, 500.0],
            maximized: false,
        };
        save_window_state(&state).expect("save");
        let path = config_dir.join("window_state.txt");
        assert!(path.exists());
    }

    #[test]
    #[cfg(windows)]
    fn test_load_window_state_prefers_registry_when_forced() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard = EnvGuard::set("APPDATA", temp.path().to_string_lossy().as_ref());

        let state = WindowState {
            pos: [42.0, 24.0],
            size: [800.0, 600.0],
            maximized: true,
        };
        force_registry_load_once(state);

        let loaded = load_window_state().expect("load");
        assert_eq!(loaded.pos, state.pos);
        assert_eq!(loaded.size, state.size);
        assert_eq!(loaded.maximized, state.maximized);
    }

    #[test]
    #[cfg(windows)]
    fn test_save_window_state_registry_error_still_writes_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard = EnvGuard::set("APPDATA", temp.path().to_string_lossy().as_ref());
        force_registry_save_error_once();

        let state = WindowState {
            pos: [10.0, 20.0],
            size: [1024.0, 768.0],
            maximized: false,
        };
        save_window_state(&state).expect("save");

        let path = temp.path().join("MarkdownView").join("window_state.txt");
        assert!(path.exists());
    }

    #[test]
    fn test_load_window_state_rejects_bad_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "10 20 30").expect("write bad data");

        assert!(load_window_state().is_none());
    }

    #[test]
    fn test_load_window_state_rejects_invalid_numbers() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "x y 10 20 true").expect("write bad data");

        assert!(load_window_state().is_none());
    }

    #[test]
    fn test_load_window_state_rejects_invalid_components() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");

        std::fs::write(&config, "10 y 30 40 1").expect("write bad data");
        assert!(load_window_state().is_none());

        std::fs::write(&config, "10 20 w 40 1").expect("write bad data");
        assert!(load_window_state().is_none());

        std::fs::write(&config, "10 20 30 h 1").expect("write bad data");
        assert!(load_window_state().is_none());
    }

    #[test]
    fn test_load_window_state_rejects_invalid_utf8() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, [0xFF, 0xFE, 0xFD]).expect("write bad data");

        assert!(load_window_state().is_none());
    }

    #[test]
    fn test_load_window_state_parses_maximized_true() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "10 20 300 400 true").expect("write data");

        let loaded = load_window_state().expect("load");
        assert!(loaded.maximized);
    }

    #[test]
    fn test_load_window_state_returns_none_when_missing() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        assert!(load_window_state().is_none());
    }

    #[test]
    fn test_config_dir_falls_back_to_xdg() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard_appdata = EnvGuard::unset("APPDATA");
        let _guard_home = EnvGuard::unset("HOME");
        let _guard_xdg = EnvGuard::set("XDG_CONFIG_HOME", temp.path().to_string_lossy().as_ref());

        let dir = config_dir().expect("config dir");
        assert!(dir.starts_with(temp.path()));
        assert!(dir.ends_with("mdmdview"));
    }

    #[test]
    fn test_config_dir_falls_back_to_home() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard_appdata = EnvGuard::unset("APPDATA");
        let _guard_xdg = EnvGuard::unset("XDG_CONFIG_HOME");
        let _guard_home = EnvGuard::set("HOME", temp.path().to_string_lossy().as_ref());

        let dir = config_dir().expect("config dir");
        assert!(dir.starts_with(temp.path()));
        assert!(dir.ends_with(std::path::Path::new(".config").join("mdmdview")));
    }

    #[test]
    fn test_config_dir_none_without_env() {
        let _lock = env_lock();
        let _guard_appdata = EnvGuard::unset("APPDATA");
        let _guard_xdg = EnvGuard::unset("XDG_CONFIG_HOME");
        let _guard_home = EnvGuard::unset("HOME");

        assert!(config_dir().is_none());
        let state = WindowState {
            pos: [1.0, 2.0],
            size: [800.0, 600.0],
            maximized: false,
        };
        save_window_state(&state).expect("save ok");
        assert!(load_window_state().is_none());
    }

    // ========== AppSettings Tests ==========

    #[test]
    fn test_load_app_settings_default_when_no_config() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        // No settings file exists - should return default
        let settings = load_app_settings();
        assert!(!settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_parses_true_value() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(&settings_path, "allow_remote_images=true\n").expect("write");

        let settings = load_app_settings();
        assert!(settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_parses_one_value() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(&settings_path, "allow_remote_images=1\n").expect("write");

        let settings = load_app_settings();
        assert!(settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_parses_false_value() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(&settings_path, "allow_remote_images=0\n").expect("write");

        let settings = load_app_settings();
        assert!(!settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_ignores_unknown_keys() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(
            &settings_path,
            "unknown_key=value\nallow_remote_images=true\nother=123\n",
        )
        .expect("write");

        let settings = load_app_settings();
        assert!(settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_ignores_lines_without_equals() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(
            &settings_path,
            "no equals sign here\nallow_remote_images=true\n",
        )
        .expect("write");

        let settings = load_app_settings();
        assert!(settings.allow_remote_images);
    }

    #[test]
    fn test_load_app_settings_handles_whitespace() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(&settings_path, "  allow_remote_images  =  true  \n").expect("write");

        let settings = load_app_settings();
        assert!(settings.allow_remote_images);
    }

    #[test]
    fn test_save_app_settings_creates_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        let settings = AppSettings {
            allow_remote_images: true,
            dark_mode: true,
        };
        save_app_settings(&settings).expect("save");

        let settings_path = config_dir.join("settings.txt");
        assert!(settings_path.exists());
        let contents = fs::read_to_string(&settings_path).expect("read");
        assert!(contents.contains("allow_remote_images=1"));
    }

    #[test]
    fn test_save_app_settings_creates_directory() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        // Ensure directory doesn't exist
        let _ = fs::remove_dir_all(&config_dir);

        let settings = AppSettings {
            allow_remote_images: false,
            dark_mode: true,
        };
        save_app_settings(&settings).expect("save");

        assert!(config_dir.exists());
        let settings_path = config_dir.join("settings.txt");
        assert!(settings_path.exists());
    }

    #[test]
    fn test_save_and_load_app_settings_roundtrip() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let settings = AppSettings {
            allow_remote_images: true,
            dark_mode: true,
        };
        save_app_settings(&settings).expect("save");

        let loaded = load_app_settings();
        assert_eq!(loaded.allow_remote_images, settings.allow_remote_images);
        assert_eq!(loaded.dark_mode, settings.dark_mode);
    }

    #[test]
    fn test_load_app_settings_default_when_no_config_dir() {
        let _lock = env_lock();
        let _guard_appdata = EnvGuard::unset("APPDATA");
        let _guard_xdg = EnvGuard::unset("XDG_CONFIG_HOME");
        let _guard_home = EnvGuard::unset("HOME");

        let settings = load_app_settings();
        assert!(!settings.allow_remote_images);
    }

    #[test]
    fn test_save_app_settings_noop_when_no_config_dir() {
        let _lock = env_lock();
        let _guard_appdata = EnvGuard::unset("APPDATA");
        let _guard_xdg = EnvGuard::unset("XDG_CONFIG_HOME");
        let _guard_home = EnvGuard::unset("HOME");

        let settings = AppSettings {
            allow_remote_images: true,
            dark_mode: true,
        };
        // Should succeed but do nothing
        save_app_settings(&settings).expect("save ok");
    }

    #[test]
    fn test_load_window_state_parses_maximized_true_capital() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "10 20 300 400 True").expect("write data");

        let loaded = load_window_state().expect("load");
        assert!(loaded.maximized);
    }

    #[test]
    fn test_load_window_state_parses_maximized_one() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "10 20 300 400 1").expect("write data");

        let loaded = load_window_state().expect("load");
        assert!(loaded.maximized);
    }

    #[test]
    fn test_load_window_state_parses_maximized_false_value() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        std::fs::create_dir_all(&config_dir).expect("create config dir");
        let config = config_dir.join("window_state.txt");
        std::fs::write(&config, "10 20 300 400 0").expect("write data");

        let loaded = load_window_state().expect("load");
        assert!(!loaded.maximized);
    }

    #[test]
    fn test_sanitize_window_state_infinity_rejected() {
        let invalid = WindowState {
            pos: [f32::INFINITY, 10.0],
            size: [800.0, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid).is_none());

        let invalid2 = WindowState {
            pos: [10.0, f32::NEG_INFINITY],
            size: [800.0, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid2).is_none());
    }

    #[test]
    fn test_sanitize_window_state_infinity_in_size_rejected() {
        let invalid = WindowState {
            pos: [10.0, 20.0],
            size: [f32::INFINITY, 600.0],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid).is_none());

        let invalid2 = WindowState {
            pos: [10.0, 20.0],
            size: [800.0, f32::NEG_INFINITY],
            maximized: false,
        };
        assert!(sanitize_window_state(invalid2).is_none());
    }

    #[test]
    fn test_settings_path_returns_some_when_config_exists() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let path = settings_path();
        assert!(path.is_some());
        assert!(path.unwrap().ends_with("settings.txt"));
    }

    #[test]
    fn test_state_path_returns_some_when_config_exists() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let path = state_path();
        assert!(path.is_some());
        assert!(path.unwrap().ends_with("window_state.txt"));
    }

    // ========== dark_mode persistence tests ==========

    #[test]
    fn test_dark_mode_default_inherits_os_dark() {
        force_os_dark_mode(true);
        assert!(AppSettings::default().dark_mode);
        reset_os_dark_mode();
    }

    #[test]
    fn test_dark_mode_default_inherits_os_light() {
        force_os_dark_mode(false);
        assert!(!AppSettings::default().dark_mode);
        reset_os_dark_mode();
    }

    #[test]
    fn test_settings_round_trip_dark_mode_true() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let settings = AppSettings {
            allow_remote_images: false,
            dark_mode: true,
        };
        save_app_settings(&settings).expect("save");
        let loaded = load_app_settings();
        assert!(loaded.dark_mode);
    }

    #[test]
    fn test_settings_round_trip_dark_mode_false() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, _config_dir) = set_config_env(temp.path());

        let settings = AppSettings {
            allow_remote_images: false,
            dark_mode: false,
        };
        save_app_settings(&settings).expect("save");
        let loaded = load_app_settings();
        assert!(!loaded.dark_mode);
    }

    #[test]
    fn test_settings_missing_dark_mode_inherits_os_dark() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        force_os_dark_mode(true);
        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        // File with only allow_remote_images — no dark_mode line
        fs::write(&settings_path, "allow_remote_images=0\n").expect("write");

        let loaded = load_app_settings();
        assert!(loaded.dark_mode); // inherits OS dark mode
        reset_os_dark_mode();
    }

    #[test]
    fn test_settings_missing_dark_mode_inherits_os_light() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        force_os_dark_mode(false);
        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        // File with only allow_remote_images — no dark_mode line
        fs::write(&settings_path, "allow_remote_images=0\n").expect("write");

        let loaded = load_app_settings();
        assert!(!loaded.dark_mode); // inherits OS light mode
        reset_os_dark_mode();
    }

    #[test]
    fn test_settings_dark_mode_zero_is_false() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let (_guard, config_dir) = set_config_env(temp.path());

        fs::create_dir_all(&config_dir).expect("create config dir");
        let settings_path = config_dir.join("settings.txt");
        fs::write(&settings_path, "dark_mode=0\n").expect("write");

        let loaded = load_app_settings();
        assert!(!loaded.dark_mode);
    }
}
