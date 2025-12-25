use std::fs;
use std::io::{Read, Write};
use std::path::PathBuf;

#[derive(Debug, Clone, Copy)]
pub struct WindowState {
    pub pos: [f32; 2],
    pub size: [f32; 2],
    pub maximized: bool,
}

fn config_dir() -> Option<PathBuf> {
    // Cross‑platform config dir without extra deps
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

pub fn load_window_state() -> Option<WindowState> {
    #[cfg(windows)]
    {
        if let Some(ws) = load_window_state_registry() {
            return Some(ws);
        }
    }
    let path = state_path()?;
    let mut f = fs::File::open(path).ok()?;
    let mut s = String::new();
    f.read_to_string(&mut s).ok()?;
    // expected format: "x y w h max"
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
    if let Some(mut dir) = config_dir() {
        if !dir.exists() {
            fs::create_dir_all(&dir)?;
        }
        dir.push("window_state.txt");
        let mut f = fs::File::create(&dir)?;
        // simple whitespace separated format
        writeln!(
            f,
            "{} {} {} {} {}",
            state.pos[0], state.pos[1], state.size[0], state.size[1], state.maximized as u8
        )?;
    }
    Ok(())
}

pub fn sanitize_window_state(ws: WindowState) -> Option<WindowState> {
    // Basic sanity: finite values
    if !ws.pos[0].is_finite()
        || !ws.pos[1].is_finite()
        || !ws.size[0].is_finite()
        || !ws.size[1].is_finite()
    {
        return None;
    }

    // Clamp to reasonable ranges
    let min_w = 600.0f32;
    let min_h = 400.0f32;
    let max_w = 10000.0f32;
    let max_h = 10000.0f32;
    let max_pos = 20000.0f32; // avoid absurdly large coordinates

    let w = ws.size[0].clamp(min_w, max_w);
    let h = ws.size[1].clamp(min_h, max_h);
    let x = ws.pos[0].max(0.0).min(max_pos);
    let y = ws.pos[1].max(0.0).min(max_pos);

    Some(WindowState {
        pos: [x, y],
        size: [w, h],
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

#[cfg(all(windows, test))]
fn load_window_state_registry() -> Option<WindowState> {
    None
}

#[cfg(all(windows, test))]
fn save_window_state_registry(_state: &WindowState) -> std::io::Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, OnceLock};
    use tempfile::TempDir;

    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static ENV_LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        ENV_LOCK.get_or_init(|| Mutex::new(())).lock().expect("env lock")
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
    fn test_save_and_load_window_state_from_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard = EnvGuard::set("APPDATA", temp.path().to_string_lossy().as_ref());

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
    fn test_load_window_state_rejects_bad_file() {
        let _lock = env_lock();
        let temp = TempDir::new().expect("temp dir");
        let _guard = EnvGuard::set("APPDATA", temp.path().to_string_lossy().as_ref());

        let mut config = temp.path().join("MarkdownView");
        std::fs::create_dir_all(&config).expect("create config dir");
        config.push("window_state.txt");
        std::fs::write(&config, "10 20 30").expect("write bad data");

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
}
