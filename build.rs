/// Build script for Windows metadata and icon resources
fn main() {
    let build_timestamp = build_timestamp_string();
    println!(
        "cargo:rustc-env=MDMDVIEW_BUILD_TIMESTAMP={}",
        build_timestamp
    );
    // Generate embedded Mermaid JS module if vendor file exists
    generate_mermaid_js();
    // Only build Windows resources on Windows
    #[cfg(windows)]
    {
        use std::env;
        let mut res = winres::WindowsResource::new();

        let pkg_name = env::var("CARGO_PKG_NAME").unwrap_or_else(|_| "mdmdview".to_string());
        let pkg_description = env::var("CARGO_PKG_DESCRIPTION").unwrap_or_default();
        let pkg_version = env::var("CARGO_PKG_VERSION").unwrap_or_else(|_| "0.0.0".to_string());
        let pkg_authors = env::var("CARGO_PKG_AUTHORS").unwrap_or_default();
        let (major, minor, patch, build) = parse_version_components(&pkg_version);
        let numeric_version = encode_windows_version(major, minor, patch, build);
        let version_string = format!("{major}.{minor}.{patch}.{build}");

        res.set_version_info(winres::VersionInfo::PRODUCTVERSION, numeric_version);
        res.set_version_info(winres::VersionInfo::FILEVERSION, numeric_version);

        let company_name = first_author(&pkg_authors);
        let description = if pkg_description.trim().is_empty() {
            pkg_name.clone()
        } else {
            pkg_description.clone()
        };

        res.set("ProductName", &pkg_name);
        res.set("FileDescription", &description);
        res.set("CompanyName", &company_name);
        res.set("OriginalFilename", &format!("{}.exe", pkg_name));
        res.set("InternalName", &pkg_name);
        res.set("ProductVersion", &version_string);
        res.set("FileVersion", &version_string);

        // Build timestamp for about dialogs/tooltips
        let build_dt = build_timestamp.clone();
        res.set("BuildDateTime", &build_dt);

        let copyright = extract_year(&build_dt)
            .map(|year| format!("Copyright (c) {} {}", year, company_name))
            .unwrap_or_else(|| format!("Copyright (c) {}", company_name));
        res.set("LegalCopyright", &copyright);

        // Icon
        if std::path::Path::new("icon.ico").exists() {
            res.set_icon("icon.ico");
        }

        // Compile resources
        match res.compile() {
            Ok(_) => println!("cargo:info=Windows resources compiled successfully"),
            Err(e) => {
                println!("cargo:warning=Failed to compile Windows resources: {}", e);
                // Don't fail the build if resources fail to compile
            }
        }
    }

    #[cfg(not(windows))]
    {
        println!("cargo:info=Skipping Windows resources on non-Windows platform");
    }
}

fn generate_mermaid_js() {
    use std::{env, fs, io::Write, path::PathBuf};
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    let dest = out_dir.join("mermaid_js.rs");

    let vendor_path = PathBuf::from("assets")
        .join("vendor")
        .join("mermaid.min.js");
    let content = if let Ok(bytes) = fs::read(&vendor_path) {
        // Emit as a byte array literal to avoid path issues
        let mut s = String::new();
        s.push_str("pub static MERMAID_JS: &[u8] = &[");
        for (i, b) in bytes.iter().enumerate() {
            if i % 20 == 0 {
                s.push_str("\n    ");
            }
            s.push_str(&format!("{}u8, ", b));
        }
        s.push_str("\n];\n");
        s
    } else {
        // No vendor file present; emit empty
        "pub static MERMAID_JS: &[u8] = &[];\n".to_string()
    };
    let mut f = fs::File::create(&dest).expect("create mermaid_js.rs");
    f.write_all(content.as_bytes())
        .expect("write mermaid_js.rs");
    println!("cargo:rerun-if-changed=assets/vendor/mermaid.min.js");
}

#[cfg(any(windows, test))]
fn parse_version_components(version: &str) -> (u16, u16, u16, u16) {
    let parts: Vec<u16> = version
        .split(['.', '-'])
        .filter_map(|part| part.parse::<u16>().ok())
        .collect();
    (
        *parts.first().unwrap_or(&0),
        *parts.get(1).unwrap_or(&0),
        *parts.get(2).unwrap_or(&0),
        *parts.get(3).unwrap_or(&0),
    )
}

#[cfg(any(windows, test))]
fn encode_windows_version(major: u16, minor: u16, patch: u16, build: u16) -> u64 {
    ((major as u64) << 48) | ((minor as u64) << 32) | ((patch as u64) << 16) | (build as u64)
}

fn build_timestamp_string() -> String {
    #[cfg(windows)]
    {
        if let Some(iso) = powershell_iso_timestamp() {
            return iso;
        }
    }
    // Fallback: format timestamp using pure Rust (no external dependencies)
    format_unix_timestamp_iso8601(
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
    )
}

/// Formats a Unix timestamp as an ISO 8601 date string (YYYY-MM-DDTHH:MM:SSZ).
/// This is a simple implementation that doesn't require external crates.
fn format_unix_timestamp_iso8601(secs: u64) -> String {
    // Days per month for non-leap and leap years
    const DAYS_IN_MONTH: [[u32; 12]; 2] = [
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], // non-leap
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], // leap
    ];

    fn is_leap_year(year: u32) -> bool {
        (year.is_multiple_of(4) && !year.is_multiple_of(100)) || year.is_multiple_of(400)
    }

    fn days_in_year(year: u32) -> u32 {
        if is_leap_year(year) {
            366
        } else {
            365
        }
    }

    let secs_per_minute = 60u64;
    let secs_per_hour = 3600u64;
    let secs_per_day = 86400u64;

    // Extract time components
    let time_of_day = secs % secs_per_day;
    let hour = (time_of_day / secs_per_hour) as u32;
    let minute = ((time_of_day % secs_per_hour) / secs_per_minute) as u32;
    let second = (time_of_day % secs_per_minute) as u32;

    // Calculate date from days since Unix epoch (1970-01-01)
    let mut days = (secs / secs_per_day) as u32;
    let mut year = 1970u32;

    // Find the year
    loop {
        let days_this_year = days_in_year(year);
        if days < days_this_year {
            break;
        }
        days -= days_this_year;
        year += 1;
    }

    // Find the month and day
    let leap = if is_leap_year(year) { 1 } else { 0 };
    let mut month = 0u32;
    for (m, &d) in DAYS_IN_MONTH[leap].iter().enumerate() {
        if days < d {
            month = m as u32 + 1;
            break;
        }
        days -= d;
    }
    let day = days + 1;

    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year, month, day, hour, minute, second
    )
}

#[cfg(windows)]
fn powershell_iso_timestamp() -> Option<String> {
    use std::process::Command;
    let out = Command::new("powershell")
        .args(["-NoProfile", "-Command", "Get-Date -Format o"])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8(out.stdout).ok()?;
    Some(s.trim().to_string())
}

#[cfg(any(windows, test))]
fn first_author(authors: &str) -> String {
    authors
        .split(';')
        .find_map(|entry| {
            let trimmed = entry.trim();
            if trimmed.is_empty() {
                None
            } else if let Some((name, _)) = trimmed.split_once('<') {
                let name = name.trim();
                if name.is_empty() {
                    None
                } else {
                    Some(name.to_string())
                }
            } else {
                Some(trimmed.to_string())
            }
        })
        .filter(|name| !name.is_empty())
        .unwrap_or_else(|| "Unknown".to_string())
}

#[cfg(any(windows, test))]
fn extract_year(timestamp: &str) -> Option<String> {
    let digits: String = timestamp
        .chars()
        .take_while(|c| c.is_ascii_digit())
        .collect();
    if digits.len() == 4 {
        Some(digits)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_version_components_handles_semver() {
        assert_eq!(parse_version_components("1.2.3"), (1, 2, 3, 0));
        assert_eq!(parse_version_components("0.3.1-alpha.5"), (0, 3, 1, 5));
    }

    #[test]
    fn encode_windows_version_packs_words() {
        assert_eq!(encode_windows_version(1, 2, 3, 4), 0x0001_0002_0003_0004u64);
    }

    #[test]
    fn first_author_prefers_name_without_email() {
        assert_eq!(
            first_author("Jane Doe <jane@example.com>;John Smith"),
            "Jane Doe"
        );
        assert_eq!(first_author(""), "Unknown");
    }

    #[test]
    fn extract_year_finds_iso_year() {
        assert_eq!(
            extract_year("2025-10-19T05:42:00.0000000Z"),
            Some("2025".to_string())
        );
        // Fallback format also starts with year
        assert_eq!(
            extract_year("2025-01-15T12:30:45Z"),
            Some("2025".to_string())
        );
        assert_eq!(extract_year("epoch:12345"), None);
    }

    #[test]
    fn format_unix_timestamp_produces_valid_iso8601() {
        // Unix epoch should produce 1970-01-01T00:00:00Z
        assert_eq!(format_unix_timestamp_iso8601(0), "1970-01-01T00:00:00Z");

        // 2025-01-15T12:30:45Z = 1736942445 seconds since epoch
        assert_eq!(
            format_unix_timestamp_iso8601(1736942445),
            "2025-01-15T12:30:45Z"
        );

        // Test leap year handling: 2024-02-29T00:00:00Z = 1709164800
        assert_eq!(
            format_unix_timestamp_iso8601(1709164800),
            "2024-02-29T00:00:00Z"
        );

        // Test end of year: 2023-12-31T23:59:59Z = 1704067199
        assert_eq!(
            format_unix_timestamp_iso8601(1704067199),
            "2023-12-31T23:59:59Z"
        );
    }
}
