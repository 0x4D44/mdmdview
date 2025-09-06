/// Build script for Windows metadata and icon resources
fn main() {
    // Only build Windows resources on Windows
    #[cfg(windows)]
    {
        let mut res = winres::WindowsResource::new();

        // Numeric version information (1.0.0.0)
        // Keep numeric version with a trailing .0 as is common for Win32 resources.
        res.set_version_info(winres::VersionInfo::PRODUCTVERSION, 0x00010000);
        res.set_version_info(winres::VersionInfo::FILEVERSION, 0x00010000);

        // Branding and descriptive strings shown in the Windows properties dialog
        res.set("ProductName", "Martin's Simple Markdown viewer");
        res.set("FileDescription", "Martin's Simple Markdown viewer");
        res.set("CompanyName", "Martin Davidson");
        res.set("OriginalFilename", "mdmdview.exe");
        res.set("InternalName", "mdmdview");

        // Version strings
        res.set("ProductVersion", "1.0.0");
        res.set("FileVersion", "1.0.0");

        // Copyright
        res.set("LegalCopyright", "Copyright (c) 2025 Martin Davidson");

        // Best-effort build timestamp as a custom string (not all dialogs show it).
        // Prefer an ISO-8601 string via PowerShell when available; fall back to epoch seconds.
        let build_dt = (|| -> Option<String> {
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
        })()
        .unwrap_or_else(|| {
            let secs = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0);
            format!("epoch:{secs}")
        });
        res.set("BuildDateTime", &build_dt);

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
