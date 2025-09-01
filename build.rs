/// Build script for Windows metadata and icon resources

fn main() {
    // Only build Windows resources on Windows
    #[cfg(windows)]
    {
        let mut res = winres::WindowsResource::new();
        
        // Set version information
        res.set_version_info(winres::VersionInfo::PRODUCTVERSION, 0x00010000); // 1.0.0.0
        res.set_version_info(winres::VersionInfo::FILEVERSION, 0x00010000);     // 1.0.0.0
        
        // Set string information
        res.set("ProductName", "MarkdownView");
        res.set("ProductVersion", "1.0.0");
        res.set("FileDescription", "A simple markdown viewer for Windows");
        res.set("FileVersion", "1.0.0");
        res.set("CompanyName", "MarkdownView Project");
        res.set("LegalCopyright", "Copyright © 2025 MarkdownView Project");
        res.set("OriginalFilename", "mdmdview.exe");
        res.set("InternalName", "mdmdview");
        
        // Set the icon (we'll create this file next)
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
