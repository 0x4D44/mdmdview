<#
.SYNOPSIS
    Generate winget manifest files for mdmdview, ready to submit to microsoft/winget-pkgs.

.DESCRIPTION
    Downloads the MSI from a GitHub release (or uses a local MSI), computes SHA256,
    extracts the MSI ProductCode, and writes the three winget manifest files to an
    output directory.

.PARAMETER Version
    The version to generate manifests for (e.g. "1.8.0"). Defaults to the version
    in Cargo.toml.

.PARAMETER MsiPath
    Path to a local MSI file. If omitted, the MSI is downloaded from the GitHub
    release for the given version.

.PARAMETER OutputDir
    Directory to write the manifest files into. Defaults to
    winget\out\<PackageIdentifier>\<Version>\ mirroring the winget-pkgs layout.

.EXAMPLE
    # Use the MSI from a GitHub release
    .\scripts\winget-submit.ps1

    # Use a local MSI
    .\scripts\winget-submit.ps1 -MsiPath target\wix\mdmdview-1.8.0-x86_64.msi
#>

param(
    [string]$Version,
    [string]$MsiPath,
    [string]$OutputDir
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$PackageIdentifier = '0x4D44.mdmdview'
$RepoOwner = '0x4D44'
$RepoName = 'mdmdview'

# ---------------------------------------------------------------------------
# Resolve version from Cargo.toml if not provided
# ---------------------------------------------------------------------------
if (-not $Version) {
    $cargoToml = Get-Content "$PSScriptRoot\..\Cargo.toml" -Raw
    if ($cargoToml -match '(?m)^version\s*=\s*"([^"]+)"') {
        $Version = $Matches[1]
    } else {
        Write-Error "Could not parse version from Cargo.toml. Pass -Version explicitly."
    }
}
Write-Host "Version: $Version" -ForegroundColor Cyan

# ---------------------------------------------------------------------------
# Download or locate the MSI
# ---------------------------------------------------------------------------
$tag = "v$Version"
$msiFilename = "mdmdview-$tag-windows-x86_64.msi"
$releaseUrl = "https://github.com/$RepoOwner/$RepoName/releases/download/$tag/$msiFilename"

if (-not $MsiPath) {
    $tempDir = Join-Path ([System.IO.Path]::GetTempPath()) "winget-submit-$Version"
    New-Item -ItemType Directory -Force -Path $tempDir | Out-Null
    $MsiPath = Join-Path $tempDir $msiFilename

    if (Test-Path $MsiPath) {
        Write-Host "Using cached MSI: $MsiPath" -ForegroundColor Yellow
    } else {
        Write-Host "Downloading MSI from $releaseUrl ..." -ForegroundColor Cyan
        try {
            Invoke-WebRequest -Uri $releaseUrl -OutFile $MsiPath -UseBasicParsing
        } catch {
            Write-Error "Failed to download MSI. Is the v$Version release published?`n$_"
        }
    }
}

if (-not (Test-Path $MsiPath)) {
    Write-Error "MSI file not found: $MsiPath"
}
Write-Host "MSI: $MsiPath" -ForegroundColor Cyan

# ---------------------------------------------------------------------------
# Compute SHA256
# ---------------------------------------------------------------------------
$sha256 = (Get-FileHash -Path $MsiPath -Algorithm SHA256).Hash
Write-Host "SHA256: $sha256" -ForegroundColor Green

# ---------------------------------------------------------------------------
# Extract ProductCode from MSI via COM (Windows Installer API)
# ---------------------------------------------------------------------------
function Get-MsiProductCode([string]$msi) {
    $installer = New-Object -ComObject WindowsInstaller.Installer
    $database = $installer.OpenDatabase($msi, 0)  # 0 = read-only
    $view = $database.OpenView("SELECT Value FROM Property WHERE Property = 'ProductCode'")
    $view.Execute()
    $record = $view.Fetch()
    $code = $record.StringData(1)
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($record)  | Out-Null
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($view)    | Out-Null
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($database)| Out-Null
    [System.Runtime.Interopservices.Marshal]::ReleaseComObject($installer)| Out-Null
    return $code
}

$productCode = Get-MsiProductCode (Resolve-Path $MsiPath).Path
Write-Host "ProductCode: $productCode" -ForegroundColor Green

# ---------------------------------------------------------------------------
# Prepare output directory
# ---------------------------------------------------------------------------
if (-not $OutputDir) {
    $idParts = $PackageIdentifier.Split('.')
    $firstLetter = $idParts[0][0]
    $OutputDir = Join-Path $PSScriptRoot "..\winget\out\manifests\$firstLetter\$($idParts[0])\$($idParts[1])\$Version"
}
New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null

# ---------------------------------------------------------------------------
# Generate manifests from templates
# ---------------------------------------------------------------------------
$templateDir = Join-Path $PSScriptRoot '..\winget'

foreach ($file in @(
    '0x4D44.mdmdview.yaml',
    '0x4D44.mdmdview.installer.yaml',
    '0x4D44.mdmdview.locale.en-US.yaml'
)) {
    $content = Get-Content (Join-Path $templateDir $file) -Raw
    $content = $content -replace '\$\{VERSION\}', $Version
    $content = $content -replace '\$\{SHA256\}', $sha256
    $content = $content -replace '\$\{PRODUCT_CODE\}', $productCode
    $outPath = Join-Path $OutputDir $file
    # Write UTF-8 without BOM (compatible with PowerShell 5.x and 7+)
    [System.IO.File]::WriteAllText($outPath, $content)
    Write-Host "Wrote: $outPath" -ForegroundColor Green
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "=== Manifests ready ===" -ForegroundColor Cyan
Write-Host "Output directory: $OutputDir"
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Fork https://github.com/microsoft/winget-pkgs"
Write-Host "  2. Copy the contents of:"
Write-Host "       $OutputDir"
Write-Host "     to your fork at:"
Write-Host "       manifests/$($PackageIdentifier[0])/$($PackageIdentifier.Replace('.', '/'))/$Version/"
Write-Host "  3. Open a pull request against microsoft/winget-pkgs"
Write-Host ""
Write-Host "Or use wingetcreate:" -ForegroundColor Yellow
Write-Host "  wingetcreate submit $OutputDir"
