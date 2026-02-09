<#
.SYNOPSIS
    Pre-release validation: format, lint, build, and test.
    Run locally before tagging a release.
.EXAMPLE
    ./scripts/prerelease.ps1
#>
Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = (Resolve-Path (Join-Path $scriptRoot "..")).Path
Push-Location $repoRoot

try {
    Write-Host "=== Format check ===" -ForegroundColor Cyan
    cargo fmt --all -- --check
    if ($LASTEXITCODE -ne 0) { throw "Format check failed" }

    Write-Host ""
    Write-Host "=== Clippy ===" -ForegroundColor Cyan
    cargo clippy --all-targets -- -D warnings
    if ($LASTEXITCODE -ne 0) { throw "Clippy failed" }

    Write-Host ""
    Write-Host "=== Build (release) ===" -ForegroundColor Cyan
    $env:RUSTFLAGS = "-D warnings"
    cargo build --release
    if ($LASTEXITCODE -ne 0) { throw "Build failed" }

    Write-Host ""
    Write-Host "=== Test ===" -ForegroundColor Cyan
    cargo test --all-targets
    if ($LASTEXITCODE -ne 0) { throw "Tests failed" }

    Write-Host ""
    Write-Host "All checks passed." -ForegroundColor Green
}
finally {
    Pop-Location
}
