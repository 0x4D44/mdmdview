<#
.SYNOPSIS
    Runs the full test suite with optional coverage report.
    Requires cargo-llvm-cov for coverage (install with: cargo install cargo-llvm-cov).
.EXAMPLE
    ./scripts/full_test.ps1
.EXAMPLE
    ./scripts/full_test.ps1 -SkipCoverage
#>
[CmdletBinding()]
param(
    [switch]$SkipCoverage
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$repoRoot = (Resolve-Path (Join-Path $scriptRoot "..")).Path
Push-Location $repoRoot

try {
    Write-Host "=== Test ===" -ForegroundColor Cyan
    $env:RUSTFLAGS = "-D warnings"
    cargo test --all-targets
    if ($LASTEXITCODE -ne 0) { throw "Tests failed" }

    if (-not $SkipCoverage) {
        $llvmCov = Get-Command cargo-llvm-cov -ErrorAction SilentlyContinue
        if ($llvmCov) {
            Write-Host ""
            Write-Host "=== Coverage ===" -ForegroundColor Cyan
            cargo llvm-cov --workspace --summary-only
            if ($LASTEXITCODE -ne 0) {
                Write-Warning "Coverage collection failed (non-fatal)"
            }
        } else {
            Write-Host ""
            Write-Host "Skipping coverage (cargo-llvm-cov not installed)" -ForegroundColor Yellow
        }
    }

    Write-Host ""
    Write-Host "All checks passed." -ForegroundColor Green
}
finally {
    Pop-Location
}
