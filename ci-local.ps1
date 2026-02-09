# ci-local.ps1 - Run the CI pipeline locally before pushing.
# Mirrors .github/workflows/ci.yml so issues are caught early.
# Usage: .\ci-local.ps1          (full pipeline)
#        .\ci-local.ps1 -Quick   (skip release build)

param(
    [switch]$Quick
)

$ErrorActionPreference = 'Stop'
$env:RUSTFLAGS = '-D warnings'

function Run-Step {
    param(
        [string]$Name,
        [scriptblock]$Command
    )
    Write-Host ""
    Write-Host "=== $Name ===" -ForegroundColor Cyan
    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    try {
        & $Command
        if ($LASTEXITCODE -ne 0) {
            throw "Exit code $LASTEXITCODE"
        }
        $sw.Stop()
        Write-Host "  PASS ($([math]::Round($sw.Elapsed.TotalSeconds, 1))s)" -ForegroundColor Green
    }
    catch {
        $sw.Stop()
        Write-Host "  FAIL ($([math]::Round($sw.Elapsed.TotalSeconds, 1))s)" -ForegroundColor Red
        Write-Host "  $($_.Exception.Message)" -ForegroundColor Red
        Write-Host ""
        Write-Host "Pipeline failed at: $Name" -ForegroundColor Red
        exit 1
    }
}

$totalSw = [System.Diagnostics.Stopwatch]::StartNew()

Write-Host "CI Local Pipeline" -ForegroundColor Yellow
Write-Host "=================" -ForegroundColor Yellow

Run-Step "Format (rustfmt)" {
    cargo fmt --all -- --check
}

Run-Step "Lint (clippy)" {
    cargo clippy --all-targets -- -D warnings
}

Run-Step "Build (debug)" {
    cargo build --all-targets
}

Run-Step "Test" {
    cargo test --all-targets -- --nocapture
}

if (-not $Quick) {
    Run-Step "Build (release)" {
        cargo build --release
    }
}

$totalSw.Stop()
Write-Host ""
Write-Host "=================" -ForegroundColor Yellow
$mode = if ($Quick) { "Quick" } else { "Full" }
Write-Host "$mode pipeline PASSED ($([math]::Round($totalSw.Elapsed.TotalSeconds, 1))s)" -ForegroundColor Green
