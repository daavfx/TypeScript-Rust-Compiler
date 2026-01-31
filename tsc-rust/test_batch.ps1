#!/usr/bin/env pwsh
$ErrorActionPreference = 'Continue'

$clawdRoot = Split-Path -Parent $PSScriptRoot
$compilerRelease = Join-Path $PSScriptRoot "target\\release\\tsc-rust.exe"
$compilerDebug = Join-Path $PSScriptRoot "target\\debug\\tsc-rust.exe"
$compiler = if (Test-Path $compilerRelease) { $compilerRelease } else { $compilerDebug }
$sourceDir = Join-Path $clawdRoot "openclaw-main\\openclaw-main"
$files = Get-ChildItem -Path $sourceDir -Recurse -Filter "*.ts" | Select-Object -First 100

$total = 0
$success = 0
$errors = @{}

foreach ($file in $files) {
    $total++
    try {
        Push-Location $clawdRoot
        try {
            $output = & $compiler $file.FullName 2>&1
        } finally {
            Pop-Location
        }
        if ($LASTEXITCODE -eq 0) {
            $success++
            Write-Host "✓ $($file.Name)" -ForegroundColor Green
        } else {
            # Extract error message
            $errorMsg = $output | Select-String -Pattern "Error:" | ForEach-Object { $_.Line }
            if ($errorMsg) {
                $errorKey = ($errorMsg -replace "Error:", "" -replace "^\s*").Trim()
                if ($errorKey) {
                    if (-not $errors.ContainsKey($errorKey)) {
                        $errors[$errorKey] = 0
                    }
                    $errors[$errorKey]++
                    Write-Host "✗ $($file.Name) - $errorKey" -ForegroundColor Red
                } else {
                    Write-Host "✗ $($file.Name) - Unknown error" -ForegroundColor Yellow
                }
            } else {
                Write-Host "✗ $($file.Name) - No error message" -ForegroundColor Yellow
            }
        }
    } catch {
        Write-Host "✗ $($file.Name) - Exception: $($_.Exception.Message)" -ForegroundColor Magenta
    }
}

Write-Host "`n========================================" -ForegroundColor Cyan
Write-Host "RESULTS: $success / $total ($([math]::Round(($success/$total)*100, 1))%)" -ForegroundColor Cyan
Write-Host "========================================`n" -ForegroundColor Cyan

Write-Host "TOP ERROR PATTERNS:" -ForegroundColor Yellow
$errors.GetEnumerator() | Sort-Object -Property Value -Descending | Select-Object -First 10 | ForEach-Object {
    Write-Host "  $($_.Value)x - $($_.Key)" -ForegroundColor Red
}
