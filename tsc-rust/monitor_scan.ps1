# 10-Minute Progress Monitor for Ouroboros Scan
$logFile = "scan_output.log"
$startTime = Get-Date

while ($true) {
    Start-Sleep -Seconds 600  # 10 minutes
    
    $elapsed = ((Get-Date) - $startTime).TotalMinutes
    
    Write-Host "`n=== SCAN PROGRESS UPDATE ($(Get-Date -Format 'HH:mm:ss')) ==="
    Write-Host "Elapsed: $([math]::Round($elapsed, 1)) minutes"
    Write-Host ""
    
    if (Test-Path $logFile) {
        $lines = Get-Content $logFile
        $analyzed = $lines | Select-String "Analyzed (\d+) files" | Select-Object -Last 1
        
        if ($analyzed) {
            Write-Host "Status: $analyzed"
        }
        
        Write-Host "`nRecent activity:"
        Get-Content $logFile -Tail 5 | ForEach-Object { Write-Host "  $_" }
    }
    
    Write-Host "============================================`n"
}
