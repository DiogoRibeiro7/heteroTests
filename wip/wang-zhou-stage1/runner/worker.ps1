param(
  [string]$Queue,
  [string]$OutDir,
  [string]$Script,
  [string]$NMc = "1000",
  [string]$NMcGate = "1000"
)
# Takes cells from the queue one at a time; a cell is claimed by creating
# claim-<i> exclusively. Before claiming, waits while free RAM is below 4 GB.
$ErrorActionPreference = "Stop"
$items = Get-Content $Queue
foreach ($i in $items) {
  $claim = Join-Path $OutDir "claim-$i"
  if (Test-Path $claim) { continue }
  while ($true) {
    $free = (Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory / 1MB
    if ($free -ge 4) { break }
    Add-Content -Path (Join-Path $OutDir "ram.txt") -Value "waiting, free $([math]::Round($free,1)) GB $(Get-Date -Format s)"
    Start-Sleep -Seconds 60
  }
  try {
    $fs = [System.IO.File]::Open($claim, [System.IO.FileMode]::CreateNew)
    $fs.Close()
  } catch {
    continue
  }
  $env:N_MC = $NMc
  $env:N_MC_GATE = $NMcGate
  $env:MC_CORES = "1"
  $env:SCENARIOS = "$i"
  $env:CSV = Join-Path $OutDir "part-$i.csv"
  $log = Join-Path $OutDir "part-$i.log"
  & Rscript $Script *> $log
  Add-Content -Path (Join-Path $OutDir "done.txt") -Value "$i exit=$LASTEXITCODE $(Get-Date -Format s)"
}
