param([int]$MaxLoops = 6)
# Starts a pass-2 queue worker whenever fewer than $MaxLoops worker loops of
# this task (pass 1 or pass 2) are alive and pass 2 still has unclaimed cells.
# Each worker loop runs at most one R process at a time, and waits for 4 GB of
# free RAM before it claims a cell.
$sp = "C:\Users\diogo\AppData\Local\Temp\claude\c--Users-diogo-work-code-heteroTests\2af77d20-4404-4ce0-91a0-04d3bce97fda\scratchpad\t-wz"
$wd = "C:\Users\diogo\work_code\heteroTests\.claude\worktrees\agent-a7f10e44f02cb85ab"
$dir = "$sp\pass2"
$total = (Get-Content "$dir\queue.txt").Count
while ($true) {
  $loops = @(Get-CimInstance Win32_Process -Filter "Name='pwsh.exe'" | Where-Object {
    $_.CommandLine -like '*t-wz\worker.ps1*'
  }).Count
  $claimed = @(Get-ChildItem $dir -Filter "claim-*").Count
  if ($claimed -ge $total) { break }
  if ($loops -lt $MaxLoops) {
    Start-Process -FilePath pwsh -ArgumentList "-NoProfile", "-File", "$sp\worker.ps1", "-Queue", "$dir\queue.txt", "-OutDir", $dir, "-Script", "inst/validation/variance-form-wz-size-power.R", "-NMc", "1000", "-NMcGate", "5000" -WorkingDirectory $wd -WindowStyle Hidden
    Add-Content "$sp\launcher-pass2.log" "pass-2 worker started $(Get-Date -Format s), loops before $loops, claimed $claimed"
    Start-Sleep -Seconds 45
  } else {
    Start-Sleep -Seconds 30
  }
}
Add-Content "$sp\launcher-pass2.log" "launcher done $(Get-Date -Format s)"
