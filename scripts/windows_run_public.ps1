param(
  [switch]$InstallPackages = $false,
  [switch]$RenderAssets = $false,
  [switch]$RunApp = $false
)

$ErrorActionPreference = "Stop"

function Find-Rscript {
  $candidates = @()

  if ($env:R_HOME) {
    $candidates += (Join-Path $env:R_HOME "bin\Rscript.exe")
    $candidates += (Join-Path $env:R_HOME "bin\x64\Rscript.exe")
  }

  $candidates += @(
    "C:\Program Files\R\R-4.4.3\bin\Rscript.exe",
    "C:\Program Files\R\R-4.4.3\bin\x64\Rscript.exe",
    "C:\Program Files\R\R-4.4.2\bin\Rscript.exe",
    "C:\Program Files\R\R-4.4.2\bin\x64\Rscript.exe",
    "C:\Program Files\R\R-4.4.1\bin\Rscript.exe",
    "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe",
    "C:\Program Files\R\R-4.4.0\bin\Rscript.exe",
    "C:\Program Files\R\R-4.4.0\bin\x64\Rscript.exe"
  )

  foreach ($candidate in $candidates | Select-Object -Unique) {
    if ($candidate -and (Test-Path $candidate)) {
      return $candidate
    }
  }

  $discovered = Get-ChildItem "C:\Program Files\R" -Recurse -Filter "Rscript.exe" -ErrorAction SilentlyContinue |
    Select-Object -First 1 -ExpandProperty FullName

  if ($discovered) {
    return $discovered
  }

  throw "Rscript.exe was not found. Open this repo in RStudio and run source('scripts/dev_run.R'), or install R 4.4 and try again."
}

function Invoke-RScriptFile {
  param(
    [string]$RscriptPath,
    [string]$ScriptPath
  )

  Write-Host "Running $ScriptPath" -ForegroundColor Cyan
  & $RscriptPath --vanilla $ScriptPath

  if ($LASTEXITCODE -ne 0) {
    throw "Failed while running $ScriptPath"
  }
}

$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

$rscript = Find-Rscript
Write-Host "Using Rscript: $rscript" -ForegroundColor Green

if (-not $InstallPackages -and -not $RenderAssets -and -not $RunApp) {
  $RenderAssets = $true
  $RunApp = $true
}

if ($InstallPackages) {
  Invoke-RScriptFile -RscriptPath $rscript -ScriptPath "scripts/windows_install_public_packages.R"
}

if ($RenderAssets) {
  Invoke-RScriptFile -RscriptPath $rscript -ScriptPath "scripts/render_public_assets.R"
}

if ($RunApp) {
  Invoke-RScriptFile -RscriptPath $rscript -ScriptPath "scripts/dev_run.R"
}
