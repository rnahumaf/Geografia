$ErrorActionPreference = "Stop"
$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location -LiteralPath $ProjectRoot

npm run build
& ".venv\Scripts\python.exe" -m pip install -r requirements-app.txt -r requirements-gpu-cuda13.txt pyinstaller==6.15.0
& ".venv\Scripts\python.exe" -m PyInstaller `
  --noconfirm `
  --clean `
  --onedir `
  --windowed `
  --name "Itupararanga-Flood-Lab" `
  --add-data "dist-web;dist-web" `
  --collect-all keyring `
  --collect-all webview `
  --collect-all cupy `
  --collect-all cupy_backends `
  --collect-all cuda_pathfinder `
  --hidden-import graphlib `
  app_server.py

Write-Host "Aplicativo gerado em dist\Itupararanga-Flood-Lab\Itupararanga-Flood-Lab.exe"
