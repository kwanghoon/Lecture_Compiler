# Find files recursively and run a command with them
# Example) ./findtorun.ps1 'stack run knorm' .\app\test "\.ml$"
#

param (
    [string]$cmd,
    [string]$path,
    [string]$pattern
)

if (-not $path -or -not $pattern) {
    Write-Host "Usage: .\find_files.ps1 <cmd> <path> <regex_pattern>"
    exit 1
}

# Get matching files recursively
$fileNames = Get-ChildItem -Path $path -Recurse -File | Where-Object { $_.Name -match $pattern } | Select-Object -ExpandProperty FullName

# Iterate over each file and execute the command
foreach ($file in $fileNames) {
    Invoke-Expression "$cmd `"$file`""
}