# winget install --id Microsoft.PowerShell --source winget
# Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser

$target = "$env:USERPROFILE\.dotfiles\nvim"
$link = "$env:USERPROFILE\AppData\Local\nvim"

if (-not (Test-Path -Path $link -PathType Leaf) -and -not (Test-Path -Path $link -PathType Container)) {
    New-Item -ItemType SymbolicLink -Path $link -Target $target
}

$target = "$env:USERPROFILE\.dotfiles\Microsoft.PowerShell_profile.ps1"
$linkDir = split-path $profile
$link = Join-Path $linkDir "Microsoft.PowerShell_profile.ps1"

if (-not (Test-Path -Path $link)) {
    New-Item -ItemType Directory -Force -Path $linkDir | Out-Null
    New-Item -ItemType SymbolicLink -Path $link -Target $target
}

$target = "$env:USERPROFILE\.dotfiles\.ideavimrc"
$link = "$env:USERPROFILE\.ideavimrc"

if (-not (Test-Path -Path $link)) {
    New-Item -ItemType SymbolicLink -Path $link -Target $target
}
