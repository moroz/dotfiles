Invoke-Expression (& { (zoxide init powershell | Out-String) })

if ($Env:OS -eq "Windows_NT") {
  $env:PATH = "$env:USERPROFILE\bin;C:\Program Files\LLVM\bin;$env:PATH"
  $env:PATH += ';C:\Program Files (x86)\Midnight Commander'
  $env:PATH += ';C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.43.34808\bin\Hostx86\x86'
} 

if ($IsLinux -or $IsMacOs) {
  $env:PATH = "$HOME/bin:/home/linuxbrew/.linuxbrew/bin:$env:PATH"
}

if ($IsMacOs) {
  $env:PATH = "$HOME/bin:/opt/homebrew/bin:$env:PATH"
}

if ($IsLinux -or $IsMacOs) {
  mise activate pwsh | Out-String | Invoke-Expression
}

function Is-JJ-Repo {
  $dir = Get-Location

  while ([System.IO.Path]::GetFullPath($dir).StartsWith($HOME)) {
    $jjDir = Join-Path -Path $dir -ChildPath '.jj'

    if (Test-Path -Path $jjDir -PathType Container) {
      return $true;
    }

    $dir = Split-Path -Parent $dir
  }

  return $false;
}

function gs {
  if (Is-JJ-Repo) {
    jj st @args
  } else {
    git status @args
  }
}

del alias:gc -Force
del alias:cd -Force
function gc {
  if (Is-JJ-Repo) {
    jj commit @args
  } else {
    git commit @args
  }
}

function gd {
  if (Is-JJ-Repo) {
    jj diff @args
  } else {
    git diff @args
  }
}

function gpd {
  if (Is-JJ-Repo) {
    jj git push --allow-new -r '@-'
  } else {
    git push -u origin HEAD
  }
}

function cdw { Set-Location $HOME\working }
function cdd { Set-Location $HOME\Downloads }
function cdf { Set-Location $HOME\.dotfiles }
function gb { jj bookmark move --to '@-' @args }

Set-Alias -Name vi -Value nvim
Set-Alias -Name cd -Value z
Set-PSReadlineOption -EditMode vi

Set-PSReadLineOption -Colors @{
  Operator  = $Host.UI.RawUI.ForegroundColor
  Parameter = $Host.UI.RawUI.ForegroundColor
  Command   = $Host.UI.RawUI.ForegroundColor
}
