if ($IsWindows) {
  $myHome = "$env:USERPROFILE"
  $env:PATH = "$env:USERPROFILE\bin;$env:PATH"
  $env:PATH += ';C:\Program Files (x86)\Midnight Commander'
} 
if ($IsLinux) {
  $myHome = "$HOME"
  $env:PATH = "$HOME/bin:/home/linuxbrew/.linuxbrew/bin:$env:PATH"
  mise activate pwsh | Out-String | Invoke-Expression
}

function Is-JJ-Repo {
  $dir = Get-Location

  if ($IsWindows) {
    $homeDir = [System.IO.Path]::GetFullPath($env:USERPROFILE)
  } else {
    $homeDir = $HOME
  }

  while ([System.IO.Path]::GetFullPath($dir).StartsWith($homeDir)) {
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

function cdw { Set-Location $myHome\working }
function cdd { Set-Location $myHome\Downloads }
function cdf { Set-Location $myHome\.dotfiles }

Set-Alias -Name vi -Value nvim

