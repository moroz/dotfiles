$env:PATH += ';C:\Program Files (x86)\Midnight Commander'

function Is-JJ-Repo {
  $dir = Get-Location

  $homeDir = [System.IO.Path]::GetFullPath($env:USERPROFILE)

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
    jj st $($PSCmdlet.MyInvocation.MyCommand.Parameters)
  } else {
    git status $($PSCmdlet.MyInvocation.MyCommand.Parameters)
  }
}

function gc {
  if (Is-JJ-Repo) {
    jj commit $($PSCmdlet.MyInvocation.MyCommand.Parameters)
  } else {
    git commit $($PSCmdlet.MyInvocation.MyCommand.Parameters)
  }
}

function cdw { Set-Location $Env:USERPROFILE\working }
function cdd { Set-Location $Env:USERPROFILE\Downloads }

Set-Alias -Name vi -Value nvim
