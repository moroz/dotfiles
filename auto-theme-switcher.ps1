#!/usr/bin/env pwsh

# Auto Dark/Light Mode Switcher for Linux (PowerShell Core)
# Switches to dark mode at sunset and light mode at sunrise
# Vibe-coded by Claude Sonnet 4
# Usage: pwsh ./Auto-Theme-Switcher.ps1 [-ForceLight] [-ForceDark] [-Check]

param(
    [switch]$ForceLight,
    [switch]$ForceDark,
    [switch]$Check
)

# Configuration
$Latitude = "52.2297"   # Warsaw latitude - change to your location
$Longitude = "21.0122"  # Warsaw longitude - change to your location
$LightWallpaper = "/home/karol/Dropbox/wallpapers/desert_sands/desert_sands_3.png"
$DarkWallpaper = "/home/karol/Dropbox/wallpapers/desert_sands/desert_sands_1.png"
$LogFile = "$HOME/.local/share/theme-switcher.log"

# Logging function
function Write-Log {
    param([string]$Message)
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    Write-Host "$Timestamp - $Message"
}


function Set-Wallpaper() {
    param([string]$WallpaperPath)
    & /usr/bin/gsettings set org.cinnamon.desktop.background picture-uri \'file://${WallpaperPath}\'
}

# Light theme function
function Set-LightTheme {
    try {
        & dconf write /org/cinnamon/desktop/interface/gtk-theme "'Mint-Y-Aqua'"
        & dconf write /org/cinnamon/desktop/wm/preferences/theme "'Mint-Y-Aqua'"
        & dconf write /org/cinnamon/theme/name "'Mint-Y-Aqua'"
        & dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
        & dconf write /org/x/apps/portal/color-scheme "'prefer-light'"
        & dconf write /org/gnome/desktop/interface/gtk-theme "'Mint-Y-Aqua'"
        Set-Wallpaper -WallpaperPath $LightWallpaper
        Write-Log "Switched to light theme"
    }
    catch {
        Write-Log "Error switching to light theme: $($_.Exception.Message)"
    }
}

# Dark theme function
function Set-DarkTheme {
    try {
        & dconf write /org/cinnamon/desktop/interface/gtk-theme "'Mint-Y-Dark-Aqua'"
        & dconf write /org/cinnamon/desktop/wm/preferences/theme "'Mint-Y-Dark-Aqua'"
        & dconf write /org/cinnamon/theme/name "'Mint-Y-Dark-Aqua'"
        & dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
        & dconf write /org/x/apps/portal/color-scheme "'prefer-dark'"
        & dconf write /org/gnome/desktop/interface/gtk-theme "'Mint-Y-Dark-Aqua'"
        Set-Wallpaper -WallpaperPath $DarkWallpaper
        Write-Log "Switched to dark theme"
    }
    catch {
        Write-Log "Error switching to dark theme: $($_.Exception.Message)"
    }
}

# Get sunrise/sunset times using REST API
function Get-SunTimes {
    try {
        $Uri = "https://api.sunrise-sunset.org/json?lat=$Latitude&lng=$Longitude&formatted=0"
        $Response = Invoke-RestMethod -Uri $Uri -Method Get -TimeoutSec 10
        
        if ($Response.status -eq "OK") {
            # Convert UTC times to local time
            $SunriseUtc = [DateTime]::Parse($Response.results.sunrise)
            $SunsetUtc = [DateTime]::Parse($Response.results.sunset)
            
            $script:SunriseTime = $SunriseUtc.ToString("HH:mm")
            $script:SunsetTime = $SunsetUtc.ToString("HH:mm")
            
            Write-Log "Retrieved sun times - Sunrise: $script:SunriseTime, Sunset: $script:SunsetTime"
            return $true
        }
    }
    catch {
        Write-Log "Error fetching sunrise/sunset times: $($_.Exception.Message)"
    }
    
    # Fallback times if API fails
    Write-Log "Warning: Could not fetch sunrise/sunset times, using fallback"
    $script:SunriseTime = "07:00"
    $script:SunsetTime = "19:00"
    return $false
}

# Check if it's currently daytime
function Test-IsDaytime {
    $CurrentTime = Get-Date -Format "HH:mm"
    
    # Convert times to minutes since midnight for comparison
    $SunriseMinutes = ([DateTime]::ParseExact($script:SunriseTime, "HH:mm", $null)).Hour * 60 + ([DateTime]::ParseExact($script:SunriseTime, "HH:mm", $null)).Minute
    $SunsetMinutes = ([DateTime]::ParseExact($script:SunsetTime, "HH:mm", $null)).Hour * 60 + ([DateTime]::ParseExact($script:SunsetTime, "HH:mm", $null)).Minute
    $CurrentMinutes = ([DateTime]::ParseExact($CurrentTime, "HH:mm", $null)).Hour * 60 + ([DateTime]::ParseExact($CurrentTime, "HH:mm", $null)).Minute
    
    return ($CurrentMinutes -ge $SunriseMinutes -and $CurrentMinutes -lt $SunsetMinutes)
}

# Main execution logic
try {
    # Handle command line switches
    if ($ForceLight) {
        Set-LightTheme
        exit 0
    }
    elseif ($ForceDark) {
        Set-DarkTheme
        exit 0
    }
    elseif ($Check) {
        Get-SunTimes | Out-Null
        Write-Host "Current time: $(Get-Date -Format 'HH:mm')"
        Write-Host "Sunrise: $script:SunriseTime"
        Write-Host "Sunset: $script:SunsetTime"
        if (Test-IsDaytime) {
            Write-Host "Status: Daytime (should use light theme)"
        }
        else {
            Write-Host "Status: Nighttime (should use dark theme)"
        }
        exit 0
    }
    
    # Ensure DISPLAY is set for GUI operations
    if (-not $env:DISPLAY) {
        $env:DISPLAY = ":0"
    }
    
    # Get current sun times
    Get-SunTimes | Out-Null
    
    # Apply appropriate theme
    if (Test-IsDaytime) {
        Set-LightTheme
    }
    else {
        Set-DarkTheme
    }
}
catch {
    Write-Log "Fatal error: $($_.Exception.Message)"
    Write-Error "Script execution failed. Check log file: $LogFile"
    exit 1
}
