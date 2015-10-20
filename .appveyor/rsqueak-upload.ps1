function uploadRsqueak {
  [CmdletBinding()]
  param([string] $pattern)
  $loc = Get-Location
  $provider = $loc.Provider
  $files = @()
  $longfiles = $pscmdlet.GetResolvedProviderPathFromPSPath($pattern, [ref] $provider)
  $longfiles | % { $file = gci $_ ; $files += $file.Name }
  foreach($item in $files) {
    $url = "https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/commits/$item"
    $fullpath = Join-Path $pwd $item
    break
  }
  Write-Host "Uploading $fullpath to $url"
  $bytes = [System.Text.Encoding]::ASCII.GetBytes($env:DeployCredentials)
  $base64 = [System.Convert]::ToBase64String($bytes)
  $basicAuthValue = "Basic $base64"
  $headers = @{ Authorization = $basicAuthValue }
  Invoke-WebRequest -Method PUT -Uri $url -InFile $fullpath -Headers $headers
}

function uploadRsqueakLatest {
  [CmdletBinding()]
  param([string] $pattern)
  $loc = Get-Location
  $provider = $loc.Provider
  $files = @()
  $longfiles = $pscmdlet.GetResolvedProviderPathFromPSPath($pattern, [ref] $provider)
  $longfiles | % { $file = gci $_ ; $files += $file.Name }
  foreach($item in $files) {
    $url = "https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/$item"
    $fullpath = Join-Path $pwd $item
    break
  }
  Write-Host "Uploading $fullpath to $url"
  $bytes = [System.Text.Encoding]::ASCII.GetBytes($env:DeployCredentials)
  $base64 = [System.Convert]::ToBase64String($bytes)
  $basicAuthValue = "Basic $base64"
  $headers = @{ Authorization = $basicAuthValue }
  Invoke-WebRequest -Method PUT -Uri $url -InFile $fullpath -Headers $headers
}

uploadRsqueak "rsqueak-win32-*.exe"
cp rsqueak.exe rsqueak-win32-latest.exe
uploadRsqueakLatest rsqueak-win32-latest.exe
