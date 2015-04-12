function uploadRsqueak {
  [CmdletBinding()]
  param([string] $pattern)
  $loc = Get-Location
  $provider = $loc.Provider
  $files = @()
  $longfiles = $pscmdlet.GetResolvedProviderPathFromPSPath($pattern, [ref] $provider)
  $longfiles | % { $file = gci $_ ; $files += $file.Name }
  foreach($item in $files) {
    $url = "http://www.lively-kernel.org/babelsberg/RSqueak/$item"
    $fullpath = Join-Path $pwd $item
    break
  }
  Write-Host "Uploading $fullpath to $url"
  Invoke-WebRequest -Method PUT -Uri $url -InFile $fullpath
}

uploadRsqueak "rsqueak-win32-*.exe"
