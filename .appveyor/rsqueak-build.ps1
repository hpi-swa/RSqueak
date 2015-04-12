echo "Building rsqueak.exe in $pwd"

function Expand-ZIPFile-Rel($file, $destination) {
    $f = Join-Path $pwd $file
    $d = Join-Path $pwd $destination
    mkdir $d
    $shell = new-object -com shell.application
    $zip = $shell.NameSpace($f)
    Write-Host -NoNewline "Extracting $file "
    foreach($item in $zip.items()) {
	Write-Host -NoNewline "."
	$shell.Namespace($d).copyhere($item)
    }
    Write-Host " done"
}

function DownloadFile-Rel($url, $destination) {
    Write-Host -NoNewline "Downloading $destination "
    $d = Join-Path $pwd $destination
    (New-Object Net.WebClient).DownloadFile($url, $d)
    Write-Host " . done"
}

function EnsureDep($url, $zip, $folder, $subDir) {
   if (!(Test-Path $folder)) {
      DownloadFile-Rel $url $zip
      Expand-ZIPFile-Rel $zip $folder
      mv $folder/$subdir/* $folder/
      rm $zip
   }
}

EnsureDep "https://bitbucket.org/pypy/pypy/get/default.zip" "pypy.zip" "pypy" "pypy-pypy*"
EnsureDep "https://bitbucket.org/pypy/rsdl/get/default.zip" "rsdl.zip" "rsdl" "pypy-rsdl*"
EnsureDep "https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.1-win32.zip" "pypy-win32.zip" "pypy-win32" "pypy*win32"
EnsureDep "http://libsdl.org/release/SDL-devel-1.2.15-VC.zip" "SDL.zip" "SDL" "SDL-*"
