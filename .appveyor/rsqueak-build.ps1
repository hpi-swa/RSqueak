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

DownloadFile-Rel "https://bitbucket.org/pypy/pypy/get/default.zip" "pypy.zip"
DownloadFile-Rel "https://bitbucket.org/pypy/rsdl/get/default.zip" "rsdl.zip"
DownloadFile-Rel "https://bitbucket.org/pypy/pypy/downloads/pypy-2.5.1-win32.zip" "pypy-win32.zip"
DownloadFile-Rel "http://libsdl.org/release/SDL-devel-1.2.15-VC.zip" "SDL.zip"

Expand-ZIPFile-Rel "pypy.zip" "pypy"
mv pypy/pypy-pypy*/* pypy/
Expand-ZIPFile-Rel "rsdl.zip" "rsdl"
mv rsdl/pypy-rsdl*/* rsdl/
Expand-ZIPFile-Rel "pypy-win32.zip" "pypy-win32"
mv pypy-win32/pypy*win32/* pypy-win32/
Expand-ZIPFile-Rel "SDL.zip" "SDL"
mv SDL/SDL-*/* SDL/

rm pypy.zip
rm rsdl.zip
rm pypy-win32.zip
rm SDL.zip
