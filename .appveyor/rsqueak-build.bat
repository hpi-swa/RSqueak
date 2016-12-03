@echo off
if not defined APPVEYOR goto :else1
  if not exist D:\ goto :else2
    set PYTHONDONTWRITEBYTECODE=1
    set PYPY_USESSION_DIR=D:\
    set TEMP=D:\
    set TMP=D:\
    goto :endif2
  :else2
    echo "Running on appveyor, but there is no drive D."
    fsutil fsinfo drives
    echo "Maybe you want to put the tempdir where the pagefile is (assuming that is the fastest drive)"
    powershell -NonInteractive -command "& { gwmi -computer . Win32_PageFileUsage }"
  :endif2
  goto :endif1
:else1
  echo "Not running this on appveyor? You should use .build\build.py"
:endif1

if not defined buildscript (
  echo You need to set the buildscript var
  set errorlevel=1
  goto :endofscript
)

set BINURL=https://www.hpi.uni-potsdam.de/hirschfeld/artefacts/rsqueak/rsqueak-win32-%APPVEYOR_REPO_COMMIT%.exe
if %buildscript%==jittests.py (
  echo Downloding %BINURL% for jittests
  appveyor DownloadFile -Url %BINURL% -FileName $~dp0\..\rsqueak.exe
  if errorlevel 1 (
    echo Download failed, cannot run jittests
    set errorlevel=1
    goto :endofscript
  )
)

@echo on
set SDL_VIDEODRIVER=dummy
C:\Python27\python %~dp0\..\.build\%buildscript% %buildargs%
if %ERRORLEVEL% GEQ 1 EXIT /B 1

if %buildscript%==build.py (
  copy %~dp0\..\rsqueak.exe %~dp0\..\rsqueak-win32-%APPVEYOR_REPO_COMMIT%.exe
)

if "%buildargs%"=="-- --plugins=RubyPlugin" (
  copy %~dp0\..\rsqueak.exe %~dp0\..\rsqueak-win32-RubyPlugin.exe
  del %~dp0\..\rsqueak-win32-%APPVEYOR_REPO_COMMIT%.exe
)

if %buildscript%==build.py (
  powershell -NonInteractive -executionpolicy Unrestricted -command "& { %~dp0\rsqueak-upload.ps1 }"
)

:endofscript
