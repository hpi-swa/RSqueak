@echo off
if not defined APPVEYOR goto :else1
  if not exist D:\ goto :else2
    set PYPY_USESSION_DIR=D:\
    set TEMP=D:\
    set TMP=D:\
    goto :endif2
  :else2
    echo "Running on appveyor, but there is no drive D."
  :endif2
  goto :endif1
:else1
  echo "Not running this on appveyor? You should use .build\build.py"
:endif1

fsutil fsinfo drives
echo "Maybe you want to put the tempdir where the pagefile is (assuming that is the fastest drive)"
powershell -NonInteractive -command "& { gwmi -computer . Win32_PageFileUsage }"

@echo on
%~dp0\..\.build\pypy-win32\pypy.exe %~dp0\..\.build\build.py
copy %~dp0\..\rsqueak.exe %~dp0\..\rsqueak-win32%OPTLVL%-%APPVEYOR_REPO_COMMIT%.exe
