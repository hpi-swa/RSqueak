C:\Python27\python %~dp0\..\.build\download_dependencies.py
%~dp0\..\.build\pypy-win32\pypy.exe %~dp0\..\.build\build.py
copy %~dp0\rsqueak.exe %~dp0\rsqueak-win32%OPTLVL%-%APPVEYOR_REPO_COMMIT%.exe
