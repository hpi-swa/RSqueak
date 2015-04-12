%WINDIR%\System32\WindowsPowerShell\v1.0\powershell.exe -NonInteractive -executionpolicy Unrestricted -command "& { .\rsqueak-build.ps1 }"
set SDL_PREFIX=%~dp0\SDL
set PYTHONPATH=%~dp0\;%~dp0\pypy;%~dp0\rsdl
echo %SDL_PREFIX%
echo %PYTHONPATH%
%~dp0\pypy-win32\pypy.exe %~dp0\pypy\rpython\bin\rpython --batch %OPTLVL% %~dp0\targetrsqueak.py
copy %~dp0\rsqueak.exe %~dp0\rsqueak-win32%OPTLVL%-%APPVEYOR_REPO_COMMIT%.exe
