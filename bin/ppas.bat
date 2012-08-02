@echo off
SET THEFILE=C:\Users\hinst\Docs\Pro\DBFF\bin\DBFF_Executable_i386_win32.exe
echo Linking %THEFILE%
C:\FPC\2.6.0\bin\i386-win32\ld.exe -b pei-i386 -m i386pe  --gc-sections    --entry=_mainCRTStartup    -o C:\Users\hinst\Docs\Pro\DBFF\bin\DBFF_Executable_i386_win32.exe C:\Users\hinst\Docs\Pro\DBFF\bin\link.res
if errorlevel 1 goto linkend
C:\FPC\2.6.0\bin\i386-win32\postw32.exe --subsystem console --input C:\Users\hinst\Docs\Pro\DBFF\bin\DBFF_Executable_i386_win32.exe --stack 16777216
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occured while assembling %THEFILE%
goto end
:linkend
echo An error occured while linking %THEFILE%
:end
