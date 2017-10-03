


REM Copy data files from original KaM TPR game
xcopy %KaMDir%\data\defines\* ..\data\defines /y /r /s

REM Copy music files from original KaM TPR game
xcopy "%KaMDir%"\data\sfx\songs\*.sng "%kam_folder%"\Music\*.mp2 /y /r /s


REM ============================================================
REM Copy data folders
REM ============================================================
xcopy ..\Campaigns "%kam_folder%"\Campaigns\ /y /r /s
xcopy ..\data "%kam_folder%"\data\ /y /r /s
xcopy ..\Maps "%kam_folder%"\Maps\ /y /r /s
xcopy ..\MapsMP "%kam_folder%"\MapsMP\ /y /r /s
xcopy ..\Sounds "%kam_folder%"\Sounds\ /y /r /s
xcopy ..\Music "%kam_folder%"\Music\ /y /r /s


REM ============================================================
REM Erase source-code files from copied "data\"
REM ============================================================
REM erase /F /Q /S .\"%kam_folder%"\*.inc


REM ============================================================
REM Copy selected executable files
REM ============================================================
@REM Adding * to the file name supresses the "Is it a file or a folder" query
REM xcopy ..\data.pack .\"%kam_folder%"\data.pack* /y /r /i
xcopy ..\KaM_Remake.exe "%kam_folder%"\KaM_Remake.exe* /y /r /i
xcopy ..\libzplay.dll "%kam_folder%"\libzplay.dll* /y /r /i
xcopy ..\ogg.dll "%kam_folder%"\ogg.dll* /y /r /i
xcopy ..\vorbis.dll "%kam_folder%"\vorbis.dll* /y /r /i
xcopy ..\vorbisfile.dll "%kam_folder%"\vorbisfile.dll* /y /r /i