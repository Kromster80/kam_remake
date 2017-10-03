


REM build RXXPacker
msbuild ..\Utils\RXXPacker\RXXPacker.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_rxxpacker.log"

REM Copy rx resorces from original game
xcopy "%KaMDir%\data\gfx\res" ..\SpriteResource\ /y /r /s
xcopy "%KaMDir%\data\gfx\*" ..\data\gfx /y /r 

@REM Backup local rxx files
xcopy ..\data\Sprites ..\data\_Sprites\ /y /r /s
@REM Delete local rxx, they could contain wrong textures
rmdir /S /Q ..\data\Sprites ..\dcu

REM Pack rx textures to rxx 
@REM call ..\Utils\RXXPacker\RXXPacker.exe gui guimain houses trees units
call ..\Utils\RXXPacker\RXXPacker.exe all


