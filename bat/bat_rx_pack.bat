REM Copy rx resorces from original game
xcopy ..\%KaMDir%\data\gfx\res\* ..\SpriteResource /y /r /s

xcopy ..\data\Sprites /y /r /s

REM Pack rx textures to rxx 
call ..\Utils\RXXPacker\RXXPacker.exe gui guimain houses trees units
