REM ============================================================
REM Create new build folder
REM ============================================================

@echo off
for /f "delims=" %%a in ('wmic OS Get localdatetime  ^| find "."') do set dt=%%a
set datestamp=%dt:~0,8%
set timestamp=%dt:~8,6%
set YYYY=%dt:~0,4%
set MM=%dt:~4,2%
set DD=%dt:~6,2%
set HH=%dt:~8,2%
set Min=%dt:~10,2%
set Sec=%dt:~12,2%

set stamp=%YYYY%-%MM%-%DD%_%HH%-%Min%-%Sec%
echo stamp: "%stamp%"
echo datestamp: "%datestamp%"
echo timestamp: "%timestamp%"

REM Read last line from the file
REM for /F "delims=" %%i in (KM_Revision.inc) do set "verinfo=%%i"

REM Replace ' with nothing to get the version info
REM @SET kam_revision=%verinfo:'=%
@SET kam_revision=8

REM Now we can have a constant with the right folder name
@SET kam_folder=%BetaVersionsDir%\kam%YYYY%-%MM%-%DD% (%kam_version% rev%kam_revision%)
echo kam_folder: "%kam_folder%"
