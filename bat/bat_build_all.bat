@REM ============================================================
@REM Build everything in the project group
@REM ============================================================

@REM msbuild needs variables set by rsvars
call bat_rsvars.bat

@REM build everything
msbuild KaMProjectGroup.groupproj /p:Configuration=Debug /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_all.log"