REM ============================================================
REM Build main exe
REM ============================================================

REM msbuild needs variables set by rsvars
call bat_rsvars.bat

REM build
msbuild ..\KaM_Remake.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_exe.log"

REM build
REM msbuild utils\ScriptValidator\ScriptValidator.dproj /p:Configuration=Debug /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_ScriptValidator_exe.log"