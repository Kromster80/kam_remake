; For each "Update" build there are two things to change. Revision and Upgradable Versions

; REVISION
#define Revision 'r2678'

; These don't need to change
#define InstallType 'Update'
#define BuildFolder 'BuildUpdate'
#define OutputEXE 'kam_remake_update'

[Code]
// UPGRADABLE VERSIONS
function CheckRemakeVersion(aVersion:string):boolean;
begin
  //Place all Remake versions that are allowed to be upgraded here
  Result := (aVersion = 'r2411') 
        {or (aVersion = 'r1234') 
         or (aVersion = 'r1234') 
         or (aVersion = 'r1234')}
         ; 
end;

function CanInstall():boolean;
var RemakeVersion:string;
begin
  Result := False;
  if RegValueExists(HKLM, 'SOFTWARE\JOYMANIA Entertainment\KnightsandMerchants TPR', 'RemakeVersion') then
  begin
    RegQueryStringValue(HKLM, 'SOFTWARE\JOYMANIA Entertainment\KnightsandMerchants TPR', 'RemakeVersion', RemakeVersion);

    if CheckRemakeVersion(RemakeVersion) or
       (RemakeVersion = ExpandConstant('{#Revision}')) then //Allow reinstalling of the same version
    begin
      Result := true;
    end;
  end;
end;

[Run]
Filename: "{app}\Clean.bat"; WorkingDir: "{app}"; Flags: runhidden

[Setup]
EnableDirDoesntExistWarning=yes
CreateUninstallRegKey=no
#include "InstallerLib.iss"

[Code]
function PrepareToInstall(var NeedsRestart: Boolean): String;
var InstallFolder: string;
begin
  Result := '';
  InstallFolder := ExpandConstant('{app}');
  if(not(
     FileExists(InstallFolder+'\KaM_Remake.exe') and
     FileExists(InstallFolder+'\data\defines\houses.dat') and
     FileExists(InstallFolder+'\data\gfx\res\guimainh.rx') and
     FileExists(InstallFolder+'\data\gfx\res\units.rx') and
     FileExists(InstallFolder+'\Resource\Tiles1.tga')
     )) then
     Result := ExpandConstant('{cm:CantUpdate}');
end;
