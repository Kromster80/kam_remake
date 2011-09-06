; For each "Update" build there are two things to change. Revision and Upgradable Versions

; REVISION
#define Revision 'r1234'

; These don't need to change
#define InstallType 'Update'
#define BuildFolder 'BuildUpdate'
#define OutputEXE 'kam_remake_update'

[Code]
// UPGRADABLE VERSIONS
function CheckRemakeVersion(aVersion:string):boolean;
begin
  //Place all Remake versions that are allowed to be upgraded here
  Result := (RemakeVersion = 'r1234') 
        {or (RemakeVersion = 'r1234') 
         or (RemakeVersion = 'r1234') 
         or (RemakeVersion = 'r1234')}
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

[Setup]                     
#include "InstallerLib.iss"
