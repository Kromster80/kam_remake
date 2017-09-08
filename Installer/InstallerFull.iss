; For each "Full" build there is only one thing to change: Revision

; REVISION
#define Revision 'r7000+'

; These don't need to change
#define InstallType 'Full'
#define BuildFolder 'BuildFull'
#define OutputEXE 'kam_remake_full'

[Code]
function CanInstall():boolean;
begin
  Result := true; //Complete version can ALWAYS be installed (this function is used for the update installer only)
end;

function CanUpdate():boolean;
begin
  Result := true; //Complete version can ALWAYS be updated (this function is used for the update installer only)
end;

[Setup]
DirExistsWarning=yes
#include "InstallerLib.iss"
