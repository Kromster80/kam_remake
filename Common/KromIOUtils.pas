unit KromIOUtils;
{$IFDEF VER140} {$DEFINE WDC} {$ENDIF}  // Delphi 6
{$IFDEF VER150} {$DEFINE WDC} {$ENDIF}  // Delphi 7
interface
uses ShellAPI, SysUtils;

function CopyDir(const fromDir, toDir: string): Boolean;
function DelDir(dir: string): Boolean;

function ReturnSize(i: int64): string;

implementation

function CopyDir(const fromDir, toDir: string): Boolean;
var fos: TSHFileOpStruct;
begin
  FillChar(fos, SizeOf(fos), #0);
  fos.wFunc  := FO_COPY;
  fos.fFlags := FOF_FILESONLY;
  fos.pFrom  := PChar(fromDir + #0);
  fos.pTo    := PChar(toDir);
  Result := ShFileOperation(fos) = 0;
end;

function DelDir(dir: string): Boolean;
var fos: TSHFileOpStruct;
begin
  FillChar(fos, SizeOf(fos), #0);
  fos.wFunc  := FO_DELETE;
  fos.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  fos.pFrom  := PChar(dir + #0);
  Result := ShFileOperation(fos) = 0;
end;

function ReturnSize(i: int64): string;
begin
  if i<2048 then Result:=inttostr(i)+'b'
  else
  if i<2048*1000 then Result:=inttostr(i shr 10)+'Kb'
  else
  if i<2048*1000000 then Result:=inttostr(i shr 20)+'Mb'
  else
  Result:=inttostr(i shr 30)+'Gb';
end;

end.
 