unit KM_Log;
interface

uses SysUtils, Windows;

type
TKMLog = class
private
logfile:string;
PreviousTick:cardinal;
procedure AddLine(text:string);
protected
public
constructor Create(path:string);
procedure AppendLog(text:string); overload;
procedure AppendLog(text:string; num:integer); overload;
procedure AppendLog(num:integer; text:string); overload;
procedure AppendLog(text:string; res:boolean); overload;
published
end;

var fl: textfile;

implementation

constructor TKMLog.Create(path:string);
begin
logfile:=path;
assignfile(fl,logfile);
rewrite(fl);
closefile(fl);
end;

procedure TKMLog.AddLine(text:string);
var Delta:cardinal;
begin
Delta:=GetTickCount - PreviousTick;
PreviousTick:=GetTickCount;
if Delta>100000 then Delta:=0; //ommit first usage
assignfile(fl,logfile);
append(fl);
writeln(fl,inttostr(Delta)+'ms'+#9+text);
closefile(fl);
end;

procedure TKMLog.AppendLog(text:string);
begin
AddLine(text);
end;

procedure TKMLog.AppendLog(text:string; num:integer);
begin
AddLine(text+' '+inttostr(num));
end;

procedure TKMLog.AppendLog(num:integer; text:string);
begin
AddLine(inttostr(num)+' '+text);
end;

procedure TKMLog.AppendLog(text:string; res:boolean);
var s:string;
begin
if res then s:='done' else s:='fail';
AddLine(text+' ... '+s);
end;

end.
