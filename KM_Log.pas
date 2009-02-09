unit KM_Log;
interface

uses SysUtils, Windows;

{This is custom logging system}
type
TKMLog = class
private
  fl:textfile;
  logfile:string;
  PreviousTick:cardinal;
  procedure AddLine(text:string);
public
  constructor Create(path:string);
  //Various kinds of entries
  procedure AppendLog(text:string); overload;
  procedure AppendLog(text:string; num:integer); overload;
  procedure AppendLog(num:integer; text:string); overload;
  procedure AppendLog(text:string; Res:boolean); overload;
  procedure AppendLog(a,b:integer); overload;
published
end;

var
  fLog: TKMLog;


implementation

{Reset log file}
constructor TKMLog.Create(path:string);
begin
  logfile:=path;
  assignfile(fl,logfile);
  rewrite(fl);
  closefile(fl);
  AppendLog('Log is up and running');
end;

{Lines are timestamped, each line invokes file open/close for writing,
meaning no lines will be lost if Remake crashes}
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

procedure TKMLog.AppendLog(text:string; Res:boolean);
var s:string;
begin
  if Res then s:='done' else s:='fail';
  AddLine(text+' ... '+s);
end;

procedure TKMLog.AppendLog(a,b:integer);
begin
  AddLine(inttostr(a)+' : '+inttostr(b));
end;

end.
