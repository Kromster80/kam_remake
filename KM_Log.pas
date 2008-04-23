unit KM_Log;
interface

uses SysUtils;

type
TKMLog = class
private
logfile:string;
procedure AddLine(line:string);
protected
public
constructor Create(path:string);
procedure AppendLog(line:string); overload;
procedure AppendLog(line:string; num:integer); overload;
procedure AppendLog(num:integer; line:string); overload;
procedure AppendLog(line:string; res:boolean); overload;
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

procedure TKMLog.AddLine(line:string);
begin
assignfile(fl,logfile);
append(fl);
writeln(fl,line);
closefile(fl);
end;

procedure TKMLog.AppendLog(line:string);
begin
AddLine(line);
end;

procedure TKMLog.AppendLog(line:string; num:integer);
begin
AddLine(line+' '+inttostr(num));
end;

procedure TKMLog.AppendLog(num:integer; line:string);
begin
AddLine(inttostr(num)+' '+line);
end;

procedure TKMLog.AppendLog(line:string; res:boolean);
var s:string;
begin
if res then s:='done' else s:='fail';
AddLine(line+' ... '+s);
end;

end.
