program DWScript_Test1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  dwsComp, dwsCompiler, dwsExprs, dwsSymbols;

procedure Execute(aText: string);
var
  DWS: TDelphiWebScript;
  dwsUnit: TdwsUnit;
  prog: IdwsProgram;
  exec: IdwsProgramExecution;
begin
  DWS := TDelphiWebScript.Create(nil);
  dwsUnit := TdwsUnit.Create(nil);
  dwsUnit.UnitName := 'Test';
  try
    dwsUnit.Script := DWS;

    prog := DWS.Compile(aText);

    if prog.Msgs.Count = 0 then
    begin
      exec := prog.Execute;
      Writeln(exec.Result.ToString);
    end
    else
      Writeln(prog.Msgs.AsInfo);
  finally
    dwsUnit.Free;
    DWS.Free;
  end;
  readln;
end;

begin
  Execute('var s: string; s := ''12345''; PrintLn(s);');
end.
