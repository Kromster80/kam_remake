program DWScript_Test1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  dwsComp, dwsCompiler, dwsExprs, dwsSymbols;


type
  TUnitType = (utSerf, utAxeman);

  TMyClass = class
  published
    function GetUnitCount{(aType: TUnitType)}: Integer;
  end;

  TTest = class
    DWS: TDelphiWebScript;
    dwsUnit: TdwsUnit;
    fStats: TMyClass;
    prog: IdwsProgram;
    exec: IdwsProgramExecution;
    procedure ExposeInstancesAfterInitTable(Sender: TObject);
    procedure Execute(aText: string);
  end;

function TMyClass.GetUnitCount{(aType: TUnitType)}: Integer;
begin
  Result := 4;
end;


procedure TTest.Execute(aText: string);
begin
  DWS := TDelphiWebScript.Create(nil);
  dwsUnit := TdwsUnit.Create(nil);
  dwsUnit.UnitName := 'Test';
  try
    dwsUnit.Script := DWS;

   cls:=dwsUnit.Classes.Add;
   cls.Name:='TMyClass';
   cls.OnCleanUp:=DoCleanupExternal;
   cst:=cls.Constructors.Add as TdwsConstructor;
   cst.Name:='Create';
   cst.OnEval:=DoCreateExternal;

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
  Readln;
end;

{ TMyClass }

begin
  TTest.Create.Execute('var s: string; s := ''12345''; PrintLn(s); '{ +
          'PrintLn(IntToStr(fStats.GetUnitCount));'});
end.
