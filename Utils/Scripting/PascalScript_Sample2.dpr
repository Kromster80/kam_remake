program PascalScript_Sample2;
{$APPTYPE CONSOLE}
uses
  uPSCompiler,
  uPSRuntime, uPSC_std, uPSR_std;


type
  TUnitType = (utSerf, utAxeman);
  TMyClass = class
  public
    Multiplier: Integer;
    function GetUnitCount(aType: TUnitType): Integer;
  end;

var
  MyClass: TMyClass;


function TMyClass.GetUnitCount(aType: TUnitType): Integer;
begin
  case aType of
    utSerf:   Result := 89999 * Multiplier;
    utAxeman: Result := 32222 * Multiplier;
    else      Result := 0;
  end;
end;

procedure MyOwnFunction(const Data: string);
begin
  WriteLn(Data);
  Readln;
end;


function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
{ the OnUses callback function is called for each "uses" in the script.
  It's always called with the parameter 'SYSTEM' at the top of the script.
  For example: uses ii1, ii2;
  This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'.
}
begin
  if Name = 'SYSTEM' then
  begin

    Sender.AddTypeS('TUnitType', '(utSerf, utAxeman)');

    with Sender.AddClassN(nil, 'TMyClass') do
    begin
      RegisterMethod('constructor Create');
      RegisterMethod('function GetUnitCount(aType: TUnitType): Integer');
      RegisterMethod('procedure Free');
    end;

    AddImportedClassVariable(Sender, 'MyClass', 'TMyClass');
    { This will register the function to the script engine. Now it can be used from
      within the script.}

    Sender.AddDelphiFunction('procedure MyOwnFunction(Data: string)');

    Result := True;
  end else
    Result := False;
end;

procedure ExecuteScript(const Script: AnsiString);
var
  Compiler: TPSPascalCompiler;
  Exec: TPSExec;
  Data: AnsiString;
  CI: TPSRuntimeClassImporter;
begin
  MyClass := TMyClass.Create;
  MyClass.Multiplier := 1000;

  Compiler := TPSPascalCompiler.Create; // create an instance of the compiler.
  Compiler.OnUses := ScriptOnUses; // assign the OnUses event.
  if not Compiler.Compile(Script) then  // Compile the Pascal script into bytecode.
  begin
    Compiler.Free;
     // You could raise an exception here.
    Exit;
  end;

  Compiler.GetOutput(Data); // Save the output of the compiler in the string Data.
  Compiler.Free; // After compiling the script, there is no need for the compiler anymore.

  CI := TPSRuntimeClassImporter.Create;
  { Create an instance of the runtime class importer.}

  with CI.Add(TMyClass) do
  begin
    RegisterConstructor(@TMyClass.Create, 'CREATE');
    RegisterMethod(@TMyClass.GetUnitCount, 'GETUNITCOUNT');
    RegisterMethod(@TMyClass.Free, 'FREE');
  end;

  Exec := TPSExec.Create;  // Create an instance of the executer.
  RegisterClassLibraryRuntime(Exec, CI);
  Exec.RegisterDelphiFunction(@MyOwnFunction, 'MYOWNFUNCTION', cdRegister);
  { This will register the function to the executer. The first parameter is a
    pointer to the function. The second parameter is the name of the function (in uppercase).
	And the last parameter is the calling convention (usually Register). }

  if not Exec.LoadData(Data) then // Load the data from the Data string.
  begin
    { For some reason the script could not be loaded. This is usually the case when a
      library that has been used at compile time isn't registered at runtime. }
    Exec.Free;
     // You could raise an exception here.
    Exit;
  end;

  SetVariantToClass(Exec.GetVarNo(Exec.GetVar('MYCLASS')), MyClass);
   // This will set the script's Application variable to the real Application variable.

  Exec.RunScript; // Run the script.
  Exec.Free; // Free the executer.
end;


const
  Script =
    'var s: string; ' +
    'begin ' +
    's := IntToStr(MyClass.GetUnitCount(utSerf)) + '' serf''; ' +
    'MyOwnFunction(s); ' +
    's := IntToStr(MyClass.GetUnitCount(utAxeman)) + '' axeman''; ' +
    'MyOwnFunction(s); ' +
    'end.';


begin
  ExecuteScript(Script);
end.
