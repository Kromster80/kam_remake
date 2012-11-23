unit KM_Scripting;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  uPSCompiler, uPSRuntime, uPSUtils,
  KM_CommonClasses, KM_Defaults, KM_ScriptingESA;

  //Dynamic scripts allow mapmakers to control the mission flow

  //In TSK, there are no enemies and you win when you build the tannery.
  //In TPR, you must defeat the enemies AND build the tannery.

type
  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fByteCode: AnsiString;
    fExec: TPSExec;
    fErrorString: string; //Info about found mistakes

    fStates: TKMScriptStates;
    fActions: TKMScriptActions;

    function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
    function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
    procedure CompileScript;
    procedure LinkRuntime;
  public
    constructor Create;
    destructor Destroy; override;

    property ErrorString: string read fErrorString;
    procedure LoadFromFile(aFileName: string);

    procedure ProcDefeated(aPlayer: TPlayerIndex);
    procedure ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


  TTestFunction = procedure (aIndex: Integer) of object;
  TTestFunction2 = procedure (aIndex, aParam: Integer) of object;

var
  fScripting: TKMScripting;


implementation
uses KM_Log, KM_ResourceHouse;


{ TKMScripting }
constructor TKMScripting.Create;
begin
  inherited;
  fExec := TPSExec.Create;  // Create an instance of the executer.
  fStates := TKMScriptStates.Create;
  fActions := TKMScriptActions.Create;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(fStates);
  FreeAndNil(fActions);
  FreeAndNil(fExec);
  inherited;
end;


procedure TKMScripting.LoadFromFile(aFileName: string);
var
  SL: TStringList;
begin
  fErrorString := '';

  if not FileExists(aFileName) then
  begin
    fLog.AddNoTime(aFileName + ' was not found. It is okay for mission to have no dynamic scripts.');
    Exit;
  end;

  //Read the file line by line and try to add valid events
  SL := TStringList.Create;
  try
    SL.LoadFromFile(aFileName);
    fScriptCode := SL.Text;
    {fScriptCode := 'var I: Integer; ' +
                   'begin ' +
                   '  if States.GameTime = 10 then ' +
                   '    I := 27; ' +
                   '  if States.GameTime = I then ' +
                   '    Actions.ShowMsg(0, I); ' +
                   'end.';}
    CompileScript;
  finally
    SL.Free;
  end;
end;


{ The OnExportCheck callback function is called for each function in the script
  (Also for the main proc, with '!MAIN' as a Proc^.Name). ProcDecl contains the
  result type and parameter types of a function using this format:
  ProcDecl: ResultType + ' ' + Parameter1 + ' ' + Parameter2 + ' '+Parameter3 + .....
  Parameter: ParameterType+TypeName
  ParameterType is @ for a normal parameter and ! for a var parameter.
  A result type of 0 means no result}
function TKMScripting.ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
begin
  Result := True;

  //Check if the proc is the proc we want
  if Proc.Name = 'ONHOUSEBUILT' then
    //Check if the proc has the correct params
    if not ExportCheck(Sender, Proc, [0, btS32, btS32], [pmIn, pmIn]) then
    begin
      //Something is wrong, so cause an error
      Sender.MakeError('', ecTypeMismatch, '');
      Result := False;
      Exit;
    end;
  if Proc.Name = 'ONPLAYERDEFEATED' then
    //Check if the proc has the correct params
    if not ExportCheck(Sender, Proc, [0, btS32], [pmIn]) then
    begin
      //Something is wrong, so cause an error
      Sender.MakeError('', ecTypeMismatch, '');
      Result := False;
      Exit;
    end;
end;


//The OnUses callback function is called for each "uses" in the script.
//It's always called with the parameter 'SYSTEM' at the top of the script.
//For example: uses ii1, ii2;
//This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'
function TKMScripting.ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
begin
  if Name = 'SYSTEM' then
  begin

    //Sender.AddTypeS('THouseType', '(utSerf, utAxeman)');

    //Register classes and methods to the script engine.
    //After that they can be used from within the script.
    with Sender.AddClassN(nil, fStates.ClassName) do
    begin
      RegisterMethod('function GameTime: Cardinal');
    end;

    with Sender.AddClassN(nil, fActions.ClassName) do
    begin
      RegisterMethod('procedure Defeat(aPlayer: Integer)');
      RegisterMethod('procedure ShowMsg(aPlayer: Integer; aIndex: Word)');
    end;

    //Register objects
    AddImportedClassVariable(Sender, 'States', fStates.ClassName);
    AddImportedClassVariable(Sender, 'Actions', fActions.ClassName);

    Result := True;
  end else
    Result := False;
end;


procedure TKMScripting.CompileScript;
var
  I: Integer;
  Compiler: TPSPascalCompiler;
begin
  Compiler := TPSPascalCompiler.Create; // create an instance of the compiler.
  try
    Compiler.OnUses := ScriptOnUses; // assign the OnUses event.
    Compiler.OnExportCheck := ScriptOnExportCheck; // Assign the onExportCheck event.

    if not Compiler.Compile(fScriptCode) then  // Compile the Pascal script into bytecode.
    begin
      for I := 0 to Compiler.MsgCount - 1 do
        fErrorString := fErrorString + Compiler.Msg[I].MessageToString + '|';
      Exit;
    end;

    Compiler.GetOutput(fByteCode); // Save the output of the compiler in the string Data.
  finally
    Compiler.Free;
  end;

  LinkRuntime;
end;


//Link the ByteCode with used functions and load it into Executioner
procedure TKMScripting.LinkRuntime;
var
  ClassImp: TPSRuntimeClassImporter;
begin
  //Create an instance of the runtime class importer
  ClassImp := TPSRuntimeClassImporter.Create;

  //Register classes and their exposed methods to Runtime (must be uppercase)
  with ClassImp.Add(TKMScriptStates) do
  begin
    RegisterMethod(@TKMScriptStates.GameTime, 'GAMETIME');
  end;

  with ClassImp.Add(TKMScriptActions) do
  begin
    RegisterMethod(@TKMScriptActions.Defeat, 'DEFEAT');
    RegisterMethod(@TKMScriptActions.ShowMsg, 'SHOWMSG');
  end;

  //Append classes info to Exec
  RegisterClassLibraryRuntime(fExec, ClassImp);

  if not fExec.LoadData(fByteCode) then // Load the data from the Data string.
  begin
    { For some reason the script could not be loaded. This is usually the case when a
      library that has been used at compile time isn't registered at runtime. }
    fErrorString := fErrorString + 'Uknown error in loading bytecode to Exec|';
    Exit;
  end;

  //Link script objects with objects
  SetVariantToClass(fExec.GetVarNo(fExec.GetVar('STATES')), fStates);
  SetVariantToClass(fExec.GetVarNo(fExec.GetVar('ACTIONS')), fActions);
end;


procedure TKMScripting.ProcDefeated(aPlayer: TPlayerIndex);
var
  TestFunc: TTestFunction;
begin
  //Check if event handler (procedure) exists and run it
  TestFunc := TTestFunction(fExec.GetProcAsMethodN('ONPLAYERDEFEATED'));
  if @TestFunc <> nil then
    TestFunc(aPlayer);
end;


procedure TKMScripting.ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);
var
  TestFunc: TTestFunction2;
begin
  //Check if event handler (procedure) exists and run it
  //Store house by its KaM index to keep it consistent with DAT scripts
  TestFunc := TTestFunction2(fExec.GetProcAsMethodN('ONHOUSEBUILT'));
  if @TestFunc <> nil then
    TestFunc(aOwner, HouseTypeToIndex[aHouseType]);
end;


procedure TKMScripting.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fScriptCode);

  CompileScript;
end;


procedure TKMScripting.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fScriptCode);
end;


procedure TKMScripting.UpdateState;
begin
  fExec.RunScript;
end;


end.
