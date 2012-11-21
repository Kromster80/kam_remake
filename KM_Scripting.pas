unit KM_Scripting;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils, StrUtils,
  uPSCompiler, uPSRuntime,
  KM_CommonClasses, KM_Defaults, KM_Points;

  //Dynamic scripts allow mapmakers to control the mission flow

  //In TSK, there are no enemies and you win when you build the tannery.
  //In TPR, you must defeat the enemies AND build the tannery.

const
  MAX_PARAMS = 4;

type
  TKMEventTrigger = (
    etDefeated,       //[] Certain player has been defeated, we rely on Conditions to generate that event
    etTime,           //[Tick] Time has come
    etHouseBuilt);    //[House] Certain house was built

  //All functions can be split into these three categories:
  // - Event, when something has happened (e.g. House was built)
  // - State, describing the state of something (e.g. Houses.Built.Count >= 1)
  // - Action, when we need to perform something (e.g. show a message)
  TKMScriptEvents = class
  private
    //We need to collect all events into one batch to be able to process them together
    fCount: Word;
    fItems: array of record
      Trigger: TKMEventTrigger;
      Params: array[0..MAX_PARAMS-1] of Integer;
    end;
    procedure Add(aTrigger: TKMEventTrigger; aParams: array of Integer);
    procedure Clear;
    function Contains(aTrigger: TKMEventTrigger; aParams: array of Integer): Word;
  public
    function Defeated(aPlayer: Integer): Boolean;
  end;

  TKMScriptStates = class
    function GameTime: Cardinal;
  end;

  TKMScriptActions = class
    procedure ShowMsg(aPlayer: Integer; aIndex: Word);
  end;


  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fByteCode: AnsiString;
    fExec: TPSExec;
    fErrorString: string; //Info about found mistakes

    fEvents: TKMScriptEvents;
    fStates: TKMScriptStates;
    fActions: TKMScriptActions;

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


var
  fScripting: TKMScripting;


implementation
uses KM_AI, KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection, KM_TextLibrary, KM_Log;


{ TKMScriptEvents }
procedure TKMScriptEvents.Add(aTrigger: TKMEventTrigger; aParams: array of Integer);
var
  I: Integer;
begin
  if fCount > High(fItems) then
    SetLength(fItems, fCount + 8);
  fItems[fCount].Trigger := aTrigger;
  for I := Low(aParams) to High(aParams) do
    fItems[fCount].Params[I] := aParams[I];
  Inc(fCount);
end;


procedure TKMScriptEvents.Clear;
begin
  fCount := 0;
end;


//See how many times requested Trigger is met in a list,
//we return Count since in rare cases two identical events could be registered
//in one tick. e.g. UnitDied event in a fight
function TKMScriptEvents.Contains(aTrigger: TKMEventTrigger; aParams: array of Integer): Word;
var
  I,K: Integer;
  Match: Boolean;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
  if fItems[I].Trigger = aTrigger then
  begin
    Match := True;
    for K := 0 to High(aParams) do
      Match := Match and (fItems[I].Params[K] = aParams[K]);
    if Match then
      Inc(Result);
  end;
end;


function TKMScriptEvents.Defeated(aPlayer: Integer): Boolean;
begin
  Result := Contains(etDefeated, [aPlayer]) > 0;
end;


function TKMScriptStates.GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


procedure TKMScriptActions.ShowMsg(aPlayer: Integer; aIndex: Word);
begin
  if MyPlayer.PlayerIndex = aPlayer then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0));
end;


{ TKMScripting }
constructor TKMScripting.Create;
begin
  inherited;
  fExec := TPSExec.Create;  // Create an instance of the executer.
  fEvents := TKMScriptEvents.Create;
  fStates := TKMScriptStates.Create;
  fActions := TKMScriptActions.Create;
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(fEvents);
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


//The OnUses callback function is called for each "uses" in the script.
//It's always called with the parameter 'SYSTEM' at the top of the script.
//For example: uses ii1, ii2;
//This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'
function TKMScripting.ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
var
  I: Integer;
  V: TPSVar;
begin
  if Name = 'SYSTEM' then
  begin

    //Register classes and methods to the script engine.
    //After that they can be used from within the script.
    with Sender.AddClassN(nil, 'TKMScriptEvents') do
    begin
      RegisterMethod('function Defeated(aPlayer: Integer): Boolean');
    end;

    with Sender.AddClassN(nil, 'TKMScriptStates') do
    begin
      RegisterMethod('function GameTime: Cardinal');
    end;

    with Sender.AddClassN(nil, 'TKMScriptActions') do
    begin
      RegisterMethod('procedure ShowMsg(aPlayer: Integer; aIndex: Word)');
    end;

    //Register objects
    AddImportedClassVariable(Sender, 'Events', 'TKMScriptEvents');
    AddImportedClassVariable(Sender, 'States', 'TKMScriptStates');
    AddImportedClassVariable(Sender, 'Actions', 'TKMScriptActions');

    Result := True;
  end else
    Result := False;
end;


procedure TKMScripting.CompileScript;
var
  I: Integer;
  Compiler: TPSPascalCompiler;
  V: TPSVar;
begin
  Compiler := TPSPascalCompiler.Create; // create an instance of the compiler.
  try
    Compiler.OnUses := ScriptOnUses; // assign the OnUses event.
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
var I: Integer; CI: TPSRuntimeClassImporter;
begin
  //Create an instance of the runtime class importer
  CI := TPSRuntimeClassImporter.Create;

  //Register classes and their methods
  with CI.Add(TKMScriptEvents) do
  begin
    RegisterMethod(@TKMScriptEvents.Defeated, 'DEFEATED');
  end;

  with CI.Add(TKMScriptStates) do
  begin
    RegisterMethod(@TKMScriptStates.GameTime, 'GAMETIME');
  end;

  with CI.Add(TKMScriptActions) do
  begin
    RegisterMethod(@TKMScriptActions.ShowMsg, 'SHOWMSG');
  end;

  //Append classes info to Exec
  RegisterClassLibraryRuntime(fExec, CI);

  if not fExec.LoadData(fByteCode) then // Load the data from the Data string.
  begin
    { For some reason the script could not be loaded. This is usually the case when a
      library that has been used at compile time isn't registered at runtime. }
    fErrorString := fErrorString + 'Uknown error in loading bytecode to Exec|';
    Exit;
  end;

  //Link script objects with objects
  SetVariantToClass(fExec.GetVarNo(fExec.GetVar('EVENTS')), fEvents);
  SetVariantToClass(fExec.GetVarNo(fExec.GetVar('STATES')), fStates);
  SetVariantToClass(fExec.GetVarNo(fExec.GetVar('ACTIONS')), fActions);
end;


procedure TKMScripting.ProcDefeated(aPlayer: TPlayerIndex);
begin
  fEvents.Add(etDefeated, [aPlayer]);
end;


procedure TKMScripting.ProcHouseBuilt(aHouseType: THouseType; aOwner: TPlayerIndex);
begin
  fEvents.Add(etHouseBuilt, [aOwner, Byte(aHouseType)]);
end;


procedure TKMScripting.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fScriptCode);

  CompileScript;
end;


procedure TKMScripting.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fScriptCode);
  Assert(fEvents.fCount = 0, 'We''d expect Events to be saved after UpdateState');
end;


procedure TKMScripting.UpdateState;
begin
  fExec.RunScript;

  //Remove any events, we need to process them only once
  fEvents.Clear;
end;


end.
