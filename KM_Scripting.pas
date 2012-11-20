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

type
  //Collection of events
  TKMScripting = class
  private
    fScriptCode: AnsiString;
    fByteCode: AnsiString;
    fExec: TPSExec;
    fErrorString: string; //Contains info about found mistakes
    procedure CompileScript;
    procedure LinkRuntime;
  public
    constructor Create;
    destructor Destroy; override;

    property ErrorString: string read fErrorString;
    procedure LoadFromFile(aFileName: string);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure UpdateState;
  end;


var
  fScripting: TKMScripting;


implementation
uses KM_AI, KM_Houses, KM_Terrain, KM_Game, KM_CommonTypes, KM_PlayersCollection, KM_TextLibrary, KM_Log;


function Defeated(aPlayer: Integer): Boolean;
begin
  Result := (fPlayers[aPlayer].AI.WonOrLost = wol_Lost);
end;


function GameTime: Cardinal;
begin
  Result := fGame.GameTickCount;
end;


procedure ShowMsg(aPlayer: Integer; aIndex: Word);
begin
  if MyPlayer.PlayerIndex = aPlayer then
    fGame.ShowMessage(mkText, fTextLibrary.GetMissionString(aIndex), KMPoint(0,0));
end;


type
  TScriptExpose = record
    Addr: Pointer;
    Name: AnsiString;
    Decl: AnsiString;
  end;
const
  //List of functions we expose to PascalScript
  //We expose standalone functions to be able to freely change internal
  //implementation, but keep interface the same
  FUNCS_COUNT = 3;
  ScriptFunctions: array [0..FUNCS_COUNT - 1] of TScriptExpose = (
    (Addr: @Defeated; Name: 'DEFEATED'; Decl: 'function Defeated(aPlayer: Integer): Boolean'),
    (Addr: @GameTime; Name: 'GAMETIME'; Decl: 'function GameTime: Cardinal'),
    (Addr: @ShowMsg;  Name: 'SHOWMSG';  Decl: 'procedure ShowMsg(aPlayer: Integer; aIndex: Word)')
  );


//The OnUses callback function is called for each "uses" in the script.
//It's always called with the parameter 'SYSTEM' at the top of the script.
//For example: uses ii1, ii2;
//This will call this function 3 times. First with 'SYSTEM' then 'II1' and then 'II2'
function ScriptOnUses(Sender: TPSPascalCompiler; const Name: AnsiString): Boolean;
var
  I: Integer;
begin
  if Name = 'SYSTEM' then
  begin
    //Register functions to the script engine.
    //After that they can be used from within the script.
    for I := Low(ScriptFunctions) to High(ScriptFunctions) do
      Sender.AddDelphiFunction(ScriptFunctions[I].Decl);
    Result := True;
  end else
    Result := False;
end;


{ TKMScripting }
constructor TKMScripting.Create;
begin
  inherited;
  fExec := TPSExec.Create;  // Create an instance of the executer.
end;


destructor TKMScripting.Destroy;
begin
  FreeAndNil(fExec);
  inherited;
end;


procedure TKMScripting.LoadFromFile(aFileName: string);
var
  I: Integer;
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
    fScriptCode := 'begin if GameTime = 20 then ShowMsg(0, 66); end.';
    CompileScript;
  finally
    SL.Free;
  end;
end;


procedure TKMScripting.CompileScript;
var
  I: Integer;
  Compiler: TPSPascalCompiler;
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
var I: Integer;
begin
  //This will register exposed functions to the executer
  // - pointer to the function
  // - name of the function (in uppercase)
  // - calling convention (usually Register)
  for I := Low(ScriptFunctions) to High(ScriptFunctions) do
    fExec.RegisterDelphiFunction(ScriptFunctions[I].Addr, ScriptFunctions[I].Name, cdRegister);

  if not fExec.LoadData(fByteCode) then // Load the data from the Data string.
  begin
    { For some reason the script could not be loaded. This is usually the case when a
      library that has been used at compile time isn't registered at runtime. }
    fErrorString := fErrorString + 'Uknown error in loading bytecode to Exec|';
    Exit;
  end;
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
