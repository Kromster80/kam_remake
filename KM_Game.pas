unit KM_Game;
{$I KaM_Remake.inc}
interface
uses Windows,
  {$IFDEF WDC} MPlayer, {$ENDIF}
  Forms, Controls, Classes, Dialogs, SysUtils, KromUtils, Math,
  KM_CommonTypes, KM_Defaults, KM_Utils,
  KM_Networking,
  KM_GameInputProcess, KM_PlayersCollection, KM_Render, KM_TextLibrary, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_MissionScript, KM_Projectiles, KM_Sound, KM_Viewport, KM_Settings, KM_Music, KM_Chat;

type TGameState = ( gsNoGame,  //No game running at all, MainMenu
                    gsPaused,  //Game is paused and responds to 'P' key only
                    gsOnHold,  //Game is paused, shows victory options (resume, win) and responds to mouse clicks only
                    gsRunning, //Game is running normally
                    gsReplay,  //Game is showing replay, no player input allowed
                    gsEditor); //Game is in MapEditor mode

type
  TKMGame = class
  private //Irrelevant to savegame
    ScreenX,ScreenY:word;
    FormControlsVisible:boolean;
    fIsExiting: boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    fGlobalTickCount:cardinal; //Not affected by Pause and anything (Music, Minimap, StatusBar update)
    fGameSpeed:integer;
    fGameState:TGameState;
    fAdvanceFrame:boolean; //Replay variable to advance 1 frame, afterwards set to false
  private //Should be saved
    fGameplayTickCount:cardinal;
    fGameName:string;
    fMissionFile:string; //Remember what we are playing incase we might want to replay
    fMissionMode: TMissionMode;
    ID_Tracker:cardinal; //Mainly Units-Houses tracker, to issue unique numbers on demand
    fActiveCampaign:TCampaign; //Campaign we are playing
    fActiveCampaignMap:byte; //Map of campaign we are playing, could be different than MaxRevealedMap

    procedure GameInit;
  public
    PlayOnState:TGameResultMsg;
    SkipReplayEndCheck:boolean;
    fGameInputProcess:TGameInputProcess;
    fProjectiles:TKMProjectiles;
    fMusicLib: TMusicLib;
    fGlobalSettings: TGlobalSettings;
    fCampaignSettings: TCampaignSettings;
    fNetworking:TKMNetworking;
    fChat:TKMChat;
    fGamePlayInterface: TKMGamePlayInterface;
    fMainMenuInterface: TKMMainMenuInterface;
    fMapEditorInterface: TKMapEdInterface;
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean; {$IFDEF WDC} aMediaPlayer:TMediaPlayer; {$ENDIF} NoMusic:boolean=false);
    destructor Destroy; override;
    procedure ToggleLocale(aLocale:shortstring);
    procedure ResizeGameArea(X,Y:integer);
    procedure ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure GameStart(aMissionFile, aGameName:string; aCamp:TCampaign=cmp_Nil; aCampMap:byte=1);
    procedure GameStartMP(aMissionFile, aGameName:string; aPlayID:byte);
    procedure GameError(aLoc:TKMPoint; aText:string); //Stop the game because of an error 
    procedure SetGameState(aNewState:TGameState);
    procedure GameHold(DoHold:boolean; Msg:TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure GameStop(const Msg:TGameResultMsg; TextMsg:string='');

    procedure MapEditorStart(const aMissionPath:string; aSizeX:integer=64; aSizeY:integer=64);
    procedure MapEditorSave(const aMissionName:string; DoExpandPath:boolean);

    function  ReplayExists:boolean;
    procedure ReplayView(Sender:TObject);

    function GetMissionTime:cardinal;
    function CheckTime(aTimeTicks:cardinal):boolean;
    property GetTickCount:cardinal read fGameplayTickCount;
    property GetMissionFile:string read fMissionFile;
    property GetGameName:string read fGameName;
    property GetCampaign:TCampaign read fActiveCampaign;
    property GetCampaignMap:byte read fActiveCampaignMap;
    property IsExiting:boolean read fIsExiting;
    property MissionMode:TMissionMode read fMissionMode write fMissionMode;
    function GetNewID:cardinal;
    property GameState:TGameState read fGameState;
    procedure SetGameSpeed(aSpeed:byte=0);
    procedure StepOneFrame;

    procedure Save(SlotID:shortint);
    function Load(SlotID:shortint):string;
    function SavegameTitle(SlotID:shortint):string;

    procedure UpdateState;
    procedure UpdateStateIdle(aFrameTime:cardinal);
    procedure PaintInterface;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Unit1, KM_Player, KM_GameInputProcess_Single, KM_GameInputProcess_Multi;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean; {$IFDEF WDC} aMediaPlayer:TMediaPlayer; {$ENDIF} NoMusic:boolean=false);
begin
  Inherited Create;
  ScreenX := aScreenX;
  ScreenY := aScreenY;
  fAdvanceFrame := false;
  ID_Tracker    := 0;
  PlayOnState   := gr_Cancel;
  fGameSpeed    := 1;
  fGameState    := gsNoGame;
  SkipReplayEndCheck  := false;
  FormControlsVisible := false;

  fGlobalSettings := TGlobalSettings.Create;
  fRender         := TRender.Create(RenderHandle, aVSync);
  fTextLibrary    := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fSoundLib       := TSoundLib.Create(fGlobalSettings.Locale, fGlobalSettings.SoundFXVolume/fGlobalSettings.SlidersMax); //Required for button click sounds
  fMusicLib       := TMusicLib.Create({$IFDEF WDC} aMediaPlayer, {$ENDIF} fGlobalSettings.MusicVolume/fGlobalSettings.SlidersMax);
  fResource       := TResource.Create(fGlobalSettings.Locale);
  fMainMenuInterface:= TKMMainMenuInterface.Create(ScreenX,ScreenY,fGlobalSettings);
  fNetworking     := TKMNetworking.Create;
  fChat           := TKMChat.Create; //Used in Gameplay and Lobby
  fCampaignSettings := TCampaignSettings.Create;

  if not NoMusic then fMusicLib.PlayMenuTrack(not fGlobalSettings.MusicOn);

  fLog.AppendLog('<== Game creation is done ==>');
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  fMusicLib.StopMusic; //Stop music imediently, so it doesn't keep playing and jerk while things closes

  FreeThenNil(fCampaignSettings);
  FreeThenNil(fChat);
  FreeThenNil(fGlobalSettings);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(fResource);
  FreeThenNil(fSoundLib);
  FreeThenNil(fMusicLib);
  FreeThenNil(fTextLibrary);
  FreeThenNil(fRender);
  Inherited;
end;


procedure TKMGame.ToggleLocale(aLocale:shortstring);
begin
  fGlobalSettings.Locale := aLocale; //Wrong Locale will be ignored
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fTextLibrary);
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fResource.LoadFonts(false, fGlobalSettings.Locale);
  fMainMenuInterface := TKMMainMenuInterface.Create(ScreenX, ScreenY, fGlobalSettings);
  fMainMenuInterface.ShowScreen(msOptions);
end;


procedure TKMGame.ResizeGameArea(X,Y:integer);
begin
  ScreenX := X;
  ScreenY := Y;
  fRender.ResizeGameArea(X,Y,rm2D);

  //Main menu is invisible while in game, but it still exists and when we return to it
  //it must be properly sized (player could resize the screen while playing)
  if fMainMenuInterface<>nil then fMainMenuInterface.ResizeGameArea(X,Y);
  if fMapEditorInterface<>nil then fMapEditorInterface.ResizeGameArea(X,Y);
  if fGamePlayInterface<>nil then fGamePlayInterface.ResizeGameArea(X,Y);
end;


procedure TKMGame.ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
begin
  Form1.ToggleFullScreen(aToggle, fGlobalSettings.ResolutionID, fGlobalSettings.VSync, ReturnToOptions);
end;


procedure TKMGame.KeyDown(Key: Word; Shift: TShiftState);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyDown(Key, Shift); //Exit if handled
    gsPaused:   fGamePlayInterface.KeyDown(Key, Shift);
    gsOnHold:   fGamePlayInterface.KeyDown(Key, Shift);
    gsRunning:  fGamePlayInterface.KeyDown(Key, Shift);
    gsReplay:   fGamePlayInterface.KeyDown(Key, Shift);
    gsEditor:   fMapEditorInterface.KeyDown(Key, Shift);
  end;
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState);
begin
  //List of conflicting keys:
  //F12 Pauses Execution and switches to debug
  //F10 sets focus on MainMenu1
  //F9 is the default key in Fraps for video capture
  //F4 and F9 are used in debug to control run-flow
  //others.. unknown

  //GLOBAL KEYS
  if Key = VK_F5 then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;
  if (Key = VK_F7) and ENABLE_DESIGN_CONTORLS then
    MODE_DESIGN_CONTORLS := not MODE_DESIGN_CONTORLS;
  if Key = VK_F11  then begin
    FormControlsVisible := not FormControlsVisible;
    Form1.ToggleControlsVisibility(FormControlsVisible);
  end;

  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyUp(Key, Shift); //Exit if handled
    gsPaused:   fGamePlayInterface.KeyUp(Key, Shift);
    gsOnHold:   fGamePlayInterface.KeyUp(Key, Shift);
    gsRunning:  fGamePlayInterface.KeyUp(Key, Shift);
    gsReplay:   fGamePlayInterface.KeyUp(Key, Shift);
    gsEditor:   fMapEditorInterface.KeyUp(Key, Shift);
  end;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseDown(Button,Shift,X,Y);
    gsPaused:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsOnHold:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsReplay:   fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsRunning:  fGamePlayInterface.MouseDown(Button,Shift,X,Y);
    gsEditor:   fMapEditorInterface.MouseDown(Button,Shift,X,Y);
  end;
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if not InRange(X,1,ScreenX-1) or not InRange(Y,1,ScreenY-1) then exit; //Exit if Cursor is outside of frame

  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseMove(Shift, X,Y);
    gsPaused:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsOnHold:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsRunning:  fGamePlayInterface.MouseMove(Shift, X,Y);
    gsReplay:   fGamePlayInterface.MouseMove(Shift, X,Y);
    gsEditor:   fMapEditorInterface.MouseMove(Shift,X,Y);
  end;

Form1.StatusBar1.Panels.Items[1].Text := Format('Cursor: %.1f:%.1f [%d:%d]', [
                                         GameCursor.Float.X, GameCursor.Float.Y,
                                         GameCursor.Cell.X, GameCursor.Cell.Y]);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseUp(Button, Shift, X,Y);
    gsPaused:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsOnHold:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsReplay:   fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsRunning:  fGamePlayInterface.MouseUp(Button, Shift, X,Y);
    gsEditor:   fMapEditorInterface.MouseUp(Button, Shift, X,Y)
  end;
end;


procedure TKMGame.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsPaused:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsOnHold:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsRunning:  fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsReplay:   fGamePlayInterface.MouseWheel(Shift, WheelDelta, X, Y);
    gsEditor:   fMapEditorInterface.MouseWheel(Shift, WheelDelta, X, Y);
  end;
end;


procedure TKMGame.GameInit;
begin
  RandSeed := 4; //Sets right from the start since it affects TKMAllPlayers.Create and other Types
  fGameSpeed := 1; //In case it was set in last run mission
  PlayOnState := gr_Cancel;

  if fResource.GetDataState<>dls_All then begin
    fMainMenuInterface.ShowScreen(msLoading, 'trees, houses and units');
    fRender.Render;
    fResource.LoadGameResources;
    fMainMenuInterface.ShowScreen(msLoading, 'tileset');
    fRender.Render;
    fRender.LoadTileSet;
  end;

  fMainMenuInterface.ShowScreen(msLoading, 'initializing');
  fRender.Render;

  fViewport := TViewport.Create;
  fGamePlayInterface := TKMGamePlayInterface.Create;

  //Here comes terrain/mission init
  fTerrain := TTerrain.Create;
  fProjectiles := TKMProjectiles.Create;

  fRender.ResizeGameArea(ScreenX,ScreenY,rm2D);
  fViewport.ResizeGameArea(ScreenX,ScreenY);

  fGameplayTickCount := 0; //Restart counter
end;


procedure TKMGame.GameStart(aMissionFile, aGameName:string; aCamp:TCampaign=cmp_Nil; aCampMap:byte=1);
var ResultMsg, LoadError:string; fMissionParser: TMissionParser;
begin
  GameInit;

  //If input is empty - replay last map
  if aMissionFile <> '' then begin
    fMissionFile := aMissionFile;
    fGameName := aGameName;
    fActiveCampaign := aCamp;
    fActiveCampaignMap := aCampMap; //MapID is incremented in CampSettings and passed on to here from outside
  end;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(fMissionFile,true) then
  begin
    fMainMenuInterface.ShowScreen(msLoading, 'script');
    fRender.Render;

    try //Catch exceptions
      fMissionParser := TMissionParser.Create(mpm_Game);
      ResultMsg := fMissionParser.LoadDATFile(fMissionFile);
      FreeAndNil(fMissionParser);
      if ResultMsg<>'' then Raise Exception.Create(ResultMsg);
      fLog.AppendLog('DAT Loaded');
    except
      on E : Exception do
      begin
        //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
        LoadError := 'An error has occured while parsing the file '+fMissionFile+'||'+
                      E.ClassName+': '+E.Message;
        if fGameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
        fMainMenuInterface.ShowScreen(msError, LoadError);
        fLog.AppendLog('DAT Load Exception: '+LoadError);
        exit;
      end;
    end;
  end
  else
  begin
    fTerrain.MakeNewMap(64, 64); //For debug we use blank mission
    fPlayers := TKMAllPlayers.Create(MAX_PLAYERS);
    MyPlayer := fPlayers.Player[1];
  end;

  fPlayers.AfterMissionInit(true);
  fViewport.SetZoom(1); //This ensures the viewport is centered on the map

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  fGamePlayInterface.MenuIconsEnabled(not (fMissionMode = mm_Tactic));

  fLog.AppendLog('Gameplay initialized',true);

  fGameState := gsRunning;

  fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
  Save(99); //Thats our base for a game record
  CopyFile(PChar(KMSlotToSaveName(99,'sav')), PChar(KMSlotToSaveName(99,'bas')), false);

  fLog.AppendLog('Gameplay recording initialized',true);
  RandSeed := 4; //Random after StartGame and ViewReplay should match
end;


procedure TKMGame.GameStartMP(aMissionFile, aGameName:string; aPlayID:byte);
var ResultMsg, LoadError:string; fMissionParser: TMissionParser; i:integer;
begin
  GameInit;

  fMissionFile := aMissionFile;
  fGameName := aGameName;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(fMissionFile) then
  begin
    fMainMenuInterface.ShowScreen(msLoading, 'script');
    fRender.Render;

    try //Catch exceptions
      fMissionParser := TMissionParser.Create(mpm_Game);
      ResultMsg := fMissionParser.LoadDATFile(fMissionFile);
      FreeAndNil(fMissionParser);
      if ResultMsg<>'' then Raise Exception.Create(ResultMsg);
      fLog.AppendLog('DAT Loaded');
    except
      on E : Exception do
      begin
        //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
        LoadError := 'An error has occured while parsing the file '+fMissionFile+'||'+
                      E.ClassName+': '+E.Message;
        if fGameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
        fMainMenuInterface.ShowScreen(msError, LoadError);
        fLog.AppendLog('DAT Load Exception: '+LoadError);
        exit;
      end;
    end;
  end;

  fMissionMode := fNetworking.MissionMode; //Tactic or normal
{  //todo: Reassign players (Human, AI, None)
  for i:=1 to fPlayers.PlayerCount do
    case fNetworking.Player(i) of
      pt_None:      fPlayers.Remove(i);
      pt_Human:     fPlayers.PlayerAI := pt_Human;
      pt_Computer:

  fPlayers.SetPlayerCount(fNetworking.GetMaxPlayers); //Trim players}

  MyPlayer := fPlayers.Player[aPlayID];

  fPlayers.AfterMissionInit(true);
  fViewport.SetZoom(1); //This ensures the viewport is centered on the map

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  fGamePlayInterface.MenuIconsEnabled(fMissionMode <> mm_Tactic);

  fLog.AppendLog('Gameplay initialized',true);

  fGameState := gsPaused;

  fGameInputProcess := TGameInputProcess_Multi.Create(gipRecording, fNetworking);
  Save(99); //Thats our base for a game record
  CopyFile(PAnsiChar(KMSlotToSaveName(99,'sav')), PAnsiChar(KMSlotToSaveName(99,'bas')), false);

  fGameState := gsRunning;

  fLog.AppendLog('Gameplay recording initialized',true);
  RandSeed := 4; //Random after StartGame and ViewReplay should match
end;


{ Set viewport and save command log }
procedure TKMGame.GameError(aLoc:TKMPoint; aText:string);
begin
  //Negotiate duplicate calls for GameError
  if fGameState = gsNoGame then exit;

  fViewport.SetCenter(aLoc.X, aLoc.Y);
  SetGameState(gsPaused);
  SHOW_UNIT_ROUTES := true;
  SHOW_UNIT_MOVEMENT := true;

  if MessageDlg(
    fTextLibrary.GetRemakeString(48)+UpperCase(aText)+eol+fTextLibrary.GetRemakeString(49)
    , mtWarning, [mbYes, mbNo], 0) <> mrYes then

    GameStop(gr_Error,'') //Exit to main menu will save the Replay data
  else
    if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
      fGameInputProcess.SaveToFile(KMSlotToSaveName(99,'rpl')); //Save replay data ourselves
end;


procedure TKMGame.SetGameState(aNewState:TGameState);
begin
  fGameState := aNewState;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold:boolean; Msg:TGameResultMsg);
begin
  PlayOnState := Msg;
  case Msg of
    gr_ReplayEnd:     begin
                        if DoHold then begin
                          SetGameState(gsOnHold);
                          fGamePlayInterface.ShowPlayMore(true, Msg);
                        end else
                          SetGameState(gsReplay);
                      end;
    gr_Win,gr_Defeat: begin
                        if DoHold then begin
                          SetGameState(gsOnHold);
                          fGamePlayInterface.ShowPlayMore(true, Msg);
                        end else
                          SetGameState(gsRunning);
                      end;
  end;
end;


procedure TKMGame.GameStop(const Msg:TGameResultMsg; TextMsg:string='');
begin
  fIsExiting := true;
  try
    fGameState := gsNoGame;

    //Take results from MyPlayer before data is flushed
    if Msg in [gr_Win, gr_Defeat, gr_Cancel] then
      fMainMenuInterface.Fill_Results;

    if (fGameInputProcess <> nil) and (fGameInputProcess.ReplayState = gipRecording) then
      fGameInputProcess.SaveToFile(KMSlotToSaveName(99,'rpl'));

    FreeThenNil(fGameInputProcess);
    FreeThenNil(fPlayers);
    FreeThenNil(fProjectiles);
    FreeThenNil(fTerrain);

    FreeThenNil(fGamePlayInterface);  //Free both interfaces
    FreeThenNil(fMapEditorInterface); //Free both interfaces
    FreeThenNil(fViewport);
    ID_Tracker := 0; //Reset ID tracker

    case Msg of
      gr_Win    :  begin
                     fLog.AppendLog('Gameplay ended - Win',true);
                     fMainMenuInterface.ShowScreen(msResults, '', Msg); //Mission results screen
                     fCampaignSettings.RevealMap(fActiveCampaign, fActiveCampaignMap+1);
                   end;
      gr_Defeat:   begin
                     fLog.AppendLog('Gameplay ended - Defeat',true);
                     fMainMenuInterface.ShowScreen(msResults, '', Msg); //Mission results screen
                   end;
      gr_Cancel:   begin
                     fLog.AppendLog('Gameplay canceled',true);
                     fMainMenuInterface.ShowScreen(msResults, '', Msg); //show the results so the user can see how they are going so far
                   end;
      gr_Error:    begin
                     fLog.AppendLog('Gameplay error',true);
                     fMainMenuInterface.ShowScreen(msError, TextMsg);
                   end;
      gr_Silent:   fLog.AppendLog('Gameplay stopped silently',true); //Used when loading new savegame from gameplay UI
      gr_ReplayEnd:begin
                     fLog.AppendLog('Replay canceled',true);
                     fMainMenuInterface.ShowScreen(msMain);
                   end;
      gr_MapEdEnd: begin
                     fLog.AppendLog('MapEditor closed',true);
                     fMainMenuInterface.ShowScreen(msMain);
                   end;
    end;
  finally
    fIsExiting := false;
  end;
end;


{Mission name accepted in 2 formats:
- absolute path, when opening a map from Form1.Menu
- relative, from Maps folder}
procedure TKMGame.MapEditorStart(const aMissionPath:string; aSizeX:integer=64; aSizeY:integer=64);
var ResultMsg:string; fMissionParser:TMissionParser; i: integer;
begin
  if not FileExists(aMissionPath) and (aSizeX*aSizeY=0) then exit; //Erroneous call

  GameStop(gr_Silent); //Stop MapEd if we are loading from existing MapEd session

  RandSeed:=4; //Sets right from the start since it affects TKMAllPlayers.Create and other Types
  fGameSpeed := 1; //In case it was set in last run mission

  if fResource.GetDataState<>dls_All then begin
    fMainMenuInterface.ShowScreen(msLoading, 'units and houses');
    fRender.Render;
    fResource.LoadGameResources;
    fMainMenuInterface.ShowScreen(msLoading, 'tileset');
    fRender.Render;
    fRender.LoadTileSet;
  end;

  fMainMenuInterface.ShowScreen(msLoading, 'initializing');
  fRender.Render;

  fViewport := TViewport.Create;
  fMapEditorInterface := TKMapEdInterface.Create;

  //Here comes terrain/mission init
  fTerrain := TTerrain.Create;

  fLog.AppendLog('Loading DAT...');
  if FileExists(aMissionPath) then begin
    fMissionParser := TMissionParser.Create(mpm_Editor);
    ResultMsg := fMissionParser.LoadDATFile(aMissionPath);
    if ResultMsg<>'' then begin
      GameStop(gr_Error, ResultMsg);
      //Show all required error messages here
      exit;
    end;
    FreeAndNil(fMissionParser);
    fPlayers.SetPlayerCount(MAX_PLAYERS); //Enable them all for editing
    fLog.AppendLog('DAT Loaded');
    fGameName := TruncateExt(ExtractFileName(aMissionPath));
  end else begin
    fTerrain.MakeNewMap(aSizeX, aSizeY);
    fPlayers := TKMAllPlayers.Create(MAX_PLAYERS); //Create MAX players
    MyPlayer := fPlayers.Player[1];
    MyPlayer.PlayerType := pt_Human; //Make Player1 human by default
    fGameName := 'New Mission';
  end;

  fMapEditorInterface.Player_UpdateColors;
  fPlayers.AfterMissionInit(false);

  for i:=1 to MAX_PLAYERS do //Reveal all players since we'll swap between them in MapEd
    fTerrain.RevealWholeMap(TPlayerID(i));

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized',true);

  fRender.ResizeGameArea(ScreenX,ScreenY,rm2D);
  fViewport.ResizeGameArea(ScreenX,ScreenY);
  fViewport.SetZoom(1);

  fGameplayTickCount := 0; //Restart counter

  fGameState := gsEditor;
end;


//DoExpandPath means that input is a mission name which should be expanded into:
//ExeDir+'Maps\'+MissionName+'\'+MissionName.dat
//ExeDir+'Maps\'+MissionName+'\'+MissionName.map
procedure TKMGame.MapEditorSave(const aMissionName:string; DoExpandPath:boolean);
var fMissionParser: TMissionParser;
begin
  if aMissionName = '' then exit;
  if DoExpandPath then begin
    CreateDir(ExeDir+'Maps');
    CreateDir(ExeDir+'Maps\'+aMissionName);
    fTerrain.SaveToMapFile(KMMapNameToPath(aMissionName, 'map'));
    fMissionParser := TMissionParser.Create(mpm_Editor);
    fMissionParser.SaveDATFile(KMMapNameToPath(aMissionName, 'dat'));
    FreeAndNil(fMissionParser);
    fGameName := aMissionName;
  end else
    Assert(false,'SaveMapEditor call with DoExpandPath=false');
end;


{ Check if replay files exist at location }
function TKMGame.ReplayExists:boolean;
begin
  Result := FileExists(KMSlotToSaveName(99,'bas')) and
            FileExists(KMSlotToSaveName(99,'rpl'));
end;


procedure TKMGame.ReplayView(Sender:TObject);
begin
  CopyFile(PChar(KMSlotToSaveName(99,'bas')), PChar(KMSlotToSaveName(99,'sav')), false);
  Load(99); //We load what was saved right before starting Recording
  FreeAndNil(fGameInputProcess); //Override GIP from savegame

  fGameInputProcess := TGameInputProcess_Single.Create(gipReplaying);
  fGameInputProcess.LoadFromFile(KMSlotToSaveName(99,'rpl'));

  RandSeed := 4; //Random after StartGame and ViewReplay should match
  fGameState := gsReplay;
end;


//Treat 10 ticks as 1 sec irregardless of user-set pace
function TKMGame.GetMissionTime:cardinal;
begin
  Result := GetTickCount div 10;
end;


//Tests whether time has past
function TKMGame.CheckTime(aTimeTicks:cardinal):boolean;
begin
  Result := (GetTickCount >= aTimeTicks);
end;


function TKMGame.GetNewID:cardinal;
begin
  inc(ID_Tracker);
  Result := ID_Tracker;
end;


procedure TKMGame.SetGameSpeed(aSpeed:byte=0);
begin
  if aSpeed=0 then //Make sure it's either 1 or Max, not something inbetween
    if fGameSpeed = 1 then
      fGameSpeed := fGlobalSettings.Speedup
    else
      fGameSpeed := 1
  else
    fGameSpeed := aSpeed;

  fGamePlayInterface.ShowClock(fGameSpeed <> 1);
end;


procedure TKMGame.StepOneFrame;
begin
  Assert(fGameState in [gsPaused,gsReplay], 'We can work step-by-step only in Replay');
  SetGameSpeed(1); //Do not allow multiple updates in fGame.UpdateState loop
  fAdvanceFrame := true;
end;


//Saves the game in all its glory
//Base savegame gets copied from save99.bas
//Saves command log to RPL file
procedure TKMGame.Save(SlotID:shortint);
var
  SaveStream:TKMemoryStream;
  i:integer;
begin
  fLog.AppendLog('Saving game');
  if not (fGameState in [gsPaused,gsRunning]) then begin
    Assert(false, 'Saving from wrong state?');
    exit;
  end;

  SaveStream := TKMemoryStream.Create;
  SaveStream.Write('KaM_Savegame');
  SaveStream.Write(SAVE_VERSION); //This is savegame version
  SaveStream.Write(fMissionFile); //Save game mission file
  SaveStream.Write(fGameName); //Save game title
  SaveStream.Write(fGameplayTickCount); //Required to be saved, e.g. messages being shown after a time
  SaveStream.Write(fMissionMode, SizeOf(fMissionMode));
  SaveStream.Write(ID_Tracker); //Units-Houses ID tracker
  SaveStream.Write(PlayOnState, SizeOf(PlayOnState));

  fTerrain.Save(SaveStream); //Saves the map
  fPlayers.Save(SaveStream); //Saves all players properties individually
  fProjectiles.Save(SaveStream);

  fViewport.Save(SaveStream); //Saves viewed area settings
  //Don't include fGameSettings.Save it's not required for settings are Game-global, not mission
  fGamePlayInterface.Save(SaveStream); //Saves message queue and school/barracks selected units

  CreateDir(ExeDir+'Saves\'); //Makes the folder incase it was deleted

  if SlotID = AUTOSAVE_SLOT then begin //Backup earlier autosaves
    DeleteFile(KMSlotToSaveName(AUTOSAVE_SLOT+AUTOSAVE_COUNT,'sav'));
    for i:=AUTOSAVE_SLOT+AUTOSAVE_COUNT downto AUTOSAVE_SLOT+1 do //13 to 11
      RenameFile(KMSlotToSaveName(i-1,'sav'), KMSlotToSaveName(i,'sav'));
  end;

  SaveStream.SaveToFile(KMSlotToSaveName(SlotID,'sav')); //Some 70ms for TPR7 map
  SaveStream.Free;

  if SlotID <> AUTOSAVE_SLOT then begin //Backup earlier autosaves
    CopyFile(PChar(KMSlotToSaveName(99,'bas')), PChar(KMSlotToSaveName(SlotID,'bas')), false); //replace Replay base savegame
    fGameInputProcess.SaveToFile(KMSlotToSaveName(SlotID,'rpl')); //Adds command queue to savegame
  end;//

  fLog.AppendLog('Saving game',true);
end;


function TKMGame.SavegameTitle(SlotID:shortint):string;
var
  FileName,s,ver:string;
  LoadStream:TKMemoryStream;
  i:cardinal;
begin
  Result := '';
  FileName := KMSlotToSaveName(SlotID,'sav'); //Full path
  if not FileExists(FileName) then begin
    Result := fTextLibrary.GetTextString(202); //Empty
    exit;
  end;

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  LoadStream.LoadFromFile(FileName);
  LoadStream.Seek(0, soFromBeginning);

  LoadStream.Read(s);
  if s = 'KaM_Savegame' then begin
    LoadStream.Read(ver);
    if ver = SAVE_VERSION then begin
      LoadStream.Read(s); //Savegame mission file
      LoadStream.Read(s); //GameName
      LoadStream.Read(i);
      Result := s + ' ' + int2time(i div 10);
      if SlotID = AUTOSAVE_SLOT then Result := fTextLibrary.GetTextString(203) + ' ' + Result;
    end else
      Result := 'Unsupported save ' + ver;
  end else
    Result := 'Unsupported format';
  LoadStream.Free;
end;


function TKMGame.Load(SlotID:shortint):string;
var
  LoadStream:TKMemoryStream;
  s,FileName:string;
begin
  fLog.AppendLog('Loading game');
  Result := '';
  FileName := KMSlotToSaveName(SlotID,'sav'); //Full path

  //Check if file exists early so that current game will not be lost if user tries to load an empty save
  if not FileExists(FileName) then
  begin
    Result := 'Savegame file not found';
    exit;
  end;

  if fGameState in [gsRunning, gsPaused] then GameStop(gr_Silent);

  //Load only from menu or stopped game
  if not (fGameState in [gsNoGame]) then begin
    Assert(false, 'Loading from wrong state?');
    exit;
  end;

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  try //Catch exceptions
    LoadStream.LoadFromFile(FileName);
    LoadStream.Seek(0, soFromBeginning);

    //Raise some exceptions if the file is invalid or the wrong save version
    LoadStream.Read(s); if s <> 'KaM_Savegame' then Raise Exception.Create('Not a valid KaM Remake save file');
    LoadStream.Read(s); if s <> SAVE_VERSION then Raise Exception.CreateFmt('Incompatible save version ''%s''. This version is ''%s''',[s,SAVE_VERSION]);

    //Create empty environment
    GameInit;

    //Substitute tick counter and id tracker
    LoadStream.Read(fMissionFile); //Savegame mission file
    LoadStream.Read(fGameName); //Savegame title
    LoadStream.Read(fGameplayTickCount);
    LoadStream.Read(fMissionMode, SizeOf(fMissionMode));
    LoadStream.Read(ID_Tracker);
    LoadStream.Read(PlayOnState, SizeOf(PlayOnState));

    fPlayers := TKMAllPlayers.Create(MAX_PLAYERS);
    MyPlayer := fPlayers.Player[1];

    //Load the data into the game
    fTerrain.Load(LoadStream);
    fPlayers.Load(LoadStream);
    fProjectiles.Load(LoadStream);

    fViewport.Load(LoadStream);
    fGamePlayInterface.Load(LoadStream);

    LoadStream.Free;

    fGameInputProcess := TGameInputProcess_Single.Create(gipRecording);
    fGameInputProcess.LoadFromFile(KMSlotToSaveName(SlotID,'rpl'));

    CopyFile(PChar(KMSlotToSaveName(SlotID,'bas')), PChar(KMSlotToSaveName(99,'bas')), false); //replace Replay base savegame

    fGamePlayInterface.MenuIconsEnabled(fMissionMode <> mm_Tactic); //Preserve disabled icons
    fPlayers.SyncLoad; //Should parse all Unit-House ID references and replace them with actual pointers
    fTerrain.SyncLoad; //IsUnit values should be replaced with actual pointers
    fViewport.SetZoom(1); //This ensures the viewport is centered on the map (game could have been saved with a different resolution/zoom)
    Result := ''; //Loading has now completed successfully :)
    Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  except
    on E : Exception do
    begin
      //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
      Result := 'An error was encountered while parsing the file '+FileName+'.|Details of the error:|'+
                    E.ClassName+' error raised with message: '+E.Message;
      if fGameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
      exit;
    end;
  end;

  fGameState := gsRunning;
  fLog.AppendLog('Loading game',true);
end;


procedure TKMGame.UpdateState;
var i:integer;
begin
  inc(fGlobalTickCount);
  case fGameState of
    gsPaused:   exit;
    gsOnHold:   exit;
    gsNoGame:   begin
                  fNetworking.UpdateState;
                  fMainMenuInterface.UpdateState;
                  if fGlobalTickCount mod 10 = 0 then //Once a sec
                  if fMusicLib.IsMusicEnded then
                    fMusicLib.PlayMenuTrack(not fGlobalSettings.MusicOn); //Menu tune
                end;
    gsRunning,
    gsReplay:   begin
                  for i:=1 to fGameSpeed do
                  begin
                    inc(fGameplayTickCount); //Thats our tick counter for gameplay events
                    fTerrain.UpdateState;
                    fPlayers.UpdateState(fGameplayTickCount); //Quite slow
                    if fGameState = gsNoGame then exit; //Quit the update if game was stopped by MyPlayer defeat
                    fProjectiles.UpdateState; //If game has stopped it's NIL

                    if (fGameplayTickCount mod 600 = 0) and fGlobalSettings.Autosave
                      and (fGameState = gsRunning) then //Each 1min of gameplay time
                      Save(AUTOSAVE_SLOT); //Autosave slot

                    fGameInputProcess.Timer(fGameplayTickCount);
                    if not SkipReplayEndCheck and fGameInputProcess.ReplayEnded then
                      GameHold(true, gr_ReplayEnd);

                    if fAdvanceFrame then begin
                      fAdvanceFrame := false;
                      SetGameState(gsPaused);
                    end;

                    if fGameState = gsNoGame then exit; //Error due to consistency fail in replay commands
                  end;

                  fNetworking.UpdateState;
                  fGamePlayInterface.UpdateState;

                  if fGlobalTickCount mod 10 = 0 then //Every 1000ms
                    fTerrain.RefreshMinimapData; //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

                  if fGlobalTickCount mod 10 = 0 then
                    if fMusicLib.IsMusicEnded then
                      fMusicLib.PlayNextTrack; //Feed new music track

                  if fGlobalTickCount mod 10 = 0 then
                    Form1.StatusBar1.Panels[2].Text:='Time: '+int2time(GetMissionTime);
                end;
    gsEditor:   begin
                  fMapEditorInterface.UpdateState;
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                  if fGlobalTickCount mod 10 = 0 then //Every 500ms
                    fTerrain.RefreshMinimapData; //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)
                end;
    end;

end;


{This is our real-time thread, use it wisely}
procedure TKMGame.UpdateStateIdle(aFrameTime:cardinal);
begin
  case fGameState of
    gsRunning,
    gsReplay:   fViewport.DoScrolling(aFrameTime); //Check to see if we need to scroll
    gsEditor:   begin
                  fViewport.DoScrolling(aFrameTime); //Check to see if we need to scroll
                  fTerrain.UpdateStateIdle;
                end;
  end;
end;


procedure TKMGame.PaintInterface;
begin
  case fGameState of
    gsNoGame:  fMainMenuInterface.Paint;
    gsPaused:  fGamePlayInterface.Paint;
    gsOnHold:  fGamePlayInterface.Paint;
    gsRunning: fGamePlayInterface.Paint;
    gsReplay:  fGamePlayInterface.Paint;
    gsEditor:  fMapEditorInterface.Paint;
  end;
end;


end.
