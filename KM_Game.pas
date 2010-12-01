unit KM_Game;
{$I KaM_Remake.inc}
interface
uses Windows,
  {$IFDEF WDC} MPlayer, {$ENDIF}
  Forms, Controls, Classes, Dialogs, SysUtils, KromUtils, Math,
  KM_CommonTypes, KM_Defaults, KM_Utils, 
  KM_Controls, KM_GameInputProcess, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_MissionScript, KM_Projectiles, KM_Sound, KM_Viewport, KM_Units, KM_Settings, KM_Music;

type TGameState = ( gsNoGame,  //No game running at all, MainMenu
                    gsPaused,  //Game is paused and responds to 'P' key only
                    gsOnHold,  //Game is paused, shows victory options (resume, win) and responds to mouse clicks only
                    gsRunning, //Game is running normally
                    gsReplay,  //Game is showing replay, no player input allowed
                    gsEditor); //Game is in MapEditor mode

type
  TKMGame = class
  private
    ScreenX,ScreenY:word;
    FormControlsVisible:boolean;
    SelectingTroopDirection:boolean;
    SelectingDirPosition: TPoint;
    SelectedDirection: TKMDirection;
    GlobalTickCount:cardinal; //Not affected by Pause and anything
    fGameplayTickCount:cardinal;
    fGameName:string;
    fMissionFile:string; //Remember what we are playing incase we might want to replay
    ID_Tracker:cardinal; //Mainly Units-Houses tracker, to issue unique numbers on demand
    ActiveCampaign:TCampaign; //Campaign we are playing
    ActiveCampaignMap:byte; //Map of campaign we are playing, could be different than MaxRevealedMap
  public
    GameSpeed:integer;
    GameState:TGameState;
    PlayOnState:gr_Message;
    SkipReplayEndCheck:boolean;
    fGameInputProcess:TGameInputProcess;
    fProjectiles:TKMProjectiles;
    fMusicLib: TMusicLib;
    fGlobalSettings: TGlobalSettings;
    fCampaignSettings: TCampaignSettings;
    fMainMenuInterface: TKMMainMenuInterface;
    fGameplayInterface: TKMGamePlayInterface;
    fMapEditorInterface: TKMapEdInterface;
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean; {$IFDEF WDC} aMediaPlayer:TMediaPlayer; {$ENDIF} NoMusic:boolean=false);
    destructor Destroy; override;
    procedure ToggleLocale();
    procedure ResizeGameArea(X,Y:integer);
    procedure ZoomInGameArea(X:single);
    procedure ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
    procedure KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure GameInit();
    procedure GameStart(aMissionFile, aGameName:string; aCamp:TCampaign=cmp_Nil; aCampMap:byte=1);
    procedure GameError(aLoc:TKMPoint; aText:string); //Stop the game because of an error ()
    procedure GameSetState(aNewState:TGameState);
    procedure GameHold(DoHold:boolean; Msg:gr_Message); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure GameStop(const Msg:gr_Message; TextMsg:string='');

    procedure MapEditorStart(aMissionPath:string; aSizeX:integer=64; aSizeY:integer=64);
    procedure MapEditorSave(aMissionName:string; DoExpandPath:boolean);


    function  ReplayExists():boolean;
    procedure ReplayView(Sender:TObject);

    function GetMissionTime:cardinal;
    function CheckTime(aTimeTicks:cardinal):boolean;
    property GetTickCount:cardinal read fGameplayTickCount;
    property GetMissionFile:string read fMissionFile;
    property GetGameName:string read fGameName;
    property GetCampaign:TCampaign read ActiveCampaign;
    property GetCampaignMap:byte read ActiveCampaignMap;
    function GetNewID():cardinal;

    function Save(SlotID:shortint):string;
    function Load(SlotID:shortint):string;

    procedure UpdateState;
    procedure UpdateStateIdle(aFrameTime:cardinal);
    procedure PaintInterface;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Unit1, KM_Houses, KM_Player, KM_Units_Warrior;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean; {$IFDEF WDC} aMediaPlayer:TMediaPlayer; {$ENDIF} NoMusic:boolean=false);
begin
  Inherited Create;
  ID_Tracker := 0;
  PlayOnState := gr_Cancel;
  SelectingTroopDirection := false;
  SelectingDirPosition := Point(0,0);
  ScreenX := aScreenX;
  ScreenY := aScreenY;

  fGlobalSettings := TGlobalSettings.Create;
  fRender         := TRender.Create(RenderHandle, aVSync);
  fTextLibrary    := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.GetLocale);
  fSoundLib       := TSoundLib.Create(fGlobalSettings.GetLocale); //Required for button click sounds
  fMusicLib       := TMusicLib.Create({$IFDEF WDC} aMediaPlayer {$ENDIF});
  //todo: @Krom: When I start the game with music disabled there is about 100ms of music
  //which then cuts off. I assume the INI file is read after starting playback or something?
  fGlobalSettings.UpdateSFXVolume;
  fResource       := TResource.Create;
  fResource.LoadMenuResources(fGlobalSettings.GetLocale);

  fMainMenuInterface:= TKMMainMenuInterface.Create(ScreenX,ScreenY,fGlobalSettings);

  if not NoMusic then fMusicLib.PlayMenuTrack(not fGlobalSettings.IsMusic);

  fCampaignSettings := TCampaignSettings.Create;
  GameSpeed := 1;
  GameState := gsNoGame;
  SkipReplayEndCheck := false;
  FormControlsVisible:=true;
  fLog.AppendLog('<== Game creation is done ==>');
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  fMusicLib.StopMusic; //Stop music imediently, so it doesn't keep playing and jerk while things closes

  FreeThenNil(fCampaignSettings);
  FreeThenNil(fGlobalSettings);
  FreeThenNil(fMainMenuInterface);
  FreeThenNil(fResource);
  FreeThenNil(fSoundLib);
  FreeThenNil(fMusicLib);
  FreeThenNil(fTextLibrary);
  FreeThenNil(fRender);
  Inherited;
end;


procedure TKMGame.ToggleLocale();
begin
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fTextLibrary);
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.GetLocale);
  fResource.LoadFonts(false, fGlobalSettings.GetLocale);
  fMainMenuInterface := TKMMainMenuInterface.Create(ScreenX, ScreenY, fGlobalSettings);
  fMainMenuInterface.ShowScreen_Options;
end;


procedure TKMGame.ResizeGameArea(X,Y:integer);
begin
  ScreenX := X;
  ScreenY := Y;
  fRender.RenderResize(X,Y,rm2D);

  if GameState = gsNoGame then
    fMainMenuInterface.SetScreenSize(X,Y)
  else begin //If game is running
    fViewport.SetVisibleScreenArea(X,Y);
    if GameState = gsEditor then
      fMapEditorInterface.SetScreenSize(X,Y)
    else
      fGamePlayInterface.SetScreenSize(X,Y);
    ZoomInGameArea(1); //Reset zoom to default
  end;
end;


procedure TKMGame.ZoomInGameArea(X:single);
begin
  if GameState in [gsRunning, gsReplay, gsEditor] then fViewport.SetZoom(X);
end;


procedure TKMGame.ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
begin
  Form1.ToggleFullScreen(aToggle, fGlobalSettings.GetResolutionID, fGlobalSettings.IsVSync, ReturnToOptions);
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
begin
  //List of conflicting keys:
  //F12 Pauses Execution and switches to debug
  //F10 sets focus on MainMenu1
  //F9 is the default key in Fraps for video capture
  //others.. unknown

  if not IsDown and (Key=VK_F5) then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;
  if not IsDown and ENABLE_DESIGN_CONTORLS and (Key = VK_F7) then
    MODE_DESIGN_CONTORLS := not MODE_DESIGN_CONTORLS;

  case GameState of
    gsNoGame:   if fMainMenuInterface.MyControls.KeyUp(Key, Shift, IsDown) then exit; //Exit if handled
    gsPaused:   if Key=ord('P') then begin //Ignore all keys if game is on 'Pause'
                  if IsDown then exit;
                  GameSetState(gsRunning);
                  fGameplayInterface.ShowPause(false); //Hide pause overlay
                end;
    gsOnHold:   ; //Ignore all keys if game is on victory 'Hold', only accept mouse clicks
    gsRunning:  begin //Game is running normally
                  if fGameplayInterface.MyControls.KeyUp(Key, Shift, IsDown) then exit;

                  //Scrolling
                  if Key = VK_LEFT  then fViewport.ScrollKeyLeft  := IsDown;
                  if Key = VK_RIGHT then fViewport.ScrollKeyRight := IsDown;
                  if Key = VK_UP    then fViewport.ScrollKeyUp    := IsDown;
                  if Key = VK_DOWN  then fViewport.ScrollKeyDown  := IsDown;

                  if IsDown then exit;
                  if Key = VK_BACK then begin
                    //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
                    //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
                    fViewport.SetZoom(1);
                    Form1.TB_Angle.Position := 0;
                    Form1.TB_Angle_Change(Form1.TB_Angle);
                  end;
                  if Key = VK_F8 then begin
                    GameSpeed := fGlobalSettings.GetSpeedup+1-GameSpeed; //1 or 11
                    if not (GameSpeed in [1,fGlobalSettings.GetSpeedup]) then GameSpeed:=1; //Reset just in case
                    fGameplayInterface.ShowClock(GameSpeed = fGlobalSettings.GetSpeedup);
                  end;
                  if Key = ord('P') then begin
                    GameSetState(gsPaused);
                    fGameplayInterface.ShowPause(true); //Display pause overlay
                  end;
                  if Key=ord('W') then
                    fTerrain.RevealWholeMap(MyPlayer.PlayerID);
                  if fGamePlayInterface <> nil then //Also send shortcut to GamePlayInterface if it is there
                    fGamePlayInterface.ShortcutPress(Key, IsDown);

                  {Thats my debug example}
                  if Key=ord('5') then fGameplayInterface.MessageIssue(msgText,'123',KMPoint(0,0));
                  if Key=ord('6') then fGameplayInterface.MessageIssue(msgHouse,'123',KMPointRound(fViewport.GetCenter));
                  if Key=ord('7') then fGameplayInterface.MessageIssue(msgUnit,'123',KMPoint(0,0));
                  if Key=ord('8') then fGameplayInterface.MessageIssue(msgHorn,'123',KMPoint(0,0));
                  if Key=ord('9') then fGameplayInterface.MessageIssue(msgQuill,'123',KMPoint(0,0));
                  if Key=ord('0') then fGameplayInterface.MessageIssue(msgScroll,'123',KMPoint(0,0));

                  if Key=ord('V') then begin GameHold(true, gr_Win); exit; end; //Instant victory
                  if Key=ord('D') then begin GameHold(true, gr_Defeat); exit; end; //Instant defeat
                end;
    gsReplay:   begin
                  if IsDown then exit;
                  if Key = VK_BACK then begin
                    //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
                    //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
                    fViewport.SetZoom(1);
                    Form1.TB_Angle.Position := 0;
                    Form1.TB_Angle_Change(Form1.TB_Angle);
                  end;
                  if Key = VK_F8 then begin
                    GameSpeed := fGlobalSettings.GetSpeedup+1-GameSpeed; //1 or 11
                    if not (GameSpeed in [1,fGlobalSettings.GetSpeedup]) then GameSpeed:=1; //Reset just in case
                    fGameplayInterface.ShowClock(GameSpeed = fGlobalSettings.GetSpeedup);
                  end;
                  if Key=ord('W') then
                    fTerrain.RevealWholeMap(MyPlayer.PlayerID);
                end;
    gsEditor:   begin
                  if fMapEditorInterface.MyControls.KeyUp(Key, Shift, IsDown) then exit;
                  
                  //Scrolling
                  if Key = VK_LEFT  then fViewport.ScrollKeyLeft  := IsDown;
                  if Key = VK_RIGHT then fViewport.ScrollKeyRight := IsDown;
                  if Key = VK_UP    then fViewport.ScrollKeyUp    := IsDown;
                  if Key = VK_DOWN  then fViewport.ScrollKeyDown  := IsDown;

                  if IsDown then exit;
                  if Key = VK_BACK then begin
                    //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
                    //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
                    fViewport.SetZoom(1);
                    Form1.TB_Angle.Position := 0;
                    Form1.TB_Angle_Change(Form1.TB_Angle);
                  end;
                end;
  end;

  {Global hotkey for menu}
  if not IsDown and (Key=VK_F11) then begin
    Form1.ToggleControlsVisibility(FormControlsVisible);
    FormControlsVisible := not FormControlsVisible;
  end;

end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MyRect: TRect; MOver:TKMControl; HitUnit: TKMUnit; HitHouse: TKMHouse;
begin
  case GameState of
    gsNoGame:   fMainMenuInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsPaused:   fGameplayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsOnHold:   fGameplayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsReplay:   fGameplayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsRunning:  begin
                  fGameplayInterface.MyControls.MouseDown(X,Y,Shift,Button);
                  MOver := fGameplayInterface.MyControls.CtrlOver;

                  //These are only for testing purposes, Later on it should be changed a lot
                  if (Button = mbRight)
                    and(MOver = nil)
                    and(fGamePlayInterface <> nil)
                    and(not fGamePlayInterface.JoiningGroups)
                    and(fGamePlayInterface.GetShownUnit is TKMUnitWarrior)
                    and(TKMUnit(fGamePlayInterface.GetShownUnit).GetOwner = MyPlayer.PlayerID)
                    then
                  begin
                    //See if we are moving or attacking
                    HitUnit := fPlayers.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                    if (HitUnit <> nil) and (not (HitUnit is TKMUnitAnimal)) and
                       (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitUnit.GetOwner) = at_Enemy) and
                      (fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.GetShownUnit).GetPosition, GameCursor.Cell, canWalk, 0, false)) then
                    begin
                      //Place attack order here rather than in mouse up; why here??
                      fGameInputProcess.CmdArmy(TKMUnitWarrior(fGamePlayInterface.GetShownUnit).GetCommander, gic_ArmyAttackUnit, HitUnit);
                    end
                    else
                    begin
                      HitHouse := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                      if (HitHouse <> nil) and (not (HitHouse.IsDestroyed)) and
                         (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitHouse.GetOwner) = at_Enemy) then
                      begin
                        fGameInputProcess.CmdArmy(TKMUnitWarrior(fGamePlayInterface.GetShownUnit).GetCommander, gic_ArmyAttackHouse, HitHouse);
                      end
                      else
                      if (fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.GetShownUnit).GetPosition, GameCursor.Cell, canWalk, 0, false)) then
                      begin
                        SelectingTroopDirection := true; //MouseMove will take care of cursor changing
                        //Record current cursor position so we can stop it from moving while we are setting direction
                        GetCursorPos(SelectingDirPosition); //First record it in referance to the screen pos for the clipcursor function
                        //Restrict cursor to a rectangle (half a rect in both axes)
                        MyRect := Rect(SelectingDirPosition.X-((DirCursorSqrSize-1) div 2),
                                       SelectingDirPosition.Y-((DirCursorSqrSize-1) div 2),
                                       SelectingDirPosition.X+((DirCursorSqrSize-1) div 2)+1,
                                       SelectingDirPosition.Y+((DirCursorSqrSize-1) div 2)+1);
                        ClipCursor(@MyRect);
                        //Now record it as Client XY
                        SelectingDirPosition := Point(X,Y);
                        SelectedDirection := dir_NA;
                        fGamePlayInterface.ShowDirectionCursor(true,X,Y,SelectedDirection);
                        Screen.Cursor := c_Invisible;
                      end;
                    end;
                  end
                  else
                  begin
                    if SelectingTroopDirection then
                      Form1.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
                    SelectingTroopDirection := false;
                    fGamePlayInterface.ShowDirectionCursor(false);
                  end;
                end;
    gsEditor:   begin
                  fMapEditorInterface.MyControls.MouseDown(X,Y,Shift,Button);
                  if fMapEditorInterface.MyControls.CtrlOver<>nil then
                    Screen.Cursor:=c_Default
                  else
                    fTerrain.ComputeCursorPosition(X,Y,Shift); //So terrain brushes start on mouse down not mouse move
                end;
  end;
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
var P:TKMPoint; HitUnit: TKMUnit; HitHouse: TKMHouse; DeltaX,DeltaY:shortint;
begin
  if InRange(X,1,ScreenX-1) and InRange(Y,1,ScreenY-1) then else exit; //Exit if Cursor is outside of frame

  case GameState of
    gsNoGame:   begin
                  fMainMenuInterface.MyControls.MouseMove(X,Y,Shift);
                  if fMainMenuInterface.MyControls.CtrlOver is TKMEdit then // Show "CanEdit" cursor
                    Screen.Cursor := c_Edit  //todo: [Lewin] Make our own 'I' cursor using textures from other cursors
                  else
                    Screen.Cursor := c_Default;
                  fMainMenuInterface.MouseMove(X,Y);
                end;
    gsPaused:   fGameplayInterface.MyControls.MouseMove(X,Y,Shift);
    gsOnHold:   begin
                  fGameplayInterface.MyControls.MouseMove(X,Y,Shift);
                  if fGameplayInterface.MyControls.CtrlOver<>nil then
                    Screen.Cursor := c_Default
                end;
    gsRunning:  begin
                  if SelectingTroopDirection then
                  begin
                    DeltaX := SelectingDirPosition.X - X;
                    DeltaY := SelectingDirPosition.Y - Y;
                    //Compare cursor position and decide which direction it is
                    SelectedDirection := KMGetCursorDirection(DeltaX, DeltaY);
                    //Update the cursor based on this direction and negate the offset
                    fGamePlayInterface.ShowDirectionCursor(true,X+DeltaX,Y+DeltaY,SelectedDirection);
                    Screen.Cursor := c_Invisible; //Keep it invisible, just in case
                  end
                  else
                  begin
                  fGameplayInterface.MyControls.MouseMove(X,Y,Shift);
                  if fGameplayInterface.MyControls.CtrlOver<>nil then
                    Screen.Cursor := c_Default
                  else begin
                    fTerrain.ComputeCursorPosition(X,Y,Shift);
                    if GameCursor.Mode=cm_None then
                      if fGamePlayInterface.JoiningGroups and
                        (fGamePlayInterface.GetShownUnit is TKMUnitWarrior) then
                      begin
                        HitUnit  := MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                        if (HitUnit <> nil) and (not TKMUnitWarrior(HitUnit).IsSameGroup(TKMUnitWarrior(fGamePlayInterface.GetShownUnit))) and
                           (UnitGroups[byte(HitUnit.UnitType)] = UnitGroups[byte(fGamePlayInterface.GetShownUnit.UnitType)]) then
                          Screen.Cursor := c_JoinYes
                        else
                          Screen.Cursor := c_JoinNo;
                      end
                      else
                        if (MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil)or
                           (MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil) then
                          Screen.Cursor := c_Info
                        else
                        if fGamePlayInterface.GetShownUnit is TKMUnitWarrior then
                        begin
                          HitUnit  := fPlayers.UnitsHitTest (GameCursor.Cell.X, GameCursor.Cell.Y);
                          HitHouse := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                          if (fTerrain.CheckTileRevelation(GameCursor.Cell.X, GameCursor.Cell.Y, MyPlayer.PlayerID)>0) and
                             (((HitUnit<>nil) and (not (HitUnit is TKMUnitAnimal)) and (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitUnit.GetOwner) = at_Enemy))or
                              ((HitHouse<>nil) and (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitHouse.GetOwner) = at_Enemy))) then
                            Screen.Cursor := c_Attack
                          else if not fViewport.Scrolling then
                            Screen.Cursor := c_Default;
                        end
                        else if not fViewport.Scrolling then
                          Screen.Cursor := c_Default;
                  end;
                  end;
                end;
    gsReplay:   begin
                  fGameplayInterface.MyControls.MouseMove(X,Y,Shift); //To control minimap                  
                  fTerrain.ComputeCursorPosition(X,Y,Shift); //To show coords in status bar
                end;
    gsEditor:   begin
                  fMapEditorInterface.MyControls.MouseMove(X,Y,Shift);
                  if fMapEditorInterface.MyControls.CtrlOver<>nil then
                    Screen.Cursor:=c_Default
                  else
                  begin
                    fTerrain.ComputeCursorPosition(X,Y,Shift);
                    if GameCursor.Mode=cm_None then
                      if (MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil)or
                         (MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil) then
                        Screen.Cursor:=c_Info
                      else if not fViewport.Scrolling then
                        Screen.Cursor:=c_Default;

                    if ssLeft in Shift then //Only allow placing of roads etc. with the left mouse button
                    begin
                      P := GameCursor.Cell; //Get cursor position tile-wise
                      case GameCursor.Mode of
                        cm_Road:  if fTerrain.CanPlaceRoad(P, mu_RoadPlan)  then MyPlayer.AddRoad(P,false);
                        cm_Field: if fTerrain.CanPlaceRoad(P, mu_FieldPlan) then MyPlayer.AddField(P,ft_Corn);
                        cm_Wine:  if fTerrain.CanPlaceRoad(P, mu_WinePlan)  then MyPlayer.AddField(P,ft_Wine);
                        //cm_Wall: if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                        cm_Objects: if GameCursor.Tag1 = 255 then fTerrain.SetTree(P, 255); //Allow many objects to be deleted at once
                        cm_Erase: begin
                                    case fMapEditorInterface.GetShownPage of
                                      esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                                      esp_Units:      MyPlayer.RemUnit(P);
                                      esp_Buildings:  begin
                                                        MyPlayer.RemHouse(P,true,false,true);
                                                        if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                                          fTerrain.RemRoad(P);
                                                        if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                                          fTerrain.RemField(P);
                                                      end;
                                    end;
                                  end;
                      end;
                    end;
                  end;
                end;
  end;

Form1.StatusBar1.Panels.Items[1].Text := Format('Cursor: %.1f:%.1f [%d:%d]', [
                                         GameCursor.Float.X, GameCursor.Float.Y,
                                         GameCursor.Cell.X, GameCursor.Cell.Y]);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TKMPoint; MOver:TKMControl; HitUnit: TKMUnit; OldSelected: TObject; OldDirSelecting: boolean;
begin
  OldDirSelecting := SelectingTroopDirection;
  if SelectingTroopDirection then
  begin
    //Reset the cursor position as it will have moved during direction selection
    SetCursorPos(Form1.Panel5.ClientToScreen(SelectingDirPosition).X,Form1.Panel5.ClientToScreen(SelectingDirPosition).Y);
    Form1.ApplyCursorRestriction; //Reset the cursor restrictions from selecting direction
    SelectingTroopDirection := false; //As soon as mouse is released
    fGamePlayInterface.ShowDirectionCursor(false);
  end;

  case GameState of //Remember clicked control
    gsNoGame:   MOver := fMainMenuInterface.MyControls.CtrlOver;
    gsPaused:   MOver := fGameplayInterface.MyControls.CtrlOver;
    gsOnHold:   MOver := fGameplayInterface.MyControls.CtrlOver;
    gsRunning:  MOver := fGameplayInterface.MyControls.CtrlOver;
    gsReplay:   MOver := fGameplayInterface.MyControls.CtrlOver;
    gsEditor:   MOver := fMapEditorInterface.MyControls.CtrlOver;
    else        MOver := nil; //MOver should always be initialized
  end;

  if (MOver <> nil) and (MOver is TKMButton) and MOver.Enabled and TKMButton(MOver).MakesSound then fSoundLib.Play(sfx_click);

  case GameState of
    gsNoGame:   fMainMenuInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsPaused:   fGameplayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsOnHold:   fGameplayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsReplay:   fGameplayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsRunning:
      begin
        P := GameCursor.Cell; //Get cursor position tile-wise
        if MOver <> nil then
          fGameplayInterface.MyControls.MouseUp(X,Y,Shift,Button)
        else begin

          if (Button = mbMiddle) and (fGameplayInterface.MyControls.CtrlOver = nil) then
            fGameInputProcess.CmdTemp(gic_TempAddScout, GameCursor.Cell);

          if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
          begin
            case GameCursor.Mode of
              cm_None:
                if not fGamePlayInterface.JoiningGroups then
                begin
                  //You cannot select nil (or unit/house from other team) simply by clicking on the terrain
                  OldSelected := fPlayers.Selected;
                  if (not fPlayers.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y)) or
                    ((fPlayers.Selected is TKMHouse) and (TKMHouse(fPlayers.Selected).GetOwner <> MyPlayer.PlayerID))or
                    ((fPlayers.Selected is TKMUnit) and (TKMUnit(fPlayers.Selected).GetOwner <> MyPlayer.PlayerID)) then
                    fPlayers.Selected := OldSelected;

                  if (fPlayers.Selected is TKMHouse) then
                    fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected));

                  if (fPlayers.Selected is TKMUnit) then begin
                    fGamePlayInterface.ShowUnitInfo(TKMUnit(fPlayers.Selected));
                    if (fPlayers.Selected is TKMUnitWarrior) and (OldSelected <> fPlayers.Selected) then
                      fSoundLib.PlayWarrior(TKMUnit(fPlayers.Selected).UnitType, sp_Select);
                  end;
                end;
              cm_Road:  if fTerrain.Land[P.Y,P.X].Markup = mu_RoadPlan then
                          fGameInputProcess.CmdBuild(gic_BuildRemovePlan, P)
                        else
                          fGameInputProcess.CmdBuild(gic_BuildRoadPlan, P);

              cm_Field: if fTerrain.Land[P.Y,P.X].Markup = mu_FieldPlan then
                          fGameInputProcess.CmdBuild(gic_BuildRemovePlan, P)
                        else
                          fGameInputProcess.CmdBuild(gic_BuildFieldPlan, P);
              cm_Wine:  if fTerrain.Land[P.Y,P.X].Markup = mu_WinePlan then
                          fGameInputProcess.CmdBuild(gic_BuildRemovePlan, P)
                        else
                          fGameInputProcess.CmdBuild(gic_BuildWinePlan, P);
              cm_Wall:  if fTerrain.Land[P.Y,P.X].Markup = mu_WallPlan then
                          fGameInputProcess.CmdBuild(gic_BuildRemovePlan, P)
                        else
                          fGameInputProcess.CmdBuild(gic_BuildWallPlan, P);
              cm_Houses: if fTerrain.CanPlaceHouse(P, THouseType(GameCursor.Tag1)) then begin
                           fGameInputProcess.CmdBuild(gic_BuildHousePlan, P, THouseType(GameCursor.Tag1));
                           fSoundLib.Play(sfx_placemarker);
                           fGamePlayInterface.Build_SelectRoad;
                         end else
                           fSoundLib.Play(sfx_CantPlace,P,false,4.0);
              cm_Erase:
                begin
                  fPlayers.Selected := MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y); //Select the house irregardless of unit below/above
                  if MyPlayer.RemHouse(P,false,true) then //Ask wherever player wants to destroy own house
                  begin
                    //don't ask about houses that are not started, they are removed bellow
                    if TKMHouse(fPlayers.Selected).GetBuildingState <> hbs_Glyph then
                    begin
                      fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected),true);
                      fSoundLib.Play(sfx_click);
                    end;
                  end;
                  if (not MyPlayer.RemPlan(P)) and (not MyPlayer.RemHouse(P,false,true)) then
                    fSoundLib.Play(sfx_CantPlace,P,false,4.0); //Otherwise there is nothing to erase
                  //Now remove houses that are not started
                  if MyPlayer.RemHouse(P,false,true) and (TKMHouse(fPlayers.Selected).GetBuildingState = hbs_Glyph) then
                  begin
                    fGameInputProcess.CmdBuild(gic_BuildRemoveHouse, P);
                    fSoundLib.Play(sfx_click);
                  end;
                end;

            end; //case CursorMode.Mode of..
            if fGamePlayInterface.JoiningGroups and (fGamePlayInterface.GetShownUnit <> nil) and
              (fGamePlayInterface.GetShownUnit is TKMUnitWarrior) then
            begin
              HitUnit  := MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
              if (HitUnit <> nil) and (not TKMUnitWarrior(HitUnit).IsSameGroup(TKMUnitWarrior(fGamePlayInterface.GetShownUnit))) and
                 (UnitGroups[byte(HitUnit.UnitType)] = UnitGroups[byte(fGamePlayInterface.GetShownUnit.UnitType)]) then
              begin
                fGameInputProcess.CmdArmy(TKMUnitWarrior(fGamePlayInterface.GetShownUnit), gic_ArmyLink, HitUnit);
                fGamePlayInterface.JoiningGroups := false;
                fGamePlayInterface.ShowUnitInfo(fGamePlayInterface.GetShownUnit); //Refresh unit display
                Screen.Cursor:=c_Default; //Reset cursor when mouse released
              end;
            end;
          end;
        end; //if MOver<>nil then else..

        //These are only for testing purposes, Later on it should be changed a lot
        if (Button = mbRight)
        and(MOver = nil)
        and(fGamePlayInterface <> nil)
        and(fGamePlayInterface.GetShownUnit <> nil)
        and(OldDirSelecting) //If this is false then we are not moving, possibly attacking
        and(SelectingDirPosition.x <> 0)
        and(fGamePlayInterface.GetShownUnit is TKMUnitWarrior)
        and(TKMUnit(fGamePlayInterface.GetShownUnit).GetOwner = MyPlayer.PlayerID)
        and(not fGamePlayInterface.JoiningGroups)
        and(fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.GetShownUnit).GetPosition, P, canWalk, 0, false))
        then
        begin
          Screen.Cursor:=c_Default; //Reset cursor when mouse released
          fGameInputProcess.CmdArmy(TKMUnitWarrior(fGamePlayInterface.GetShownUnit), gic_ArmyWalk, P, SelectedDirection);
        end;

        if (Button = mbRight) and (MOver = nil) then
        begin
          fGameplayInterface.RightClickCancel; //Right clicking closes some menus
          if (Screen.Cursor = c_JoinYes) or (Screen.Cursor = c_JoinNo) then
            Screen.Cursor:=c_Default; //Reset cursor if it was joining
        end;

      end; //gsRunning
    gsEditor: begin
                if MOver <> nil then
                  fMapEditorInterface.MyControls.MouseUp(X,Y,Shift,Button)
                else
                begin
                fTerrain.ComputeCursorPosition(X,Y,Shift); //Update the cursor position and shift state in case it's changed
                P := GameCursor.Cell; //Get cursor position tile-wise
                if Button = mbRight then
                begin
                  fMapEditorInterface.RightClick_Cancel;

                  //Right click performs some special functions and shortcuts
                  case GameCursor.Mode of
                    cm_Tiles:   fMapEditorInterface.SetTileDirection(GameCursor.Tag2+1); //Rotate tile direction
                    cm_Objects: fTerrain.Land[P.Y,P.X].Obj := 255; //Delete object
                  end;
                  //Move the selected object to the cursor location
                  if fPlayers.Selected is TKMHouse then
                    TKMHouse(fPlayers.Selected).SetPosition(P); //Can place is checked in SetPosition

                  if fPlayers.Selected is TKMUnit then
                    if fTerrain.CanPlaceUnit(P, TKMUnit(fPlayers.Selected).UnitType) then
                      TKMUnit(fPlayers.Selected).SetPosition(P);

                end
                else
                if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
                  case GameCursor.Mode of
                    cm_None:  begin
                                fPlayers.HitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                                if fPlayers.Selected is TKMHouse then
                                  fMapEditorInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected));
                                if fPlayers.Selected is TKMUnit then
                                  fMapEditorInterface.ShowUnitInfo(TKMUnit(fPlayers.Selected));
                              end;
                    cm_Road:  if fTerrain.CanPlaceRoad(P, mu_RoadPlan) then MyPlayer.AddRoad(P,false);
                    cm_Field: if fTerrain.CanPlaceRoad(P, mu_FieldPlan) then MyPlayer.AddField(P,ft_Corn);
                    cm_Wine:  if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                    //cm_Wall:
                    cm_Houses:if fTerrain.CanPlaceHouse(P, THouseType(GameCursor.Tag1)) then
                              begin
                                MyPlayer.AddHouse(THouseType(GameCursor.Tag1),P);
                                fMapEditorInterface.Build_SelectRoad;
                              end;
                    cm_Height:; //handled in UpdateStateIdle
                    cm_Objects: fTerrain.SetTree(P, GameCursor.Tag1);
                    cm_Units: if fTerrain.CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                              begin //Check if we can really add a unit
                                if TUnitType(GameCursor.Tag1) in [ut_Serf..ut_Barbarian] then
                                  MyPlayer.AddUnit(TUnitType(GameCursor.Tag1), P, false)
                                else
                                  fPlayers.PlayerAnimals.AddUnit(TUnitType(GameCursor.Tag1), P, false);
                              end;
                    cm_Erase:
                              case fMapEditorInterface.GetShownPage of
                                esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                                esp_Units:      begin
                                                  MyPlayer.RemUnit(P);
                                                  fPlayers.PlayerAnimals.RemUnit(P); //Animals are common for all
                                                end;
                                esp_Buildings:  begin
                                                  MyPlayer.RemHouse(P,true,false,true);
                                                  if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                                    fTerrain.RemRoad(P);
                                                  if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                                    fTerrain.RemField(P);
                                                end;
                              end;
                  end;
                  end;
              end;
  end;

end;


procedure TKMGame.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X, Y: Integer);
var AllowZoom: boolean;
begin
  //e.g. if we're over a scrollbar it shouldn't zoom map, but this can apply for all controls (i.e. only zoom when over the map not controls)
  AllowZoom := true;
  case GameState of //Remember clicked control
    gsNoGame:   fMainMenuInterface.MyControls.MouseWheel(X, Y, WheelDelta);
    gsPaused:   ;
    gsOnHold:   ;
    gsRunning:  begin
                  fGameplayInterface.MyControls.MouseWheel(X, Y, WheelDelta);
                  AllowZoom := (fGameplayInterface.MyControls.CtrlOver = nil);
                end;
    gsReplay:   fGameplayInterface.MyControls.MouseWheel(X, Y, WheelDelta);
    gsEditor:   begin
                  fMapEditorInterface.MyControls.MouseWheel(X, Y, WheelDelta);
                  AllowZoom := (fMapEditorInterface.MyControls.CtrlOver = nil);
                end;
  end;

  if (MOUSEWHEEL_ZOOM_ENABLE) and (GameState in [gsRunning,gsEditor]) and (AllowZoom) then
    fViewport.SetZoom(fViewport.Zoom+WheelDelta/2000);
end;


procedure TKMGame.GameInit();
begin
  RandSeed := 4; //Sets right from the start since it affects TKMAllPlayers.Create and other Types
  GameSpeed := 1; //In case it was set in last run mission
  PlayOnState := gr_Cancel;

  if fResource.GetDataState<>dls_All then begin
    fMainMenuInterface.ShowScreen_Loading('units and houses');
    fRender.Render;
    fResource.LoadGameResources();
    fMainMenuInterface.ShowScreen_Loading('tileset');
    fRender.Render;
    fRender.LoadTileSet();
  end;

  fMainMenuInterface.ShowScreen_Loading('initializing');
  fRender.Render;

  fViewport := TViewport.Create;
  fGamePlayInterface := TKMGamePlayInterface.Create();

  //Here comes terrain/mission init
  fTerrain := TTerrain.Create;
  fProjectiles := TKMProjectiles.Create;

  fRender.RenderResize(ScreenX,ScreenY,rm2D);
  fViewport.SetVisibleScreenArea(ScreenX,ScreenY);

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
    ActiveCampaign := aCamp;
    ActiveCampaignMap := aCampMap; //MapID is incremented in CampSettings and passed on to here from outside
  end;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(fMissionFile,true) then
  begin
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
        LoadError := 'An error was encountered while parsing the file '+fMissionFile+'.|Details of the error:|'+
                      E.ClassName+' error raised with message: '+E.Message;
        if GameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
        fMainMenuInterface.ShowScreen_Error(LoadError);
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
  fViewPort.SetZoom(1); //This ensures the viewport is centered on the map

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  fGamePlayInterface.EnableOrDisableMenuIcons(not (fPlayers.fMissionMode = mm_Tactic));

  fLog.AppendLog('Gameplay initialized',true);

  GameState := gsRunning;

  fGameInputProcess := TGameInputProcess.Create(gipRecording);
  Save(99); //Thats our base for a game record
  CopyFile(PChar(KMSlotToSaveName(99,'sav')), PChar(KMSlotToSaveName(99,'bas')), false);

  fLog.AppendLog('Gameplay recording initialized',true);
  RandSeed := 4; //Random after StartGame and ViewReplay should match
end;


{ Set viewport and save command log }
procedure TKMGame.GameError(aLoc:TKMPoint; aText:string);
begin
  //Negotiate duplicate calls for GameError
  if GameState = gsNoGame then exit;

  fViewport.SetCenter(aLoc.X, aLoc.Y);
  GameSetState(gsPaused);
  SHOW_UNIT_ROUTES := true;
  SHOW_UNIT_MOVEMENT := true;
  if fTerrain.TileInMapCoords(aLoc.X, aLoc.Y) then
    fTerrain.Land[aLoc.Y, aLoc.X].IsUnit := 128;

  if MessageDlg(
    fTextLibrary.GetRemakeString(48)+UpperCase(aText)+eol+fTextLibrary.GetRemakeString(49)
    , mtWarning, [mbYes, mbNo], 0) <> mrYes then

    GameStop(gr_Error,'') //Exit to main menu will save the Replay data
  else
    if (fGameInputProcess <> nil) and (fGameInputProcess.State = gipRecording) then
      fGameInputProcess.SaveToFile(KMSlotToSaveName(99,'rpl')); //Save replay data ourselves
end;


procedure TKMGame.GameSetState(aNewState:TGameState);
begin
  GameState := aNewState;
end;


//Put the game on Hold for Victory screen
procedure TKMGame.GameHold(DoHold:boolean; Msg:gr_Message);
begin
  PlayOnState := Msg;
  case Msg of
    gr_ReplayEnd:     begin
                        if DoHold then begin
                          GameSetState(gsOnHold);
                          fGamePlayInterface.ShowPlayMore(true, Msg);
                        end else
                          GameSetState(gsReplay);
                      end;
    gr_Win,gr_Defeat: begin
                        if DoHold then begin
                          GameSetState(gsOnHold);
                          fGamePlayInterface.ShowPlayMore(true, Msg);
                        end else
                          GameSetState(gsRunning);
                      end;
  end;
end;


procedure TKMGame.GameStop(const Msg:gr_Message; TextMsg:string='');
begin
  GameState := gsNoGame;

  //Take results from MyPlayer before data is flushed
  if Msg in [gr_Win, gr_Defeat, gr_Cancel] then
    fMainMenuInterface.Fill_Results;

  if (fGameInputProcess <> nil) and (fGameInputProcess.State = gipRecording) then
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
                   fMainMenuInterface.ShowScreen_Results(Msg); //Mission results screen
                   fCampaignSettings.RevealMap(ActiveCampaign, ActiveCampaignMap+1);
                 end;
    gr_Defeat:   begin
                   fLog.AppendLog('Gameplay ended - Defeat',true);
                   fMainMenuInterface.ShowScreen_Results(Msg); //Mission results screen
                 end;
    gr_Cancel:   begin
                   fLog.AppendLog('Gameplay canceled',true);
                   fMainMenuInterface.ShowScreen_Results(Msg); //show the results so the user can see how they are going so far
                 end;
    gr_Error:    begin
                   fLog.AppendLog('Gameplay error',true);
                   fMainMenuInterface.ShowScreen_Error(TextMsg);
                 end;
    gr_Silent:   fLog.AppendLog('Gameplay stopped silently',true); //Used when loading new savegame from gameplay UI
    gr_ReplayEnd:begin
                   fLog.AppendLog('Replay canceled',true);
                   fMainMenuInterface.ShowScreen_Main;
                 end;
    gr_MapEdEnd: begin
                   fLog.AppendLog('MapEditor closed',true);
                   fMainMenuInterface.ShowScreen_Main;
                 end;
  end;
end;


{Mission name accepted in 2 formats:
- absolute path, when opening a map from Form1.Menu
- relative, from Maps folder}
procedure TKMGame.MapEditorStart(aMissionPath:string; aSizeX:integer=64; aSizeY:integer=64);
var ResultMsg:string; fMissionParser:TMissionParser; i: integer;
begin
  if not FileExists(aMissionPath) and (aSizeX*aSizeY=0) then exit; //Erroneous call

  GameStop(gr_Silent); //Stop MapEd if we are loading from existing MapEd session

  RandSeed:=4; //Sets right from the start since it affects TKMAllPlayers.Create and other Types
  GameSpeed := 1; //In case it was set in last run mission

  if fResource.GetDataState<>dls_All then begin
    fMainMenuInterface.ShowScreen_Loading('units and houses');
    fRender.Render;
    fResource.LoadGameResources();
    fMainMenuInterface.ShowScreen_Loading('tileset');
    fRender.Render;
    fRender.LoadTileSet();
  end;

  fMainMenuInterface.ShowScreen_Loading('initializing');
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

  fMapEditorInterface.Player_UpdateColors();
  fPlayers.AfterMissionInit(false);

  for i:=1 to MAX_PLAYERS do //Reveal all players since we'll swap between them in MapEd
    fTerrain.RevealWholeMap(TPlayerID(i));

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized',true);

  fRender.RenderResize(ScreenX,ScreenY,rm2D);
  fViewport.SetVisibleScreenArea(ScreenX,ScreenY);
  fViewport.SetZoom(1);

  fGameplayTickCount := 0; //Restart counter

  GameState := gsEditor;
end;


//DoExpandPath means that input is a mission name which should be expanded into:
//ExeDir+'Maps\'+MissionName+'\'+MissionName.dat
//ExeDir+'Maps\'+MissionName+'\'+MissionName.map
procedure TKMGame.MapEditorSave(aMissionName:string; DoExpandPath:boolean);
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
function TKMGame.ReplayExists():boolean;
begin
  Result := FileExists(KMSlotToSaveName(99,'bas')) and
            FileExists(KMSlotToSaveName(99,'rpl'));
end;


procedure TKMGame.ReplayView(Sender:TObject);
begin
  CopyFile(PChar(KMSlotToSaveName(99,'bas')), PChar(KMSlotToSaveName(99,'sav')), false);
  Load(99); //We load what was saved right before starting Recording
  FreeAndNil(fGameInputProcess); //Override GIP from savegame

  fGameInputProcess := TGameInputProcess.Create(gipReplaying);
  fGameInputProcess.LoadFromFile(KMSlotToSaveName(99,'rpl'));

  RandSeed := 4; //Random after StartGame and ViewReplay should match
  GameState := gsReplay;
end;


function TKMGame.GetMissionTime:cardinal;
begin
  //Treat 10 ticks as 1 sec irregardless of user-set pace
  Result := MyPlayer.fMissionSettings.GetMissionTime + (GetTickCount div 10);
end;


//Tests whether time has past
function TKMGame.CheckTime(aTimeTicks:cardinal):boolean;
begin
  Result := (GetTickCount >= aTimeTicks);
end;


function TKMGame.GetNewID():cardinal;
begin
  inc(ID_Tracker);
  Result := ID_Tracker;
end;


//Saves the game and returns string for savegame name OR empty if save failed
//Base savegame gets copied from save99.bas
//Saves command log to RPL file
function TKMGame.Save(SlotID:shortint):string;
var
  SaveStream:TKMemoryStream;
  i:integer;
begin
  fLog.AppendLog('Saving game');
  case GameState of
    gsNoGame:   exit; //Don't need to save the game if we are in menu. Never call Save from menu anyhow
    gsEditor:   exit; //MapEd gets saved differently from SaveMapEd
    gsOnHold:   exit; //No sense to save from victory?
    gsReplay:   exit;
    gsPaused,gsRunning: //Can't save from Paused state yet, but we could add it later
    begin
      SaveStream := TKMemoryStream.Create;
      SaveStream.Write('KaM_Savegame');
      SaveStream.Write(SAVE_VERSION); //This is savegame version
      SaveStream.Write(fMissionFile); //Save game mission file
      SaveStream.Write(fGameName); //Save game title
      SaveStream.Write(fGameplayTickCount); //Required to be saved, e.g. messages being shown after a time
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
        DeleteFile(KMSlotToSaveName(AUTOSAVE_SLOT+5,'sav'));
        for i:=AUTOSAVE_SLOT+5 downto AUTOSAVE_SLOT+1 do //15 to 11
          RenameFile(KMSlotToSaveName(i-1,'sav'), KMSlotToSaveName(i,'sav'));
      end;

      SaveStream.SaveToFile(KMSlotToSaveName(SlotID,'sav')); //Some 70ms for TPR7 map
      SaveStream.Free;

      if SlotID <> AUTOSAVE_SLOT then begin //Backup earlier autosaves
        CopyFile(PChar(KMSlotToSaveName(99,'bas')), PChar(KMSlotToSaveName(SlotID,'bas')), false); //replace Replay base savegame
        fGameInputProcess.SaveToFile(KMSlotToSaveName(SlotID,'rpl')); //Adds command queue to savegame
      end;//

      Result := GetGameName + ' ' + int2time(GetMissionTime);
      if (fGlobalSettings.IsAutosave) and (SlotID = AUTOSAVE_SLOT) then
        Result := fTextLibrary.GetTextString(203); //Autosave
    end;
  end;
  fLog.AppendLog('Saving game',true);
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

  if GameState in [gsRunning, gsPaused] then GameStop(gr_Silent);

  LoadStream := TKMemoryStream.Create; //Read data from file into stream
  case GameState of
    gsEditor, gsPaused, gsOnHold, gsRunning, gsReplay:   exit;
    gsNoGame: begin  //Load only from menu or stopped game
      try //Catch exceptions
        LoadStream.LoadFromFile(FileName);
        LoadStream.Seek(0, soFromBeginning);

        //Raise some exceptions if the file is invalid or the wrong save version
        LoadStream.Read(s); if s <> 'KaM_Savegame' then Raise Exception.Create('Not a valid KaM Remake save file');
        LoadStream.Read(s); if s <> SAVE_VERSION then Raise Exception.CreateFmt('Incompatible save version ''%s''. This version is ''%s''',[s,SAVE_VERSION]);

        //Create empty environment
        GameInit();

        //Substitute tick counter and id tracker
        LoadStream.Read(fMissionFile); //Savegame mission file
        LoadStream.Read(fGameName); //Savegame title
        LoadStream.Read(fGameplayTickCount);
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

        fGameInputProcess := TGameInputProcess.Create(gipRecording);
        fGameInputProcess.LoadFromFile(KMSlotToSaveName(SlotID,'rpl'));

        CopyFile(PChar(KMSlotToSaveName(SlotID,'bas')), PChar(KMSlotToSaveName(99,'bas')), false); //replace Replay base savegame

        fGamePlayInterface.EnableOrDisableMenuIcons(not (fPlayers.fMissionMode = mm_Tactic)); //Preserve disabled icons
        fPlayers.SyncLoad(); //Should parse all Unit-House ID references and replace them with actual pointers
        fViewPort.SetZoom(1); //This ensures the viewport is centered on the map (game could have been saved with a different resolution/zoom)
        Result := ''; //Loading has now completed successfully :)
        Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
      except
        on E : Exception do
        begin
          //Trap the exception and show the user. Note: While debugging, Delphi will still stop execution for the exception, but normally the dialouge won't show.
          Result := 'An error was encountered while parsing the file '+FileName+'.|Details of the error:|'+
                        E.ClassName+' error raised with message: '+E.Message;
          if GameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
          exit;
        end;
      end;
    end;
  end;

  GameState := gsRunning;
  fLog.AppendLog('Loading game',true);
end;


procedure TKMGame.UpdateState;
var i:integer;
begin
  inc(GlobalTickCount);
  case GameState of
    gsPaused:   exit;
    gsOnHold:   exit;
    gsNoGame:   begin
                  fMainMenuInterface.UpdateState;
                  if GlobalTickCount mod 10 = 0 then //Once a sec
                  if fMusicLib.IsMusicEnded then
                    fMusicLib.PlayMenuTrack(not fGlobalSettings.IsMusic); //Menu tune
                end;
    gsRunning,
    gsReplay:   begin
                  for i:=1 to GameSpeed do
                  begin
                    inc(fGameplayTickCount); //Thats our tick counter for gameplay events
                    fTerrain.UpdateState;
                    fPlayers.UpdateState(fGameplayTickCount); //Quite slow
                    if GameState = gsNoGame then exit; //Quit the update if game was stopped by MyPlayer defeat
                    fProjectiles.UpdateState; //If game has stopped it's NIL

                    if (fGameplayTickCount mod 600 = 0) and fGlobalSettings.IsAutosave then //Each 1min of gameplay time
                      Save(AUTOSAVE_SLOT); //Autosave slot

                    if GameState = gsReplay then begin
                      fGameInputProcess.Tick(fGameplayTickCount);
                      if not SkipReplayEndCheck and fGameInputProcess.Ended then
                        GameHold(true, gr_ReplayEnd);
                    end;

                    if GameState = gsNoGame then exit; //Error due to consistency fail in replay commands
                  end;

                  fGamePlayInterface.UpdateState;

                  if GlobalTickCount mod 10 = 0 then //Every 1000ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

                  if GlobalTickCount mod 10 = 0 then
                    if fMusicLib.IsMusicEnded then
                      fMusicLib.PlayNextTrack(); //Feed new music track

                  if GlobalTickCount mod 10 = 0 then
                    Form1.StatusBar1.Panels[2].Text:='Time: '+int2time(GetMissionTime);
                end;
    gsEditor:   begin
                  fMapEditorInterface.UpdateState;
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                  if GlobalTickCount mod 10 = 0 then //Every 500ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)
                end;
    end;

end;


{This is our real-time thread, use it wisely}
procedure TKMGame.UpdateStateIdle(aFrameTime:cardinal);
begin
  case GameState of
    gsRunning,
    gsReplay:   begin
                  fViewport.DoScrolling(aFrameTime); //Check to see if we need to scroll
                end;
    gsEditor:   begin
                  fViewport.DoScrolling(aFrameTime); //Check to see if we need to scroll
                  case GameCursor.Mode of
                    cm_Height:
                              if (ssLeft in GameCursor.SState) or (ssRight in GameCursor.SState) then
                              fTerrain.MapEdHeight(KMPointF(GameCursor.Float.X+1,GameCursor.Float.Y+1), GameCursor.Tag1, GameCursor.Tag2, ssLeft in GameCursor.SState);
                    cm_Tiles:
                              if (ssLeft in GameCursor.SState) then
                                if fMapEditorInterface.GetTilesRandomized then
                                  fTerrain.MapEdTile(GameCursor.Cell, GameCursor.Tag1, Random(4))
                                else
                                  fTerrain.MapEdTile(GameCursor.Cell, GameCursor.Tag1, GameCursor.Tag2)
                  end;
                end;
  end;
end;


procedure TKMGame.PaintInterface;
begin
  case GameState of
    gsNoGame:  fMainMenuInterface.Paint;
    gsPaused:  fGameplayInterface.Paint;
    gsOnHold:  fGameplayInterface.Paint;
    gsRunning: fGameplayInterface.Paint;
    gsReplay:  fGameplayInterface.Paint;
    gsEditor:  fMapEditorInterface.Paint;
  end;
end;


end.
