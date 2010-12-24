unit KM_Game;
{$I KaM_Remake.inc}
interface
uses Windows,
  {$IFDEF WDC} MPlayer, {$ENDIF}
  Forms, Controls, Classes, Dialogs, SysUtils, KromUtils, Math,
  KM_CommonTypes, KM_Defaults, KM_Utils,
  KM_Controls, KM_GameInputProcess, KM_PlayersCollection, KM_Render, KM_TextLibrary, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_MissionScript, KM_Projectiles, KM_Sound, KM_Viewport, KM_Units, KM_Settings, KM_Music;

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
    SelectingTroopDirection:boolean;
    fIsExiting: boolean; //Set this to true on Exit and unit/house pointers will be released without cross-checking
    SelectingDirPosition: TPoint;
    SelectedDirection: TKMDirection;
    fGlobalTickCount:cardinal; //Not affected by Pause and anything (Music, Minimap, StatusBar update)
    fGameSpeed:integer;
    fGameState:TGameState;
    fAdvanceFrame:boolean; //Replay variable to advance 1 frame, afterwards set to false
  private //Should be saved
    fGameplayTickCount:cardinal;
    fGameName:string;
    fMissionFile:string; //Remember what we are playing incase we might want to replay
    ID_Tracker:cardinal; //Mainly Units-Houses tracker, to issue unique numbers on demand
    fActiveCampaign:TCampaign; //Campaign we are playing
    fActiveCampaignMap:byte; //Map of campaign we are playing, could be different than MaxRevealedMap
  public
    PlayOnState:TGameResultMsg;
    SkipReplayEndCheck:boolean;
    fGameInputProcess:TGameInputProcess;
    fProjectiles:TKMProjectiles;
    fMusicLib: TMusicLib;
    fGlobalSettings: TGlobalSettings;
    fCampaignSettings: TCampaignSettings;
    fMainMenuInterface: TKMMainMenuInterface;
    fGamePlayInterface: TKMGamePlayInterface;
    fMapEditorInterface: TKMapEdInterface;
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean; {$IFDEF WDC} aMediaPlayer:TMediaPlayer; {$ENDIF} NoMusic:boolean=false);
    destructor Destroy; override;
    procedure ResetRender(RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean);
    procedure ToggleLocale(aLocale:shortstring);
    procedure ResizeGameArea(X,Y:integer);
    procedure ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean; ReInitGame:boolean=true);
    procedure KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);

    procedure GameInit();
    procedure GameStart(aMissionFile, aGameName:string; aCamp:TCampaign=cmp_Nil; aCampMap:byte=1);
    procedure GameError(aLoc:TKMPoint; aText:string); //Stop the game because of an error ()
    procedure SetGameState(aNewState:TGameState);
    procedure GameHold(DoHold:boolean; Msg:TGameResultMsg); //Hold the game to ask if player wants to play after Victory/Defeat/ReplayEnd
    procedure GameStop(const Msg:TGameResultMsg; TextMsg:string='');

    procedure MapEditorStart(const aMissionPath:string; aSizeX:integer=64; aSizeY:integer=64);
    procedure MapEditorSave(const aMissionName:string; DoExpandPath:boolean);

    function  ReplayExists():boolean;
    procedure ReplayView(Sender:TObject);

    function GetMissionTime:cardinal;
    function CheckTime(aTimeTicks:cardinal):boolean;
    property GetTickCount:cardinal read fGameplayTickCount;
    property GetMissionFile:string read fMissionFile;
    property GetGameName:string read fGameName;
    property GetCampaign:TCampaign read fActiveCampaign;
    property GetCampaignMap:byte read fActiveCampaignMap;
    property IsExiting:boolean read fIsExiting;
    function GetNewID():cardinal;
    property GameState:TGameState read fGameState;
    procedure SetGameSpeed(aSpeed:byte=0);
    procedure StepOneFrame();

    procedure Save(SlotID:shortint);
    function Load(SlotID:shortint):string;
    function LoadName(SlotID:shortint):string;

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
  fTextLibrary    := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fSoundLib       := TSoundLib.Create(fGlobalSettings.Locale, fGlobalSettings.SoundFXVolume/fGlobalSettings.SlidersMax); //Required for button click sounds
  fMusicLib       := TMusicLib.Create({$IFDEF WDC} aMediaPlayer {$ENDIF}, fGlobalSettings.MusicVolume/fGlobalSettings.SlidersMax);
  fResource       := TResource.Create;
  fResource.LoadMenuResources(fGlobalSettings.Locale);

  fMainMenuInterface:= TKMMainMenuInterface.Create(ScreenX,ScreenY,fGlobalSettings);

  if not NoMusic then fMusicLib.PlayMenuTrack(not fGlobalSettings.MusicOn);

  fCampaignSettings := TCampaignSettings.Create;
  fAdvanceFrame := false;
  fGameSpeed := 1;
  fGameState := gsNoGame;
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


procedure TKMGame.ResetRender(RenderHandle:HWND; aScreenX,aScreenY:integer; aVSync:boolean);
begin
  //@Krom: I left this here in case you figure out a way to make it work. Otherwise it can all be removed/disabled.
  FreeAndNil(fRender);
  ScreenX := aScreenX;
  ScreenY := aScreenY;
  fRender := TRender.Create(RenderHandle, aVSync);
end;


procedure TKMGame.ToggleLocale(aLocale:shortstring);
begin
  fGlobalSettings.Locale := aLocale; //Wrong Locale will be ignored
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fTextLibrary);
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', fGlobalSettings.Locale);
  fResource.LoadFonts(false, fGlobalSettings.Locale);
  fMainMenuInterface := TKMMainMenuInterface.Create(ScreenX, ScreenY, fGlobalSettings);
  fMainMenuInterface.ShowScreen_Options;
end;


procedure TKMGame.ResizeGameArea(X,Y:integer);
begin
  ScreenX := X;
  ScreenY := Y;
  fRender.RenderResize(X,Y,rm2D);

  if fGameState = gsNoGame then
    fMainMenuInterface.SetScreenSize(X,Y)
  else begin //If game is running
    fViewport.SetVisibleScreenArea(X,Y);
    fMainMenuInterface.SetScreenSize(X,Y); //It's not visible but exists
    if fGameState = gsEditor then
      fMapEditorInterface.SetScreenSize(X,Y)
    else
      fGamePlayInterface.SetScreenSize(X,Y);
    fViewport.SetZoom(fViewport.Zoom); //Zoom depends on ViewWidth/Height values
  end;
end;


procedure TKMGame.ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean; ReInitGame:boolean=true);
begin
  Form1.ToggleFullScreen(aToggle, fGlobalSettings.ResolutionID, fGlobalSettings.VSync, ReturnToOptions);
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
begin
  //List of conflicting keys:
  //F12 Pauses Execution and switches to debug
  //F10 sets focus on MainMenu1
  //F9 is the default key in Fraps for video capture
  //others.. unknown

  //GLOBAL KEYS
  if not IsDown and (Key=VK_F5) then SHOW_CONTROLS_OVERLAY := not SHOW_CONTROLS_OVERLAY;
  if not IsDown and ENABLE_DESIGN_CONTORLS and (Key = VK_F7) then
    MODE_DESIGN_CONTORLS := not MODE_DESIGN_CONTORLS;
  if not IsDown and (Key=VK_F11) then begin
    Form1.ToggleControlsVisibility(FormControlsVisible);
    FormControlsVisible := not FormControlsVisible;
  end;
  //Alt+Enter toggles fullscreen
  if not IsDown and (Key=VK_RETURN) and (ssAlt in Shift) then
  begin
    fGlobalSettings.FullScreen := not fGlobalSettings.FullScreen;
    ToggleFullScreen(fGlobalSettings.FullScreen, false, false);
  end;

  case fGameState of
    gsNoGame:   fMainMenuInterface.KeyUp(Key, Shift, IsDown); //Exit if handled
    gsPaused:   fGamePlayInterface.KeyUp(Key, Shift, IsDown);
    gsOnHold:   fGamePlayInterface.KeyUp(Key, Shift, IsDown);
    gsRunning:  fGamePlayInterface.KeyUp(Key, Shift, IsDown);
    gsReplay:   fGamePlayInterface.KeyUp(Key, Shift, IsDown);
    gsEditor:   fMapEditorInterface.KeyUp(Key, Shift, IsDown);
  end;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MyRect: TRect; MOver:TKMControl; HitUnit: TKMUnit; HitHouse: TKMHouse;
begin
  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseDown(Button,Shift,X,Y);
    gsPaused:   fGamePlayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsOnHold:   fGamePlayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsReplay:   fGamePlayInterface.MyControls.MouseDown(X,Y,Shift,Button);
    gsRunning:  begin
                  fGamePlayInterface.MyControls.MouseDown(X,Y,Shift,Button);
                  MOver := fGamePlayInterface.MyControls.CtrlOver;

                  //These are only for testing purposes, Later on it should be changed a lot
                  if (Button = mbRight)
                    and(MOver = nil)
                    and(fGamePlayInterface <> nil)
                    and(not fGamePlayInterface.JoiningGroups)
                    and(fGamePlayInterface.ShownUnit is TKMUnitWarrior)
                    and(TKMUnit(fGamePlayInterface.ShownUnit).GetOwner = MyPlayer.PlayerID)
                    then
                  begin
                    //See if we are moving or attacking
                    HitUnit := fTerrain.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                    if (HitUnit <> nil) and (not (HitUnit is TKMUnitAnimal)) and
                       (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitUnit.GetOwner) = at_Enemy) and
                      (fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.ShownUnit).GetPosition, GameCursor.Cell, CanWalk, 0, false)) then
                    begin
                      //Place attack order here rather than in mouse up; why here??
                      fGameInputProcess.CmdArmy(gic_ArmyAttackUnit, TKMUnitWarrior(fGamePlayInterface.ShownUnit).GetCommander, HitUnit);
                    end
                    else
                    begin
                      HitHouse := fPlayers.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                      if (HitHouse <> nil) and (not (HitHouse.IsDestroyed)) and
                         (fPlayers.CheckAlliance(MyPlayer.PlayerID, HitHouse.GetOwner) = at_Enemy) then
                      begin
                        fGameInputProcess.CmdArmy(gic_ArmyAttackHouse, TKMUnitWarrior(fGamePlayInterface.ShownUnit).GetCommander, HitHouse);
                      end
                      else
                      if (fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.ShownUnit).GetPosition, GameCursor.Cell, CanWalk, 0, false)) then
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
    gsEditor:   fMapEditorInterface.MouseDown(Button,Shift,X,Y);
  end;
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
var HitUnit: TKMUnit; HitHouse: TKMHouse; DeltaX,DeltaY:shortint;
begin
  if not InRange(X,1,ScreenX-1) or not InRange(Y,1,ScreenY-1) then exit; //Exit if Cursor is outside of frame

  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseMove(Shift, X,Y);
    gsPaused:   fGamePlayInterface.MyControls.MouseMove(X,Y,Shift);
    gsOnHold:   begin
                  fGamePlayInterface.MyControls.MouseMove(X,Y,Shift);
                  if fGamePlayInterface.MyControls.CtrlOver<>nil then
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
                  fGamePlayInterface.MyControls.MouseMove(X,Y,Shift);
                  if fGamePlayInterface.MyControls.CtrlOver<>nil then
                    Screen.Cursor := c_Default
                  else begin
                    fTerrain.ComputeCursorPosition(X,Y,Shift);
                    if GameCursor.Mode=cm_None then
                      if fGamePlayInterface.JoiningGroups and
                        (fGamePlayInterface.ShownUnit is TKMUnitWarrior) then
                      begin
                        HitUnit  := MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
                        if (HitUnit <> nil) and (not TKMUnitWarrior(HitUnit).IsSameGroup(TKMUnitWarrior(fGamePlayInterface.ShownUnit))) and
                           (UnitGroups[byte(HitUnit.UnitType)] = UnitGroups[byte(fGamePlayInterface.ShownUnit.UnitType)]) then
                          Screen.Cursor := c_JoinYes
                        else
                          Screen.Cursor := c_JoinNo;
                      end
                      else
                        if (MyPlayer.HousesHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil)or
                           (MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y)<>nil) then
                          Screen.Cursor := c_Info
                        else
                        if fGamePlayInterface.ShownUnit is TKMUnitWarrior then
                        begin
                          HitUnit  := fTerrain.UnitsHitTest (GameCursor.Cell.X, GameCursor.Cell.Y);
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
                  fGamePlayInterface.MyControls.MouseMove(X,Y,Shift); //To control minimap
                  fTerrain.ComputeCursorPosition(X,Y,Shift); //To show coords in status bar
                end;
    gsEditor:   fMapEditorInterface.MouseMove(Shift,X,Y);
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

  case fGameState of //Remember clicked control
    gsNoGame:   MOver := nil;
    gsPaused:   MOver := fGamePlayInterface.MyControls.CtrlOver;
    gsOnHold:   MOver := fGamePlayInterface.MyControls.CtrlOver;
    gsRunning:  MOver := fGamePlayInterface.MyControls.CtrlOver;
    gsReplay:   MOver := fGamePlayInterface.MyControls.CtrlOver;
    gsEditor:   MOver := nil;
    else        MOver := nil; //MOver should always be initialized
  end;

  if (MOver <> nil) and (MOver is TKMButton) and MOver.Enabled and TKMButton(MOver).MakesSound then fSoundLib.Play(sfx_click);

  case fGameState of
    gsNoGame:   fMainMenuInterface.MouseUp(Button, Shift, X,Y);
    gsPaused:   fGamePlayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsOnHold:   fGamePlayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsReplay:   fGamePlayInterface.MyControls.MouseUp(X,Y,Shift,Button);
    gsRunning:
      begin
        P := GameCursor.Cell; //Get cursor position tile-wise
        if MOver <> nil then
          fGamePlayInterface.MyControls.MouseUp(X,Y,Shift,Button)
        else begin

          if (Button = mbMiddle) and (fGamePlayInterface.MyControls.CtrlOver = nil) then
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
                    if TKMHouse(fPlayers.Selected).BuildingState <> hbs_Glyph then
                    begin
                      fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected),true);
                      fSoundLib.Play(sfx_click);
                    end;
                  end;
                  if (not MyPlayer.RemPlan(P)) and (not MyPlayer.RemHouse(P,false,true)) then
                    fSoundLib.Play(sfx_CantPlace,P,false,4.0); //Otherwise there is nothing to erase
                  //Now remove houses that are not started
                  if MyPlayer.RemHouse(P,false,true) and (TKMHouse(fPlayers.Selected).BuildingState = hbs_Glyph) then
                  begin
                    fGameInputProcess.CmdBuild(gic_BuildRemoveHouse, P);
                    fSoundLib.Play(sfx_click);
                  end;
                end;

            end; //case CursorMode.Mode of..
            if fGamePlayInterface.JoiningGroups and (fGamePlayInterface.ShownUnit <> nil) and
              (fGamePlayInterface.ShownUnit is TKMUnitWarrior) then
            begin
              HitUnit  := MyPlayer.UnitsHitTest(GameCursor.Cell.X, GameCursor.Cell.Y);
              if (HitUnit <> nil) and (not TKMUnitWarrior(HitUnit).IsSameGroup(TKMUnitWarrior(fGamePlayInterface.ShownUnit))) and
                 (UnitGroups[byte(HitUnit.UnitType)] = UnitGroups[byte(fGamePlayInterface.ShownUnit.UnitType)]) then
              begin
                fGameInputProcess.CmdArmy(gic_ArmyLink, TKMUnitWarrior(fGamePlayInterface.ShownUnit), HitUnit);
                fGamePlayInterface.JoiningGroups := false;
                fGamePlayInterface.ShowUnitInfo(fGamePlayInterface.ShownUnit); //Refresh unit display
                Screen.Cursor:=c_Default; //Reset cursor when mouse released
              end;
            end;
          end;
        end; //if MOver<>nil then else..

        //These are only for testing purposes, Later on it should be changed a lot
        if (Button = mbRight)
        and(MOver = nil)
        and(fGamePlayInterface <> nil)
        and(fGamePlayInterface.ShownUnit <> nil)
        and(OldDirSelecting) //If this is false then we are not moving, possibly attacking
        and(SelectingDirPosition.x <> 0)
        and(fGamePlayInterface.ShownUnit is TKMUnitWarrior)
        and(TKMUnit(fGamePlayInterface.ShownUnit).GetOwner = MyPlayer.PlayerID)
        and(not fGamePlayInterface.JoiningGroups)
        and(fTerrain.Route_CanBeMade(TKMUnit(fGamePlayInterface.ShownUnit).GetPosition, P, CanWalk, 0, false))
        then
        begin
          Screen.Cursor:=c_Default; //Reset cursor when mouse released
          fGameInputProcess.CmdArmy(gic_ArmyWalk, TKMUnitWarrior(fGamePlayInterface.ShownUnit), P, SelectedDirection);
        end;

        if (Button = mbRight) and (MOver = nil) then
        begin
          fGamePlayInterface.RightClickCancel; //Right clicking closes some menus
          if (Screen.Cursor = c_JoinYes) or (Screen.Cursor = c_JoinNo) then
            Screen.Cursor:=c_Default; //Reset cursor if it was joining
        end;

      end; //gsRunning
    gsEditor: fMapEditorInterface.MouseUp(Button, Shift, X,Y)
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


procedure TKMGame.GameInit();
begin
  RandSeed := 4; //Sets right from the start since it affects TKMAllPlayers.Create and other Types
  fGameSpeed := 1; //In case it was set in last run mission
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
    fActiveCampaign := aCamp;
    fActiveCampaignMap := aCampMap; //MapID is incremented in CampSettings and passed on to here from outside
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
        LoadError := 'An error has occured while parsing the file '+fMissionFile+'||'+
                      E.ClassName+': '+E.Message;
        if fGameState in [gsRunning, gsPaused] then GameStop(gr_Silent); //Stop the game so that the main menu error can be shown
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
  fViewport.SetZoom(1); //This ensures the viewport is centered on the map

  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  fGamePlayInterface.EnableOrDisableMenuIcons(not (fPlayers.fMissionMode = mm_Tactic));

  fLog.AppendLog('Gameplay initialized',true);

  fGameState := gsRunning;

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
    if (fGameInputProcess <> nil) and (fGameInputProcess.State = gipRecording) then
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
                     fCampaignSettings.RevealMap(fActiveCampaign, fActiveCampaignMap+1);
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


function TKMGame.GetNewID():cardinal;
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


procedure TKMGame.StepOneFrame();
begin
  Assert(fGameState=gsReplay, 'We can work step-by-step only in Replay');
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


function TKMGame.LoadName(SlotID:shortint):string;
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
    fTerrain.SyncLoad(); //IsUnit values should be replaced with actual pointers
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

                    if (fGameplayTickCount mod 600 = 0) and fGlobalSettings.Autosave then //Each 1min of gameplay time
                      Save(AUTOSAVE_SLOT); //Autosave slot

                    if fGameState = gsReplay then begin
                      fGameInputProcess.Tick(fGameplayTickCount);
                      if not SkipReplayEndCheck and fGameInputProcess.Ended then
                        GameHold(true, gr_ReplayEnd);
                    end;

                    if fAdvanceFrame then begin
                      fAdvanceFrame := false;
                      SetGameState(gsPaused);
                    end;

                    if fGameState = gsNoGame then exit; //Error due to consistency fail in replay commands
                  end;

                  fGamePlayInterface.UpdateState;

                  if fGlobalTickCount mod 10 = 0 then //Every 1000ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

                  if fGlobalTickCount mod 10 = 0 then
                    if fMusicLib.IsMusicEnded then
                      fMusicLib.PlayNextTrack(); //Feed new music track

                  if fGlobalTickCount mod 10 = 0 then
                    Form1.StatusBar1.Panels[2].Text:='Time: '+int2time(GetMissionTime);
                end;
    gsEditor:   begin
                  fMapEditorInterface.UpdateState;
                  fTerrain.IncAnimStep;
                  fPlayers.IncAnimStep;
                  if fGlobalTickCount mod 10 = 0 then //Every 500ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)
                end;
    end;

end;


{This is our real-time thread, use it wisely}
procedure TKMGame.UpdateStateIdle(aFrameTime:cardinal);
begin
  case fGameState of
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
