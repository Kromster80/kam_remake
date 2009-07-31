unit KM_Game;
interface
uses Windows, MPlayer, Forms, Controls, Classes, SysUtils, KromUtils, Math,
  KM_Defaults, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_LoadDAT, KM_SoundFX, KM_Viewport, KM_Units, KM_Settings, KM_Utils;


type
  TKMGame = class
  private
    FormControlsVisible:boolean;
    GameplayTickCount:cardinal; //So that first tick will be #1
  public
    ScreenX,ScreenY:word;
    GameSpeed:integer;
    GameIsRunning:boolean;
    GameIsPaused:boolean;
    fMainMenuInterface: TKMMainMenuInterface;
    fGamePlayInterface: TKMGamePlayInterface;
  public
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; NoMusic:boolean=false);
    destructor Destroy; override;
    procedure ToggleLocale();
    procedure ResizeGameArea(X,Y:integer);
    procedure ZoomInGameArea(X:single);
    procedure ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
    procedure KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartGame(MissionFile:string);
    procedure PauseGame(DoPause:boolean);
    procedure StopGame(const Msg:gr_Message; TextMsg:string=''; ShowResults:boolean=true);
    function GetMissionTime:cardinal;
    property GetTickCount:cardinal read GameplayTickCount;
    procedure UpdateState;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Unit1, KM_Controls, KM_Houses, KM_CommonTypes;


{ Creating everything needed for MainMenu, game stuff is created on StartGame }
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; NoMusic:boolean=false);
begin
  ScreenX:=aScreenX;
  ScreenY:=aScreenY;
  fGameSettings         := TGameSettings.Create;
  fLog.AppendLog('<== Render init follows ==>');
  fRender:= TRender.Create(RenderHandle);
  fLog.AppendLog('<== TextLib init follows ==>');
  fTextLibrary:= TTextLibrary.Create(ExeDir+'data\misc\');
  fLog.AppendLog('<== SoundLib init follows ==>');
  fSoundLib:= TSoundLib.Create(); //Needed for button click sounds and etc?
  fMusicLib:= TMusicLib.Create(); //Needed for button click sounds and etc?
  fGameSettings.UpdateSFXVolume;
  fLog.AppendLog('<== ReadGFX init follows ==>');
  fResource:=TResource.Create;
  fResource.LoadMenuResources;
  fLog.AppendLog('<== Main menu interface follows ==>');
  fMainMenuInterface    := TKMMainMenuInterface.Create(ScreenX,ScreenY);
  fLog.AppendLog('<== Sound playback follows ==>');

  if not NoMusic then fMusicLib.PlayMenuTrack;

  GameSpeed:=1;
  GameIsRunning:=false;
  GameIsPaused:=false;
  FormControlsVisible:=true;
  fLog.AppendLog('<== Game creation is done ==>');
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  //Stop music imediently, so it doesn't keep playing and jerk while things closes
  fMusicLib.StopMusic;

  FreeAndNil(fGameSettings);
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fResource);
  FreeAndNil(fSoundLib);
  FreeAndNil(fMusicLib);
  FreeAndNil(fMissionParser);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fRender);
  inherited;
end;


procedure TKMGame.ToggleLocale();
begin
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fTextLibrary);
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\');
  fResource.LoadFonts(false);
  fMainMenuInterface := TKMMainMenuInterface.Create(ScreenX,ScreenY);
  fMainMenuInterface.ShowScreen_Options;
end;


procedure TKMGame.ResizeGameArea(X,Y:integer);
begin
  ScreenX:=X;
  ScreenY:=Y;
  fRender.RenderResize(X,Y,rm2D);
  if GameIsRunning then begin //If game is running
    fViewport.SetVisibleScreenArea(X,Y);
    ZoomInGameArea(1);
  end else begin
    //Should resize all Controls somehow...
    //Remember last page and all relevant menu settings
    FreeAndNil(fMainMenuInterface);
    fMainMenuInterface:= TKMMainMenuInterface.Create(X,Y);
    GameSpeed:=1;
    GameIsRunning:=false;
    fMainMenuInterface.SetScreenSize(X,Y);
  end;
end;


procedure TKMGame.ZoomInGameArea(X:single);
begin
  if GameIsRunning then fViewport.SetZoom(X);
end;


procedure TKMGame.ToggleFullScreen(aToggle:boolean; ReturnToOptions:boolean);
begin
  Form1.ToggleFullScreen(aToggle, ReturnToOptions);
end;


procedure TKMGame.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false);
begin
  //List of conflicting keys:
  //F12 Pauses Execution and switches to debug
  //F10 sets focus on MainMenu1
  //F9 is the default key in Fraps for video capture
  //others.. unknown
  if GameIsPaused and not (Key=ord('P')) then exit; //Ignore all keys if game is on 'Pause'
  if not IsDown then
  begin
    if Key=VK_F11 then begin
      Form1.ToggleControlsVisibility(FormControlsVisible);
      FormControlsVisible := not FormControlsVisible;
    end;
    if Key=VK_BACK then begin
      //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
      //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
      fViewport.SetZoom(1);
      Form1.TB_Angle.Position := 0;
      Form1.TB_Angle_Change(Form1.TB_Angle);
    end;
    if (Key=VK_F8) and GameIsRunning then begin
      GameSpeed:=11-GameSpeed; //1 or 11
      if not (GameSpeed in [1,10]) then GameSpeed:=1; //Reset just in case
      fGameplayInterface.ShowClock((GameSpeed=10)or GameIsPaused);
    end;
    if (Key=ord('P')) and GameIsRunning then begin
      GameIsPaused := not GameIsPaused;
      fGameplayInterface.ShowPause(GameIsPaused,GameSpeed=10);
    end;
    {Thats my debug example}
    {if (Key=ord('5')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgText,'123');
    end;
    if (Key=ord('6')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgHouse,'123');
    end;
    if (Key=ord('7')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgUnit,'123');
    end;
    if (Key=ord('8')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgHorn,'123');
    end;
    if (Key=ord('9')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgQuill,'123');
    end;
    if (Key=ord('0')) and GameIsRunning then begin
      fGameplayInterface.IssueMessage(msgScroll,'123');
    end;}
  end;

  //Also send shortcut to GamePlayInterface if it is there
  if (GameIsRunning) and (fGamePlayInterface <> nil) then
    fGamePlayInterface.ShortcutPress(Key,IsDown);

  //Scrolling
  if (GameIsRunning) and (Key=VK_LEFT)  then fViewport.ScrollKeyLeft  := IsDown;
  if (GameIsRunning) and (Key=VK_RIGHT) then fViewport.ScrollKeyRight := IsDown;
  if (GameIsRunning) and (Key=VK_UP)    then fViewport.ScrollKeyUp    := IsDown;
  if (GameIsRunning) and (Key=VK_DOWN)  then fViewport.ScrollKeyDown  := IsDown;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MOver:TKMControl;
begin

  if GameIsPaused then exit; //No clicking when paused

  if GameIsRunning then begin

    MOver := fGameplayInterface.MyControls.MouseOverControl(); //Remember control that was clicked
    if MOver<>nil then
      fGameplayInterface.MyControls.OnMouseDown(X,Y,Button)
    else



    if Button = mbMiddle then
      MyPlayer.AddUnit(ut_HorseScout, KMPoint(CursorXc,CursorYc));

  end else
    fMainMenuInterface.MyControls.OnMouseDown(X,Y,Button);


  MouseMove(Shift,X,Y);
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if GameIsPaused then exit; //No clicking when paused
  if InRange(X,1,ScreenX-1) and InRange(Y,1,ScreenY-1) then else exit; //Exit if Cursor is outside of frame

  if GameIsRunning then begin
    fGameplayInterface.MyControls.OnMouseOver(X,Y,Shift);
    if fGameplayInterface.MyControls.MouseOverControl()<>nil then
      Screen.Cursor:=c_Default
    else begin
      CursorX:=fViewport.GetCenter.X+(X-fViewport.ViewRect.Right/2-ToolBarWidth/2)/CELL_SIZE_PX/fViewport.Zoom;
      CursorY:=fViewport.GetCenter.Y+(Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;

      CursorY:=fTerrain.ConvertCursorToMapCoord(CursorX,CursorY);

      CursorXc:=EnsureRange(round(CursorX+0.5),1,fTerrain.MapX); //Cell below cursor
      CursorYc:=EnsureRange(round(CursorY+0.5),1,fTerrain.MapY);

      if CursorMode.Mode=cm_None then
        if (MyPlayer.HousesHitTest(CursorXc, CursorYc)<>nil)or
           (MyPlayer.UnitsHitTest(CursorXc, CursorYc)<>nil) then
          Screen.Cursor:=c_Info
        else if not Scrolling then
          Screen.Cursor:=c_Default;

      fTerrain.UpdateCursor(CursorMode.Mode,KMPoint(CursorXc,CursorYc));
    end;
  end else begin
    fMainMenuInterface.MyControls.OnMouseOver(X,Y,Shift);
  end;

Form1.StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(CursorX*10)/10)+' '+floattostr(round(CursorY*10)/10)
+' | '+inttostr(CursorXc)+' '+inttostr(CursorYc);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TKMPoint; FoundUnit:boolean; SelectedHouse: TKMHouse; SelectedUnit: TKMUnit; MOver:TKMControl;
begin
  if GameIsPaused then exit; //No clicking when paused
  P:=KMPoint(CursorXc,CursorYc); //Get cursor position tile-wise

  //Find what control was clicked and make sound 
  if GameIsRunning then
    MOver := fGameplayInterface.MyControls.MouseOverControl() //Remember control that was clicked
  else
    MOver := fMainMenuInterface.MyControls.MouseOverControl(); //Remember control that was clicked

  if MOver <> nil then
    if (MOver is TKMButton) and MOver.Enabled then
      fSoundLib.Play(sfx_click);

  if GameIsRunning then begin

    if MOver <> nil then
      fGameplayInterface.MyControls.OnMouseUp(X,Y,Button)
    else begin

      if Button = mbRight then
        fGameplayInterface.Build_RightClickCancel; //Right clicking with the build menu open will close it

      FoundUnit := false;
      if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
        case CursorMode.Mode of
          cm_None:
            begin
              SelectedUnit:=MyPlayer.UnitsHitTest(CursorXc, CursorYc);
              if SelectedUnit<>nil then begin
                fPlayers.SelectedUnit:=SelectedUnit;
                //if (fPlayers.SelectedUnit is TKMUnitWarrior) and (not TKMUnitWarrior(fPlayers.SelectedUnit).fIsCommander) then
                //  fPlayers.SelectedUnit:=TKMUnitWarrior(fPlayers.SelectedUnit).fCommanderID;
                if fGameplayInterface<>nil then fGamePlayInterface.ShowUnitInfo(fPlayers.SelectedUnit);
                FoundUnit := true;
              end; //Houses have priority over units, so you can't select an occupant. However, this is only true if the house is built
              SelectedHouse:=MyPlayer.HousesHitTest(CursorXc, CursorYc);
              if SelectedHouse<>nil then
                if (not FoundUnit)or((SelectedHouse.GetBuildingState in [hbs_Stone,hbs_Done])and FoundUnit) then begin
                  fPlayers.SelectedUnit:=nil;
                  fPlayers.SelectedHouse:=SelectedHouse;
                  if fGameplayInterface<>nil then fGamePlayInterface.ShowHouseInfo(fPlayers.SelectedHouse);
              end;
            end;
          cm_Road: if fTerrain.Land[P.Y,P.X].Markup = mu_RoadPlan then
                     MyPlayer.RemPlan(P)
                   else
                     MyPlayer.AddRoadPlan(P, mu_RoadPlan, false, MyPlayer.PlayerID);

          cm_Field: if fTerrain.Land[P.Y,P.X].Markup = mu_FieldPlan then
                      MyPlayer.RemPlan(P)
                    else
                      MyPlayer.AddRoadPlan(P, mu_FieldPlan, false, MyPlayer.PlayerID);

          cm_Wine: if fTerrain.Land[P.Y,P.X].Markup = mu_WinePlan then
                     MyPlayer.RemPlan(P)
                   else
                     MyPlayer.AddRoadPlan(P, mu_WinePlan, false, MyPlayer.PlayerID);

          cm_Erase:
            begin
              fPlayers.SelectedHouse:=MyPlayer.HousesHitTest(CursorXc, CursorYc);
              if (fGameplayInterface<>nil) and (MyPlayer.RemHouse(P,false,true)) then //Ask wherever player wants to destroy own house
              begin
                if fPlayers.SelectedHouse.GetBuildingState = hbs_Glyph then
                  MyPlayer.RemHouse(P,false) //don't ask about houses that are not started
                else begin
                  fGamePlayInterface.ShowHouseInfo(fPlayers.SelectedHouse,true);
                  fSoundLib.Play(sfx_click);
                end;
              end;
              if (not MyPlayer.RemPlan(P)) and (not MyPlayer.RemHouse(P,false,true)) then
                fSoundLib.Play(sfx_CantPlace,P,false,4.0);
            end;
            
          cm_Houses:
            begin
              if MyPlayer.AddHousePlan(THouseType(CursorMode.Param),P,false,MyPlayer.PlayerID) then
                if fGameplayInterface<>nil then fGamePlayInterface.Build_SelectRoad;
            end;
        end;
    end;

    //These are only for testing purposes, Later on it should be changed a lot
    if (Button = mbRight)
    and(MOver=nil) then
      if (fPlayers <> nil)
      and(fPlayers.SelectedUnit <> nil)
      and(fPlayers.SelectedUnit.IsArmyUnit)
      and(fPlayers.SelectedUnit.GetOwner = MyPlayer.PlayerID)
      and(fPlayers.SelectedUnit is TKMUnitWarrior)
      and(fTerrain.Route_CanBeMade(fPlayers.SelectedUnit.GetPosition,P,canWalk,true))
      then
        TKMUnitWarrior(fPlayers.SelectedUnit).PlaceOrder(wo_walk,P);

  end else begin //If GameIsRunning=false
    fMainMenuInterface.MyControls.OnMouseUp(X,Y,Button);
  end;
end;


procedure TKMGame.StartGame(MissionFile:string);
var ResultMsg:string;
begin
  RandSeed:=4; //Sets right from the start since it afects TKMAllPlayers.Create and other Types
  GameIsPaused := false;
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

  fViewport:=TViewport.Create;
  fGamePlayInterface:= TKMGamePlayInterface.Create;

  //Here comes terrain/mission init
  fMissionParser:= TMissionParser.Create;
  fTerrain:= TTerrain.Create;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(MissionFile,true) then begin
    ResultMsg := fMissionParser.LoadDATFile(MissionFile);
    if ResultMsg<>'' then begin
      StopGame(gr_Error,ResultMsg);
      //Show all required error messages here
      exit;
    end;
    fLog.AppendLog('DAT Loaded');
  end else begin
    fTerrain.MakeNewMap(64,64); //For debug we use blank mission
    fPlayers:=TKMAllPlayers.Create(MAX_PLAYERS); //Create 6 players
    MyPlayer:=fPlayers.Player[1];
  end;
  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);
  fGamePlayInterface.EnableOrDisableMenuIcons(not (MissionMode = mm_Tactic));

  fLog.AppendLog('Gameplay initialized',true);

  fRender.RenderResize(ScreenX,ScreenY,rm2D);
  fViewport.SetVisibleScreenArea(ScreenX,ScreenY);
  fViewport.SetZoom(1);
  //fSoundLib.PlayNextTrack();  //Discussed. No need to feed new music track.

  GameplayTickCount:=0; //Restart counter

  GameIsRunning:=true;
end;


procedure TKMGame.PauseGame(DoPause:boolean);
begin
  GameSpeed:=1-byte(DoPause);
end;

                     
procedure TKMGame.StopGame(const Msg:gr_Message; TextMsg:string=''; ShowResults:boolean=true);
begin
  GameIsRunning:=false;

  if Msg in [gr_Win, gr_Defeat, gr_Cancel] then
  fMainMenuInterface.Fill_Results;

  FreeAndNil(fPlayers);
  FreeAndNil(fTerrain);

  FreeAndNil(fMissionParser);
  FreeAndNil(fGamePlayInterface);
  FreeAndNil(fViewport);

  if (Msg = gr_Win) or (Msg = gr_Defeat) then begin
    fLog.AppendLog('Gameplay ended',true);
    fMainMenuInterface.ShowScreen_Results(Msg); //Mission results screen
  end else
  if Msg = gr_Cancel then begin
    fLog.AppendLog('Gameplay canceled',true);
    fMainMenuInterface.ShowScreen_Results(Msg);
  end else
  if Msg = gr_Error then begin
    fLog.AppendLog('Gameplay error',true);
    fMainMenuInterface.ShowScreen_Error(TextMsg);
  end;
end;


function TKMGame.GetMissionTime:cardinal;
begin
  Result := MyPlayer.fMissionSettings.GetMissionTime + (GameplayTickCount div (1000 div GAME_LOGIC_PACE));
end;


procedure TKMGame.UpdateState;
var i:integer;
begin

  inc(GlobalTickCount);

  if not GameIsRunning then begin
    fMainMenuInterface.UpdateState;
    if GlobalTickCount mod 10 = 0 then //Once a sec
    if fMusicLib.IsMusicEnded then
      fMusicLib.PlayMenuTrack(); //Menu tune
    exit; //If game is not running
  end;

  if fGame.GameIsPaused then exit;

  fViewport.DoScrolling; //Check to see if we need to scroll
  for i:=1 to GameSpeed do begin
    inc(GameplayTickCount); //Thats our tick counter for gameplay events
    fTerrain.UpdateState;
    fPlayers.UpdateState(GameplayTickCount); //Quite slow
    if not GameIsRunning then exit; //Quit the update if game was stopped by MyPlayer defeat
  end;

  if fGamePlayInterface<>nil then fGamePlayInterface.UpdateState;

  if GlobalTickCount mod 5 = 0 then //Every 500ms
    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

  if GlobalTickCount mod 10 = 0 then
    if fMusicLib.IsMusicEnded then
      fMusicLib.PlayNextTrack(); //Feed new music track
end;



end.
