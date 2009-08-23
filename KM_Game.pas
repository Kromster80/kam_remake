unit KM_Game;
interface
uses Windows, MPlayer, Forms, Controls, Classes, SysUtils, KromUtils, Math,
  KM_Defaults, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_InterfaceMapEditor, KM_InterfaceGamePlay, KM_InterfaceMainMenu,
  KM_ResourceGFX, KM_Terrain, KM_LoadDAT, KM_SoundFX, KM_Viewport, KM_Units, KM_Settings, KM_Utils;

type TGameState = (gsNoGame, gsPaused, gsRunning, gsEditor);

type
  TKMGame = class
  private
    FormControlsVisible:boolean;
    GameplayTickCount:cardinal; //So that first tick will be #1
  public
    ScreenX,ScreenY:word;
    GameSpeed:integer;
    GameState:TGameState;
    fMainMenuInterface: TKMMainMenuInterface;
    fGamePlayInterface: TKMGamePlayInterface;
    fMapEditorInterface: TKMMapEditorInterface;
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
  public
    procedure StartGame(MissionFile:string);
    procedure PauseGame(DoPause:boolean);
    procedure StopGame(const Msg:gr_Message; TextMsg:string=''; ShowResults:boolean=true);
    procedure StartMapEditor(MissionFile:string);
    function GetMissionTime:cardinal;
    property GetTickCount:cardinal read GameplayTickCount;
    procedure UpdateState;
    procedure PaintInterface;
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

  GameSpeed := 1;
  GameState := gsNoGame;
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
  if GameState in [gsPaused, gsRunning, gsEditor] then begin //If game is running
    fViewport.SetVisibleScreenArea(X,Y);
    if GameState in [gsPaused, gsRunning] then fGamePlayInterface.SetScreenSize(X,Y);
    if GameState in [gsEditor] then fMapEditorInterface.SetScreenSize(X,Y); 
    ZoomInGameArea(1);
  end else begin
    //Should resize all Controls somehow...
    //Remember last page and all relevant menu settings
    FreeAndNil(fMainMenuInterface);
    fMainMenuInterface:= TKMMainMenuInterface.Create(X,Y);
    GameSpeed:=1;
    fMainMenuInterface.SetScreenSize(X,Y);
  end;
end;


procedure TKMGame.ZoomInGameArea(X:single);
begin
  if GameState in [gsRunning, gsEditor] then fViewport.SetZoom(X);
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
  if (GameState = gsPaused) and not (Key=ord('P')) then exit; //Ignore all keys if game is on 'Pause'
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
    if (Key = VK_F8) and (GameState = gsRunning) then begin
      GameSpeed:=SPEEDUP_MULTIPLIER+1-GameSpeed; //1 or 11
      if not (GameSpeed in [1,SPEEDUP_MULTIPLIER]) then GameSpeed:=1; //Reset just in case
      fGameplayInterface.ShowClock(GameSpeed = SPEEDUP_MULTIPLIER);
    end;
    if (Key=ord('P')) and (GameState in [gsPaused, gsRunning]) then begin
      if GameState = gsRunning then
        GameState := gsPaused
      else
        GameState := gsRunning;
      fGameplayInterface.ShowPause(GameState = gsPaused);
    end;
    if (Key=ord('W')) and (GameState = gsRunning) then begin
      fTerrain.RevealWholeMap(MyPlayer.PlayerID);
    end;
    {Thats my debug example}
    {if (Key=ord('5')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgText,'123');
    end;
    if (Key=ord('6')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgHouse,'123');
    end;
    if (Key=ord('7')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgUnit,'123');
    end;
    if (Key=ord('8')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgHorn,'123');
    end;
    if (Key=ord('9')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgQuill,'123');
    end;
    if (Key=ord('0')) and (GameState = gsRunning) then begin
      fGameplayInterface.IssueMessage(msgScroll,'123');
    end;}
  end;

  //Also send shortcut to GamePlayInterface if it is there
  if (GameState = gsRunning) and (fGamePlayInterface <> nil) then
    fGamePlayInterface.ShortcutPress(Key,IsDown);

  //Scrolling
  if (GameState = gsRunning) and (Key=VK_LEFT)  then fViewport.ScrollKeyLeft  := IsDown;
  if (GameState = gsRunning) and (Key=VK_RIGHT) then fViewport.ScrollKeyRight := IsDown;
  if (GameState = gsRunning) and (Key=VK_UP)    then fViewport.ScrollKeyUp    := IsDown;
  if (GameState = gsRunning) and (Key=VK_DOWN)  then fViewport.ScrollKeyDown  := IsDown;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case GameState of
    gsNoGame:   fMainMenuInterface.MyControls.OnMouseDown(X,Y,Button);
    gsPaused:   exit; //No clicking when paused
    gsRunning:  begin
                  fGameplayInterface.MyControls.OnMouseDown(X,Y,Button);
                  if (Button = mbMiddle) and (fGameplayInterface.MyControls.MouseOverControl = nil) then
                    MyPlayer.AddUnit(ut_HorseScout, KMPoint(CursorXc,CursorYc)); //Add only when cursor is over the map
                end;
    gsEditor:   fMapEditorInterface.MyControls.OnMouseDown(X,Y,Button);
  end;
  MouseMove(Shift,X,Y);
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
var P:TKMPoint;
begin
  if InRange(X,1,ScreenX-1) and InRange(Y,1,ScreenY-1) then else exit; //Exit if Cursor is outside of frame

  case GameState of
    gsNoGame:   fMainMenuInterface.MyControls.OnMouseOver(X,Y,Shift);
    gsPaused:   exit; //No clicking when paused
    gsRunning:  begin
                  fGameplayInterface.MyControls.OnMouseOver(X,Y,Shift);
                  if fGameplayInterface.MyControls.MouseOverControl()<>nil then
                    Screen.Cursor:=c_Default
                  else begin
                    fTerrain.ComputeCursorPosition(X,Y);
                    if CursorMode.Mode=cm_None then
                      if (MyPlayer.HousesHitTest(CursorXc, CursorYc)<>nil)or
                         (MyPlayer.UnitsHitTest(CursorXc, CursorYc)<>nil) then
                        Screen.Cursor:=c_Info
                      else if not Scrolling then
                        Screen.Cursor:=c_Default;
                    fTerrain.UpdateCursor(CursorMode.Mode,KMPoint(CursorXc,CursorYc));
                  end;
                end;
    gsEditor:   begin
                  fMapEditorInterface.MyControls.OnMouseOver(X,Y,Shift);
                  if fMapEditorInterface.MyControls.MouseOverControl()<>nil then
                    Screen.Cursor:=c_Default
                  else
                  begin
                    fTerrain.ComputeCursorPosition(X,Y);
                    if CursorMode.Mode=cm_None then
                      if (MyPlayer.HousesHitTest(CursorXc, CursorYc)<>nil)or
                         (MyPlayer.UnitsHitTest(CursorXc, CursorYc)<>nil) then
                        Screen.Cursor:=c_Info
                      else if not Scrolling then
                        Screen.Cursor:=c_Default;
                    fTerrain.UpdateCursor(CursorMode.Mode,KMPoint(CursorXc,CursorYc));

                    if ssLeft in Shift then //Only allow placing of roads etc. with the left mouse button
                    begin
                      P := KMPoint(CursorXc,CursorYc); //Get cursor position tile-wise
                      case CursorMode.Mode of
                        cm_Road:  if fTerrain.CanPlaceRoad(P, mu_RoadPlan) then MyPlayer.AddRoad(P,false);
                        cm_Field: if fTerrain.CanPlaceRoad(P, mu_FieldPlan) then MyPlayer.AddField(P,ft_Corn);
                        cm_Wine:  if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                        //cm_Wall: if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                        cm_Erase: begin
                                    MyPlayer.RemHouse(P,false,false,true);
                                    fTerrain.RemRoad(P);
                                    fTerrain.RemField(P); //@Lewin: How do we now which tile terrain had before it became field?
                                  end;                    //@Krom: I added extra variables to terrain to remember the tile and rotation.
                      end;
                    end;
                  end;
                end;
  end;

Form1.StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(CursorX*10)/10)+' '+floattostr(round(CursorY*10)/10)
+' | '+inttostr(CursorXc)+' '+inttostr(CursorYc);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TKMPoint; MOver:TKMControl;
begin
  case GameState of //Remember clicked control
    gsNoGame:   MOver := fMainMenuInterface.MyControls.MouseOverControl();
    gsPaused:   exit; //No clicking allowed when game is paused
    gsRunning:  MOver := fGameplayInterface.MyControls.MouseOverControl();
    gsEditor:   MOver := fMapEditorInterface.MyControls.MouseOverControl();
    else        MOver := nil; //MOver should always be initialized
  end;

  if (MOver <> nil) and (MOver is TKMButton) and MOver.Enabled then fSoundLib.Play(sfx_click);

  case GameState of
    gsNoGame:   fMainMenuInterface.MyControls.OnMouseUp(X,Y,Button);
    gsRunning:
      begin
        P := KMPoint(CursorXc,CursorYc); //Get cursor position tile-wise
        if MOver <> nil then
          fGameplayInterface.MyControls.OnMouseUp(X,Y,Button)
        else begin
          if Button = mbRight then fGameplayInterface.Build_RightClickCancel; //Right clicking with the build menu open will close it

          if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
            case CursorMode.Mode of
              cm_None:
                begin
                  fPlayers.HitTest(CursorXc, CursorYc);
                  if fPlayers.Selected is TKMHouse then
                    fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected));
                  if fPlayers.Selected is TKMUnit then
                    fGamePlayInterface.ShowUnitInfo(TKMUnit(fPlayers.Selected));
                end;
              cm_Road:  if fTerrain.Land[P.Y,P.X].Markup = mu_RoadPlan then
                          MyPlayer.RemPlan(P)
                        else
                          MyPlayer.AddRoadPlan(P, mu_RoadPlan, false, MyPlayer.PlayerID);
              cm_Field: if fTerrain.Land[P.Y,P.X].Markup = mu_FieldPlan then
                          MyPlayer.RemPlan(P)
                        else
                          MyPlayer.AddRoadPlan(P, mu_FieldPlan, false, MyPlayer.PlayerID);
              cm_Wine:  if fTerrain.Land[P.Y,P.X].Markup = mu_WinePlan then
                          MyPlayer.RemPlan(P)
                        else
                          MyPlayer.AddRoadPlan(P, mu_WinePlan, false, MyPlayer.PlayerID);
              cm_Wall:  if fTerrain.Land[P.Y,P.X].Markup = mu_WallPlan then
                          MyPlayer.RemPlan(P)
                        else
                          MyPlayer.AddRoadPlan(P, mu_WallPlan, false, MyPlayer.PlayerID);
              cm_Houses: if MyPlayer.AddHousePlan(THouseType(CursorMode.Param),P,false,MyPlayer.PlayerID) then
                           fGamePlayInterface.Build_SelectRoad;
              cm_Erase:
                begin
                  fPlayers.Selected := MyPlayer.HousesHitTest(CursorXc, CursorYc); //Select the house irregardless of unit below/above
                  if MyPlayer.RemHouse(P,false,true) then //Ask wherever player wants to destroy own house
                  begin
                    if TKMHouse(fPlayers.Selected).GetBuildingState = hbs_Glyph then
                      MyPlayer.RemHouse(P,false) //don't ask about houses that are not started
                    else begin
                      fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected),true);
                      fSoundLib.Play(sfx_click);
                    end;
                  end;
                  if (not MyPlayer.RemPlan(P)) and (not MyPlayer.RemHouse(P,false,true)) then
                    fSoundLib.Play(sfx_CantPlace,P,false,4.0);
                end;

            end; //case CursorMode.Mode of..
        end; //if MOver<>nil then else..

        //These are only for testing purposes, Later on it should be changed a lot
        if (Button = mbRight)
        and(MOver=nil)
        and(fPlayers <> nil)
        and(fPlayers.Selected <> nil)
        and(fPlayers.Selected is TKMUnitWarrior)
        and(TKMUnit(fPlayers.Selected).GetOwner = MyPlayer.PlayerID)
        and(fTerrain.Route_CanBeMade(TKMUnit(fPlayers.Selected).GetPosition,P,canWalk,true))
        then
          TKMUnitWarrior(fPlayers.Selected).PlaceOrder(wo_walk,P);

      end; //gsRunning
    gsEditor: begin
                P := KMPoint(CursorXc,CursorYc); //Get cursor position tile-wise        
                if MOver <> nil then
                  fMapEditorInterface.MyControls.OnMouseUp(X,Y,Button)
                else
                if Button = mbRight then fMapEditorInterface.Build_RightClickCancel
                else
                if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
                  case CursorMode.Mode of
                    cm_None:
                      begin
                        fPlayers.HitTest(CursorXc, CursorYc);
                        if fPlayers.Selected is TKMHouse then
                          fGamePlayInterface.ShowHouseInfo(TKMHouse(fPlayers.Selected));
                        if fPlayers.Selected is TKMUnit then
                          fGamePlayInterface.ShowUnitInfo(TKMUnit(fPlayers.Selected));
                        //if (fPlayers.SelectedUnit is TKMUnitWarrior) and (not TKMUnitWarrior(fPlayers.SelectedUnit).fIsCommander) then
                        //  fPlayers.SelectedUnit:=TKMUnitWarrior(fPlayers.SelectedUnit).fCommanderID;
                      end;
                    cm_Road:  if fTerrain.CanPlaceRoad(P, mu_RoadPlan) then MyPlayer.AddRoad(P,false);
                    cm_Field: if fTerrain.CanPlaceRoad(P, mu_FieldPlan) then MyPlayer.AddField(P,ft_Corn);
                    cm_Wine:  if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                    //cm_Wall: if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
                    cm_Houses:
                    if fTerrain.CanPlaceHouse(P, THouseType(CursorMode.Param)) then
                                MyPlayer.AddHouse(THouseType(CursorMode.Param),P);
              cm_Erase:
                begin
                  MyPlayer.RemHouse(P,false); //don't ask about houses that are not started
                  fTerrain.RemRoad(P);
                  fTerrain.RemField(P);
                end;


                  end;
              end;

  end;

end;


procedure TKMGame.StartGame(MissionFile:string);
var ResultMsg:string;
begin
  RandSeed:=4; //Sets right from the start since it afects TKMAllPlayers.Create and other Types
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

  GameState:=gsRunning;
end;


procedure TKMGame.PauseGame(DoPause:boolean);
begin
  GameSpeed:=1-byte(DoPause);
end;

                     
procedure TKMGame.StopGame(const Msg:gr_Message; TextMsg:string=''; ShowResults:boolean=true);
begin
  GameState := gsNoGame;

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


procedure TKMGame.StartMapEditor(MissionFile:string);
var ResultMsg:string;
begin
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

  fViewport:=TViewport.Create;
  fMapEditorInterface:= TKMMapEditorInterface.Create;

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
    fPlayers:=TKMAllPlayers.Create(MAX_PLAYERS); //Create MAX players
    MyPlayer:=fPlayers.Player[1];
  end;
  fTerrain.RevealWholeMap(play_1); //@Lewin: Should be all players?
  Form1.StatusBar1.Panels[0].Text:='Map size: '+inttostr(fTerrain.MapX)+' x '+inttostr(fTerrain.MapY);

  fLog.AppendLog('Gameplay initialized',true);

  fRender.RenderResize(ScreenX,ScreenY,rm2D);
  fViewport.SetVisibleScreenArea(ScreenX,ScreenY);
  fViewport.SetZoom(1);

  GameplayTickCount:=0; //Restart counter

  GameState := gsEditor;
end;


function TKMGame.GetMissionTime:cardinal;
begin
  Result := MyPlayer.fMissionSettings.GetMissionTime + (GameplayTickCount div (1000 div GAME_LOGIC_PACE));
end;


procedure TKMGame.UpdateState;
var i:integer;
begin
  inc(GlobalTickCount);
  case GameState of
    gsPaused:   exit;
    gsNoGame:   begin
                  fMainMenuInterface.UpdateState;
                  if GlobalTickCount mod 10 = 0 then //Once a sec
                  if fMusicLib.IsMusicEnded then
                    fMusicLib.PlayMenuTrack(); //Menu tune
                end;
    gsRunning:  begin
                  fViewport.DoScrolling; //Check to see if we need to scroll
                  for i:=1 to GameSpeed do begin
                    inc(GameplayTickCount); //Thats our tick counter for gameplay events
                    fTerrain.UpdateState;
                    fPlayers.UpdateState(GameplayTickCount); //Quite slow
                    if GameState = gsNoGame then exit; //Quit the update if game was stopped by MyPlayer defeat
                  end;

                  fGamePlayInterface.UpdateState;

                  if GlobalTickCount mod 5 = 0 then //Every 500ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

                  if GlobalTickCount mod 10 = 0 then
                    if fMusicLib.IsMusicEnded then
                      fMusicLib.PlayNextTrack(); //Feed new music track
                end;
    gsEditor:   begin
                  fViewport.DoScrolling; //Check to see if we need to scroll
                  fMapEditorInterface.UpdateState;
                  fTerrain.IncAnimStep;
                  if GlobalTickCount mod 10 = 0 then //Every 500ms
                    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)
                end;
    end;
end;


procedure TKMGame.PaintInterface;
begin
  case GameState of
    gsNoGame:  fMainMenuInterface.Paint;
    gsPaused:  fGameplayInterface.Paint;
    gsRunning: fGameplayInterface.Paint;
    gsEditor:  fMapEditorInterface.Paint;
  end;
end;


end.
