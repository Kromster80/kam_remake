unit KM_Game;
interface
uses Windows, MPlayer, Forms, Controls, Classes, SysUtils, KromUtils, Math,
  KM_Users, KM_Render, KM_LoadLib, KM_GamePlayInterface, KM_ReadGFX1, KM_Terrain, KM_LoadDAT,
  KM_LoadSFX, KM_Viewport, KM_Units, KM_Settings;


type
  TKMGame = class
  private
  public
    ScreenX,ScreenY:word;
    GameSpeed:integer;
    GameIsRunning:boolean;
    fMainMenuInterface: TKMMainMenuInterface;
    fGamePlayInterface: TKMGamePlayInterface;
  public
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aMediaPlayer: TMediaPlayer);
    destructor Destroy; override;
    procedure ResizeGameArea(X,Y:integer);
    procedure ZoomInGameArea(X:single);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartGame(MissionFile:string);
    procedure PauseGame(DoPause:boolean);
    procedure StopGame(const StoppedCosOfError:boolean=false);
    procedure UpdateState;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Defaults, KM_Unit1;


{ Creating everything needed for MainMenu, game stuff is created on StartGame } 
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer; aMediaPlayer: TMediaPlayer);
begin
  ScreenX:=aScreenX;
  ScreenY:=aScreenY;
  fLog.AppendLog('<== Render init follows ==>');
  fRender:= TRender.Create(RenderHandle);
  fLog.AppendLog('<== TextLib init follows ==>');
  fTextLibrary:= TTextLibrary.Create(ExeDir+'data\misc\');
  fLog.AppendLog('<== SoundLib init follows ==>');
  fSoundLib:= TSoundLib.Create(aMediaPlayer); //Needed for button click sounds and etc?
  fLog.AppendLog('<== ReadGFX init follows ==>');
  fResource:=TResource.Create;
  fResource.LoadMenuResources;

  fGameSettings         := TGameSettings.Create;
  fMainMenuInterface    := TKMMainMenuInterface.Create(ScreenX,ScreenY);  

  fSoundLib.PlayMenuTrack;

  GameSpeed:=1;
  GameIsRunning:=false;
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
  FreeAndNil(fGameSettings);
  FreeAndNil(fMainMenuInterface);
  FreeAndNil(fSoundLib);
  FreeAndNil(fMissionParser);
  FreeAndNil(fTextLibrary);
  FreeAndNil(fRender);
  inherited;
end;


procedure TKMGame.ResizeGameArea(X,Y:integer);
begin
  ScreenX:=X;
  ScreenY:=Y;
  fRender.RenderResize(X,Y,rm2D);
  if GameIsRunning then begin //If game is running
    fViewport.SetArea(X,Y);
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


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GameIsRunning then begin
    if fGameplayInterface.MyControls.MouseOverControl()<>nil then
      fGameplayInterface.MyControls.OnMouseDown(X,Y,Button)
    else

    //example for units need change
    //Removed right since it interfers with the school buttons
    if Button = mbMiddle then
      fPlayers.Player[1].AddUnit(ut_HorseScout, KMPoint(CursorXc,CursorYc));
  end else begin
    fMainMenuInterface.MyControls.OnMouseDown(X,Y,Button);
  end;

  MouseMove(Shift,X,Y);
end;


procedure TKMGame.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
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
        if (fPlayers.HousesHitTest(CursorXc, CursorYc)<>nil)or
           (fPlayers.UnitsHitTest(CursorXc, CursorYc)<>nil) then
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
var P:TKMPoint;
begin
  P:=KMPoint(CursorXc,CursorYc); //Get cursor position tile-wise

  if GameIsRunning then begin
    if fGameplayInterface.MyControls.MouseOverControl()<>nil then begin

      fGameplayInterface.MyControls.OnMouseUp(X,Y,Button);
      //if Button = mbRight then fGameplayInterface.RightClickCancel; //Right clicking with the build menu open will close it

    end else begin

      if Button = mbRight then
        fGameplayInterface.Build_RightClickCancel; //Right clicking with the build menu open will close it

      if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
        case CursorMode.Mode of
          cm_None:
            begin
              if fPlayers.UnitsHitTest(CursorXc, CursorYc)<>nil then begin
                fPlayers.SelectedUnit:=fPlayers.UnitsHitTest(CursorXc, CursorYc);
                if fGameplayInterface<>nil then fGamePlayInterface.ShowUnitInfo(fPlayers.SelectedUnit);
              end; //Houses have priority over units, so you can't select an occupant
              if fPlayers.HousesHitTest(CursorXc, CursorYc)<>nil then begin
                fPlayers.SelectedHouse:=fPlayers.HousesHitTest(CursorXc, CursorYc);
                if fGameplayInterface<>nil then fGamePlayInterface.ShowHouseInfo(fPlayers.SelectedHouse);
              end;
            end;
          cm_Road: if fTerrain.Land[P.Y,P.X].Markup = mu_RoadPlan then
                     MyPlayer.RemPlan(P)
                   else
                     MyPlayer.AddRoadPlan(P,mu_RoadPlan, false,MyPlayer.PlayerID);

          cm_Field: if fTerrain.Land[P.Y,P.X].Markup = mu_FieldPlan then
                      MyPlayer.RemPlan(P)
                    else
                      MyPlayer.AddRoadPlan(P,mu_FieldPlan, false,MyPlayer.PlayerID);

          cm_Wine: if fTerrain.Land[P.Y,P.X].Markup = mu_WinePlan then
                     MyPlayer.RemPlan(P)
                   else
                     MyPlayer.AddRoadPlan(P,mu_WinePlan, false,MyPlayer.PlayerID);

          cm_Erase:
            begin
              fPlayers.SelectedHouse:=MyPlayer.HousesHitTest(CursorXc, CursorYc);
              if (fGameplayInterface<>nil) and (MyPlayer.RemHouse(P,false,true)) then //Ask wherever player wants to destroy own house
              begin
                if fPlayers.SelectedHouse.GetBuildingState = hbs_Glyph then
                  MyPlayer.RemHouse(P,false) //don't ask about houses that are not started
                else begin
                  fGamePlayInterface.ShowHouseInfo(fPlayers.SelectedHouse,true);
                  fSoundLib.Play(sfx_click,P,false);
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
    if Button = mbRight then
    if fPlayers<>nil then
    if fPlayers.SelectedUnit<>nil then
    if fPlayers.SelectedUnit.GetUnitType=ut_HorseScout then
    TKMUnitWarrior(fPlayers.SelectedUnit).PlaceOrder(wo_walk,P);
//    SetAction(TUnitActionWalkTo.Create(fPlayers.SelectedUnit,P,KMPoint(0,0)));

  end else begin //If GameIsRunning=false
    fMainMenuInterface.MyControls.OnMouseUp(X,Y,Button);
  end;
end;


procedure TKMGame.StartGame(MissionFile:string);
begin
  RandSeed:=4;

  fMainMenuInterface.ShowScreen_Loading;
  fRender.Render;

  if fResource.GetDataState<>dls_All then begin
    fResource.LoadGameResources();
    fRender.LoadTileSet();
  end;

  fViewport:=TViewport.Create;
  fGamePlayInterface:= TKMGamePlayInterface.Create;

  //Here comes terrain/mission init
  fMissionParser:= TMissionParser.Create;
  fTerrain:= TTerrain.Create;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(MissionFile,true) then begin
    if not fMissionParser.LoadDATFile(MissionFile) then begin
      StopGame(true);
      //Show all required error messages here
      exit;
    end;
    fLog.AppendLog('DAT Loaded');
  end else begin
    fTerrain.MakeNewMap(64,64); //For debug we use blank mission
    fPlayers:=TKMAllPlayers.Create(MAX_PLAYERS); //Create 6 players
    MyPlayer:=fPlayers.Player[1];
  end;
  fGamePlayInterface.EnableOrDisableMenuIcons(not (MissionMode = mm_Tactic));

  fLog.AppendLog('Gameplay initialized',true);

  fRender.RenderResize(ScreenX,ScreenY,rm2D);
  fViewport.SetArea(ScreenX,ScreenY);
  fViewport.SetZoom(1);
  //fSoundLib.PlayNextTrack(); //Feed new music track. @Krom: Why? Shouldn't we just keep playing the same tune?
  
  GameIsRunning:=true;
end;


procedure TKMGame.PauseGame(DoPause:boolean);
begin
  GameSpeed:=1-byte(DoPause);
end;

                     
procedure TKMGame.StopGame(const StoppedCosOfError:boolean=false);
begin
  GameIsRunning:=false;
  FreeAndNil(fPlayers);
  FreeAndNil(fTerrain);

  FreeAndNil(fMissionParser);
  FreeAndNil(fGamePlayInterface);
  FreeAndNil(fViewport);

  if StoppedCosOfError then begin
    fLog.AppendLog('Gameplay error',true);
    fMainMenuInterface.ShowScreen_Main;
  end else begin
    fLog.AppendLog('Gameplay free',true);
    fMainMenuInterface.ShowScreen_Results;//Should be mission results screen
  end;
end;


procedure TKMGame.UpdateState;
var i:integer;
begin

  inc(GlobalTickCount);

  if not GameIsRunning then begin
    if GlobalTickCount mod 10 = 0 then //Once a sec
    if fSoundLib.IsMusicEnded then
      fSoundLib.PlayMenuTrack(); //Menu tune
   exit; //If game is not running
  end;

  fViewport.DoScrolling; //Check to see if we need to scroll
  for i:=1 to GameSpeed do begin
    fTerrain.UpdateState;
    fPlayers.UpdateState; //Quite slow
  end;

  fGamePlayInterface.UpdateState;

  if GlobalTickCount mod 5 = 0 then //Every 500ms
    fTerrain.RefreshMinimapData(); //Since this belongs to UI it should refresh at UI refresh rate, not Terrain refresh (which is affected by game speed-up)

  if GlobalTickCount mod 10 = 0 then
    if fSoundLib.IsMusicEnded then
      fSoundLib.PlayNextTrack(); //Feed new music track
end;

end.
