unit KM_Game;
interface
uses Windows, Forms, Controls, Classes, SysUtils, KromUtils, Math,
  KM_Users, KM_Render, KM_LoadLib, KM_GamePlayInterface, KM_ReadGFX1, KM_Terrain, KM_LoadDAT,
  KM_LoadSFX, KM_Viewport, KM_Units, KM_Settings;

type TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and rest

type
  TKMGame = class
  public
    ScreenX,ScreenY:word;
    GameSpeed:integer;
    GameIsRunning:boolean;
    DataState:TDataLoadingState;
  public
    constructor Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer);
    destructor Destroy; override;
    procedure ResizeGameArea(X,Y:integer);
    procedure ZoomInGameArea(X:single);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartGame(MissionFile:string);
    procedure StopGame;
    procedure UpdateState;
  end;

  var
    fGame:TKMGame;

implementation
uses
  KM_Defaults;


{ Creating everything needed for MainMenu, game stuff is created on StartGame } 
constructor TKMGame.Create(ExeDir:string; RenderHandle:HWND; aScreenX,aScreenY:integer);
begin
  DataState:=dls_None;
  ScreenX:=aScreenX;
  ScreenY:=aScreenY;
  fLog.AppendLog('<== Render init follows ==>');
  fRender:= TRender.Create(RenderHandle);
  fLog.AppendLog('Render init',true);
  fLog.AppendLog('<== TextLib init follows ==>');
  fTextLibrary:= TTextLibrary.Create(ExeDir+'data\misc\');
  fLog.AppendLog('TextLib init',true);
  fLog.AppendLog('<== ReadGFX init follows ==>');
  ReadGFX(ExeDir, true); //Should load only GUI part of it
  DataState:=dls_Menu;
  fLog.AppendLog('<== SoundLib init follows ==>');
  fSoundLib:= TSoundLib.Create; //Needed for button click sounds and etc?
  fLog.AppendLog('SoundLib init',true);
  fLog.AppendLog('<== MainMenu init follows ==>');
  fMainMenuInterface:= TKMMainMenuInterface.Create(ScreenX,ScreenY);
  fLog.AppendLog('fMainMenuInterface init',true);
  GameSpeed:=1;
  GameIsRunning:=false;
end;


{ Destroy what was created }
destructor TKMGame.Destroy;
begin
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
  fRender.RenderResize(X,Y);
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
  if GameIsRunning then fViewport.SetZoom:=X;
end;


procedure TKMGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GameIsRunning then begin
    if X<=ToolBarWidth then
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
    if X<=ToolBarWidth then begin
      Screen.Cursor:=c_Default;
      fGameplayInterface.MyControls.OnMouseOver(X,Y,Shift);
    end else begin
      CursorX:=fViewport.GetCenter.X+(X-fViewport.ViewRect.Right/2-ToolBarWidth/2)/CELL_SIZE_PX/fViewport.Zoom;
      CursorY:=fViewport.GetCenter.Y+(Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;

      CursorY:=fTerrain.ConvertCursorToMapCoord(CursorX,CursorY);

      CursorXc:=EnsureRange(round(CursorX+0.5),1,fTerrain.MapX); //Cell below cursor
      CursorYc:=EnsureRange(round(CursorY+0.5),1,fTerrain.MapY);
      CursorXn:=EnsureRange(round(CursorX+1),1,fTerrain.MapX); //Node below cursor
      CursorYn:=EnsureRange(round(CursorY+1),1,fTerrain.MapY);

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

//Form1.StatusBar1.Panels.Items[1].Text:='Cursor: '+floattostr(round(CursorX*10)/10)+' '+floattostr(round(CursorY*10)/10)
//+' | '+inttostr(CursorXc)+' '+inttostr(CursorYc);
end;


procedure TKMGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P:TKMPoint;
begin
  P.X:=CursorXc;
  P.Y:=CursorYc;

  if GameIsRunning then begin
    if X<=ToolBarWidth then begin
      fGameplayInterface.MyControls.OnMouseUp(X,Y,Button);
      //if Button = mbRight then fGameplayInterface.RightClickCancel; //Right clicking with the build menu open will close it
    end else begin
      if Button = mbRight then fGameplayInterface.RightClickCancel; //Right clicking with the build menu open will close it
      if Button = mbLeft then //Only allow placing of roads etc. with the left mouse button
        case CursorMode.Mode of
          cm_None:
            begin
              if fPlayers.UnitsHitTest(CursorXc, CursorYc)<>nil then begin
                if fGameplayInterface<>nil then fGamePlayInterface.ShowUnitInfo(fPlayers.UnitsHitTest(CursorXc, CursorYc));
                fPlayers.SelectedUnit:=fPlayers.UnitsHitTest(CursorXc, CursorYc);
              end; //Houses have priority over units, so you can't select an occupant
              if fPlayers.HousesHitTest(CursorXc, CursorYc)<>nil then begin
                fPlayers.SelectedHouse:=fPlayers.HousesHitTest(CursorXc, CursorYc);
                if fGameplayInterface<>nil then fGamePlayInterface.ShowHouseInfo(fPlayers.HousesHitTest(CursorXc, CursorYc));
              end;
            end;
          cm_Road: MyPlayer.AddRoadPlan(P,mu_RoadPlan, false);
          cm_Field: MyPlayer.AddRoadPlan(P,mu_FieldPlan, false);
          cm_Wine: MyPlayer.AddRoadPlan(P,mu_WinePlan, false);

          cm_Erase:
            begin
              MyPlayer.RemPlan(P);
              MyPlayer.RemHouse(P);
            end;
          cm_Houses:
            begin
              if MyPlayer.AddHousePlan(THouseType(CursorMode.Param),P,false) then
                if fGameplayInterface<>nil then fGamePlayInterface.SelectRoad;
            end;
        end;
    end;

    //These are only for testing purposes, Later on it should be changed a lot
    if fPlayers<>nil then
    if fPlayers.SelectedUnit<>nil then
    if fPlayers.SelectedUnit.GetUnitType=ut_HorseScout then
    fPlayers.SelectedUnit.SetAction(TUnitActionWalkTo.Create(fPlayers.SelectedUnit.GetPosition,P));

  end else begin
    fMainMenuInterface.MyControls.OnMouseUp(X,Y,Button);
  end;
end;


procedure TKMGame.StartGame(MissionFile:string);
begin
  fMainMenuInterface.ShowScreen_Loading;
  fRender.Render;

  if DataState<>dls_All then begin
    ReadGFX(ExeDir, false); //Should load the rest part of data
    DataState:=dls_All;
  end;

  fViewport:=TViewport.Create;
  fGameSettings:= TGameSettings.Create;
  fGamePlayInterface:= TKMGamePlayInterface.Create;

  //Here comes terrain/mission init
  fMissionParser:= TMissionParser.Create;
  fTerrain:= TTerrain.Create;

  fLog.AppendLog('Loading DAT...');
  if CheckFileExists(MissionFile,true) then begin
    fMissionParser.LoadDATFile(MissionFile);
    // fTerrain.LoadMapFromFile / fPlayers should be somewhere there
    //@Lewin: LoadDATFile should return true/false whenever mission is succesfully loaded or not
    //if not - we abort whole thing, show script errors if any and return to main menu 
    fLog.AppendLog('DAT Loaded');            
  end else begin
    fTerrain.MakeNewMap(96,96); //For debug we use blank mission
    fPlayers:=TKMAllPlayers.Create(MAX_PLAYERS); //Create 6 players
    MyPlayer:=fPlayers.Player[1];
  end;

  fLog.AppendLog('Gameplay initialized',true);

  fRender.RenderResize(ScreenX,ScreenY);
  fViewport.SetArea(ScreenX,ScreenY);
  fViewport.SetZoom:=1;
  
  GameIsRunning:=true;
end;


procedure TKMGame.StopGame;
begin
  GameIsRunning:=false;
  FreeAndNil(fPlayers);
  FreeAndNil(fTerrain);

  FreeAndNil(fMissionParser);
  FreeAndNil(fGamePlayInterface);
  FreeAndNil(fGameSettings);
  FreeAndNil(fViewport);
  fLog.AppendLog('Gameplay free',true);

  fMainMenuInterface.ShowScreen_Results;//Should be mission results screen
end;


procedure TKMGame.UpdateState;
var i:integer;
begin
  if not GameIsRunning then exit; //If game is not running
  inc(GlobalTickCount);

  fViewport.DoScrolling; //Check to see if we need to scroll
  for i:=1 to GameSpeed do begin
    fTerrain.UpdateState;
    fPlayers.UpdateState;
  end;
  fGamePlayInterface.UpdateState;

end;

end.
 