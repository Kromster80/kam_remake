unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses
  Classes, Controls, Math, SysUtils, StrUtils,
  {$IFDEF WDC} IOUtils, {$ENDIF}
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Terrain, KM_RenderPool,
  KM_TerrainDeposits, KM_TerrainPainter, KM_TerrainSelection, KM_FileIO;


type
  TMarkerType = (mtNone, mtDefence, mtRevealFOW);

  TKMMapEdMarker = record
    MarkerType: TMarkerType;
    Owner: TKMHandIndex;
    Index: SmallInt;
  end;

  //Collection of map editing classes and map editor specific data
  TKMMapEditor = class
  private
    fTerrainPainter: TKMTerrainPainter;
    fDeposits: TKMDeposits;
    fSelection: TKMSelection;
    fRevealers: array [0..MAX_HANDS-1] of TKMPointTagList;
    fVisibleLayers: TMapEdLayerSet;
    //When you load a map script/libx/wav/etc. files are "attached" then copied when
    //saving if the path is different
    fAttachedFiles: array of record
                               Filename, Ext: UnicodeString;
                             end;

    function GetRevealer(aIndex: Byte): TKMPointTagList;
    procedure ProceedUnitsCursorMode;
    procedure UpdateField(aInc: Integer; aCheckPrevCell: Boolean);
  public
    ActiveMarker: TKMMapEdMarker;

    RevealAll: array [0..MAX_HANDS-1] of Boolean;
    DefaultHuman: TKMHandIndex;
    PlayerHuman: array [0..MAX_HANDS - 1] of Boolean;
    PlayerAI: array [0..MAX_HANDS - 1] of Boolean;
    constructor Create;
    destructor Destroy; override;
    property TerrainPainter: TKMTerrainPainter read fTerrainPainter;
    property Deposits: TKMDeposits read fDeposits;
    property Selection: TKMSelection read fSelection;
    property Revealers[aIndex: Byte]: TKMPointTagList read GetRevealer;
    property VisibleLayers: TMapEdLayerSet read fVisibleLayers write fVisibleLayers;
    procedure DetectAttachedFiles(const aMissionFile: UnicodeString);
    procedure SaveAttachements(const aMissionFile: UnicodeString);
    function HitTest(X,Y: Integer): TKMMapEdMarker;
    function HumanCount: Integer;
    procedure MouseDown(Button: TMouseButton);
    procedure MouseMove;
    procedure MouseUp(Button: TMouseButton; aOverMap: Boolean);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    procedure Update;
    procedure UpdateStateIdle;
    procedure Paint(aLayer: TKMPaintLayer; aClipRect: TKMRect);
  end;


implementation
uses
  KM_HandsCollection, KM_RenderAux, KM_AIDefensePos, KM_Units, KM_UnitGroups, KM_GameCursor,
  KM_ResHouses, KM_ResMapElements, KM_Hand, KM_Houses, KM_HouseBarracks, KM_Game, KM_InterfaceMapEditor;


{ TKMMapEditor }
constructor TKMMapEditor.Create;
var
  I: Integer;
begin
  inherited Create;

  for I := 0 to MAX_HANDS - 1 do
  begin
    PlayerHuman[I] := true;
    PlayerAI[I] := true;
  end;

  fDeposits := TKMDeposits.Create;

  fTerrainPainter := TKMTerrainPainter.Create;
  fSelection := TKMSelection.Create(fTerrainPainter);

  fVisibleLayers := [mlObjects, mlHouses, mlUnits, mlDeposits];

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I] := TKMPointTagList.Create;
end;


destructor TKMMapEditor.Destroy;
var
  I: Integer;
begin
  FreeAndNil(fTerrainPainter);
  FreeAndNil(fDeposits);
  FreeAndNil(fSelection);

  for I := Low(fRevealers) to High(fRevealers) do
    fRevealers[I].Free;

  inherited;
end;


function TKMMapEditor.GetRevealer(aIndex: Byte): TKMPointTagList;
begin
  Result := fRevealers[aIndex];
end;


procedure TKMMapEditor.DetectAttachedFiles(const aMissionFile: UnicodeString);
var
  SearchRec: TSearchRec;
  MissionName, RecExt: UnicodeString;
begin
  SetLength(fAttachedFiles, 0);
  MissionName := ChangeFileExt(ExtractFileName(aMissionFile), '');
  FindFirst(ChangeFileExt(aMissionFile, '.*'), faAnyFile - faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      SetLength(fAttachedFiles, Length(fAttachedFiles)+1);
      //Can't use ExtractFileExt because we want .eng.libx not .libx
      RecExt := RightStr(SearchRec.Name, Length(SearchRec.Name)-Length(MissionName));
      if (LowerCase(RecExt) = '.map') or (LowerCase(RecExt) = '.dat')
      or (LowerCase(RecExt) = '.mi' ) or (LowerCase(RecExt) = '.dat.txt') then
        Continue;

      with fAttachedFiles[Length(fAttachedFiles)-1] do
      begin
        Filename := ExtractFilePath(aMissionFile) + SearchRec.Name;
        Ext := RecExt;
      end;
    end;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;


procedure TKMMapEditor.SaveAttachements(const aMissionFile: UnicodeString);
var
  I: Integer;
  Dest: UnicodeString;
begin
  for I := 0 to Length(fAttachedFiles)-1 do
    if FileExists(fAttachedFiles[I].Filename) then
    begin
      Dest := ChangeFileExt(aMissionFile, fAttachedFiles[I].Ext);
      if not SameFileName(Dest, fAttachedFiles[I].Filename) then
      begin
        if FileExists(Dest) then
          DeleteFile(Dest);
        KMCopyFile(fAttachedFiles[I].Filename, Dest);
      end;
    end;

  //Update attached files to be in the new path
  SetLength(fAttachedFiles, 0);
  DetectAttachedFiles(aMissionFile);
end;


function TKMMapEditor.HitTest(X, Y: Integer): TKMMapEdMarker;
var I,K: Integer;
begin
  if mlDefences in fVisibleLayers then
  begin
    for I := 0 to gHands.Count - 1 do
      for K := 0 to gHands[I].AI.General.DefencePositions.Count - 1 do
        if (gHands[I].AI.General.DefencePositions[K].Position.Loc.X = X)
        and (gHands[I].AI.General.DefencePositions[K].Position.Loc.Y = Y) then
        begin
          Result.MarkerType := mtDefence;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;

  if mlRevealFOW in fVisibleLayers then
  begin
    for I := 0 to gHands.Count - 1 do
      for K := 0 to fRevealers[I].Count - 1 do
        if (fRevealers[I][K].X = X) and (fRevealers[I][K].Y = Y) then
        begin
          Result.MarkerType := mtRevealFOW;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;

  //Else nothing is found
  Result.MarkerType := mtNone;
  Result.Owner := PLAYER_NONE;
  Result.Index := -1;
end;


//How many human players there are in the mission
function TKMMapEditor.HumanCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(PlayerHuman) do
  if PlayerHuman[I] then
    Inc(Result);
end;


procedure TKMMapEditor.UpdateField(aInc: Integer; aCheckPrevCell: Boolean);
var
  P: TKMPoint;
  FieldStage: Integer;
begin
  if aInc = 0 then Exit;

  FieldStage := -1;
  P := gGameCursor.Cell;
  case gGameCursor.Mode of
    cmField:  begin
                if gTerrain.TileIsCornField(P) then
                begin
                  if not KMSamePoint(P, gGameCursor.PrevCell) or not aCheckPrevCell then
                    FieldStage := (gTerrain.GetCornStage(P) + aInc + CORN_STAGES_COUNT) mod CORN_STAGES_COUNT;
                end else if gMySpectator.Hand.CanAddFieldPlan(P, ft_Corn) then
                  FieldStage := 0;
                if FieldStage >= 0 then
                  gMySpectator.Hand.AddField(P, ft_Corn, FieldStage);
              end;
    cmWine:   begin
                if gTerrain.TileIsWineField(P) then
                begin
                  if not KMSamePoint(P, gGameCursor.PrevCell) or not aCheckPrevCell then
                    FieldStage := (gTerrain.GetWineStage(P) + aInc + WINE_STAGES_COUNT) mod WINE_STAGES_COUNT;
                end else if gMySpectator.Hand.CanAddFieldPlan(P, ft_Wine) then
                  FieldStage := 0;
                if FieldStage >= 0 then
                  gMySpectator.Hand.AddField(P, ft_Wine, FieldStage);
              end;
  end;
end;


procedure TKMMapEditor.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
begin
  UpdateField(Sign(WheelDelta), False);
end;


procedure TKMMapEditor.MouseDown(Button: TMouseButton);
var
  P: TKMPoint;
begin
  P := gGameCursor.Cell;
  if (Button = mbLeft) then
    case gGameCursor.Mode of
      cmSelection:  fSelection.Selection_Start;
      cmField,
      cmWine:       UpdateField(1, False);
  end;
end;


procedure TKMMapEditor.MouseMove;
var
  P: TKMPoint;
begin
  if ssLeft in gGameCursor.SState then //Only allow placing of roads etc. with the left mouse button
  begin
    P := gGameCursor.Cell;
    case gGameCursor.Mode of
      cmRoad:       if gMySpectator.Hand.CanAddFieldPlan(P, ft_Road) then
                    begin
                      //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                      if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                        gTerrain.RemField(P);
                      gMySpectator.Hand.AddRoad(P);
                    end;
      cmField,
      cmWine:       UpdateField(1, True);
      cmUnits:      ProceedUnitsCursorMode;
      cmErase:      begin
                      gHands.RemAnyHouse(P);
                      if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                        gTerrain.RemRoad(P);
                      if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                        gTerrain.RemField(P);
                    end;
      cmSelection:  fSelection.Selection_Resize;
    end;
  end;
end;


procedure TKMMapEditor.ProceedUnitsCursorMode;
var P: TKMPoint;
    Obj: TObject;
begin
  P := gGameCursor.Cell;

  if gGameCursor.Tag1 = 255 then
  begin
    Obj := gMySpectator.HitTestCursor(True);
    if Obj is TKMUnit then
      gHands.RemAnyUnit(TKMUnit(Obj).GetPosition);
  end else
  if gTerrain.CanPlaceUnit(P, TUnitType(gGameCursor.Tag1)) then
  begin
    //Check if we can really add a unit
    if TUnitType(gGameCursor.Tag1) in [CITIZEN_MIN..CITIZEN_MAX] then
      gMySpectator.Hand.AddUnit(TUnitType(gGameCursor.Tag1), P, False)
    else
    if TUnitType(gGameCursor.Tag1) in [WARRIOR_MIN..WARRIOR_MAX] then
      gMySpectator.Hand.AddUnitGroup(TUnitType(gGameCursor.Tag1), P, dir_S, 1, 1)
    else
      gHands.PlayerAnimals.AddUnit(TUnitType(gGameCursor.Tag1), P);
  end;
end;


procedure TKMMapEditor.MouseUp(Button: TMouseButton; aOverMap: Boolean);
var
  P: TKMPoint;
begin
  //If the mouse is released over controls, most actions don't happen
  if not aOverMap then
  begin
    //Still need to make a checkpoint since painting has now stopped
    if gGameCursor.Mode in [cmElevate, cmEqualize, cmBrush, cmObjects, cmTiles] then
      fTerrainPainter.MakeCheckpoint;
    Exit;
  end;

  P := gGameCursor.Cell; //Get cursor position tile-wise
  case Button of
    mbLeft:   case gGameCursor.Mode of
                cmRoad:       if gMySpectator.Hand.CanAddFieldPlan(P, ft_Road) then
                              begin
                                //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                                if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                  gTerrain.RemField(P);
                                gMySpectator.Hand.AddRoad(P);
                              end;
                cmHouses:     if gMySpectator.Hand.CanAddHousePlan(P, THouseType(gGameCursor.Tag1)) then
                              begin
                                gMySpectator.Hand.AddHouse(THouseType(gGameCursor.Tag1), P.X, P.Y, true);
                                //Holding shift allows to place that house multiple times
                                if not (ssShift in gGameCursor.SState) then
                                  gGameCursor.Mode := cmRoad;
                              end;
                cmElevate, cmEqualize,
                cmBrush, cmObjects,
                cmTiles:      fTerrainPainter.MakeCheckpoint;
                cmMagicWater: fTerrainPainter.MagicWater(P);
                cmEyedropper: begin
                                fTerrainPainter.Eyedropper(P);
                                if (gGame.ActiveInterface is TKMapEdInterface) then
                                  TKMapEdInterface(gGame.ActiveInterface).GuiTerrain.GuiTiles.TilesTableScrollToTileTexId(gGameCursor.Tag1);
                                if not (ssShift in gGameCursor.SState) then  //Holding shift allows to choose another tile
                                  gGameCursor.Mode := cmTiles;
                              end;
                cmRotateTile: fTerrainPainter.RotateTile(P);
                cmUnits:      ProceedUnitsCursorMode;
                cmMarkers:    case gGameCursor.Tag1 of
                                MARKER_REVEAL:        fRevealers[gMySpectator.HandIndex].Add(P, gGameCursor.MapEdSize);
                                MARKER_DEFENCE:       gMySpectator.Hand.AI.General.DefencePositions.Add(KMPointDir(P, dir_N), gt_Melee, 10, adt_FrontLine);
                                MARKER_CENTERSCREEN:  begin
                                                        gMySpectator.Hand.CenterScreen := P;
                                                        //Updating XY display is done in InterfaceMapEd
                                                      end;
                                MARKER_AISTART:       gMySpectator.Hand.AI.Setup.StartPosition := P;
                                MARKER_RALLY_POINT:   if gMySpectator.Selected is TKMHouseBarracks then
                                                        TKMHouseBarracks(gMySpectator.Selected).RallyPoint := P;
                                MARKER_CUTTING_POINT: if gMySpectator.Selected is TKMHouseWoodcutters then
                                                        TKMHouseWoodcutters(gMySpectator.Selected).CuttingPoint := P;
                              end;
                cmErase:      begin
                                gHands.RemAnyHouse(P);
                                if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                  gTerrain.RemRoad(P);
                                if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                  gTerrain.RemField(P);
                              end;
              end;
    mbRight:  case gGameCursor.Mode of
                cmElevate,
                cmEqualize:   begin
                                //Actual change was made in UpdateStateIdle, we just register it is done here
                                fTerrainPainter.MakeCheckpoint;
                              end;
                cmEyedropper,
                cmRotateTile: gGameCursor.Mode := cmNone;
              end;
  end;
end;


procedure TKMMapEditor.Update;
begin
  if mlDeposits in VisibleLayers then
    fDeposits.UpdateAreas([rdStone, rdCoal, rdIron, rdGold, rdFish]);

  //todo: if mlNavMesh in VisibleLayers then
    //gAIFields.NavMesh.Init;
end;


procedure TKMMapEditor.Paint(aLayer: TKMPaintLayer; aClipRect: TKMRect);
var
  I, K: Integer;
  Loc, P: TKMPoint;
  G: TKMUnitGroup;
  DP: TAIDefencePosition;
begin
  P := gGameCursor.Cell;

  if aLayer = plCursors then
    //With Buildings tab see if we can remove Fields or Houses
    if gGameCursor.Mode = cmErase then
      if gTerrain.TileIsCornField(P)
      or gTerrain.TileIsWineField(P)
      or (gTerrain.Land[P.Y,P.X].TileOverlay=to_Road)
      or (gHands.HousesHitTest(P.X, P.Y) <> nil) then
        gRenderPool.RenderWireTile(P, $FFFFFF00) //Cyan quad
      else
        gRenderPool.RenderSpriteOnTile(P, TC_BLOCK); //Red X


  if mlDefences in fVisibleLayers then
  begin
    case aLayer of
      plCursors:  for I := 0 to gHands.Count - 1 do
                  for K := 0 to gHands[I].AI.General.DefencePositions.Count - 1 do
                  begin
                    DP := gHands[I].AI.General.DefencePositions[K];
                    gRenderPool.RenderSpriteOnTile(DP.Position.Loc, 510 + Byte(DP.Position.Dir), gHands[I].FlagColor);
                  end;
      plTerrain:  if ActiveMarker.MarkerType = mtDefence then
                    //Render the radius only for the selected defence position, otherwise it's too much overlap
                    if InRange(ActiveMarker.Index, 0, gHands[ActiveMarker.Owner].AI.General.DefencePositions.Count - 1) then
                    begin
                      DP := gHands[ActiveMarker.Owner].AI.General.DefencePositions[ActiveMarker.Index];
                      gRenderAux.CircleOnTerrain(DP.Position.Loc.X-0.5, DP.Position.Loc.Y-0.5, DP.Radius,
                                                 gHands[ActiveMarker.Owner].FlagColor AND $40FFFF80,
                                                 gHands[ActiveMarker.Owner].FlagColor);
                    end;
    end;
  end;

  if mlRevealFOW in fVisibleLayers then
  for I := 0 to gHands.Count - 1 do
  for K := 0 to fRevealers[I].Count - 1 do
  begin
    Loc := fRevealers[I][K];
    case aLayer of
      plTerrain:  gRenderAux.CircleOnTerrain(Loc.X-0.5, Loc.Y-0.5,
                                           fRevealers[I].Tag[K],
                                           gHands[I].FlagColor and $20FFFFFF,
                                           gHands[I].FlagColor);
      plCursors:  gRenderPool.RenderSpriteOnTile(Loc,
                      394, gHands[I].FlagColor);
    end;
  end;

  if mlCenterScreen in fVisibleLayers then
  for I := 0 to gHands.Count - 1 do
  if gHands[I].HasAssets then
  begin
    Loc := gHands[I].CenterScreen;
    case aLayer of
      plTerrain:  gRenderAux.SquareOnTerrain(Loc.X - 3, Loc.Y - 2.5,
                                             Loc.X + 2, Loc.Y + 1.5,
                                             gHands[I].FlagColor);
      plCursors:  gRenderPool.RenderSpriteOnTile(Loc, 391, gHands[I].FlagColor);
    end;
  end;

  if mlAIStart in fVisibleLayers then
  for I := 0 to gHands.Count - 1 do
  if gHands[I].HasAssets then
  begin
    Loc := gHands[I].AI.Setup.StartPosition;
    case aLayer of
      plTerrain:  gRenderAux.SquareOnTerrain(Loc.X - 3, Loc.Y - 2.5,
                                             Loc.X + 2, Loc.Y + 1.5,
                                             gHands[I].FlagColor);
      plCursors:  gRenderPool.RenderSpriteOnTile(Loc, 390, gHands[I].FlagColor);
    end;
  end;

  if mlSelection in fVisibleLayers then
    fSelection.Paint(aLayer, aClipRect);

  if mlWaterFlow in fVisibleLayers then
  begin
    for I := aClipRect.Top to aClipRect.Bottom do
    for K := aClipRect.Left to aClipRect.Right do
    if gTerrain.TileIsWater(K,I) then
    begin
      //TODO: Waterflow indication here
      //gRenderPool.RenderSpriteOnTile(KMPoint(K,I), )
    end;
  end;


  //Show selected group order target
  if gMySpectator.Selected is TKMUnitGroup then
  begin
    G := TKMUnitGroup(gMySpectator.Selected);
    if G.MapEdOrder.Order <> ioNoOrder then
    begin
      gRenderAux.Quad(G.MapEdOrder.Pos.Loc.X, G.MapEdOrder.Pos.Loc.Y, $40FF00FF);
      gRenderAux.LineOnTerrain(G.Position.X - 0.5, G.Position.Y - 0.5, G.MapEdOrder.Pos.Loc.X - 0.5, G.MapEdOrder.Pos.Loc.Y - 0.5, $FF0000FF);
    end;
  end;
end;


procedure TKMMapEditor.UpdateStateIdle;
begin
  fTerrainPainter.UpdateStateIdle;
end;


end.
