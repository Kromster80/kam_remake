unit KM_MapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, Controls, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_Terrain, KM_Units, KM_RenderPool,
  KM_TerrainDeposits, KM_TerrainPainter, KM_TerrainSelection;


type
  TSelectionManipulation = (smNone, smNewRect, smResizeX1, smResizeY1, smResizeX2, smResizeY2, smMove);
  TMarkerType = (mtNone, mtDefence, mtRevealFOW);

  TKMMapEdMarker = record
    MarkerType: TMarkerType;
    Owner: TPlayerIndex;
    Index: SmallInt;
  end;

  //Designed to store MapEd specific data and methods
  TKMMapEditor = class
  private
    fTerrainPainter: TKMTerrainPainter;
    fDeposits: TKMDeposits;
    fSelection: TKMSelection;
    fRevealers: array [0..MAX_PLAYERS-1] of TKMPointTagList;
    fVisibleLayers: TMapEdLayerSet;

    fSelectionMan: TSelectionManipulation;
    fPrevX, fPrevY: Integer;

    function GetRevealer(aIndex: Byte): TKMPointTagList;
  public
    ActiveMarker: TKMMapEdMarker;

    RevealAll: array [0..MAX_PLAYERS-1] of Boolean;
    DefaultHuman: TPlayerIndex;
    PlayerHuman: array [0..MAX_PLAYERS - 1] of Boolean;
    PlayerAI: array [0..MAX_PLAYERS - 1] of Boolean;
    constructor Create;
    destructor Destroy; override;
    property TerrainPainter: TKMTerrainPainter read fTerrainPainter;
    property Deposits: TKMDeposits read fDeposits;
    property Selection: TKMSelection read fSelection;
    property Revealers[aIndex: Byte]: TKMPointTagList read GetRevealer;
    property VisibleLayers: TMapEdLayerSet read fVisibleLayers write fVisibleLayers;
    function HitTest(X,Y: Integer): TKMMapEdMarker;
    procedure Selection_Resize;
    procedure Selection_Start;
    procedure MouseDown(Button: TMouseButton);
    procedure MouseMove;
    procedure MouseUp(Button: TMouseButton);
    procedure Update;
    procedure Paint(aLayer: TPaintLayer);
  end;


implementation
uses KM_PlayersCollection, KM_RenderAux, KM_AIDefensePos, KM_UnitGroups, KM_GameCursor, KM_ResHouses;


{ TKMMapEditor }
constructor TKMMapEditor.Create;
var
  I: Integer;
begin
  inherited Create;

  fTerrainPainter := TKMTerrainPainter.Create;
  fDeposits := TKMDeposits.Create;
  fSelection := TKMSelection.Create;

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


function TKMMapEditor.HitTest(X, Y: Integer): TKMMapEdMarker;
var I,K: Integer;
begin
  if mlDefences in fVisibleLayers then
  begin
    for I := 0 to gPlayers.Count - 1 do
      for K := 0 to gPlayers[I].AI.General.DefencePositions.Count - 1 do
        if (gPlayers[I].AI.General.DefencePositions[K].Position.Loc.X = X)
        and (gPlayers[I].AI.General.DefencePositions[K].Position.Loc.Y = Y) then
        begin
          Result.MarkerType := mtDefence;
          Result.Owner := I;
          Result.Index := K;
          Exit;
        end;
  end;

  if mlRevealFOW in fVisibleLayers then
  begin
    for I := 0 to gPlayers.Count - 1 do
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


procedure TKMMapEditor.Selection_Resize;
var
  Rect: TKMRect;
  RectF: TKMRectF;
begin
  RectF := Selection.RawRect;

  case fSelectionMan of
    smNone:       ;
    smNewRect:    begin
                    RectF.Right := GameCursor.Float.X;
                    RectF.Bottom := GameCursor.Float.Y;
                  end;
    smResizeX1:   RectF.Left := GameCursor.Float.X;
    smResizeY1:   RectF.Top := GameCursor.Float.Y;
    smResizeX2:   RectF.Right := GameCursor.Float.X;
    smResizeY2:   RectF.Bottom := GameCursor.Float.Y;
    smMove:       begin
                    Rect := Selection.Rect;
                    Rect := KMRectMove(Rect, GameCursor.Cell.X - fPrevX, GameCursor.Cell.Y - fPrevY);
                    RectF := KMRectF(Rect);

                    fPrevX := GameCursor.Cell.X;
                    fPrevY := GameCursor.Cell.Y;
                  end;
  end;

  Selection.RawRect := RectF;
end;


procedure TKMMapEditor.Selection_Start;
const
  EDGE = 0.25;
var
  Rect: TKMRect;
  RawRect: TKMRectF;
begin
  Rect := Selection.Rect;

  if Selection.SelectionMode = smSelecting then
  begin
    if InRange(GameCursor.Float.Y, Rect.Top, Rect.Bottom)
    and (Abs(GameCursor.Float.X - Rect.Left) < EDGE) then
      fSelectionMan := smResizeX1
    else
    if InRange(GameCursor.Float.Y, Rect.Top, Rect.Bottom)
    and (Abs(GameCursor.Float.X - Rect.Right) < EDGE) then
      fSelectionMan := smResizeX2
    else
    if InRange(GameCursor.Float.X, Rect.Left, Rect.Right)
    and (Abs(GameCursor.Float.Y - Rect.Top) < EDGE) then
      fSelectionMan := smResizeY1
    else
    if InRange(GameCursor.Float.X, Rect.Left, Rect.Right)
    and (Abs(GameCursor.Float.Y - Rect.Bottom) < EDGE) then
      fSelectionMan := smResizeY2
    else
    if KMInRect(GameCursor.Float, Rect) then
    begin
      fSelectionMan := smMove;
      fPrevX := GameCursor.Cell.X;
      fPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionMan := smNewRect;
      RawRect := Selection.RawRect;
      RawRect.Left := GameCursor.Float.X;
      RawRect.Top := GameCursor.Float.Y;
      RawRect.Right := GameCursor.Float.X;
      RawRect.Bottom := GameCursor.Float.Y;
      Selection.RawRect := RawRect;
    end;
  end
  else
  begin
    if KMInRect(GameCursor.Float, Rect) then
    begin
      fSelectionMan := smMove;
      //Grab and move
      fPrevX := GameCursor.Cell.X;
      fPrevY := GameCursor.Cell.Y;
    end
    else
    begin
      fSelectionMan := smMove;
      //Selection edge will jump to under cursor
      fPrevX := EnsureRange(GameCursor.Cell.X, Rect.Left + 1, Rect.Right);
      fPrevY := EnsureRange(GameCursor.Cell.Y, Rect.Top + 1, Rect.Bottom);
    end;
  end;
end;


procedure TKMMapEditor.MouseDown(Button: TMouseButton);
begin
  if (Button = mbLeft) and (GameCursor.Mode = cmSelection) then
    Selection_Start;
end;


procedure TKMMapEditor.MouseMove;
var
  P: TKMPoint;
begin
  if ssLeft in GameCursor.SState then //Only allow placing of roads etc. with the left mouse button
  begin
    P := GameCursor.Cell;
    case GameCursor.Mode of
      cmRoad:       if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Road) then
                    begin
                      //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                      if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                        gTerrain.RemField(P);
                      gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Road);
                    end;
      cmField:      if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Corn) then
                      gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Corn);
      cmWine:       if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Wine) then
                      gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Wine);
      cmUnits:      if GameCursor.Tag1 = 255 then gPlayers.RemAnyUnit(P);
      cmErase:      begin
                      gPlayers.RemAnyHouse(P);
                      if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                        gTerrain.RemRoad(P);
                      if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                        gTerrain.RemField(P);
                    end;
      cmSelection:  Selection_Resize;
    end;
  end;
end;


procedure TKMMapEditor.MouseUp(Button: TMouseButton);
var
  P: TKMPoint;
begin
  P := GameCursor.Cell; //Get cursor position tile-wise
  case Button of
    mbLeft:   case GameCursor.Mode of
                cmRoad:       if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Road) then
                              begin
                                //If there's a field remove it first so we don't get road on top of the field tile (undesired in MapEd)
                                if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                  gTerrain.RemField(P);
                                gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Road);
                              end;
                cmField:      if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Corn) then
                                gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Corn);
                cmWine:       if gPlayers[MySpectator.PlayerIndex].CanAddFieldPlan(P, ft_Wine) then
                                gPlayers[MySpectator.PlayerIndex].AddField(P, ft_Wine);
                cmHouses:     if gPlayers[MySpectator.PlayerIndex].CanAddHousePlan(P, THouseType(GameCursor.Tag1)) then
                              begin
                                gPlayers[MySpectator.PlayerIndex].AddHouse(THouseType(GameCursor.Tag1), P.X, P.Y, true);
                                //Holding shift allows to place that house multiple times
                                if not (ssShift in GameCursor.SState) then
                                  GameCursor.Mode := cmRoad;
                              end;
                cmElevate, cmEqualize,
                cmBrush, cmObjects,
                cmTiles:      fTerrainPainter.MakeCheckpoint;
                cmMagicWater: fTerrainPainter.MagicWater(P);
                cmUnits:      if GameCursor.Tag1 = 255 then
                                gPlayers.RemAnyUnit(P)
                              else
                              if gTerrain.CanPlaceUnit(P, TUnitType(GameCursor.Tag1)) then
                              begin
                                //Check if we can really add a unit
                                if TUnitType(GameCursor.Tag1) in [CITIZEN_MIN..CITIZEN_MAX] then
                                  gPlayers[MySpectator.PlayerIndex].AddUnit(TUnitType(GameCursor.Tag1), P, False)
                                else
                                if TUnitType(GameCursor.Tag1) in [WARRIOR_MIN..WARRIOR_MAX] then
                                  gPlayers[MySpectator.PlayerIndex].AddUnitGroup(TUnitType(GameCursor.Tag1), P, dir_S, 1, 1)
                                else
                                  gPlayers.PlayerAnimals.AddUnit(TUnitType(GameCursor.Tag1), P);
                              end;
                cmMarkers:    case GameCursor.Tag1 of
                                MARKER_REVEAL:        fRevealers[MySpectator.PlayerIndex].Add(P, GameCursor.MapEdSize);
                                MARKER_DEFENCE:       gPlayers[MySpectator.PlayerIndex].AI.General.DefencePositions.Add(KMPointDir(P, dir_N), gt_Melee, 10, adt_FrontLine);
                                MARKER_CENTERSCREEN:  begin
                                                        gPlayers[MySpectator.PlayerIndex].CenterScreen := P;
                                                        //Update XY display
                                                      end;
                              end;
                cmErase:      begin
                                gPlayers.RemAnyHouse(P);
                                if gTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                  gTerrain.RemRoad(P);
                                if gTerrain.TileIsCornField(P) or gTerrain.TileIsWineField(P) then
                                  gTerrain.RemField(P);
                              end;
              end;
    mbRight:  case GameCursor.Mode of
                cmTiles:    begin
                              GameCursor.MapEdDir := (GameCursor.MapEdDir + 1) mod 4; //Rotate tile direction
                              fTerrainPainter.MakeCheckpoint;
                            end;
                cmElevate,
                cmEqualize: begin
                              //Actual change was made in UpdateStateIdle, we just register it is done here
                              fTerrainPainter.MakeCheckpoint;
                            end;
                cmObjects:  begin
                              gTerrain.Land[P.Y,P.X].Obj := 255; //Delete object
                              fTerrainPainter.MakeCheckpoint;
                            end;
              end;
  end;
end;


procedure TKMMapEditor.Update;
begin
  if mlDeposits in VisibleLayers then
    fDeposits.UpdateAreas([rdStone, rdCoal, rdIron, rdGold, rdFish]);

  //todo: if mlNavMesh in VisibleLayers then
    //fAIFields.NavMesh.Init;
end;


procedure TKMMapEditor.Paint(aLayer: TPaintLayer);
var
  I, K: Integer;
  Loc, P: TKMPoint;
  G: TKMUnitGroup;
  DP: TAIDefencePosition;
begin
  P := GameCursor.Cell;

  if aLayer = plCursors then
  //With Buildings tab see if we can remove Fields or Houses
  if (GameCursor.Mode = cmErase)
  and (    gTerrain.TileIsCornField(P)
           or gTerrain.TileIsWineField(P)
           or (gTerrain.Land[P.Y,P.X].TileOverlay=to_Road)
           or (gPlayers.HousesHitTest(P.X, P.Y) <> nil))
  then
    fRenderPool.RenderWireTile(P, $FFFFFF00); //Cyan quad


  if mlDefences in fVisibleLayers then
  begin
    if aLayer = plCursors then
      for I := 0 to gPlayers.Count - 1 do
      for K := 0 to gPlayers[I].AI.General.DefencePositions.Count - 1 do
      begin
        DP := gPlayers[I].AI.General.DefencePositions[K];
        fRenderPool.RenderSpriteOnTile(DP.Position.Loc, 510 + Byte(DP.Position.Dir), gPlayers[I].FlagColor);
      end;

    if ActiveMarker.MarkerType = mtDefence then
    if InRange(ActiveMarker.Index, 0, gPlayers[ActiveMarker.Owner].AI.General.DefencePositions.Count - 1) then
    begin
      DP := gPlayers[ActiveMarker.Owner].AI.General.DefencePositions[ActiveMarker.Index];
      fRenderAux.CircleOnTerrain(DP.Position.Loc.X, DP.Position.Loc.Y, DP.Radius,
                                 gPlayers[ActiveMarker.Owner].FlagColor AND $20FFFF80,
                                 gPlayers[ActiveMarker.Owner].FlagColor);
    end;
  end;

  if mlRevealFOW in fVisibleLayers then
  for I := 0 to gPlayers.Count - 1 do
  for K := 0 to fRevealers[I].Count - 1 do
  begin
    Loc := fRevealers[I][K];
    case aLayer of
      plTerrain:  fRenderAux.CircleOnTerrain(Loc.X, Loc.Y,
                                           fRevealers[I].Tag[K],
                                           gPlayers[I].FlagColor and $20FFFFFF,
                                           gPlayers[I].FlagColor);
      plCursors:  fRenderPool.RenderSpriteOnTile(Loc,
                      394, gPlayers[I].FlagColor);
    end;
  end;

  if mlCenterScreen in fVisibleLayers then
  for I := 0 to gPlayers.Count - 1 do
  begin
    Loc := gPlayers[I].CenterScreen;
    case aLayer of
      plTerrain:  fRenderAux.SquareOnTerrain(Loc.X - 3, Loc.Y - 2.5,
                                             Loc.X + 2, Loc.Y + 1.5,
                                             gPlayers[I].FlagColor);
      plCursors:  fRenderPool.RenderSpriteOnTile(Loc,
                      391, gPlayers[I].FlagColor);
    end;
  end;

  if mlSelection in fVisibleLayers then
    fSelection.Paint;

  //Show selected group order target
  if MySpectator.Selected is TKMUnitGroup then
  begin
    G := TKMUnitGroup(MySpectator.Selected);
    if G.MapEdOrder.Order <> ioNoOrder then
    begin
      fRenderAux.Quad(G.MapEdOrder.Pos.Loc.X, G.MapEdOrder.Pos.Loc.Y, $40FF00FF);
      fRenderAux.LineOnTerrain(G.Position.X - 0.5, G.Position.Y - 0.5, G.MapEdOrder.Pos.Loc.X - 0.5, G.MapEdOrder.Pos.Loc.Y - 0.5, $FF0000FF);
    end;
  end;
end;


end.
