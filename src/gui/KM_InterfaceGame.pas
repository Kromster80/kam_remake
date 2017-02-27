unit KM_InterfaceGame;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Controls, Classes, Math, KM_Defaults, KM_Controls, KM_Points,
  KM_InterfaceDefaults,
  KM_GameCursor, KM_Render, KM_Minimap, KM_Viewport, KM_ResHouses, KM_ResWares;


type
  // Common class for ingame interfaces (Gameplay, MapEd)
  TKMUserInterfaceGame = class(TKMUserInterfaceCommon)
  protected
    fMinimap: TKMMinimap;
    fViewport: TKMViewport;
  public
    constructor Create(aRender: TRender); reintroduce;
    destructor Destroy; override;

    property Minimap: TKMMinimap read fMinimap;
    property Viewport: TKMViewport read fViewport;

    function CursorToMapCoord(X, Y: Integer): TKMPointF;

    procedure KeyDown(Key: Word; Shift: TShiftState); override;
    procedure KeyUp(Key: Word; Shift: TShiftState); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer); override;

    procedure SyncUI(aMoveViewport: Boolean = True); virtual;
    procedure SyncUIView(aCenter: TKMPointF; aZoom: Single = 1);
    procedure UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
    procedure UpdateStateIdle(aFrameTime: Cardinal); virtual; abstract;
  end;


const
  // Toolbar pads
  TB_PAD = 9; // Picked up empirically
  TB_WIDTH = 180; // Max width of sidebar elements
  PAGE_TITLE_Y = 5; // Page title offset

  // Shortcuts
  // All shortcuts are in English and are the same for all languages to avoid
  // naming collisions and confusion in discussions

  GUI_HOUSE_COUNT = 27;   // Number of KaM houses to show in GUI
  GUIHouseOrder: array [1..GUI_HOUSE_COUNT] of THouseType = (
    ht_School, ht_Inn, ht_Quary, ht_Woodcutters, ht_Sawmill,
    ht_Farm, ht_Mill, ht_Bakery, ht_Swine, ht_Butchers,
    ht_Wineyard, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_WeaponWorkshop,
    ht_Tannery, ht_ArmorWorkshop, ht_Stables, ht_IronMine, ht_IronSmithy,
    ht_WeaponSmithy, ht_ArmorSmithy, ht_Barracks, ht_Store, ht_WatchTower,
    ht_FisherHut, ht_Marketplace);

  // Template for how resources are shown in Barracks
  BARRACKS_RES_COUNT = 11;
  BarracksResType: array [1..BARRACKS_RES_COUNT] of TWareType =
    (wt_Shield, wt_MetalShield, wt_Armor, wt_MetalArmor, wt_Axe, wt_Sword,
     wt_Pike, wt_Hallebard, wt_Bow, wt_Arbalet, wt_Horse);

  // Layout of resources in Store
  STORE_RES_COUNT = 28;
  StoreResType: array [1..STORE_RES_COUNT] of TWareType =
    (wt_Trunk,    wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
     wt_Coal,     wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
     wt_Bread,    wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
     wt_Skin,     wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
     wt_Axe,      wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
     wt_Arbalet,  wt_Horse,   wt_Fish);

  School_Order: array [0..13] of TUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);

  Barracks_Order: array [0..8] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  // Stats get stacked by UI logic (so that on taller screens they all were
  // in nice pairs, and would stack up only on short screens)
  StatPlan: array [0..12] of record
    HouseType: array [0..3] of THouseType;
    UnitType: array [0..1] of TUnitType;
  end = (
    (HouseType: (ht_Quary, ht_None, ht_None, ht_None); UnitType: (ut_StoneCutter, ut_None)),
    (HouseType: (ht_Woodcutters, ht_None, ht_None, ht_None); UnitType: (ut_Woodcutter, ut_None)),
    (HouseType: (ht_FisherHut, ht_None, ht_None, ht_None); UnitType: (ut_Fisher, ut_None)),
    (HouseType: (ht_Farm, ht_Wineyard, ht_None, ht_None); UnitType: (ut_Farmer, ut_None)),
    (HouseType: (ht_Mill, ht_Bakery, ht_None, ht_None); UnitType: (ut_Baker, ut_None)),
    (HouseType: (ht_Swine, ht_Stables, ht_None, ht_None); UnitType: (ut_AnimalBreeder, ut_None)),
    (HouseType: (ht_Butchers, ht_Tannery, ht_None, ht_None); UnitType: (ut_Butcher, ut_None)),
    (HouseType: (ht_Metallurgists, ht_IronSmithy, ht_None, ht_None); UnitType: (ut_Metallurgist, ut_None)),
    (HouseType: (ht_ArmorSmithy, ht_WeaponSmithy, ht_None, ht_None); UnitType: (ut_Smith, ut_None)),
    (HouseType: (ht_CoalMine, ht_IronMine, ht_GoldMine, ht_None); UnitType: (ut_Miner, ut_None)),
    (HouseType: (ht_Sawmill, ht_WeaponWorkshop, ht_ArmorWorkshop, ht_None); UnitType: (ut_Lamberjack, ut_None)),
    (HouseType: (ht_Barracks, ht_WatchTower, ht_None, ht_None); UnitType: (ut_Recruit, ut_None)),
    (HouseType: (ht_Store, ht_School, ht_Inn, ht_Marketplace); UnitType: (ut_Serf, ut_Worker))
    );

  MapEd_Order: array [0..13] of TUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian,
    ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman);

  MapEd_Icon: array [0..13] of Word = (
    61, 62, 63, 64, 65,
    66, 67, 68, 69, 70,
    79, 80, 81, 82);

  Animal_Order: array [0..7] of TUnitType = (
    ut_Wolf, ut_Fish,        ut_Watersnake, ut_Seastar,
    ut_Crab, ut_Waterflower, ut_Waterleaf,  ut_Duck);

  Animal_Icon: array [0..7] of word = (
    71, 72, 73, 74,
    75, 76, 77, 78);


  // Amounts for placing orders
  ORDER_WHEEL_AMOUNT = 5;

  MARKET_RES_HEIGHT = 35;

  // Big tab buttons in MapEd
  BIG_TAB_W = 36;
  BIG_PAD_W = 36;
  BIG_TAB_H = 36;
  // Small sub-tab buttons in MapEd
  SMALL_TAB_W = 30;
  SMALL_PAD_W = 30;
  SMALL_TAB_H = 26;

  MESSAGE_AREA_HEIGHT = 173+17; // Image_ChatHead + Image_ChatBody
  MESSAGE_AREA_RESIZE_Y = 200; // How much can we resize it


implementation
uses
  KM_Terrain, KM_RenderPool, KM_ResKeys;


{ TKMUserInterfaceGame }
constructor TKMUserInterfaceGame.Create(aRender: TRender);
begin
  inherited Create(aRender.ScreenX, aRender.ScreenY);

  fMinimap := TKMMinimap.Create(False, False);
  fViewport := TKMViewport.Create(aRender.ScreenX, aRender.ScreenY);
  gRenderPool := TRenderPool.Create(fViewport, aRender);
end;


destructor TKMUserInterfaceGame.Destroy;
begin
  FreeAndNil(fMinimap);
  FreeAndNil(fViewport);
  FreeAndNil(gRenderPool);
  Inherited;
end;


procedure TKMUserInterfaceGame.KeyDown(Key: Word; Shift: TShiftState);
begin
  //Scrolling
  if Key = gResKeys[SC_SCROLL_LEFT].Key  then fViewport.ScrollKeyLeft  := True;
  if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := True;
  if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := True;
  if Key = gResKeys[SC_SCROLL_DOWN].Key  then fViewport.ScrollKeyDown  := True;
  if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := True;
  if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := True;
end;


procedure TKMUserInterfaceGame.KeyUp(Key: Word; Shift: TShiftState);
begin
  //Scrolling
  if Key = gResKeys[SC_SCROLL_LEFT].Key  then fViewport.ScrollKeyLeft  := False;
  if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := False;
  if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := False;
  if Key = gResKeys[SC_SCROLL_DOWN].Key  then fViewport.ScrollKeyDown  := False;
  if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := False;
  if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := False;
  if Key = gResKeys[SC_ZOOM_RESET].Key   then fViewport.ResetZoom;
end;


procedure TKMUserInterfaceGame.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer);
var
  PrevCursor: TKMPointF;
begin
  inherited;

  if (X < 0) or (Y < 0) then Exit; // This happens when you use the mouse wheel on the window frame

  // Allow to zoom only when cursor is over map. Controls handle zoom on their own
  if (fMyControls.CtrlOver = nil) then
  begin
    UpdateGameCursor(X, Y, Shift); // Make sure we have the correct cursor position to begin with
    PrevCursor := gGameCursor.Float;
    fViewport.Zoom := fViewport.Zoom + WheelDelta / 2000;
    UpdateGameCursor(X, Y, Shift); // Zooming changes the cursor position
    // Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
    fViewport.Position := KMPointF(fViewport.Position.X + PrevCursor.X-gGameCursor.Float.X,
                                   fViewport.Position.Y + PrevCursor.Y-gGameCursor.Float.Y);
    UpdateGameCursor(X, Y, Shift); // Recentering the map changes the cursor position
  end;
end;


procedure TKMUserInterfaceGame.SyncUI(aMoveViewport: Boolean = True);
begin
  fMinimap.LoadFromTerrain;
  fMinimap.Update(False);

  if aMoveViewport then
  begin
    fViewport.ResizeMap(gTerrain.MapX, gTerrain.MapY, gTerrain.TopHill / CELL_SIZE_PX);
    fViewport.ResetZoom;
  end;
end;


procedure TKMUserInterfaceGame.SyncUIView(aCenter: TKMPointF; aZoom: Single = 1);
begin
  fViewport.Position := aCenter;
  fViewport.Zoom := aZoom;
end;


function TKMUserInterfaceGame.CursorToMapCoord(X, Y: Integer): TKMPointF;
begin
  Result.X := fViewport.Position.X + (X-fViewport.ViewRect.Right/2-TOOLBAR_WIDTH/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := fViewport.Position.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := gTerrain.ConvertCursorToMapCoord(Result.X, Result.Y);
end;


// Compute cursor position and store it in global variables
procedure TKMUserInterfaceGame.UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
begin
  with gGameCursor do
  begin
    Pixel.X := X;
    Pixel.Y := Y;
    Float := CursorToMapCoord(X, Y);

    PrevCell := Cell; //Save previous cell

    // Cursor cannot reach row MapY or column MapX, they're not part of the map (only used for vertex height)
    Cell.X := EnsureRange(round(Float.X+0.5), 1, gTerrain.MapX-1); // Cell below cursor in map bounds
    Cell.Y := EnsureRange(round(Float.Y+0.5), 1, gTerrain.MapY-1);

    ObjectUID := gRenderPool.RenderList.GetSelectionUID(Float);
    SState := Shift;
  end;
end;


end.
