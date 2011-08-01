unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses
     {$IFDEF MSWindows} Windows, {$ENDIF}
     {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
     Classes, Controls, KromUtils, Math, SysUtils, KromOGLUtils, Forms,
     KM_Controls, KM_Defaults, KM_Houses, KM_Units, KM_Points;

type
  TKMapEdInterface = class
  private
    MyControls: TKMMasterControl;

    fShownUnit:TKMUnit;
    fShownHouse:TKMHouse;
    fShowPassability:byte;
    PrevHint:TObject;
    StorehouseItem:byte; //Selected ware in storehouse
    BarracksItem:byte; //Selected ware in barracks
    TileDirection: byte;

    procedure Create_Terrain_Page;
    procedure Create_Village_Page;
    procedure Create_Player_Page;
    procedure Create_Mission_Page;
    procedure Create_Menu_Page;
    procedure Create_MenuSave_Page;
    procedure Create_MenuLoad_Page;
    procedure Create_MenuQuit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_Barracks_Page;

    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure Minimap_Update(Sender: TObject);

    procedure Menu_Save(Sender:TObject);
    procedure Menu_Load(Sender:TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Terrain_HeightChange(Sender: TObject);
    procedure Terrain_TilesChange(Sender: TObject);
    procedure Terrain_ObjectsChange(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure House_HealthChange(Sender:TObject; AButton:TMouseButton);
    procedure Unit_ButtonClick(Sender: TObject);
    procedure Unit_ArmyChange1(Sender:TObject); overload;
    procedure Unit_ArmyChange2(Sender:TObject; AButton:TMouseButton); overload;
    procedure Barracks_Fill(Sender:TObject);
    procedure Barracks_SelectWare(Sender:TObject);
    procedure Barracks_EditWareCount(Sender:TObject; AButton:TMouseButton);
    procedure Store_Fill(Sender:TObject);
    procedure Store_SelectWare(Sender:TObject);
    procedure Store_EditWareCount(Sender:TObject; AButton:TMouseButton);
    procedure Player_ChangeActive(Sender: TObject);
    procedure Player_ColorClick(Sender:TObject);
    procedure Mission_AlliancesChange(Sender:TObject);
    procedure Mission_PlayerTypesChange(Sender:TObject);
    procedure View_Passability(Sender:TObject);

    function GetSelectedTile: TObject;
    function GetSelectedObject: TObject;
    function GetSelectedUnit: TObject;
  protected
    Panel_Main:TKMPanel;
      Image_Main1,Image_Main2,Image_Main3,Image_Main4,Image_Main5:TKMImage; //Toolbar background
      KMMinimap:TKMMinimap;
      RatioRow_Passability:TKMRatioRow;
      Label_Passability:TKMLabel;
      Button_PlayerSelect:array[0..MAX_PLAYERS-1]of TKMFlatButtonShape; //Animals are common for all
      Label_Stat,Label_Hint:TKMLabel;
    Panel_Common:TKMPanel;
      Button_Main:array[1..5]of TKMButton; //5 buttons
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu below
      Label_MissionName: TKMLabel;

    Panel_Terrain:TKMPanel;
      Button_Terrain:array[1..4]of TKMButton;
      Panel_Brushes:TKMPanel;
        BrushSize:TKMRatioRow;
        BrushCircle,BrushSquare:TKMButtonFlat;
      Panel_Heights:TKMPanel;
        HeightSize,HeightShape:TKMRatioRow;
        HeightCircle,HeightSquare:TKMButtonFlat;
      Panel_Tiles:TKMPanel;
        TilesTable:array[1..MAPED_TILES_COLS*MAPED_TILES_ROWS] of TKMButtonFlat; //how many are visible?
        TilesScroll:TKMScrollBar;
        TilesRandom:TKMCheckBox;
      Panel_Objects:TKMPanel;
        ObjectErase:TKMButtonFlat;
        ObjectsTable:array[1..4] of TKMButtonFlat;
        ObjectsScroll:TKMScrollBar;

    Panel_Village:TKMPanel;
      Button_Village:array[1..3]of TKMButton;
      Panel_Build:TKMPanel;
        Button_BuildRoad,Button_BuildField,Button_BuildWine,Button_BuildWall,Button_BuildCancel:TKMButtonFlat;
        Button_Build:array[1..GUI_HOUSE_COUNT]of TKMButtonFlat;
      Panel_Units:TKMPanel;
        Button_UnitCancel:TKMButtonFlat;
        Button_Citizen:array[1..14]of TKMButtonFlat;
        Button_Warriors:array[1..10]of TKMButtonFlat;
        Button_Animals:array[1..8]of TKMButtonFlat;
      Panel_Script:TKMPanel;

    Panel_Player:TKMPanel;
      Button_Player:array[1..2]of TKMButton;
      Panel_Goals:TKMPanel;
      Panel_Color:TKMPanel;
        ColorSwatch_Color:TKMColorSwatch;

    Panel_Mission:TKMPanel;
      Button_Mission:array[1..2]of TKMButton;
      Panel_Alliances:TKMPanel;
        CheckBox_Alliances: array[0..MAX_PLAYERS-1,0..MAX_PLAYERS-1] of TKMCheckBox;
        CheckBox_AlliancesSym:TKMCheckBox;
      Panel_PlayerTypes:TKMPanel;
        CheckBox_PlayerTypes: array[0..MAX_PLAYERS-1,0..1] of TKMCheckBox;

    Panel_Menu:TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_Settings,Button_Menu_Quit:TKMButton;

      Panel_Save:TKMPanel;
        Edit_SaveName:TKMEdit;
        Label_SaveExists:TKMLabel;
        CheckBox_SaveExists:TKMCheckBox;
        Button_SaveSave:TKMButton;
        Button_SaveCancel:TKMButton;

      Panel_Load:TKMPanel;
        FileList_Load:TKMFileList;
        Button_LoadLoad:TKMButton;
        Button_LoadCancel:TKMButton;

      Panel_Quit:TKMPanel;
        Button_Quit_Yes,Button_Quit_No:TKMButton;

    Panel_Unit:TKMPanel;
      Label_UnitName:TKMLabel;
      Label_UnitCondition:TKMLabel;
      Label_UnitDescription:TKMLabel;
      KMConditionBar_Unit:TKMPercentBar;
      Image_UnitPic:TKMImage;

      Panel_Army:TKMPanel;
        Button_Army_RotCW,Button_Army_RotCCW:TKMButton;
        Button_Army_ForUp,Button_Army_ForDown:TKMButton;
        ImageStack_Army:TKMImageStack;
        Button_ArmyDec,Button_ArmyFood,Button_ArmyInc:TKMButton;

    Panel_House:TKMPanel;
      Label_House:TKMLabel;
      Image_House_Logo,Image_House_Worker:TKMImage;
      Label_HouseHealth:TKMLabel;
      KMHealthBar_House:TKMPercentBar;
      Button_HouseHealthDec,Button_HouseHealthInc:TKMButton;

    Panel_HouseStore:TKMPanel;
      Button_Store:array[1..28]of TKMButtonFlat;
      Label_Store_WareCount:TKMLabel;
      Button_StoreDec100,Button_StoreDec:TKMButton;
      Button_StoreInc100,Button_StoreInc:TKMButton;
    Panel_HouseBarracks:TKMPanel;
      Button_Barracks:array[1..11]of TKMButtonFlat;
      Label_Barracks_WareCount:TKMLabel;
      Button_BarracksDec100,Button_BarracksDec:TKMButton;
      Button_BarracksInc100,Button_BarracksInc:TKMButton;
  public
    constructor Create(aScreenX, aScreenY: word);
    destructor Destroy; override;
    procedure Player_UpdateColors;
    procedure Resize(X,Y:word);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    property ShowPassability:byte read fShowPassability;
    procedure RightClick_Cancel;
    procedure KeyDown(Key:Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key:Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
    function GetShownPage:TKMMapEdShownPage;
    procedure SetTileDirection(aTileDirection: byte);
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Units_Warrior, KM_PlayersCollection, KM_Player, KM_TextLibrary, KM_Terrain,
     KM_Utils, KM_Viewport, KM_Game, KM_ResourceGFX;


{Switch between pages}
procedure TKMapEdInterface.SwitchPage(Sender: TObject);
var i,k:integer;
begin

  //Reset cursor mode
  GameCursor.Mode := cm_None;
  GameCursor.Tag1 := 0;
  GameCursor.Tag2 := 0;

  //If the user clicks on the tab that is open, it closes it (main buttons only)
  if ((Sender = Button_Main[1]) and Panel_Terrain.Visible) or
     ((Sender = Button_Main[2]) and Panel_Village.Visible) or
     ((Sender = Button_Main[3]) and Panel_Player.Visible) or
     ((Sender = Button_Main[4]) and Panel_Mission.Visible) or
     ((Sender = Button_Main[5]) and Panel_Menu.Visible) then
     Sender := nil;

  //Reset shown item if user clicked on any of the main buttons
  if (Sender=Button_Main[1])or(Sender=Button_Main[2])or
     (Sender=Button_Main[3])or(Sender=Button_Main[4])or
     (Sender=Button_Main[5])or
     (Sender=Button_Menu_Settings)or(Sender=Button_Menu_Quit) then begin
    fShownHouse:=nil;
    fShownUnit:=nil;
    fPlayers.Selected:=nil;
  end;

  Label_MenuTitle.Caption := '';
  //Now hide all existing pages
    for i:=1 to Panel_Common.ChildCount do
      if Panel_Common.Childs[i] is TKMPanel then
      begin
        for k:=1 to TKMPanel(Panel_Common.Childs[i]).ChildCount do
          if TKMPanel(Panel_Common.Childs[i]).Childs[k] is TKMPanel then
            TKMPanel(Panel_Common.Childs[i]).Childs[k].Hide;
        Panel_Common.Childs[i].Hide;
      end;

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[1]) then begin
    Panel_Terrain.Show;
    Panel_Brushes.Show;
    Label_MenuTitle.Caption:='Terrain - Brushes';
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[2]) then begin
    Panel_Terrain.Show;
    Panel_Heights.Show;
    Label_MenuTitle.Caption:='Terrain - Heights';
    Terrain_HeightChange(HeightCircle); //Select the default mode
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[3]) then begin
    Panel_Terrain.Show;
    Panel_Tiles.Show;
    Label_MenuTitle.Caption:='Terrain - Tiles';
    SetTileDirection(TileDirection); //ensures tags are in allowed ranges
    Terrain_TilesChange(GetSelectedTile);
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[4]) then begin
    Panel_Terrain.Show;
    Panel_Objects.Show;
    Label_MenuTitle.Caption:='Terrain - Objects';
    Terrain_ObjectsChange(GetSelectedObject);
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[1]) then begin
    Panel_Village.Show;
    Panel_Build.Show;
    Label_MenuTitle.Caption:='Village - Buildings';
    Build_ButtonClick(Button_BuildRoad);
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[2]) then begin
    Panel_Village.Show;
    Panel_Units.Show;
    Label_MenuTitle.Caption:='Village - Units';
    Unit_ButtonClick(GetSelectedUnit);
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[3]) then begin
    Panel_Village.Show;
    Panel_Script.Show;
    Label_MenuTitle.Caption:='Village - Script';
  end else

  if (Sender = Button_Main[3])or(Sender = Button_Player[1]) then begin
    Panel_Player.Show;
    Panel_Goals.Show;
    Label_MenuTitle.Caption:='Player - Goals';
  end else

  if (Sender = Button_Main[3])or(Sender = Button_Player[2]) then begin
    Panel_Player.Show;
    Panel_Color.Show;
    Label_MenuTitle.Caption:='Player - Color';
  end else

  if (Sender = Button_Main[4])or(Sender = Button_Mission[1]) then begin
    Panel_Mission.Show;
    Panel_Alliances.Show;
    Label_MenuTitle.Caption:='Mission - Alliances';
    Mission_AlliancesChange(nil);
  end else

  if (Sender = Button_Main[4])or(Sender = Button_Mission[2]) then begin
    Panel_Mission.Show;
    Panel_PlayerTypes.Show;
    Label_MenuTitle.Caption:='Mission - Player Types';
    Mission_PlayerTypesChange(nil);
  end else

  if (Sender=Button_Main[5]) or
     (Sender=Button_Quit_No) or
     (Sender = Button_LoadCancel) or
     (Sender = Button_SaveCancel) then begin
    Panel_Menu.Show;
    Label_MenuTitle.Caption:=fTextLibrary.GetTextString(170);
  end else

  if Sender=Button_Menu_Quit then begin
    Panel_Quit.Show;
  end;

  if Sender = Button_Menu_Save then begin
    Edit_SaveName.Text := fGame.GetGameName;
    Menu_Save(Edit_SaveName);
    Panel_Save.Show;
  end;

  if Sender = Button_Menu_Load then begin
    FileList_Load.RefreshList(ExeDir+'Maps\', 'dat', 'map', true);
    FileList_Load.ItemIndex := 0; //Try to select first map by default
    Panel_Load.Show;
  end;

  //Now process all other kinds of pages
  if Sender=Panel_Unit then begin
    TKMPanel(Sender).Show;
  end else

  if Sender=Panel_House then begin
    TKMPanel(Sender).Show;
  end;

  if Sender=Panel_HouseBarracks then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end else

  if Sender=Panel_HouseStore then begin
    TKMPanel(Sender).Parent.Show;
    TKMPanel(Sender).Show;
  end;

end;


procedure TKMapEdInterface.DisplayHint(Sender: TObject);
begin
  if (PrevHint = Sender) then exit; //Hint didn't changed

  if Sender=nil then Label_Hint.Caption:=''
                else Label_Hint.Caption:=TKMControl(Sender).Hint;

  PrevHint := Sender;
end;


{Update minimap data}
procedure TKMapEdInterface.Minimap_Update(Sender: TObject);
begin
  if Sender=nil then begin //UpdateState loop
    KMMinimap.MapSize:=KMPoint(fTerrain.MapX,fTerrain.MapY);
  end else
    if KMMinimap.BoundRectAt.X*KMMinimap.BoundRectAt.Y <> 0 then //Quick bugfix incase minimap yet not inited it will center vp on 0;0
      fViewport.SetCenter(KMMinimap.BoundRectAt.X,KMMinimap.BoundRectAt.Y);

  KMMinimap.BoundRectAt := KMPointRound(fViewport.GetCenter);
  KMMinimap.ViewArea   := fViewport.GetMinimapClip;

  Label_MissionName.Caption := fGame.GetGameName;
end;


constructor TKMapEdInterface.Create(aScreenX, aScreenY: word);
var i:integer;
begin
  Inherited Create;
  Assert(fViewport<>nil, 'fViewport required to be init first');

  fShownUnit  := nil;
  fShownHouse := nil;
  BarracksItem   := 1; //First ware selected by default
  StorehouseItem := 1; //First ware selected by default
  TileDirection := 0;

{Parent Page for whole toolbar in-game}
  MyControls := TKMMasterControl.Create;
  Panel_Main := TKMPanel.Create(MyControls, 0, 0, aScreenX, aScreenY);

    Image_Main1 := TKMImage.Create(Panel_Main,0,0,224,200,407); //Minimap place

    //todo: player selection and other "universal" stuff (i.e. which player are we placing for)
    Image_Main3 := TKMImage.Create(Panel_Main,0, 200,224,400,404);
    Image_Main4 := TKMImage.Create(Panel_Main,0, 600,224,400,404);
    Image_Main5 := TKMImage.Create(Panel_Main,0,1000,224,400,404); //For 1600x1200 this is needed

    KMMinimap := TKMMinimap.Create(Panel_Main,10,10,176,176);
    KMMinimap.OnChange := Minimap_Update;

    TKMLabel.Create(Panel_Main,8,200,100,30,'View passsability',fnt_Metal,kaLeft);
    RatioRow_Passability := TKMRatioRow.Create(Panel_Main, 8, 220, 192, 20, 0, 13);
    RatioRow_Passability.Position := 0;
    RatioRow_Passability.MaxValue := length(PassabilityStr);
    RatioRow_Passability.OnChange := View_Passability;
    Label_Passability := TKMLabel.Create(Panel_Main,8,240,100,30,'Off',fnt_Metal,kaLeft);

    TKMLabel.Create(Panel_Main,8,270,100,30,'Player',fnt_Metal,kaLeft);
    for i:=0 to MAX_PLAYERS-1 do begin
      Button_PlayerSelect[i]         := TKMFlatButtonShape.Create(Panel_Main, 8 + i*23, 290, 21, 32, inttostr(i+1), fnt_Grey, $FF0000FF);
      Button_PlayerSelect[i].CapOffsetY := -3;
      Button_PlayerSelect[i].Tag     := i;
      Button_PlayerSelect[i].OnClick := Player_ChangeActive;
    end;

    Label_MissionName := TKMLabel.Create(Panel_Main, 8, 340, 100, 10, '', fnt_Metal, kaLeft);

    Label_Stat:=TKMLabel.Create(Panel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    Label_Hint:=TKMLabel.Create(Panel_Main,224+8,Panel_Main.Height-16,0,0,'',fnt_Outline,kaLeft);
    Label_Hint.Anchors := [akLeft, akBottom];

  Panel_Common := TKMPanel.Create(Panel_Main,0,300,224,768);

    {5 big tabs}
    Button_Main[1] := TKMButton.Create(Panel_Common,   8, 72, 36, 36, 381);
    Button_Main[2] := TKMButton.Create(Panel_Common,  46, 72, 36, 36, 368);
    Button_Main[3] := TKMButton.Create(Panel_Common,  84, 72, 36, 36,  41);
    Button_Main[4] := TKMButton.Create(Panel_Common, 122, 72, 36, 36, 441);
    Button_Main[5] := TKMButton.Create(Panel_Common, 160, 72, 36, 36, 389);
    Button_Main[1].Hint := fTextLibrary[TX_MAPEDITOR_TERRAIN];
    Button_Main[2].Hint := fTextLibrary[TX_MAPEDITOR_VILLAGE];
    Button_Main[3].Hint := fTextLibrary[TX_MAPEDITOR_SCRIPTS_VISUAL];
    Button_Main[4].Hint := fTextLibrary[TX_MAPEDITOR_SCRIPTS_GLOBAL];
    Button_Main[5].Hint := fTextLibrary[TX_MAPEDITOR_MENU];
    for i:=1 to 5 do Button_Main[i].OnClick := SwitchPage;

    Label_MenuTitle:=TKMLabel.Create(Panel_Common,8,112,138,36,'',fnt_Metal,kaLeft); //Should be one-line


{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Terrain_Page;
  Create_Village_Page;
  Create_Player_Page;
  Create_Mission_Page;

  Create_Menu_Page;
    Create_MenuSave_Page;
    Create_MenuLoad_Page;
    Create_MenuQuit_Page;

  Create_Unit_Page;
  Create_House_Page;
    Create_Store_Page;
    Create_Barracks_Page;
    //Create_TownHall_Page;

  MyControls.OnHint := DisplayHint;

  SwitchPage(nil); //Update
end;


destructor TKMapEdInterface.Destroy;
begin
  MyControls.Free;
  Inherited;
end;


//Update Hint position and etc..
procedure TKMapEdInterface.Resize(X,Y:word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
  fViewport.Resize(X,Y);
end;


{Terrain page}
procedure TKMapEdInterface.Create_Terrain_Page;
var i,k:integer;
begin
  Panel_Terrain := TKMPanel.Create(Panel_Common,0,128,196,28);
    Button_Terrain[1] := TKMButton.Create(Panel_Terrain,   8, 4, 36, 24, 383);
    Button_Terrain[2] := TKMButton.Create(Panel_Terrain,  48, 4, 36, 24, 388);
    Button_Terrain[3] := TKMButton.Create(Panel_Terrain,  88, 4, 36, 24, 382);
    Button_Terrain[4] := TKMButton.Create(Panel_Terrain, 128, 4, 36, 24, 385);
    for i:=1 to 4 do Button_Terrain[i].OnClick := SwitchPage;

    Panel_Brushes := TKMPanel.Create(Panel_Terrain,0,28,196,400);
      BrushSize   := TKMRatioRow.Create(Panel_Brushes, 8, 10, 100, 20, 1, 12);
      BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 114, 8, 24, 24, 359);
      BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 142, 8, 24, 24, 352);
      TKMButtonFlat.Create(Panel_Brushes, 8, 30, 32, 32, 1, 8);

      {BrushSize.OnChange   := TerrainBrush_Change;
      BrushCircle.OnChange := TerrainBrush_Change;
      BrushSquare.OnChange := TerrainBrush_Change;}

    Panel_Heights := TKMPanel.Create(Panel_Terrain,0,28,196,400);
      HeightSize   := TKMRatioRow.Create(Panel_Heights, 8, 10, 100, 20, 1, 12);
      HeightCircle := TKMButtonFlat.Create(Panel_Heights, 114, 8, 24, 24, 359);
      HeightSquare := TKMButtonFlat.Create(Panel_Heights, 142, 8, 24, 24, 352);
      HeightShape  := TKMRatioRow.Create(Panel_Heights, 8, 30, 100, 20, 1, 12);
      HeightSize.MaxValue := 15; //4bit for size
      HeightShape.MaxValue := 15; //4bit for slope shape
      HeightSize.OnChange   := Terrain_HeightChange;
      HeightShape.OnChange  := Terrain_HeightChange;
      HeightCircle.OnClick  := Terrain_HeightChange;
      HeightSquare.OnClick  := Terrain_HeightChange;

    Panel_Tiles := TKMPanel.Create(Panel_Terrain,0,28,196,400);
      TilesRandom := TKMCheckBox.Create(Panel_Tiles, 8, 4, 100, 20, 'Random Direction', fnt_Metal);
      TilesRandom.Checked := true;
      TilesRandom.OnClick := Terrain_TilesChange;
      TilesScroll := TKMScrollBar.Create(Panel_Tiles, 2, 30 + 4 + MAPED_TILES_ROWS * 32, 194, 20, sa_Horizontal, bsGame);
      TilesScroll.MaxValue := 256 div MAPED_TILES_ROWS - MAPED_TILES_COLS; // 16 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := Terrain_TilesChange;
      for i:=1 to MAPED_TILES_COLS do for k:=1 to MAPED_TILES_ROWS do begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k] := TKMButtonFlat.Create(Panel_Tiles,2+(i-1)*32,30+(k-1)*32,32,32,1,8); //2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Tag := (k-1)*MAPED_TILES_COLS+i; //Store ID
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnClick := Terrain_TilesChange;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnMouseWheel := TilesScroll.MouseWheel;
      end;
      Terrain_TilesChange(TilesScroll); //This ensures that the displayed images get updated the first time
      Terrain_TilesChange(TilesTable[1]);

    Panel_Objects := TKMPanel.Create(Panel_Terrain,0,28,196,400);
      ObjectsScroll := TKMScrollBar.Create(Panel_Objects, 8, 268, 180, 20, sa_Horizontal, bsGame);
      ObjectsScroll.MinValue := 1;
      ObjectsScroll.MaxValue := ActualMapElemQty div 2;
      ObjectsScroll.Position := 1;
      ObjectsScroll.OnChange := Terrain_ObjectsChange;
      ObjectErase := TKMButtonFlat.Create(Panel_Objects, 8, 8,32,32,340);
      ObjectsTable[1] := TKMButtonFlat.Create(Panel_Objects, 8, 40,90,110,1,1); //RXid=1  // 1 2
      ObjectsTable[2] := TKMButtonFlat.Create(Panel_Objects, 8,150,90,110,1,1); //RXid=1  // 3 4
      ObjectsTable[3] := TKMButtonFlat.Create(Panel_Objects,98, 40,90,110,1,1); //RXid=1
      ObjectsTable[4] := TKMButtonFlat.Create(Panel_Objects,98,150,90,110,1,1); //RXid=1
      for i:=1 to 4 do begin
        ObjectsTable[i].Tag := i; //Store ID
        ObjectsTable[i].OnClick := Terrain_ObjectsChange;
        ObjectsTable[i].OnMouseWheel := ObjectsScroll.MouseWheel;
      end;
      ObjectErase.Tag := 255; //no object
      ObjectErase.OnClick := Terrain_ObjectsChange;
    Terrain_ObjectsChange(ObjectsScroll); //This ensures that the displayed images get updated the first time
    Terrain_ObjectsChange(ObjectsTable[1]);
end;

{Build page}
procedure TKMapEdInterface.Create_Village_Page;
var i:integer;
begin
  Panel_Village := TKMPanel.Create(Panel_Common,0,128,196,28);
    Button_Village[1] := TKMButton.Create(Panel_Village,   8, 4, 36, 24, 454);
    Button_Village[2] := TKMButton.Create(Panel_Village,  48, 4, 36, 24, 141);
    Button_Village[3] := TKMButton.Create(Panel_Village,  88, 4, 36, 24, 327);
    for i:=1 to 3 do Button_Village[i].OnClick := SwitchPage;

    Panel_Build := TKMPanel.Create(Panel_Village,0,28,196,400);
      TKMLabel.Create(Panel_Build,100,10,100,30,'Roadworks',fnt_Outline,kaCenter);
      Button_BuildRoad   := TKMButtonFlat.Create(Panel_Build,  8,28,33,33,335);
      Button_BuildField  := TKMButtonFlat.Create(Panel_Build, 45,28,33,33,337);
      Button_BuildWine   := TKMButtonFlat.Create(Panel_Build, 82,28,33,33,336);
      Button_BuildWall   := TKMButtonFlat.Create(Panel_Build,119,28,33,33,339);
      Button_BuildCancel := TKMButtonFlat.Create(Panel_Build,156,28,33,33,340);
      Button_BuildRoad.OnClick  := Build_ButtonClick;
      Button_BuildField.OnClick := Build_ButtonClick;
      Button_BuildWine.OnClick  := Build_ButtonClick;
      Button_BuildWall.OnClick  := Build_ButtonClick;
      Button_BuildCancel.OnClick:= Build_ButtonClick;
      Button_BuildRoad.Hint     := fTextLibrary.GetTextString(213);
      Button_BuildField.Hint    := fTextLibrary.GetTextString(215);
      Button_BuildWine.Hint     := fTextLibrary.GetTextString(219);
      Button_BuildWall.Hint     := 'Build a wall';
      Button_BuildCancel.Hint   := fTextLibrary.GetTextString(211);

      TKMLabel.Create(Panel_Build,100,65,100,30,'Houses',fnt_Outline,kaCenter);
      for i:=1 to GUI_HOUSE_COUNT do
        if GUIHouseOrder[i] <> ht_None then begin
          Button_Build[i]:=TKMButtonFlat.Create(Panel_Build, 8+((i-1) mod 5)*37,83+((i-1) div 5)*37,33,33,fResource.HouseDat[GUIHouseOrder[i]].GUIIcon);
          Button_Build[i].OnClick:=Build_ButtonClick;
          Button_Build[i].Hint := fResource.HouseDat[GUIHouseOrder[i]].HouseName;
        end;

    Panel_Units := TKMPanel.Create(Panel_Village,0,28,196,400);

      TKMLabel.Create(Panel_Units,100,10,100,30,'Citizens',fnt_Outline,kaCenter);
      for i:=1 to length(Button_Citizen) do
      begin
        Button_Citizen[i] := TKMButtonFlat.Create(Panel_Units,8+((i-1) mod 5)*37,28+((i-1) div 5)*37,33,33,byte(School_Order[i])+140); //List of tiles 5x5
        Button_Citizen[i].Hint := TypeToString(School_Order[i]);
        Button_Citizen[i].Tag := byte(School_Order[i]); //Returns unit ID
        Button_Citizen[i].OnClick := Unit_ButtonClick;
      end;
      Button_UnitCancel := TKMButtonFlat.Create(Panel_Units,8+(length(Button_Citizen) mod 5)*37,30+(length(Button_Citizen) div 5)*37,33,33,340);
      Button_UnitCancel.Hint := fTextLibrary.GetTextString(211);
      Button_UnitCancel.OnClick := Unit_ButtonClick;

      TKMLabel.Create(Panel_Units,100,140,100,30,'Warriors',fnt_Outline,kaCenter);
      for i:=1 to length(Button_Warriors) do
      begin
        Button_Warriors[i] := TKMButtonFlat.Create(Panel_Units,8+((i-1) mod 5)*37,158+((i-1) div 5)*37,33,33, MapEd_Icon[i], 7);
        Button_Warriors[i].Hint := TypeToString(MapEd_Order[i]);
        Button_Warriors[i].Tag := byte(MapEd_Order[i]); //Returns unit ID
        Button_Warriors[i].OnClick := Unit_ButtonClick;
      end;

      TKMLabel.Create(Panel_Units,100,230,100,30,'Animals',fnt_Outline,kaCenter);
      for i:=1 to length(Button_Animals) do
      begin
        Button_Animals[i] := TKMButtonFlat.Create(Panel_Units,8+((i-1) mod 5)*37,248+((i-1) div 5)*37,33,33, Animal_Icon[i], 7);
        Button_Animals[i].Hint := TypeToString(Animal_Order[i]);
        Button_Animals[i].Tag := byte(Animal_Order[i]); //Returns animal ID
        Button_Animals[i].OnClick := Unit_ButtonClick;
      end;
      Unit_ButtonClick(Button_Citizen[1]); //Select serf as default

    Panel_Script := TKMPanel.Create(Panel_Village,0,28,196,400);
      TKMLabel.Create(Panel_Script,100,10,100,30,'Scripts',fnt_Outline,kaCenter);
      {Button_ScriptReveal         := TKMButtonFlat.Create(Panel_Script,  8,28,33,33,335);
      Button_ScriptReveal.OnClick := Script_ButtonClick;
      Button_ScriptReveal.Hint    := 'Reveal a portion of map';}
end;


procedure TKMapEdInterface.Create_Player_Page;
var i:integer; Col:array[0..255] of TColor4;
begin
  Panel_Player := TKMPanel.Create(Panel_Common,0,128,196,28);
    Button_Player[1] := TKMButton.Create(Panel_Player,   8, 4, 36, 24, 41);
    Button_Player[2] := TKMButton.Create(Panel_Player,  48, 4, 36, 24, 382);
    for i:=1 to 2 do Button_Player[i].OnClick := SwitchPage;

    Panel_Goals := TKMPanel.Create(Panel_Player,0,28,196,400);
      TKMLabel.Create(Panel_Goals,100,10,100,30,'Goals',fnt_Outline,kaCenter);

    Panel_Color := TKMPanel.Create(Panel_Player,0,28,196,400);
      TKMLabel.Create(Panel_Color,100,10,100,30,'Colors',fnt_Outline,kaCenter);
      TKMBevel.Create(Panel_Color,8,30,180,210);
      ColorSwatch_Color := TKMColorSwatch.Create(Panel_Color, 10, 32, 16, 16, 11);
      for i:=0 to 255 do Col[i] := fResource.GetColor32(i);
      ColorSwatch_Color.SetColors(Col);
      ColorSwatch_Color.OnClick := Player_ColorClick;
end;


procedure TKMapEdInterface.Create_Mission_Page;
var i,k:integer;
begin
  Panel_Mission := TKMPanel.Create(Panel_Common,0,128,196,28);
    Button_Mission[1] := TKMButton.Create(Panel_Mission,  8, 4, 36, 24, 41);
    Button_Mission[2] := TKMButton.Create(Panel_Mission, 48, 4, 36, 24, 41);
    for i:=1 to 2 do Button_Mission[i].OnClick := SwitchPage;

    Panel_Alliances := TKMPanel.Create(Panel_Mission,0,28,196,400);
      TKMLabel.Create(Panel_Alliances,100,10,100,30,'Alliances',fnt_Outline,kaCenter);
      for i:=0 to MAX_PLAYERS-1 do begin
        TKMLabel.Create(Panel_Alliances,32+i*20+2,30,100,20,inttostr(i+1),fnt_Outline,kaLeft);
        TKMLabel.Create(Panel_Alliances,12,50+i*25,100,20,inttostr(i+1),fnt_Outline,kaLeft);
        for k:=0 to MAX_PLAYERS-1 do begin
          //@Lewin: i=k allows some exotic cases where in theory player could fight with itself
          CheckBox_Alliances[i,k] := TKMCheckBox.Create(Panel_Alliances, 28+k*20, 46+i*25, 20, 20, '', fnt_Metal);
          CheckBox_Alliances[i,k].Tag       := i * MAX_PLAYERS + k;
          CheckBox_Alliances[i,k].FlatStyle := true;
          CheckBox_Alliances[i,k].OnClick   := Mission_AlliancesChange;
        end;
      end;

      //It does not have OnClick event for a reason:
      // - we don't have a rule to make alliances symmetrical yet
      CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 12, 50+MAX_PLAYERS*25, 20, 20, 'Symmetrical', fnt_Metal);
      CheckBox_AlliancesSym.Checked := true;
      CheckBox_AlliancesSym.Disable;

    Panel_PlayerTypes := TKMPanel.Create(Panel_Mission,0,28,196,400);
      TKMLabel.Create(Panel_PlayerTypes,100,10,100,30,'Player types',fnt_Outline,kaCenter);
      for i:=0 to MAX_PLAYERS-1 do begin
        TKMLabel.Create(Panel_PlayerTypes,12,30,100,20,'#',fnt_Grey,kaLeft);
        TKMLabel.Create(Panel_PlayerTypes,32,30,100,20,'Human',fnt_Grey,kaLeft);
        TKMLabel.Create(Panel_PlayerTypes,102,30,100,20,'Computer',fnt_Grey,kaLeft);
        TKMLabel.Create(Panel_PlayerTypes,12,50+i*25,100,20,inttostr(i+1),fnt_Outline,kaLeft);
        for k:=0 to 1 do
        begin
          CheckBox_PlayerTypes[i,k] := TKMCheckBox.Create(Panel_PlayerTypes, 52+k*70, 48+i*25, 20, 20, '', fnt_Metal);
          CheckBox_PlayerTypes[i,k].Tag       := i;
          CheckBox_PlayerTypes[i,k].FlatStyle := true;
          CheckBox_PlayerTypes[i,k].OnClick   := Mission_PlayerTypesChange;
        end;
      end;

      //It does not have OnClick event for a reason:
      // - we don't have a rule to make alliances symmetrical yet
      CheckBox_AlliancesSym := TKMCheckBox.Create(Panel_Alliances, 12, 50+MAX_PLAYERS*25, 20, 20, 'Symmetrical', fnt_Metal);
      CheckBox_AlliancesSym.Checked := true;
      CheckBox_AlliancesSym.Disable;
end;


{Menu page}
procedure TKMapEdInterface.Create_Menu_Page;
begin
  Panel_Menu:=TKMPanel.Create(Panel_Common,0,128,196,400);
    Button_Menu_Save:=TKMButton.Create(Panel_Menu,8,20,180,30,fTextLibrary.GetTextString(175),fnt_Metal);
    Button_Menu_Save.OnClick:=SwitchPage;
    Button_Menu_Save.Hint:=fTextLibrary.GetTextString(175);
    Button_Menu_Load:=TKMButton.Create(Panel_Menu,8,60,180,30,fTextLibrary.GetTextString(174),fnt_Metal);
    Button_Menu_Load.OnClick:=SwitchPage;
    Button_Menu_Load.Hint:=fTextLibrary.GetTextString(174);
    Button_Menu_Settings:=TKMButton.Create(Panel_Menu,8,100,180,30,fTextLibrary.GetTextString(179),fnt_Metal);
    Button_Menu_Settings.Hint:=fTextLibrary.GetTextString(179);
    Button_Menu_Settings.Disable;
    Button_Menu_Quit:=TKMButton.Create(Panel_Menu,8,180,180,30,fTextLibrary.GetTextString(180),fnt_Metal);
    Button_Menu_Quit.Hint:=fTextLibrary.GetTextString(180);
    Button_Menu_Quit.OnClick:=SwitchPage;
end;


{Save page}
procedure TKMapEdInterface.Create_MenuSave_Page;
begin
  Panel_Save := TKMPanel.Create(Panel_Common,0,128,196,400);
    TKMLabel.Create(Panel_Save,100,30,100,30,'Save map',fnt_Outline,kaCenter);
    Edit_SaveName       := TKMEdit.Create(Panel_Save,8,50,180,20, fnt_Grey);
    Label_SaveExists    := TKMLabel.Create(Panel_Save,100,80,100,30,'Map already exists',fnt_Outline,kaCenter);
    CheckBox_SaveExists := TKMCheckBox.Create(Panel_Save,12,100,100,20,'Overwrite', fnt_Metal);
    Button_SaveSave     := TKMButton.Create(Panel_Save,8,120,180,30,'Save',fnt_Metal);
    Button_SaveCancel   := TKMButton.Create(Panel_Save,8,160,180,30,'Cancel',fnt_Metal);
    Edit_SaveName.OnChange      := Menu_Save;
    CheckBox_SaveExists.OnClick := Menu_Save;
    Button_SaveSave.OnClick     := Menu_Save;
    Button_SaveCancel.OnClick   := SwitchPage;
end;


{Load page}
procedure TKMapEdInterface.Create_MenuLoad_Page;
begin
  Panel_Load := TKMPanel.Create(Panel_Common,0,128,196,400);
    TKMLabel.Create(Panel_Load, 16, 0, 100, 30, 'Available maps', fnt_Outline, kaLeft);
    FileList_Load := TKMFileList.Create(Panel_Load, 8, 20, 200, 200);
    Button_LoadLoad     := TKMButton.Create(Panel_Load,8,250,180,30,'Load',fnt_Metal);
    Button_LoadCancel   := TKMButton.Create(Panel_Load,8,290,180,30,'Cancel',fnt_Metal);
    Button_LoadLoad.OnClick     := Menu_Load;
    Button_LoadCancel.OnClick   := SwitchPage;
end;


{Quit page}
procedure TKMapEdInterface.Create_MenuQuit_Page;
begin
  Panel_Quit:=TKMPanel.Create(Panel_Common,0,128,200,400);
    TKMLabel.Create(Panel_Quit,100,40,100,30,'Any unsaved|changes will be lost',fnt_Outline,kaCenter);
    Button_Quit_Yes   := TKMButton.Create(Panel_Quit,8,100,180,30,'Quit',fnt_Metal);
    Button_Quit_No    := TKMButton.Create(Panel_Quit,8,140,180,30,fTextLibrary.GetTextString(178),fnt_Metal);
    Button_Quit_Yes.Hint      := fTextLibrary.GetTextString(177);
    Button_Quit_No.Hint       := fTextLibrary.GetTextString(178);
    Button_Quit_Yes.OnClick   := Menu_QuitMission;
    Button_Quit_No.OnClick    := SwitchPage;
end;


{Unit page}
procedure TKMapEdInterface.Create_Unit_Page;
begin
  Panel_Unit:=TKMPanel.Create(Panel_Common,0,112,200,400);
    Label_UnitName        := TKMLabel.Create(Panel_Unit,100,16,100,30,'',fnt_Outline,kaCenter);
    Image_UnitPic         := TKMImage.Create(Panel_Unit,8,38,54,100,521);
    Label_UnitCondition   := TKMLabel.Create(Panel_Unit,120,40,100,30,fTextLibrary.GetTextString(254),fnt_Grey,kaCenter);
    KMConditionBar_Unit   := TKMPercentBar.Create(Panel_Unit,73,55,116,15,80);
    Label_UnitDescription := TKMLabel.Create(Panel_Unit,8,152,236,200,'',fnt_Grey,kaLeft); //Taken from LIB resource

  Panel_Army:=TKMPanel.Create(Panel_Unit,0,160,200,400);
    Button_Army_RotCW   := TKMButton.Create(Panel_Army,  8, 0, 56, 40, 23);
    Button_Army_RotCCW  := TKMButton.Create(Panel_Army,132, 0, 56, 40, 24);
    Button_Army_ForUp   := TKMButton.Create(Panel_Army,  8, 46, 56, 40, 33);
    ImageStack_Army     := TKMImageStack.Create(Panel_Army, 70, 46, 56, 40, 43);
    Button_Army_ForDown := TKMButton.Create(Panel_Army,132, 46, 56, 40, 32);
    Button_Army_RotCW.OnClick   := Unit_ArmyChange1;
    Button_Army_RotCCW.OnClick  := Unit_ArmyChange1;
    Button_Army_ForUp.OnClick   := Unit_ArmyChange1;
    Button_Army_ForDown.OnClick := Unit_ArmyChange1;

    Button_ArmyDec      := TKMButton.Create(Panel_Army,  8,92,56,40,'-', fnt_Metal);
    Button_ArmyFood     := TKMButton.Create(Panel_Army, 70,92,56,40,29);
    Button_ArmyInc      := TKMButton.Create(Panel_Army,132,92,56,40,'+', fnt_Metal);
    Button_ArmyDec.OnClickEither := Unit_ArmyChange2;
    Button_ArmyFood.OnClick := Unit_ArmyChange1;
    Button_ArmyInc.OnClickEither := Unit_ArmyChange2;
end;


{House description page}
procedure TKMapEdInterface.Create_House_Page;
begin
  Panel_House:=TKMPanel.Create(Panel_Common,0,112,200,400);
    //Thats common things
    Label_House:=TKMLabel.Create(Panel_House,100,14,100,30,'',fnt_Outline,kaCenter);
    Image_House_Logo:=TKMImage.Create(Panel_House,8,41,32,32,338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker:=TKMImage.Create(Panel_House,38,41,32,32,141);
    Image_House_Worker.ImageCenter;
    Label_HouseHealth:=TKMLabel.Create(Panel_House,130,41,30,50,fTextLibrary.GetTextString(228),fnt_Mini,kaCenter,$FFFFFFFF);
    KMHealthBar_House:=TKMPercentBar.Create(Panel_House,100,53,60,20,50);
    Button_HouseHealthDec := TKMButton.Create(Panel_House,80,53,20,20,'-', fnt_Metal);
    Button_HouseHealthInc := TKMButton.Create(Panel_House,160,53,20,20,'+', fnt_Metal);
    Button_HouseHealthDec.OnClickEither := House_HealthChange;
    Button_HouseHealthInc.OnClickEither := House_HealthChange;
end;


{Store page}
procedure TKMapEdInterface.Create_Store_Page;
var i:integer;
begin
    Panel_HouseStore:=TKMPanel.Create(Panel_House,0,76,200,400);
    for i:=1 to 28 do begin
      Button_Store[i]:=TKMButtonFlat.Create(Panel_HouseStore, 8+((i-1)mod 5)*36,8+((i-1)div 5)*42,32,36,350+i);
      Button_Store[i].Tag:=i;
      Button_Store[i].Hint:=TypeToString(TResourceType(i));
      Button_Store[i].OnClick := Store_SelectWare;
    end;
    Button_StoreDec100   := TKMButton.Create(Panel_HouseStore,116,218,20,20,'<', fnt_Metal);
    Button_StoreDec      := TKMButton.Create(Panel_HouseStore,116,238,20,20,'-', fnt_Metal);
    Label_Store_WareCount:= TKMLabel.Create (Panel_HouseStore,156,230,100,30,'',fnt_Metal,kaCenter);
    Button_StoreInc100   := TKMButton.Create(Panel_HouseStore,176,218,20,20,'>', fnt_Metal);
    Button_StoreInc      := TKMButton.Create(Panel_HouseStore,176,238,20,20,'+', fnt_Metal);
    Button_StoreDec100.OnClickEither := Store_EditWareCount;
    Button_StoreDec.OnClickEither    := Store_EditWareCount;
    Button_StoreInc100.OnClickEither := Store_EditWareCount;
    Button_StoreInc.OnClickEither    := Store_EditWareCount;
end;


{Barracks page}
procedure TKMapEdInterface.Create_Barracks_Page;
var i:integer;
begin
    Panel_HouseBarracks:=TKMPanel.Create(Panel_House,0,76,200,400);
      for i:=1 to 11 do
      begin
        Button_Barracks[i]:=TKMButtonFlat.Create(Panel_HouseBarracks, 8+((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,366+i);
        Button_Barracks[i].Tag := i;
        Button_Barracks[i].TexOffsetX:=1;
        Button_Barracks[i].TexOffsetY:=1;
        Button_Barracks[i].CapOffsetY:=2;
        Button_Barracks[i].Hint:=TypeToString(TResourceType(16+i));
        Button_Barracks[i].OnClick := Barracks_SelectWare;
      end;
    Button_BarracksDec100   := TKMButton.Create(Panel_HouseBarracks,116,218,20,20,'<', fnt_Metal);
    Button_BarracksDec      := TKMButton.Create(Panel_HouseBarracks,116,238,20,20,'-', fnt_Metal);
    Label_Barracks_WareCount:= TKMLabel.Create (Panel_HouseBarracks,156,230,100,30,'',fnt_Metal,kaCenter);
    Button_BarracksInc100   := TKMButton.Create(Panel_HouseBarracks,176,218,20,20,'>', fnt_Metal);
    Button_BarracksInc      := TKMButton.Create(Panel_HouseBarracks,176,238,20,20,'+', fnt_Metal);
    Button_BarracksDec100.OnClickEither := Barracks_EditWareCount;
    Button_BarracksDec.OnClickEither    := Barracks_EditWareCount;
    Button_BarracksInc100.OnClickEither := Barracks_EditWareCount;
    Button_BarracksInc.OnClickEither    := Barracks_EditWareCount;
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMapEdInterface.UpdateState;
begin
  Minimap_Update(nil); //Even this Update could be excluded from MapEd interface
end;


procedure TKMapEdInterface.Player_UpdateColors;
var i:integer;
begin
  //Set player colors
  for i:=0 to MAX_PLAYERS-1 do
    Button_PlayerSelect[i].ShapeColor := fPlayers.Player[i].FlagColor;

  if MyPlayer <> nil then
    Button_PlayerSelect[MyPlayer.PlayerIndex].Down := true;
end;


procedure TKMapEdInterface.Player_ChangeActive(Sender: TObject);
var i:integer;
begin
  for i:=0 to MAX_PLAYERS-1 do
    Button_PlayerSelect[i].Down := false;

  if (TKMControl(Sender).Tag in [0..MAX_PLAYERS-1]) and (fPlayers.Player[TKMControl(Sender).Tag] <> nil) then begin
    MyPlayer := fPlayers.Player[TKMControl(Sender).Tag];
    Button_PlayerSelect[TKMControl(Sender).Tag].Down := true;
  end;

  if (fShownHouse <> nil) or (fShownUnit <> nil) then SwitchPage(nil);

  fShownHouse := nil; //Drop selection
  fShownUnit := nil;
  fPlayers.Selected := nil;
end;


procedure TKMapEdInterface.Terrain_HeightChange(Sender: TObject);
begin
  GameCursor.Tag1 := (HeightShape.Position AND $F) shl 4 +  HeightSize.Position AND $F;

  if Sender = HeightCircle then
  begin
    HeightCircle.Down := true;
    HeightSquare.Down := false;
    GameCursor.Mode  := cm_Height;
    GameCursor.Tag2 := MAPED_HEIGHT_CIRCLE;
  end else
  if Sender = HeightSquare then
  begin
    HeightSquare.Down := true;
    HeightCircle.Down := false;
    GameCursor.Mode  := cm_Height;
    GameCursor.Tag2 := MAPED_HEIGHT_SQUARE;
  end;
end;


procedure TKMapEdInterface.Terrain_TilesChange(Sender: TObject);

  function GetTileIDFromTag(aTag: byte):byte;
  var Tile:byte;
  begin
    Tile := 32*((aTag-1) div MAPED_TILES_COLS) + (aTag-1) mod MAPED_TILES_COLS + TilesScroll.Position;
    Result := MapEdTileRemap[EnsureRange(Tile+1,1,256)];
  end;

var i,k,TileID:integer;
begin
  if Sender = TilesRandom then
    GameCursor.Tag2 := 4 * byte(TilesRandom.Checked); //Defined=0..3 or Random=4

  if Sender = TilesScroll then //Shift tiles
    for i:=1 to MAPED_TILES_COLS do
    for k:=1 to MAPED_TILES_ROWS do
    begin
      if GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i) <> 0 then
      begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i); //icons are in 2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Enable;
      end
      else
      begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := 0;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Disable;
      end;
      if GameCursor.Mode = cm_Tiles then
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (GameCursor.Tag1+1 = GetTileIDFromTag((k-1)*MAPED_TILES_COLS+i));
    end;
  if Sender is TKMButtonFlat then
  begin
    TileID := GetTileIDFromTag(TKMButtonFlat(Sender).Tag);
    if TileID <> 0 then
    begin
      GameCursor.Mode := cm_Tiles;
      GameCursor.Tag1 := TileID-1; //MapEdTileRemap is 1 based, tag is 0 based
      if TilesRandom.Checked then
        GameCursor.Tag2 := 4;
      for i:=1 to MAPED_TILES_COLS do
      for k:=1 to MAPED_TILES_ROWS do
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (Sender = TilesTable[(i-1)*MAPED_TILES_ROWS+k]);
    end;
  end;
end;


procedure TKMapEdInterface.Terrain_ObjectsChange(Sender: TObject);
var i,ObjID:integer;
begin
  for i:=1 to 4 do
    ObjectsTable[i].Down := false;
  ObjectErase.Down := false;

  if Sender = ObjectsScroll then
  begin
    for i:=1 to 4 do
    begin
      ObjID := ObjectsScroll.Position*2 - 2 + i;
      if ActualMapElem[ObjID]<>0 then
      begin
        ObjectsTable[i].TexID := MapElem[ActualMapElem[ObjID]].Step[1] + 1;
        ObjectsTable[i].Caption := inttostr(ObjID);
        ObjectsTable[i].Enable;
      end
      else
      begin
        ObjectsTable[i].TexID := 0;
        ObjectsTable[i].Caption := '';
        ObjectsTable[i].Disable;
      end;
      ObjectsTable[i].Down := ObjID = OriginalMapElem[GameCursor.Tag1+1]; //Mark the selected one using reverse lookup
    end;
    ObjectErase.Down := (GameCursor.Tag1 = 255); //or delete button
  end;

  if Sender is TKMButtonFlat then
  begin
    ObjID := ObjectsScroll.Position*2-1 + (TKMButtonFlat(Sender).Tag-1); //1..n
    if (not InRange(ObjID,1,ActualMapElemQty)) and not (TKMButtonFlat(Sender).Tag = 255) then exit; //Don't let them click if it is out of range
    GameCursor.Mode := cm_Objects;
    if TKMButtonFlat(Sender).Tag = 255 then
      GameCursor.Tag1 := 255 //erase object
    else
      GameCursor.Tag1 := ActualMapElem[ObjID]-1; //0..n
    GameCursor.Tag2 := 0;
    for i:=1 to 4 do
      ObjectsTable[i].Down := (Sender = ObjectsTable[i]); //Mark the selected one
    ObjectErase.Down := (Sender = ObjectErase); //or delete button
  end;
end;


procedure TKMapEdInterface.Build_ButtonClick(Sender: TObject);
var i:integer;
begin
  //Release all buttons
  for i:=1 to Panel_Build.ChildCount do
    if Panel_Build.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(Panel_Build.Childs[i]).Down:=false;

  //Press the button
  TKMButtonFlat(Sender).Down := true;

  //Reset cursor and see if it needs to be changed
  GameCursor.Mode:=cm_None;
  GameCursor.Tag1:=0;
  GameCursor.Tag2:=0;

  if Button_BuildCancel.Down then begin
    GameCursor.Mode:=cm_Erase;
  end;
  if Button_BuildRoad.Down then begin
    GameCursor.Mode:=cm_Road;
  end;
  if Button_BuildField.Down then begin
    GameCursor.Mode:=cm_Field;
  end;
  if Button_BuildWine.Down then begin
    GameCursor.Mode:=cm_Wine;
  end;
{  if Button_BuildWall.Down then begin
    GameCursor.Mode:=cm_Wall;
  end;}

  for i:=1 to GUI_HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if Button_Build[i].Down then begin
     GameCursor.Mode := cm_Houses;
     GameCursor.Tag1 := byte(GUIHouseOrder[i]);
  end;
end;


procedure TKMapEdInterface.Unit_ButtonClick(Sender: TObject);
var i:integer;
begin
  if Sender=nil then begin GameCursor.Mode:=cm_None; exit; end;

  //Release all buttons
  for i:=1 to Panel_Units.ChildCount do
    if Panel_Units.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(Panel_Units.Childs[i]).Down := false;

  //Press the button
  TKMButtonFlat(Sender).Down:=true;

  //Reset cursor and see if it needs to be changed
  GameCursor.Mode:=cm_None;
  GameCursor.Tag1:=0;
  GameCursor.Tag2:=0;
  //Label_Build.Caption := '';

  if Button_UnitCancel.Down then begin
    GameCursor.Mode:=cm_Erase;
    //Label_Build.Caption := fTextLibrary.GetTextString(210);
  end;

  if (TKMButtonFlat(Sender).Tag in [byte(ut_Serf)..byte(ut_Duck)]) then
  begin
    GameCursor.Mode := cm_Units;
    GameCursor.Tag1 := byte(TKMButtonFlat(Sender).Tag);
  end;

end;


procedure TKMapEdInterface.View_Passability(Sender:TObject);
begin
  SHOW_TERRAIN_WIRES := TKMRatioRow(Sender).Position <> 0;
  fShowPassability := TKMRatioRow(Sender).Position;
  if TKMRatioRow(Sender).Position <> 0 then
    Label_Passability.Caption := PassabilityStr[TPassability(TKMRatioRow(Sender).Position)]
  else
    Label_Passability.Caption := 'Off';
end;


procedure TKMapEdInterface.ShowHouseInfo(Sender:TKMHouse);
begin
  fShownUnit  := nil;
  fShownHouse := Sender;

  if not Assigned(Sender) then begin //=nil produces wrong result when there's no object at all?
    SwitchPage(nil);
    exit;
  end;

  {Common data}
  Label_House.Caption:=fResource.HouseDat[Sender.GetHouseType].HouseName;
  Image_House_Logo.TexID:=fResource.HouseDat[Sender.GetHouseType].GUIIcon;
  Image_House_Worker.TexID:=140+byte(fResource.HouseDat[Sender.GetHouseType].OwnerType);
  Image_House_Worker.Hint := TypeToString(fResource.HouseDat[Sender.GetHouseType].OwnerType);
  KMHealthBar_House.Caption:=inttostr(round(Sender.GetHealth))+'/'+inttostr(fResource.HouseDat[Sender.GetHouseType].MaxHealth);
  KMHealthBar_House.Position:=round( Sender.GetHealth / fResource.HouseDat[Sender.GetHouseType].MaxHealth * 100 );

  Image_House_Worker.Visible := fResource.HouseDat[Sender.GetHouseType].OwnerType <> ut_None;


  case Sender.GetHouseType of
    ht_Store: begin
          Store_Fill(nil);
          SwitchPage(Panel_HouseStore);
          Store_SelectWare(Button_Store[StorehouseItem]); //Reselect the ware so the display is updated
        end;

    ht_Barracks: begin
          Barracks_Fill(nil);
          Image_House_Worker.Enable; //In the barrack the recruit icon is always enabled
          SwitchPage(Panel_HouseBarracks);
          Barracks_SelectWare(Button_Barracks[BarracksItem]); //Reselect the ware so the display is updated
          end;
    ht_TownHall:;
    else SwitchPage(Panel_House);
  end;
end;


procedure TKMapEdInterface.ShowUnitInfo(Sender:TKMUnit);
var Commander:TKMUnitWarrior;
begin
  fShownUnit:=Sender;
  fShownHouse:=nil;
  if (not Assigned(Sender))or(not Sender.Visible)or((Sender<>nil)and(Sender.IsDead)) then begin
    SwitchPage(nil);
    fShownUnit:=nil; //Make sure it doesn't come back again, especially if it's dead!
    exit;
  end;
  SwitchPage(Panel_Unit);
  Label_UnitName.Caption:=TypeToString(Sender.UnitType);
  Image_UnitPic.TexID:=520+byte(Sender.UnitType);
  KMConditionBar_Unit.Position:=EnsureRange(round(Sender.Condition / UNIT_MAX_CONDITION * 100),-10,110);
  if Sender is TKMUnitWarrior then
  begin
    //Warrior specific
    Label_UnitDescription.Hide;
    Commander := TKMUnitWarrior(Sender).GetCommander;
    if Commander<>nil then
      ImageStack_Army.SetCount(Commander.fMapEdMembersCount + 1,Commander.UnitsPerRow); //Count+commander, Columns
    Panel_Army.Show;
  end
  else
  begin
    //Citizen specific
    Label_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender.UnitType));
    Label_UnitDescription.Show;
  end;
end;


procedure TKMapEdInterface.Menu_Save(Sender:TObject);
begin
  if Sender = Edit_SaveName then begin
    CheckBox_SaveExists.Enabled := CheckFileExists(KMMapNameToPath(Edit_SaveName.Text,'dat'), true);
    Label_SaveExists.Visible := CheckBox_SaveExists.Enabled;
    CheckBox_SaveExists.Checked := false;
    Button_SaveSave.Enabled := not CheckBox_SaveExists.Enabled;
  end;

  if Sender = CheckBox_SaveExists then
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;

  if Sender = Button_SaveSave then begin
    //Should we expand the path here?
    fGame.SaveMapEditor(Edit_SaveName.Text, true);
    SwitchPage(Button_SaveCancel); //return to previous menu
  end;
end;


{Show mission loading dialogue}
procedure TKMapEdInterface.Menu_Load(Sender:TObject);
begin
  fGame.StartMapEditor(FileList_Load.FileName, 0, 0);
end;


{Quit the mission and return to main menu}
procedure TKMapEdInterface.Menu_QuitMission(Sender:TObject);
begin
  fGame.Stop(gr_MapEdEnd);
end;


//This function will be called if the user right clicks on the screen.
procedure TKMapEdInterface.RightClick_Cancel;
begin
  //We should drop the tool but don't close opened tab. This allows eg: Place a warrior, right click so you are not placing more warriors, select the placed warrior.
  //Before you would have had to close the tab to do this.
  if GetShownPage = esp_Terrain then exit; //Terrain uses both buttons for relief changing, tile rotation etc.
  GameCursor.Mode:=cm_None;
  GameCursor.Tag1:=0;
  GameCursor.Tag2:=0;
end;


procedure TKMapEdInterface.SetTileDirection(aTileDirection: byte);
begin
  TileDirection := aTileDirection mod 4; //0..3
  GameCursor.Tag2 := TileDirection;
end;


function TKMapEdInterface.GetSelectedTile: TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to MAPED_TILES_COLS*MAPED_TILES_ROWS do
    if TilesTable[i].Down then Result := TilesTable[i];
end;


function TKMapEdInterface.GetSelectedObject: TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to 4 do
    if ObjectsTable[i].Down then Result := ObjectsTable[i];
end;


function TKMapEdInterface.GetSelectedUnit: TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to 14 do
    if Button_Citizen[i].Down then Result := Button_Citizen[i];
  for i:=1 to 10 do
    if Button_Warriors[i].Down then Result := Button_Warriors[i];
  for i:=1 to 8 do
    if Button_Animals[i].Down then Result := Button_Animals[i];
end;


procedure TKMapEdInterface.Store_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;
  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(fPlayers.Selected).CheckResIn(TResourceType(i));
    if Tmp=0 then Button_Store[i].Caption:='-' else
    //if Tmp>999 then Button_Store[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  Button_Store[i].Caption:=inttostr(Tmp);
  end;
end;


procedure TKMapEdInterface.Barracks_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  if not (fPlayers.Selected is TKMHouseBarracks) then exit;
  for i:=1 to 11 do begin
    Tmp:=TKMHouseBarracks(fPlayers.Selected).CheckResIn(TResourceType(i+16));
    if Tmp=0 then Button_Barracks[i].Caption:='-' else
    //if Tmp>999 then Button_Barracks[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  Button_Barracks[i].Caption:=inttostr(Tmp);
  end;
end;


procedure TKMapEdInterface.House_HealthChange(Sender:TObject; AButton:TMouseButton);
var Amt:byte;
begin
  if fShownHouse = nil then exit;
  Amt := 0;
  if AButton = mbLeft then Amt:=1;
  if AButton = mbRight then Amt:=50;
  if Sender = Button_HouseHealthDec then fShownHouse.AddDamage(Amt, true);
  if Sender = Button_HouseHealthInc then fShownHouse.AddRepair(Amt);
  if fShownHouse.IsDestroyed then
    ShowHouseInfo(nil)
  else
    ShowHouseInfo(fShownHouse);
end;


procedure TKMapEdInterface.Unit_ArmyChange1(Sender:TObject);
var Commander:TKMUnitWarrior;
begin
  if fShownUnit = nil then exit;
  if not (fShownUnit is TKMUnitWarrior) then exit;

  Commander := TKMUnitWarrior(fShownUnit).GetCommander;
  if Sender = Button_Army_ForUp then Commander.UnitsPerRow := max(Commander.UnitsPerRow-1,1);
  if Sender = Button_Army_ForDown then Commander.UnitsPerRow := min(Commander.UnitsPerRow+1,Commander.fMapEdMembersCount+1);
  ImageStack_Army.SetCount(Commander.fMapEdMembersCount + 1,Commander.UnitsPerRow);

  if Sender = Button_Army_RotCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)-1);
  if Sender = Button_Army_RotCCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)+1);
  Commander.AnimStep := UnitStillFrames[Commander.Direction];

  //Toggle between full and half condition
  if Sender = Button_ArmyFood then
  begin
    if Commander.Condition = UNIT_MAX_CONDITION then
      Commander.Condition := UNIT_MAX_CONDITION div 2
    else
      Commander.Condition := UNIT_MAX_CONDITION;
    KMConditionBar_Unit.Position := EnsureRange(round(Commander.Condition / UNIT_MAX_CONDITION * 100),-10,110);
  end;
end;


procedure TKMapEdInterface.Unit_ArmyChange2(Sender:TObject; AButton:TMouseButton);
var Amt:shortint; Commander:TKMUnitWarrior;
begin
  if fShownUnit = nil then exit;
  if not (fShownUnit is TKMUnitWarrior) then exit;

  Amt := 0;
  if Sender = Button_ArmyDec then Amt := -1; //Decrease
  if Sender = Button_ArmyInc then Amt := 1; //Increase
  if AButton = mbLeft then Amt  := Amt * 1;
  if AButton = mbRight then Amt := Amt * 10;

  Commander := TKMUnitWarrior(fShownUnit).GetCommander;
  Commander.fMapEdMembersCount := EnsureRange(Commander.fMapEdMembersCount + Amt, 0, 200); //max members
  Commander.UnitsPerRow := min(Commander.UnitsPerRow,Commander.fMapEdMembersCount+1); //Ensure units per row is <= unit count
  ImageStack_Army.SetCount(Commander.fMapEdMembersCount + 1,Commander.UnitsPerRow);
end;


procedure TKMapEdInterface.Barracks_SelectWare(Sender:TObject);
var i:integer;
begin
  if not Panel_HouseBarracks.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag
  for i:=1 to length(Button_Barracks) do
    Button_Barracks[i].Down := false;
  TKMButtonFlat(Sender).Down := true;
  BarracksItem := TKMButtonFlat(Sender).Tag;
  Barracks_EditWareCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.Store_SelectWare(Sender:TObject);
var i:integer;
begin
  if not Panel_HouseStore.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag
  for i:=1 to length(Button_Store) do
    Button_Store[i].Down := false;
  TKMButtonFlat(Sender).Down := true;
  StorehouseItem := TKMButtonFlat(Sender).Tag;
  Store_EditWareCount(Sender, mbLeft);
end;


procedure TKMapEdInterface.Barracks_EditWareCount(Sender:TObject; AButton:TMouseButton);
var Res:TResourceType; Barracks:TKMHouseBarracks; Amt:byte;
begin
  if not Panel_HouseBarracks.Visible then exit;

  Res := TResourceType(BarracksItem+16);
  Barracks := TKMHouseBarracks(fShownHouse);

  Amt := 0;
  if AButton = mbLeft then Amt := 1;
  if AButton = mbRight then Amt := 10;

  if Sender = Button_BarracksDec100 then Barracks.ResTakeFromOut(Res, Amt*100);
  if Sender = Button_BarracksDec    then Barracks.ResTakeFromOut(Res, Amt*1);
  if Sender = Button_BarracksInc    then Barracks.ResAddToIn(Res, Amt*1);
  if Sender = Button_BarracksInc100 then Barracks.ResAddToIn(Res, Amt*100);

  Label_Barracks_WareCount.Caption := inttostr(Barracks.CheckResIn(Res));
  Barracks_Fill(nil);
end;


procedure TKMapEdInterface.Store_EditWareCount(Sender:TObject; AButton:TMouseButton);
var Res:TResourceType; Store:TKMHouseStore; Amt:byte;
begin
  if not Panel_HouseStore.Visible then exit;

  Res := TResourceType(StorehouseItem);
  Store := TKMHouseStore(fShownHouse);

  Amt := 0;
  if AButton = mbLeft then Amt := 1;
  if AButton = mbRight then Amt := 10;

  if Sender = Button_StoreDec100 then Store.ResTakeFromOut(Res, Amt*100);
  if Sender = Button_StoreDec    then Store.ResTakeFromOut(Res, Amt*1);
  if Sender = Button_StoreInc    then Store.ResAddToIn(Res, Amt*1);
  if Sender = Button_StoreInc100 then Store.ResAddToIn(Res, Amt*100);

  Label_Store_WareCount.Caption := inttostr(Store.CheckResIn(Res));
  Store_Fill(nil);
end;


procedure TKMapEdInterface.Player_ColorClick(Sender:TObject);
begin
  if not (Sender = ColorSwatch_Color) then exit;
  MyPlayer.FlagColor := ColorSwatch_Color.GetColor;
  Player_UpdateColors;
end;


procedure TKMapEdInterface.Mission_AlliancesChange(Sender:TObject);
var i,k:integer;
begin
  if Sender = nil then begin
    for i:=0 to fPlayers.Count-1 do
    for k:=0 to fPlayers.Count-1 do
      if (fPlayers.Player[i]<>nil)and(fPlayers.Player[k]<>nil) then
        CheckBox_Alliances[i,k].Checked := (fPlayers.CheckAlliance(fPlayers.Player[i].PlayerIndex, fPlayers.Player[k].PlayerIndex)=at_Ally)
      else
        CheckBox_Alliances[i,k].Disable; //Player does not exist?
    exit;
  end;

  i := TKMCheckBox(Sender).Tag div fPlayers.Count;
  k := TKMCheckBox(Sender).Tag mod fPlayers.Count;
  if CheckBox_Alliances[i,k].Checked then fPlayers.Player[i].Alliances[k] := at_Ally
                                     else fPlayers.Player[i].Alliances[k] := at_Enemy;

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then begin
    CheckBox_Alliances[k,i].Checked := CheckBox_Alliances[i,k].Checked;
    fPlayers.Player[k].Alliances[i] := fPlayers.Player[i].Alliances[k];
  end;
end;


procedure TKMapEdInterface.Mission_PlayerTypesChange(Sender:TObject);
var i:integer;
begin
  if Sender = nil then begin
    for i:=0 to fPlayers.Count-1 do
    begin
      CheckBox_PlayerTypes[i,0].Enabled := fPlayers[i]<>nil;
      CheckBox_PlayerTypes[i,1].Enabled := fPlayers[i]<>nil;
      CheckBox_PlayerTypes[i,0].Checked := (fPlayers[i]<>nil) and (fPlayers[i].PlayerType = pt_Human);
      CheckBox_PlayerTypes[i,1].Checked := (fPlayers[i]<>nil) and (fPlayers[i].PlayerType = pt_Computer);
    end;
    Exit;
  end;

  //@Lewin: Are we allowed to define players freely, e.g. make 5 Human players?
  //How is it working in multiplayer?
  //@Krom: In KaM it works like this: Single player missions have 1 human player and the others computer.
  //       For multiplayer the players are all humans. (although they are not defined in the script with !SET_HUMAN_PLAYER as that is for single missions)
  //       I think we should be a bit more relaxed about it. Here are some cases:
  //       1. Campaign/single missions: Only 1 player is human, the others are computer and this cannot be changed. (usually story based)
  //       2. Tournament missions: (like single missions from TPR but configurable for every game) All players start equal and any can be
  //                               human/AI/not-participating. This means you can chose the number of enemies you wish to fight, and configure alliances.
  //                               e.g. I can chose to fight with me plus 1 computer (team 1) against 3 computers allied together. (team 2)
  //       3. Multiplayer tournament: Same as a single tournament mission, but you can have many humans and many AI. (with configurable teams or deathmatch)
  //       4. Multiplayer cooperative: Similar to a tournament but one or more AI are fixed and cannot be made human. (and the mission is usually story based)
  //                                   This allows a mission to be made where many humans siege AI players in a castle, where the AI have an obvious
  //                                   advantage and the humans must work together to defeat them.
  //       These are just some ideas for the kinds of missions I think we should allow. Note that TPR only allows for types 1 and 3.
  //       I do NOT think that each mission should be given a "type" of the ones mentioned above. That just makes things complicated having 4 mission types.
  //       We do not even need a single/multiplayer distinction. I think we can make this work by having two kinds of players:
  //       a) General: which can be controlled by a human or a computer
  //       b) Fixed AI: which can only be computers, never human controlled.
  //       Therefore both single and multiplayer tournament missions will only use General players, but the other two types will use
  //       some Fixed AI for the players which must always be computer controlled, and some General players which can be either or not-participating.
  //       When you make a mission you can define AI options for the General players if you wish, otherwise they will use defaults. (and figure it out
  //       automatically) Fixed AI will mostly need to be told how to behave, for the story make sense and fit with the circumstances.
  //       (e.g. so they don't try to build a city when it is a siege mission) This allows single player missions to be used for multiplayer and vice versa.
  //       These are just ideas and I think they could be redesigned in a less confusing way for both the players and mission creators.
  //       Let me know what you think. Maybe we should discuss this.
  //@Lewin: Looks like we can't achieve it without changing(adding) mission scripts.. discussed in ICQ.
  //Reset everything
  for i:=0 to fPlayers.Count-1 do
  begin
    CheckBox_PlayerTypes[i,0].Checked := false;
    CheckBox_PlayerTypes[i,1].Checked := true;
    fPlayers[i].PlayerType := pt_Computer;
  end;

  //Define only 1 human player
  i := TKMCheckBox(Sender).Tag;
  if Sender = CheckBox_PlayerTypes[i,0] then
  begin
    CheckBox_PlayerTypes[i,0].Checked := true;
    CheckBox_PlayerTypes[i,1].Checked := false;
    fPlayers[i].PlayerType := pt_Human
  end;
end;


procedure TKMapEdInterface.KeyDown(Key:Word; Shift: TShiftState);
begin
  if MyControls.KeyDown(Key, Shift) then exit; //Handled by Controls

  //1-5 game menu shortcuts
  if Key in [49..53] then
    Button_Main[Key-48].DoPress;

  //Scrolling
  if Key = VK_LEFT  then fViewport.ScrollKeyLeft  := true;
  if Key = VK_RIGHT then fViewport.ScrollKeyRight := true;
  if Key = VK_UP    then fViewport.ScrollKeyUp    := true;
  if Key = VK_DOWN  then fViewport.ScrollKeyDown  := true;
end;


procedure TKMapEdInterface.KeyPress(Key: Char);
begin
  MyControls.KeyPress(Key);
end;


procedure TKMapEdInterface.KeyUp(Key:Word; Shift: TShiftState);
begin
  if MyControls.KeyUp(Key, Shift) then
    Exit; //Handled by Controls

  //1-5 game menu shortcuts
  if Key in [49..53] then
    Button_Main[Key-48].DoClick;

  //Scrolling
  if Key = VK_LEFT  then fViewport.ScrollKeyLeft  := false;
  if Key = VK_RIGHT then fViewport.ScrollKeyRight := false;
  if Key = VK_UP    then fViewport.ScrollKeyUp    := false;
  if Key = VK_DOWN  then fViewport.ScrollKeyDown  := false;

  //Backspace resets the zoom and view, similar to other RTS games like Dawn of War.
  //This is useful because it is hard to find default zoom using the scroll wheel, and if not zoomed 100% things can be scaled oddly (like shadows)
  if Key = VK_BACK  then fViewport.ResetZoom;
end;


procedure TKMapEdInterface.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  MyControls.MouseDown(X,Y,Shift,Button);
  if MyControls.CtrlOver<>nil then
    Screen.Cursor := c_Default
  else
    fTerrain.ComputeCursorPosition(X,Y,Shift); //So terrain brushes start on mouse down not mouse move
end;


procedure TKMapEdInterface.MouseMove(Shift: TShiftState; X,Y: Integer);
var P:TKMPoint;
begin
  MyControls.MouseMove(X,Y,Shift);
  if MyControls.CtrlOver<>nil then begin
    Screen.Cursor:=c_Default;
    GameCursor.SState := []; //Don't do real-time elevate when the mouse is over controls, only terrain
    exit;
  end;

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
      cm_Road:      if fTerrain.CanPlaceRoad(P, mu_RoadPlan, MyPlayer)  then MyPlayer.AddRoad(P);
      cm_Field:     if fTerrain.CanPlaceRoad(P, mu_FieldPlan, MyPlayer) then MyPlayer.AddField(P,ft_Corn);
      cm_Wine:      if fTerrain.CanPlaceRoad(P, mu_WinePlan, MyPlayer)  then MyPlayer.AddField(P,ft_Wine);
      //cm_Wall:  if fTerrain.CanPlaceRoad(P, mu_WinePlan) then MyPlayer.AddField(P,ft_Wine);
      cm_Objects:   if GameCursor.Tag1 = 255 then fTerrain.SetTree(P, 255); //Allow many objects to be deleted at once
      cm_Erase:     case GetShownPage of
                      esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                      esp_Units:      fPlayers.RemAnyUnit(P);
                      esp_Buildings:  begin
                                        fPlayers.RemAnyHouse(P,true,false,true);
                                        if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                          fTerrain.RemRoad(P);
                                        if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                          fTerrain.RemField(P);
                                      end;
                    end;
    end;
  end;
end;


procedure TKMapEdInterface.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
var P:TKMPoint;
begin
  if MyControls.CtrlOver <> nil then begin
    MyControls.MouseUp(X,Y,Shift,Button);
    exit; //We could have caused fGame reinit, so exit at once
  end;

  P := GameCursor.Cell; //Get cursor position tile-wise
  if Button = mbRight then
  begin
    RightClick_Cancel;

    //Right click performs some special functions and shortcuts
    case GameCursor.Mode of
      cm_Tiles:   begin
                    SetTileDirection(GameCursor.Tag2+1); //Rotate tile direction
                    TilesRandom.Checked := false; //Reset
                  end;
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
                    ShowHouseInfo(TKMHouse(fPlayers.Selected));
                  if fPlayers.Selected is TKMUnit then
                    ShowUnitInfo(TKMUnit(fPlayers.Selected));
                end;
      cm_Road:  if fTerrain.CanPlaceRoad(P, mu_RoadPlan, MyPlayer) then MyPlayer.AddRoad(P);
      cm_Field: if fTerrain.CanPlaceRoad(P, mu_FieldPlan, MyPlayer) then MyPlayer.AddField(P,ft_Corn);
      cm_Wine:  if fTerrain.CanPlaceRoad(P, mu_WinePlan, MyPlayer) then MyPlayer.AddField(P,ft_Wine);
      //cm_Wall:
      cm_Houses:if fTerrain.CanPlaceHouse(P, THouseType(GameCursor.Tag1), MyPlayer) then
                begin
                  MyPlayer.AddHouse(THouseType(GameCursor.Tag1), P.X, P.Y, true);
                  Build_ButtonClick(Button_BuildRoad);
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
                case GetShownPage of
                  esp_Terrain:    fTerrain.Land[P.Y,P.X].Obj := 255;
                  esp_Units:      begin
                                    fPlayers.RemAnyUnit(P);
                                  end;
                  esp_Buildings:  begin
                                    fPlayers.RemAnyHouse(P,true,false,true);
                                    if fTerrain.Land[P.Y,P.X].TileOverlay = to_Road then
                                      fTerrain.RemRoad(P);
                                    if fTerrain.TileIsCornField(P) or fTerrain.TileIsWineField(P) then
                                      fTerrain.RemField(P);
                                  end;
                end;
    end;
end;


procedure TKMapEdInterface.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer);
var PrevCursor, ViewCenter: TKMPointF;
begin
  MyControls.MouseWheel(X, Y, WheelDelta);
  if (X < 0) or (Y < 0) then exit; //This occours when you use the mouse wheel on the window frame
  if MOUSEWHEEL_ZOOM_ENABLE and (MyControls.CtrlOver = nil) then
  begin
    fTerrain.ComputeCursorPosition(X, Y, Shift); //Make sure we have the correct cursor position to begin with
    PrevCursor := GameCursor.Float;
    fViewport.Zoom := fViewport.Zoom + WheelDelta/2000;
    fTerrain.ComputeCursorPosition(X, Y, Shift); //Zooming changes the cursor position
    //Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
    ViewCenter := fViewport.GetCenter; //Required for Linux compatibility
    fViewport.SetCenter(ViewCenter.X + PrevCursor.X-GameCursor.Float.X,
                        ViewCenter.Y + PrevCursor.Y-GameCursor.Float.Y);
    fTerrain.ComputeCursorPosition(X, Y, Shift); //Recentering the map changes the cursor position
  end;
end;


function TKMapEdInterface.GetShownPage:TKMMapEdShownPage;
begin
  Result := esp_Unknown;
  if Panel_Terrain.Visible then
    Result := esp_Terrain;
  if Panel_Build.Visible then
    Result := esp_Buildings;
  if Panel_Units.Visible then
    Result := esp_Units;
end;


procedure TKMapEdInterface.Paint;
begin
  MyControls.Paint;
end;


end.
