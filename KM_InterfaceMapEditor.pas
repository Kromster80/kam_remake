unit KM_InterfaceMapEditor;
{$I KaM_Remake.inc}
interface
uses Classes, Controls, KromUtils, Math, SysUtils, KromOGLUtils,
     KM_Controls, KM_Defaults, KM_Houses, KM_Units;

type TKMapEdInterface = class
  private
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    PrevHint:TObject;
    StorehouseItem:byte; //Selected ware in storehouse
    BarracksItem:byte; //Selected ware in barracks
    TileDirection: byte;
  protected
    Panel_Main:TKMPanel;
      Image_Main1,Image_Main2,Image_Main3,Image_Main4,Image_Main5:TKMImage; //Toolbar background
      Button_PlayerSelect:array[1..MAX_PLAYERS]of TKMFlatButtonShape; //Animals are common for all
      KMMinimap:TKMMinimap;
      Label_Stat,Label_Hint:TKMLabel;
      Button_Main:array[1..5]of TKMButton; //5 buttons
      Label_MenuTitle: TKMLabel; //Displays the title of the current menu below
      Label_MissionName: TKMLabel;

    Panel_Terrain:TKMPanel;
      Button_Terrain:array[1..4]of TKMButton;
      Panel_Brushes:TKMPanel;
        BrushSize:TKMRatioRow;
        BrushCircle,BrushSquare:TKMButtonFlat;
      Panel_Heights:TKMPanel;
        HeightSize:TKMRatioRow;
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
        Label_Build:TKMLabel;
        Button_BuildRoad,Button_BuildField,Button_BuildWine,Button_BuildWall,Button_BuildCancel:TKMButtonFlat;
        Button_Build:array[1..HOUSE_COUNT]of TKMButtonFlat;
      Panel_Units:TKMPanel;
        Label_Units:TKMLabel;
        Button_UnitCancel:TKMButtonFlat;
        Button_Citizen:array[1..14]of TKMButtonFlat;
        Button_Warriors:array[1..10]of TKMButtonFlat;
        Button_Animals:array[1..8]of TKMButtonFlat;
      Panel_Script:TKMPanel;

    Panel_Player:TKMPanel;
      Button_Player:array[1..2]of TKMButton;
      Panel_Goals:TKMPanel;
        Label_Goals:TKMLabel;
      Panel_Color:TKMPanel;
        Label_Color:TKMLabel;
        ColorSwatch_Color:TKMColorSwatch;

    Panel_Mission:TKMPanel;
      Button_Mission:array[1..1]of TKMButton;
      Panel_Alliances:TKMPanel;
        Label_Alliances:TKMLabel;
        CheckBox_Alliances: array[1..MAX_PLAYERS,1..MAX_PLAYERS] of TKMCheckBox;
        CheckBox_AlliancesSym:TKMCheckBox;

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
        Button_ArmyDec,Button_ArmyInc:TKMButton;

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
  private
    procedure Create_Terrain_Page;
    procedure Create_Village_Page;
    procedure Create_Player_Page;
    procedure Create_Mission_Page;
    procedure Create_Menu_Page;
    procedure Create_Save_Page;
    procedure Create_Load_Page;
    procedure Create_Quit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_Barracks_Page;

    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject);
    procedure Minimap_Update(Sender: TObject);
    procedure Player_ChangeActive(Sender: TObject);
    procedure Terrain_HeightChange(Sender: TObject);
    procedure Terrain_TilesChange(Sender: TObject);
    procedure Terrain_ObjectsChange(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Unit_ButtonClick(Sender: TObject);
    procedure Barracks_Fill(Sender:TObject);
    procedure Store_Fill(Sender:TObject);
    procedure House_HealthChange(Sender:TObject; AButton:TMouseButton);
    procedure Unit_ArmyChange1(Sender:TObject); overload;
    procedure Unit_ArmyChange2(Sender:TObject; AButton:TMouseButton); overload;
    procedure Barracks_SelectWare(Sender:TObject);
    procedure Barracks_EditWareCount(Sender:TObject; AButton:TMouseButton);
    procedure Store_SelectWare(Sender:TObject);
    procedure Store_EditWareCount(Sender:TObject; AButton:TMouseButton);
    procedure Player_ColorClick(Sender:TObject);
    procedure Mission_AlliancesChange(Sender:TObject);
  public
    MyControls: TKMControlsCollection;
    constructor Create;
    destructor Destroy; override;
    procedure Player_UpdateColors;
    procedure SetScreenSize(X,Y:word);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure Menu_Save(Sender:TObject);
    procedure Menu_Load(Sender:TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Build_SelectRoad;
    procedure RightClick_Cancel;
    function GetTilesRandomized: boolean;
    procedure SetTileDirection(aTileDirection: byte);
    function GetSelectedTile(): TObject;
    function GetSelectedObject(): TObject;
    function GetSelectedUnit(): TObject;
    procedure OnKeyUp(Key:Word; IsDown:boolean=false);
    property GetShownUnit: TKMUnit read ShownUnit;
    function GetShownPage:TKMMapEdShownPage;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Units_Warrior, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_Terrain, KM_Utils, KM_Viewport, KM_Game, KM_CommonTypes;


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
    ShownHouse:=nil;
    ShownUnit:=nil;
    fPlayers.Selected:=nil;
  end;

  Label_MenuTitle.Caption := '';
  //Now hide all existing pages
    for i:=1 to Panel_Main.ChildCount do
      if Panel_Main.Childs[i] is TKMPanel then
      begin
        for k:=1 to TKMPanel(Panel_Main.Childs[i]).ChildCount do
          if TKMPanel(Panel_Main.Childs[i]).Childs[k] is TKMPanel then
            TKMPanel(Panel_Main.Childs[i]).Childs[k].Hide;
        Panel_Main.Childs[i].Hide;
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
    Terrain_TilesChange(GetSelectedTile());
    SetTileDirection(TileDirection);
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[4]) then begin
    Panel_Terrain.Show;
    Panel_Objects.Show;
    Label_MenuTitle.Caption:='Terrain - Objects';
    Terrain_ObjectsChange(GetSelectedObject());
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[1]) then begin
    Panel_Village.Show;
    Panel_Build.Show;
    Label_MenuTitle.Caption:='Village - Buildings';
    Build_SelectRoad;
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
    FileList_Load.RefreshList(ExeDir+'Maps\', 'dat', true);
    if FileList_Load.fFiles.Count > 0 then
      FileList_Load.ItemIndex := 0; //Select first map by default
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


constructor TKMapEdInterface.Create();
var i:integer;
begin
  Inherited;
  fLog.AssertToLog(fViewport<>nil,'fViewport required to be init first');

  MyControls := TKMControlsCollection.Create;

  ShownUnit  := nil;
  ShownHouse := nil;
  BarracksItem   := 1; //First ware selected by default
  StorehouseItem := 1; //First ware selected by default
  TileDirection := 0;

{Parent Page for whole toolbar in-game}
  Panel_Main := MyControls.AddPanel(nil,0,0,224,768);

    Image_Main1 := MyControls.AddImage(Panel_Main,0,0,224,200,407); //Minimap place

    //todo: player selection and other "universal" stuff (i.e. which player are we placing for)
    //
    Image_Main3 := MyControls.AddImage(Panel_Main,0, 200,224,400,404);
    Image_Main4 := MyControls.AddImage(Panel_Main,0, 600,224,400,404);
    Image_Main5 := MyControls.AddImage(Panel_Main,0,1000,224,400,404); //For 1600x1200 this is needed

    MyControls.AddLabel(Panel_Main,8,200,100,30,'Player',fnt_Metal,kaLeft);
    for i:=1 to MAX_PLAYERS do begin
      Button_PlayerSelect[i]         := MyControls.AddFlatButtonShape(Panel_Main, 8 + (i-1)*23, 220, 21, 32, inttostr(i), fnt_Grey, $FF0000FF);
      Button_PlayerSelect[i].CapOffsetY := -3;
      Button_PlayerSelect[i].Tag     := i;
      Button_PlayerSelect[i].OnClick := Player_ChangeActive;
    end;

    KMMinimap:=MyControls.AddMinimap(Panel_Main,10,10,176,176);
    KMMinimap.OnChange:=Minimap_Update;

    {5 big tabs}
    Button_Main[1] := MyControls.AddButton(Panel_Main,   8, 372, 36, 36, 381);
    Button_Main[2] := MyControls.AddButton(Panel_Main,  48, 372, 36, 36, 368);
    Button_Main[3] := MyControls.AddButton(Panel_Main,  88, 372, 36, 36,  41);
    Button_Main[4] := MyControls.AddButton(Panel_Main, 128, 372, 36, 36, 441);
    Button_Main[5] := MyControls.AddButton(Panel_Main, 168, 372, 36, 36, 389);
    Button_Main[1].Hint := fTextLibrary.GetRemakeString(54);
    Button_Main[2].Hint := fTextLibrary.GetRemakeString(55);
    Button_Main[3].Hint := fTextLibrary.GetRemakeString(56);
    Button_Main[4].Hint := fTextLibrary.GetRemakeString(57);
    Button_Main[5].Hint := fTextLibrary.GetRemakeString(58);
    //Button_Main[i].Hint := fTextLibrary.GetTextString(160+i);
    for i:=1 to 5 do Button_Main[i].OnClick := SwitchPage;

    Label_MenuTitle:=MyControls.AddLabel(Panel_Main,8,412,138,36,'',fnt_Metal,kaLeft); //Should be one-line

    Label_Stat:=MyControls.AddLabel(Panel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    Label_Hint:=MyControls.AddLabel(Panel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,'',fnt_Outline,kaLeft);
    Label_Hint.Anchors := [akLeft, akBottom]; 

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Terrain_Page();
  Create_Village_Page();
  Create_Player_Page();
  Create_Mission_Page();

  Create_Menu_Page();
    Create_Save_Page();
    Create_Load_Page();
    Create_Quit_Page();

  Create_Unit_Page();
  Create_House_Page();
    Create_Store_Page();
    Create_Barracks_Page();
    //Create_TownHall_Page();

  Label_MissionName := MyControls.AddLabel(Panel_Main, 8, 350, 100, 10, '', fnt_Metal, kaLeft);

  //Here we must go through every control and set the hint event to be the parameter
  for i := 0 to MyControls.Count - 1 do
    if MyControls.Items[i] <> nil then
      TKMControl(MyControls.Items[i]).OnMouseOver := DisplayHint;

  SwitchPage(nil); //Update
end;


destructor TKMapEdInterface.Destroy;
begin
  FreeAndNil(MyControls);
  Inherited;
end;


//Update Hint position and etc..
procedure TKMapEdInterface.SetScreenSize(X,Y:word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
end;


{Terrain page}
procedure TKMapEdInterface.Create_Terrain_Page;
var i,k:integer;
begin
  Panel_Terrain := MyControls.AddPanel(Panel_Main,0,428,196,28);
    Button_Terrain[1] := MyControls.AddButton(Panel_Terrain,   8, 4, 36, 24, 383);
    Button_Terrain[2] := MyControls.AddButton(Panel_Terrain,  48, 4, 36, 24, 388);
    Button_Terrain[3] := MyControls.AddButton(Panel_Terrain,  88, 4, 36, 24, 382);
    Button_Terrain[4] := MyControls.AddButton(Panel_Terrain, 128, 4, 36, 24, 385);
    for i:=1 to 4 do Button_Terrain[i].OnClick := SwitchPage;

    Panel_Brushes := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      BrushSize   := MyControls.AddRatioRow(Panel_Brushes, 8, 10, 100, 20, 1, 12);
      BrushCircle := MyControls.AddButtonFlat(Panel_Brushes, 114, 8, 24, 24, 359);
      BrushSquare := MyControls.AddButtonFlat(Panel_Brushes, 142, 8, 24, 24, 352);
      {BrushSize.OnChange   := TerrainBrush_Change;
      BrushCircle.OnChange := TerrainBrush_Change;
      BrushSquare.OnChange := TerrainBrush_Change;}

    Panel_Heights := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      HeightSize   := MyControls.AddRatioRow(Panel_Heights, 8, 10, 100, 20, 1, 12);
      HeightCircle := MyControls.AddButtonFlat(Panel_Heights, 114, 8, 24, 24, 359);
      HeightSquare := MyControls.AddButtonFlat(Panel_Heights, 142, 8, 24, 24, 352);
      HeightSize.OnChange   := Terrain_HeightChange;
      HeightCircle.OnClick  := Terrain_HeightChange;
      HeightSquare.OnClick  := Terrain_HeightChange;

    Panel_Tiles := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      TilesRandom := MyControls.AddCheckBox(Panel_Tiles, 8, 4, 100, 20, 'Random Direction', fnt_Metal);
      TilesRandom.Checked := true;
      TilesRandom.OnClick := Terrain_TilesChange;
      TilesScroll := MyControls.AddScrollBar(Panel_Tiles, 8, 30 + 4 + MAPED_TILES_ROWS * 32, 180, 20, sa_Horizontal);
      TilesScroll.MinValue := 0;
      TilesScroll.MaxValue := 256 div MAPED_TILES_ROWS - MAPED_TILES_COLS; // 16 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := Terrain_TilesChange;
      for i:=1 to MAPED_TILES_COLS do for k:=1 to MAPED_TILES_ROWS do begin
        //@Krom: I have an idea: Lets make the terrain tiles be an RX number, so say RX=10 means
        //       load the ID as a terrain tile ID. Even though it's not an RX, this special case
        //       method would involve less changes to the code. (buttons have no reason to render
        //       tiles in other situations)
        //@Lewin: Good idea. In fact we might just create GFXData[7] entries which will reference
        //        to tiles atlas with proper UV values.
        //todo: implement this
        TilesTable[(i-1)*MAPED_TILES_ROWS+k] := MyControls.AddButtonFlat(Panel_Tiles,8+(i-1)*32,30+(k-1)*32,32,32,((i-1)*MAPED_TILES_ROWS+k)mod 8+2); //2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Tag := (i-1)*MAPED_TILES_ROWS+k; //Store ID
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnClick := Terrain_TilesChange;
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnMouseWheel := TilesScroll.MouseWheel;
      end;
      Terrain_TilesChange(TilesScroll); //This ensures that the displayed images get updated the first time
      Terrain_TilesChange(TilesTable[1]);

    Panel_Objects := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      ObjectsScroll := MyControls.AddScrollBar(Panel_Objects, 8, 268, 180, 20, sa_Horizontal);
      ObjectsScroll.MinValue := 1;
      ObjectsScroll.MaxValue := ActualMapElemQty div 2;
      ObjectsScroll.Position := 1;
      ObjectsScroll.OnChange := Terrain_ObjectsChange;
      ObjectErase := MyControls.AddButtonFlat(Panel_Objects, 8, 8,32,32,340);
      ObjectsTable[1] := MyControls.AddButtonFlat(Panel_Objects, 8, 40,90,110,1,1); //RXid=1  // 1 2
      ObjectsTable[2] := MyControls.AddButtonFlat(Panel_Objects, 8,150,90,110,1,1); //RXid=1  // 3 4
      ObjectsTable[3] := MyControls.AddButtonFlat(Panel_Objects,98, 40,90,110,1,1); //RXid=1
      ObjectsTable[4] := MyControls.AddButtonFlat(Panel_Objects,98,150,90,110,1,1); //RXid=1
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
  Panel_Village := MyControls.AddPanel(Panel_Main,0,428,196,28);
    Button_Village[1] := MyControls.AddButton(Panel_Village,   8, 4, 36, 24, 454);
    Button_Village[2] := MyControls.AddButton(Panel_Village,  48, 4, 36, 24, 141);
    Button_Village[3] := MyControls.AddButton(Panel_Village,  88, 4, 36, 24, 327);
    for i:=1 to 3 do Button_Village[i].OnClick := SwitchPage;

    Panel_Build := MyControls.AddPanel(Panel_Village,0,28,196,400);
      Label_Build := MyControls.AddLabel(Panel_Build,100,10,100,30,'',fnt_Outline,kaCenter);
      Button_BuildRoad   := MyControls.AddButtonFlat(Panel_Build,  8,40,33,33,335);
      Button_BuildField  := MyControls.AddButtonFlat(Panel_Build, 45,40,33,33,337);
      Button_BuildWine   := MyControls.AddButtonFlat(Panel_Build, 82,40,33,33,336);
      Button_BuildWall   := MyControls.AddButtonFlat(Panel_Build,119,40,33,33,339);
      Button_BuildCancel := MyControls.AddButtonFlat(Panel_Build,156,40,33,33,340);
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

      for i:=1 to HOUSE_COUNT do
        if GUIHouseOrder[i] <> ht_None then begin
          Button_Build[i]:=MyControls.AddButtonFlat(Panel_Build, 8+((i-1) mod 5)*37,80+((i-1) div 5)*37,33,33,GUIBuildIcons[byte(GUIHouseOrder[i])]);
          Button_Build[i].OnClick:=Build_ButtonClick;
          Button_Build[i].Hint:=fTextLibrary.GetTextString(GUIBuildIcons[byte(GUIHouseOrder[i])]-300);
        end;

    Panel_Units := MyControls.AddPanel(Panel_Village,0,28,196,400);
      Label_Units := MyControls.AddLabel(Panel_Units,100,10,100,30,'',fnt_Outline,kaCenter);

      for i:=1 to length(Button_Citizen) do
      begin
        Button_Citizen[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,30+((i-1) div 5)*37,33,33,byte(School_Order[i])+140); //List of tiles 5x5
        Button_Citizen[i].Hint := TypeToString(School_Order[i]);
        Button_Citizen[i].Tag := byte(School_Order[i]); //Returns unit ID
        Button_Citizen[i].OnClick := Unit_ButtonClick;
      end;
      Button_UnitCancel := MyControls.AddButtonFlat(Panel_Units,8+(length(Button_Citizen) mod 5)*37,30+(length(Button_Citizen) div 5)*37,33,33,340);
      Button_UnitCancel.Hint := fTextLibrary.GetTextString(211);
      Button_UnitCancel.OnClick := Unit_ButtonClick;

      for i:=1 to length(Button_Warriors) do
      begin
        Button_Warriors[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,160+((i-1) div 5)*37,33,33, MapEd_Icon[i]);
        Button_Warriors[i].Hint := TypeToString(MapEd_Order[i]);
        Button_Warriors[i].Tag := byte(MapEd_Order[i]); //Returns unit ID
        Button_Warriors[i].OnClick := Unit_ButtonClick;
      end;

      for i:=1 to length(Button_Animals) do
      begin
        Button_Animals[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,250+((i-1) div 5)*37,33,33,Animal_Icon[i]);
        Button_Animals[i].Hint := TypeToString(Animal_Order[i]);
        Button_Animals[i].Tag := byte(Animal_Order[i]); //Returns animal ID
        Button_Animals[i].OnClick := Unit_ButtonClick;
      end;
      Unit_ButtonClick(Button_Citizen[1]); //Select serf as default

    Panel_Script := MyControls.AddPanel(Panel_Village,0,28,196,400);
end;


procedure TKMapEdInterface.Create_Player_Page;
var i:integer;
begin
  Panel_Player := MyControls.AddPanel(Panel_Main,0,428,196,28);
    Button_Player[1] := MyControls.AddButton(Panel_Player,   8, 4, 36, 24, 41);
    Button_Player[2] := MyControls.AddButton(Panel_Player,  48, 4, 36, 24, 382);
    for i:=1 to 2 do Button_Player[i].OnClick := SwitchPage;

    Panel_Goals := MyControls.AddPanel(Panel_Player,0,28,196,400);
      Label_Goals := MyControls.AddLabel(Panel_Goals,100,10,100,30,'Goals',fnt_Outline,kaCenter);

    Panel_Color := MyControls.AddPanel(Panel_Player,0,28,196,400);
      Label_Color := MyControls.AddLabel(Panel_Color,100,10,100,30,'Colors',fnt_Outline,kaCenter);
      MyControls.AddBevel(Panel_Color,8,30,180,210);
      ColorSwatch_Color := MyControls.AddColorSwatch(Panel_Color, 10, 32, 16, 16);
      ColorSwatch_Color.OnClick := Player_ColorClick;
end;


procedure TKMapEdInterface.Create_Mission_Page;
var i,k:integer;
begin
  Panel_Mission := MyControls.AddPanel(Panel_Main,0,428,196,28);
    Button_Mission[1] := MyControls.AddButton(Panel_Mission, 8, 4, 36, 24, 41);
    for i:=1 to 1 do Button_Mission[i].OnClick := SwitchPage;

    Panel_Alliances := MyControls.AddPanel(Panel_Mission,0,28,196,400);
      Label_Alliances := MyControls.AddLabel(Panel_Alliances,100,10,100,30,'Alliances',fnt_Outline,kaCenter);
      MyControls.AddBevel(Panel_Alliances, 9, 28, 180, 180);
      for i:=1 to MAX_PLAYERS do begin
        MyControls.AddLabel(Panel_Alliances,12+i*20+2,30,100,20,inttostr(i),fnt_Outline,kaLeft);
        MyControls.AddLabel(Panel_Alliances,12,30+i*20,100,20,inttostr(i),fnt_Outline,kaLeft);
        for k:=1 to MAX_PLAYERS do begin
          //@Lewin: i=k allows some exotic cases where in theory player could fight with itself
          CheckBox_Alliances[i,k] := MyControls.AddCheckBox(Panel_Alliances, 12+k*20, 30+i*20, 20, 20, '', fnt_Metal);
          CheckBox_Alliances[i,k].Tag := (i-1)*MAX_PLAYERS + (k-1);
          CheckBox_Alliances[i,k].OnClick := Mission_AlliancesChange;
        end;
      end;
      CheckBox_AlliancesSym := MyControls.AddCheckBox(Panel_Alliances, 12, 30+MAX_PLAYERS*20+20, 20, 20, 'Symmetrical', fnt_Metal);
      CheckBox_AlliancesSym.Checked := true;
      CheckBox_AlliancesSym.OnClick := Mission_AlliancesChange;
end;


{Menu page}
procedure TKMapEdInterface.Create_Menu_Page;
begin
  Panel_Menu:=MyControls.AddPanel(Panel_Main,0,412,196,400);
    Button_Menu_Save:=MyControls.AddButton(Panel_Menu,8,20,180,30,fTextLibrary.GetTextString(175),fnt_Metal);
    Button_Menu_Save.OnClick:=SwitchPage;
    Button_Menu_Save.Hint:=fTextLibrary.GetTextString(175);
    Button_Menu_Load:=MyControls.AddButton(Panel_Menu,8,60,180,30,fTextLibrary.GetTextString(174),fnt_Metal);
    Button_Menu_Load.OnClick:=SwitchPage;
    Button_Menu_Load.Hint:=fTextLibrary.GetTextString(174);
    Button_Menu_Settings:=MyControls.AddButton(Panel_Menu,8,100,180,30,fTextLibrary.GetTextString(179),fnt_Metal);
    Button_Menu_Settings.Hint:=fTextLibrary.GetTextString(179);
    Button_Menu_Settings.Disable;
    Button_Menu_Quit:=MyControls.AddButton(Panel_Menu,8,180,180,30,fTextLibrary.GetTextString(180),fnt_Metal);
    Button_Menu_Quit.Hint:=fTextLibrary.GetTextString(180);
    Button_Menu_Quit.OnClick:=SwitchPage;
end;


{Save page}
procedure TKMapEdInterface.Create_Save_Page;
begin
  Panel_Save := MyControls.AddPanel(Panel_Main,0,412,196,400);
    MyControls.AddLabel(Panel_Save,100,30,100,30,'Save map',fnt_Outline,kaCenter);
    Edit_SaveName       := MyControls.AddEdit(Panel_Save,8,50,180,20, fnt_Grey);
    Label_SaveExists    := MyControls.AddLabel(Panel_Save,100,80,100,30,'Map already exists',fnt_Outline,kaCenter);
    CheckBox_SaveExists := MyControls.AddCheckBox(Panel_Save,12,100,100,20,'Overwrite', fnt_Metal);
    Button_SaveSave     := MyControls.AddButton(Panel_Save,8,120,180,30,'Save',fnt_Metal);
    Button_SaveCancel   := MyControls.AddButton(Panel_Save,8,160,180,30,'Cancel',fnt_Metal);
    Edit_SaveName.OnChange      := Menu_Save;
    CheckBox_SaveExists.OnClick := Menu_Save;
    Button_SaveSave.OnClick     := Menu_Save;
    Button_SaveCancel.OnClick   := SwitchPage;
end;


{Load page}
procedure TKMapEdInterface.Create_Load_Page;
begin
  Panel_Load := MyControls.AddPanel(Panel_Main,0,432,196,400);
    MyControls.AddLabel(Panel_Load, 16, 0, 100, 30, 'Available maps', fnt_Outline, kaLeft);
    FileList_Load := MyControls.AddFileList(Panel_Load, 8, 20, 200, 200);
    Button_LoadLoad     := MyControls.AddButton(Panel_Load,8,250,180,30,'Load',fnt_Metal);
    Button_LoadCancel   := MyControls.AddButton(Panel_Load,8,290,180,30,'Cancel',fnt_Metal);
    Button_LoadLoad.OnClick     := Menu_Load;
    Button_LoadCancel.OnClick   := SwitchPage;
end;


{Quit page}
procedure TKMapEdInterface.Create_Quit_Page;
begin
  Panel_Quit:=MyControls.AddPanel(Panel_Main,0,412,200,400);
    MyControls.AddLabel(Panel_Quit,100,40,100,30,'Any unsaved|changes will be lost',fnt_Outline,kaCenter);
    Button_Quit_Yes   := MyControls.AddButton(Panel_Quit,8,100,180,30,'Quit',fnt_Metal);
    Button_Quit_No    := MyControls.AddButton(Panel_Quit,8,140,180,30,fTextLibrary.GetTextString(178),fnt_Metal);
    Button_Quit_Yes.Hint      := fTextLibrary.GetTextString(177);
    Button_Quit_No.Hint       := fTextLibrary.GetTextString(178);
    Button_Quit_Yes.OnClick   := Menu_QuitMission;
    Button_Quit_No.OnClick    := SwitchPage;
end;


{Unit page}
procedure TKMapEdInterface.Create_Unit_Page;
begin
  Panel_Unit:=MyControls.AddPanel(Panel_Main,0,412,200,400);
    Label_UnitName        := MyControls.AddLabel(Panel_Unit,100,16,100,30,'',fnt_Outline,kaCenter);
    Image_UnitPic         := MyControls.AddImage(Panel_Unit,8,38,54,100,521);
    Label_UnitCondition   := MyControls.AddLabel(Panel_Unit,120,40,100,30,fTextLibrary.GetTextString(254),fnt_Grey,kaCenter);
    KMConditionBar_Unit   := MyControls.AddPercentBar(Panel_Unit,73,55,116,15,80);
    Label_UnitDescription := MyControls.AddLabel(Panel_Unit,8,152,236,200,'',fnt_Grey,kaLeft); //Taken from LIB resource

  Panel_Army:=MyControls.AddPanel(Panel_Unit,0,160,200,400);
    Button_Army_RotCW   := MyControls.AddButton(Panel_Army,  8, 0, 56, 40, 23);
    Button_Army_RotCCW  := MyControls.AddButton(Panel_Army,132, 0, 56, 40, 24);
    Button_Army_ForUp   := MyControls.AddButton(Panel_Army,  8, 46, 56, 40, 33);
    ImageStack_Army     := MyControls.AddImageStack(Panel_Army, 70, 46, 56, 40, 43);
    Button_Army_ForDown := MyControls.AddButton(Panel_Army,132, 46, 56, 40, 32);
    Button_Army_RotCW.OnClick   := Unit_ArmyChange1;
    Button_Army_RotCCW.OnClick  := Unit_ArmyChange1;
    Button_Army_ForUp.OnClick   := Unit_ArmyChange1;
    Button_Army_ForDown.OnClick := Unit_ArmyChange1;

    Button_ArmyDec      := MyControls.AddButton(Panel_Army, 80,92,20,20,'-', fnt_Metal);
    Button_ArmyInc      := MyControls.AddButton(Panel_Army,160,92,20,20,'+', fnt_Metal);
    Button_ArmyDec.OnClickEither := Unit_ArmyChange2;
    Button_ArmyInc.OnClickEither := Unit_ArmyChange2;
end;


{House description page}
procedure TKMapEdInterface.Create_House_Page;
begin
  Panel_House:=MyControls.AddPanel(Panel_Main,0,412,200,400);
    //Thats common things
    Label_House:=MyControls.AddLabel(Panel_House,100,14,100,30,'',fnt_Outline,kaCenter);
    Image_House_Logo:=MyControls.AddImage(Panel_House,8,41,32,32,338);
    Image_House_Logo.ImageCenter;
    Image_House_Worker:=MyControls.AddImage(Panel_House,38,41,32,32,141);
    Image_House_Worker.ImageCenter;
    Label_HouseHealth:=MyControls.AddLabel(Panel_House,130,41,30,50,fTextLibrary.GetTextString(228),fnt_Mini,kaCenter,$FFFFFFFF);
    KMHealthBar_House:=MyControls.AddPercentBar(Panel_House,100,53,60,20,50);
    Button_HouseHealthDec := MyControls.AddButton(Panel_House,80,53,20,20,'-', fnt_Metal);
    Button_HouseHealthInc := MyControls.AddButton(Panel_House,160,53,20,20,'+', fnt_Metal);
    Button_HouseHealthDec.OnClickEither := House_HealthChange;
    Button_HouseHealthInc.OnClickEither := House_HealthChange;
end;


{Store page}
procedure TKMapEdInterface.Create_Store_Page;
var i:integer;
begin
    Panel_HouseStore:=MyControls.AddPanel(Panel_House,0,76,200,400);
    for i:=1 to 28 do begin
      Button_Store[i]:=MyControls.AddButtonFlat(Panel_HouseStore, 8+((i-1)mod 5)*36,8+((i-1)div 5)*42,32,36,350+i);
      Button_Store[i].Tag:=i;
      Button_Store[i].Hint:=TypeToString(TResourceType(i));
      Button_Store[i].OnClick := Store_SelectWare;
    end;
    Button_StoreDec100   := MyControls.AddButton(Panel_HouseStore,116,218,20,20,'<', fnt_Metal);
    Button_StoreDec      := MyControls.AddButton(Panel_HouseStore,116,238,20,20,'-', fnt_Metal);
    Label_Store_WareCount:= MyControls.AddLabel (Panel_HouseStore,156,230,100,30,'',fnt_Metal,kaCenter);
    Button_StoreInc100   := MyControls.AddButton(Panel_HouseStore,176,218,20,20,'>', fnt_Metal);
    Button_StoreInc      := MyControls.AddButton(Panel_HouseStore,176,238,20,20,'+', fnt_Metal);
    Button_StoreDec100.OnClickEither := Store_EditWareCount;
    Button_StoreDec.OnClickEither    := Store_EditWareCount;
    Button_StoreInc100.OnClickEither := Store_EditWareCount;
    Button_StoreInc.OnClickEither    := Store_EditWareCount;
end;


{Barracks page}
procedure TKMapEdInterface.Create_Barracks_Page;
var i:integer;
begin
    Panel_HouseBarracks:=MyControls.AddPanel(Panel_House,0,76,200,400);
      for i:=1 to 11 do
      begin
        Button_Barracks[i]:=MyControls.AddButtonFlat(Panel_HouseBarracks, 8+((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,366+i);
        Button_Barracks[i].Tag := i;
        Button_Barracks[i].TexOffsetX:=1;
        Button_Barracks[i].TexOffsetY:=1;
        Button_Barracks[i].CapOffsetY:=2;
        Button_Barracks[i].Hint:=TypeToString(TResourceType(16+i));
        Button_Barracks[i].OnClick := Barracks_SelectWare;
      end;
    Button_BarracksDec100   := MyControls.AddButton(Panel_HouseBarracks,116,218,20,20,'<', fnt_Metal);
    Button_BarracksDec      := MyControls.AddButton(Panel_HouseBarracks,116,238,20,20,'-', fnt_Metal);
    Label_Barracks_WareCount:= MyControls.AddLabel (Panel_HouseBarracks,156,230,100,30,'',fnt_Metal,kaCenter);
    Button_BarracksInc100   := MyControls.AddButton(Panel_HouseBarracks,176,218,20,20,'>', fnt_Metal);
    Button_BarracksInc      := MyControls.AddButton(Panel_HouseBarracks,176,238,20,20,'+', fnt_Metal);
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
  for i:=1 to MAX_PLAYERS do
    Button_PlayerSelect[i].ShapeColor := fPlayers.Player[i].PlayerColor;

  if MyPlayer <> nil then
    Button_PlayerSelect[byte(MyPlayer.PlayerID)].Down := true;
end;
                  

procedure TKMapEdInterface.Player_ChangeActive(Sender: TObject);
var i:integer;
begin
  for i:=1 to MAX_PLAYERS do
    Button_PlayerSelect[i].Down := false;

  if (TKMControl(Sender).Tag in [1..MAX_PLAYERS]) and (fPlayers.Player[TKMControl(Sender).Tag] <> nil) then begin
    MyPlayer := fPlayers.Player[TKMControl(Sender).Tag];
    Button_PlayerSelect[TKMControl(Sender).Tag].Down := true;
  end;

  ShownHouse := nil; //Drop selection
  ShownUnit := nil;
end;


procedure TKMapEdInterface.Terrain_HeightChange(Sender: TObject);
begin
  if Sender = HeightCircle then
  begin
    HeightCircle.Down := true;
    HeightSquare.Down := false;
    GameCursor.Mode  := cm_Height;
    GameCursor.Tag1 := HeightSize.Position;
    GameCursor.Tag2 := MAPED_HEIGHT_CIRCLE;
  end;
  if Sender = HeightSquare then
  begin
    HeightSquare.Down := true;
    HeightCircle.Down := false;
    GameCursor.Mode  := cm_Height;
    GameCursor.Tag1 := HeightSize.Position;
    GameCursor.Tag2 := MAPED_HEIGHT_SQUARE;
  end;
  if Sender = HeightSize then
    GameCursor.Tag1 := HeightSize.Position;
end;


procedure TKMapEdInterface.Terrain_TilesChange(Sender: TObject);
var i,k:integer;
begin
  if Sender = TilesRandom then
  begin
    TilesRandom.Checked := not TilesRandom.Checked;
  end;
  if Sender = TilesScroll then //Shift tiles
    for i:=1 to MAPED_TILES_COLS do
    for k:=1 to MAPED_TILES_ROWS do
    begin
      TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := (TilesScroll.Position*MAPED_TILES_ROWS+(i-1)*MAPED_TILES_ROWS+k)mod 8+2; //icons are in 2..9
      TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (GameCursor.Tag1 = TilesScroll.Position*MAPED_TILES_ROWS + TilesTable[(i-1)*MAPED_TILES_ROWS+k].Tag);
    end;
  if Sender is TKMButtonFlat then
  begin
    GameCursor.Mode := cm_Tiles;
    GameCursor.Tag1 := EnsureRange(TilesScroll.Position*MAPED_TILES_ROWS + TKMButtonFlat(Sender).Tag, 0, 247); //Offset+Tag without road overlays?
    for i:=1 to MAPED_TILES_COLS do
    for k:=1 to MAPED_TILES_ROWS do
      TilesTable[(i-1)*MAPED_TILES_ROWS+k].Down := (Sender = TilesTable[(i-1)*MAPED_TILES_ROWS+k]);
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
        ObjectsTable[i].TexID := MapElem[ActualMapElem[ObjID]].Step[1] + 1
      else
        ObjectsTable[i].TexID := 0;
      ObjectsTable[i].Down := ObjID = OriginalMapElem[GameCursor.Tag1+1]; //Mark the selected one using reverse lookup
      ObjectsTable[i].Caption := inttostr(ObjID);
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
  Label_Build.Caption := '';

  if Button_BuildCancel.Down then begin
    GameCursor.Mode:=cm_Erase;
    Label_Build.Caption := fTextLibrary.GetTextString(210);
  end;
  if Button_BuildRoad.Down then begin
    GameCursor.Mode:=cm_Road;
    Label_Build.Caption := fTextLibrary.GetTextString(212);
  end;
  if Button_BuildField.Down then begin
    GameCursor.Mode:=cm_Field;
    Label_Build.Caption := fTextLibrary.GetTextString(214);
  end;
  if Button_BuildWine.Down then begin
    GameCursor.Mode:=cm_Wine;
    Label_Build.Caption := fTextLibrary.GetTextString(218);
  end;
{  if Button_BuildWall.Down then begin
    GameCursor.Mode:=cm_Wall;
    Label_BuildCost_Wood.Caption:='1';
    //Label_Build.Caption := fTextLibrary.GetTextString(218);
  end;}

  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if Button_Build[i].Down then begin
     GameCursor.Mode:=cm_Houses;
     GameCursor.Tag1:=byte(GUIHouseOrder[i]);
     Label_Build.Caption := TypeToString(THouseType(byte(GUIHouseOrder[i])));
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
    Label_Units.Caption := TypeToString(TUnitType(byte(TKMButtonFlat(Sender).Tag)));
  end;

end;


procedure TKMapEdInterface.ShowHouseInfo(Sender:TKMHouse);
begin
  ShownUnit:=nil;
  ShownHouse:=Sender;

  if (not Assigned(Sender)) then begin //=nil produces wrong result when there's no object at all
    SwitchPage(nil);
    exit;
  end;

  {Common data}
  Label_House.Caption:=TypeToString(Sender.GetHouseType);
  Image_House_Logo.TexID:=300+byte(Sender.GetHouseType);
  Image_House_Worker.TexID:=140+HouseDAT[byte(Sender.GetHouseType)].OwnerType+1;
  Image_House_Worker.Hint := TypeToString(TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1));
  KMHealthBar_House.Caption:=inttostr(round(Sender.GetHealth))+'/'+inttostr(HouseDAT[byte(Sender.GetHouseType)].MaxHealth);
  KMHealthBar_House.Position:=round( Sender.GetHealth / HouseDAT[byte(Sender.GetHouseType)].MaxHealth * 100 );

  Image_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  

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
  ShownUnit:=Sender;
  ShownHouse:=nil;
  if (not Assigned(Sender))or(not Sender.IsVisible)or((Sender<>nil)and(Sender.IsDead)) then begin
    SwitchPage(nil);
    ShownUnit:=nil; //Make sure it doesn't come back again, especially if it's dead!
    exit;
  end;
  SwitchPage(Panel_Unit);
  Label_UnitName.Caption:=TypeToString(Sender.UnitType);
  Image_UnitPic.TexID:=520+byte(Sender.UnitType);
  KMConditionBar_Unit.Position:=EnsureRange(round(Sender.GetCondition / UNIT_MAX_CONDITION * 100),-10,110);
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

  if Sender = CheckBox_SaveExists then begin
    CheckBox_SaveExists.Checked := not CheckBox_SaveExists.Checked;
    Button_SaveSave.Enabled := CheckBox_SaveExists.Checked;
  end;

  if Sender = Button_SaveSave then begin
    //Should we expand the path here?
    fGame.MapEditorSave(Edit_SaveName.Text, true);
    SwitchPage(Button_SaveCancel); //return to previous menu
  end;
end;


{Show mission loading dialogue}
procedure TKMapEdInterface.Menu_Load(Sender:TObject);
begin
  fGame.MapEditorStart(FileList_Load.FileName, 0, 0);
end;


{Quit the mission and return to main menu}
procedure TKMapEdInterface.Menu_QuitMission(Sender:TObject);
var i:integer;
begin
  Panel_Main.Hide;
  for i:=1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  fGame.GameStop(gr_MapEdEnd);
end;


{Virtually press BuildRoad button when changing page to BuildingPage or after house plan is placed}
procedure TKMapEdInterface.Build_SelectRoad;
begin
  Build_ButtonClick(Button_BuildRoad);
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


function TKMapEdInterface.GetTilesRandomized: boolean;
begin
  Result := TilesRandom.Checked;
end;


procedure TKMapEdInterface.SetTileDirection(aTileDirection: byte);
begin
  TileDirection := aTileDirection;
  if TileDirection > 3 then TileDirection := 0;
  GameCursor.Tag2 := TileDirection;
end;


function TKMapEdInterface.GetSelectedTile(): TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to MAPED_TILES_COLS*MAPED_TILES_ROWS do
    if TilesTable[i].Down then Result := TilesTable[i];
end;


function TKMapEdInterface.GetSelectedObject(): TObject;
var i: byte;
begin
  Result := nil;
  for i:=1 to 4 do
    if ObjectsTable[i].Down then Result := ObjectsTable[i];
end;  


function TKMapEdInterface.GetSelectedUnit(): TObject;
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
  if ShownHouse = nil then exit;
  Amt := 0;
  if AButton = mbLeft then Amt:=1;
  if AButton = mbRight then Amt:=50;
  if Sender = Button_HouseHealthDec then ShownHouse.AddDamage(Amt);
  if Sender = Button_HouseHealthInc then ShownHouse.AddRepair(Amt);
  ShowHouseInfo(ShownHouse);
end;


procedure TKMapEdInterface.Unit_ArmyChange1(Sender:TObject);
var Commander:TKMUnitWarrior;
begin
  if ShownUnit = nil then exit;
  if not (ShownUnit is TKMUnitWarrior) then exit;

  Commander := TKMUnitWarrior(ShownUnit).GetCommander;
  if Sender = Button_Army_ForUp then Commander.UnitsPerRow := max(Commander.UnitsPerRow-1,1);
  if Sender = Button_Army_ForDown then Commander.UnitsPerRow := min(Commander.UnitsPerRow+1,Commander.fMapEdMembersCount+1);
  ImageStack_Army.SetCount(Commander.fMapEdMembersCount + 1,Commander.UnitsPerRow);

  if Sender = Button_Army_RotCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)-1);
  if Sender = Button_Army_RotCCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)+1);
end;


procedure TKMapEdInterface.Unit_ArmyChange2(Sender:TObject; AButton:TMouseButton);
var Amt:shortint; Commander:TKMUnitWarrior;
begin
  if ShownUnit = nil then exit;
  if not (ShownUnit is TKMUnitWarrior) then exit;

  Amt := 0;
  if Sender = Button_ArmyDec then Amt := -1; //Decrease
  if Sender = Button_ArmyInc then Amt := 1; //Increase
  if AButton = mbLeft then Amt  := Amt * 1;
  if AButton = mbRight then Amt := Amt * 10;

  Commander := TKMUnitWarrior(ShownUnit).GetCommander;
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
  Barracks := TKMHouseBarracks(ShownHouse);

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
  Store := TKMHouseStore(ShownHouse);

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
  MyPlayer.PlayerColor := ColorSwatch_Color.GetColor;
  Player_UpdateColors;
end;


procedure TKMapEdInterface.Mission_AlliancesChange(Sender:TObject);
var i,k:integer;
begin
  if Sender = CheckBox_AlliancesSym then begin
    CheckBox_AlliancesSym.Checked := not CheckBox_AlliancesSym.Checked;
    exit;
  end;

  if Sender = nil then begin
    for i:=1 to MAX_PLAYERS do
    for k:=1 to MAX_PLAYERS do
      if (fPlayers.Player[i]<>nil)and(fPlayers.Player[k]<>nil) then
        CheckBox_Alliances[i,k].Checked := (fPlayers.CheckAlliance(fPlayers.Player[i].PlayerID, fPlayers.Player[k].PlayerID)=at_Ally)
      else
        CheckBox_Alliances[i,k].Enabled := false;
    exit;
  end;

  i := TKMCheckBox(Sender).Tag div MAX_PLAYERS + 1;
  k := TKMCheckBox(Sender).Tag mod MAX_PLAYERS + 1;
  CheckBox_Alliances[i,k].Checked := not CheckBox_Alliances[i,k].Checked;

  if CheckBox_Alliances[i,k].Checked then fPlayers.Player[i].fAlliances[k] := at_Ally
                                     else fPlayers.Player[i].fAlliances[k] := at_Enemy;

  //Copy status to symmetrical item
  if CheckBox_AlliancesSym.Checked then begin
    CheckBox_Alliances[k,i].Checked := CheckBox_Alliances[i,k].Checked;
    fPlayers.Player[k].fAlliances[i] := fPlayers.Player[i].fAlliances[k];
  end;
end;


procedure TKMapEdInterface.OnKeyUp(Key:Word; IsDown:boolean=false);
begin
  //1-5 game menu shortcuts
  if Key in [49..53] then
  begin
    if Button_Main[Key-48].Visible then MyControls.CtrlDown := Button_Main[Key-48];
    if not IsDown then SwitchPage(Button_Main[Key-48]);
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
