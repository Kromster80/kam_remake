unit KM_InterfaceMapEditor;
interface
uses Classes, Controls, KromUtils, Math, SysUtils, KromOGLUtils,
     KM_Controls, KM_Defaults, KM_Houses, KM_Units;

type TKMapEdInterface = class
  protected
    ToolBarX:word;
  protected
    ShownUnit:TKMUnit;
    ShownHouse:TKMHouse;
    ShownHint:TObject;
    StorehouseItem:byte; //Selected ware in storehouse

    Panel_Main:TKMPanel;
      Image_Main1,Image_Main2,Image_Main3,Image_Main4,Image_Main5:TKMImage; //Toolbar background
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

    Panel_Menu:TKMPanel;
      Button_Menu_Save,Button_Menu_Load,Button_Menu_Settings,Button_Menu_Quit:TKMButton;

      Panel_Save:TKMPanel;
        TextEdit_SaveName:TKMTextEdit;
        Label_SaveExists:TKMLabel;
        CheckBox_SaveExists:TKMCheckBox;
        Button_SaveSave:TKMButton;
        Button_SaveCancel:TKMButton;

      Panel_Quit:TKMPanel;
        Button_Quit_Yes,Button_Quit_No:TKMButton;

    Panel_Stats:TKMPanel;
      Stat_HousePic,Stat_UnitPic:array[1..32]of TKMImage;
      Stat_HouseQty,Stat_UnitQty:array[1..32]of TKMLabel;

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
      Button_Barracks:array[1..12]of TKMButtonFlat;
      Label_Barracks_Unit:TKMLabel;
      Image_Barracks_Right,Image_Barracks_Train,Image_Barracks_Left:TKMImage;
      Button_Barracks_Right,Button_Barracks_Train,Button_Barracks_Left:TKMButton;
  private
    procedure Create_Terrain_Page;
    procedure Create_Village_Page;
    procedure Create_Stats_Page;
    procedure Create_Menu_Page;
    procedure Create_Save_Page;
    procedure Create_Quit_Page;
    procedure Create_Unit_Page;
    procedure Create_House_Page;
    procedure Create_Store_Page;
    procedure Create_Barracks_Page;

    procedure SwitchPage(Sender: TObject);
    procedure DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
    procedure Minimap_Update(Sender: TObject);
    procedure TerrainHeight_Change(Sender: TObject);
    procedure TerrainTiles_Change(Sender: TObject);
    procedure TerrainObjects_Change(Sender: TObject);
    procedure Build_ButtonClick(Sender: TObject);
    procedure Unit_ButtonClick(Sender: TObject);
    procedure Store_Fill(Sender:TObject);
    procedure Stats_Fill(Sender:TObject);
    procedure House_HealthChange(Sender:TObject; AButton:TMouseButton);
    procedure Unit_ArmyChange(Sender:TObject; AButton:TMouseButton);
    procedure Store_SelectWare(Sender:TObject);
    procedure Store_EditWareCount(Sender:TObject; AButton:TMouseButton);
  public
    MyControls: TKMControlsCollection;
    constructor Create;
    destructor Destroy; override;
    procedure SetScreenSize(X,Y:word);
    procedure ShowHouseInfo(Sender:TKMHouse);
    procedure ShowUnitInfo(Sender:TKMUnit);
    procedure Menu_Save(Sender:TObject);
    procedure Menu_Load(Sender:TObject);
    procedure Menu_QuitMission(Sender:TObject);
    procedure Build_SelectRoad;
    procedure RightClick_Cancel;
    procedure OnKeyUp(Key:Word; IsDown:boolean=false);
    property GetShownUnit: TKMUnit read ShownUnit;
    function GetShownPage:TKMMapEdShownPage;
    procedure ClearShownUnit;
    procedure UpdateState;
    procedure Paint;
  end;


implementation
uses KM_Unit1, KM_Units_Warrior, KM_PlayersCollection, KM_Render, KM_LoadLib, KM_Terrain, KM_Utils, KM_Viewport, KM_Game, KM_CommonTypes;


{Switch between pages}
procedure TKMapEdInterface.SwitchPage(Sender: TObject);
var i,k:integer;
begin

  //Reset cursor mode
  CursorMode.Mode := cm_None;
  CursorMode.Tag1 := 0;
  CursorMode.Tag2 := 0;

  //Reset shown item if user clicked on any of the main buttons
  if (Sender=Button_Main[1])or(Sender=Button_Main[2])or
     (Sender=Button_Main[3])or(Sender=Button_Main[4])or
     (Sender=Button_Main[5])or
     (Sender=Button_Menu_Settings)or(Sender=Button_Menu_Quit) then begin
    ShownHouse:=nil;
    ShownUnit:=nil;
    fPlayers.Selected:=nil;
  end;

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
    TerrainHeight_Change(HeightCircle); //Select the default mode
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[3]) then begin
    Panel_Terrain.Show;
    Panel_Tiles.Show;
    Label_MenuTitle.Caption:='Terrain - Tiles';
  end else

  if (Sender = Button_Main[1])or(Sender = Button_Terrain[4]) then begin
    Panel_Terrain.Show;
    Panel_Objects.Show;
    Label_MenuTitle.Caption:='Terrain - Objects'; 
    TerrainObjects_Change(ObjectsScroll); //This ensures that the displayed images get updated (i.e. if it's the first time)
    TerrainObjects_Change(ObjectsTable[1]);
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
    Unit_ButtonClick(Button_Citizen[1]);
  end else

  if (Sender = Button_Main[2])or(Sender = Button_Village[3]) then begin
    Panel_Village.Show;
    Panel_Script.Show;
    Label_MenuTitle.Caption:='Village - Script';
  end else

  if Sender=Button_Main[3] then begin
    Stats_Fill(nil);
    Panel_Stats.Show;
    Label_MenuTitle.Caption:=fTextLibrary.GetTextString(168);
  end else

  if (Sender=Button_Main[5]) or (Sender=Button_Quit_No) or (Sender = Button_SaveCancel) then begin
    Panel_Menu.Show;
    Label_MenuTitle.Caption:=fTextLibrary.GetTextString(170);
  end else

  if Sender=Button_Menu_Quit then begin
    Panel_Quit.Show;
  end;

  if Sender = Button_Menu_Save then begin
    TextEdit_SaveName.Text := fGame.GetGameName;
    Menu_Save(TextEdit_SaveName);
    Panel_Save.Show;
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


procedure TKMapEdInterface.DisplayHint(Sender: TObject; AShift:TShiftState; X,Y:integer);
begin
  ShownHint:=Sender;
  if((ShownHint<>nil) and ((not TKMControl(ShownHint).CursorOver) or (not TKMControl(ShownHint).Visible)) ) then ShownHint:=nil; //only set if cursor is over and control is visible
  if ((ShownHint<>nil) and (TKMControl(ShownHint).Parent <> nil)) then //only set if parent is visible (e.g. panel)
    if (ShownHint<>nil)and(not (ShownHint as TKMControl).Parent.Visible) then ShownHint:=nil;

  Label_Hint.Top:=fRender.GetRenderAreaSize.Y-16;
  //If hint hasn't changed then don't refresh it
  if ((ShownHint<>nil) and (Label_Hint.Caption = TKMControl(Sender).Hint)) then exit;
  if ((ShownHint=nil) and (Label_Hint.Caption = '')) then exit;
  if ShownHint=nil then Label_Hint.Caption:='' else
    Label_Hint.Caption:=(Sender as TKMControl).Hint;
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

  ShownUnit:=nil;
  ShownHouse:=nil;
  StorehouseItem := 1; //First ware selected by default

{Parent Page for whole toolbar in-game}
  Panel_Main := MyControls.AddPanel(nil,0,0,224,768);

    Image_Main1 := MyControls.AddImage(Panel_Main,0,0,224,200,407);

    //todo: For the map editor mode, remove swords/logo and instead have
    //player selection and other "universal" stuff? (i.e. which player are we placing for)
    Image_Main2 := MyControls.AddImage(Panel_Main,0,200,224,168,554);
    Image_Main3 := MyControls.AddImage(Panel_Main,0,368,224,400,404);
    Image_Main4 := MyControls.AddImage(Panel_Main,0,768,224,400,404);
    Image_Main5 := MyControls.AddImage(Panel_Main,0,1168,224,400,404); //For 1600x1200 this is needed

    KMMinimap:=MyControls.AddMinimap(Panel_Main,10,10,176,176);
    KMMinimap.OnChange:=Minimap_Update;

    {5 big tabs}
    Button_Main[1] := MyControls.AddButton(Panel_Main,   8, 372, 36, 36, 381);
    Button_Main[2] := MyControls.AddButton(Panel_Main,  48, 372, 36, 36, 368);
    Button_Main[3] := MyControls.AddButton(Panel_Main,  88, 372, 36, 36,  41);
    Button_Main[4] := MyControls.AddButton(Panel_Main, 128, 372, 36, 36,  41);
    Button_Main[5] := MyControls.AddButton(Panel_Main, 168, 372, 36, 36, 389);
    Button_Main[1].Hint := 'Terrain editing';
    Button_Main[2].Hint := 'Village planning';
    Button_Main[3].Hint := 'Visual scripts';
    Button_Main[4].Hint := 'Global scripting';
    Button_Main[5].Hint := 'Menu';
    //Button_Main[i].Hint := fTextLibrary.GetTextString(160+i);
    for i:=1 to 5 do Button_Main[i].OnClick := SwitchPage;

    Label_MenuTitle:=MyControls.AddLabel(Panel_Main,8,412,138,36,'',fnt_Metal,kaLeft); //Should be one-line

    Label_Stat:=MyControls.AddLabel(Panel_Main,224+8,16,0,0,'',fnt_Outline,kaLeft);
    Label_Hint:=MyControls.AddLabel(Panel_Main,224+8,fRender.GetRenderAreaSize.Y-16,0,0,'',fnt_Outline,kaLeft);

{I plan to store all possible layouts on different pages which gets displayed one at a time}
{==========================================================================================}
  Create_Terrain_Page();
  Create_Village_Page();

  Create_Stats_Page();

  Create_Menu_Page();
    Create_Save_Page();
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
      TKMControl(MyControls.Items[i]).OnHint := DisplayHint;

  SwitchPage(nil); //Update
end;


destructor TKMapEdInterface.Destroy;
begin
  FreeAndNil(MyControls);
  inherited;
end;


procedure TKMapEdInterface.SetScreenSize(X,Y:word);
begin
  //Update Hint position and etc..
  Label_Hint.Top:=Y-16;
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
      HeightSize.OnChange   := TerrainHeight_Change;
      HeightCircle.OnClick  := TerrainHeight_Change;
      HeightSquare.OnClick  := TerrainHeight_Change;

    Panel_Tiles := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      for i:=1 to MAPED_TILES_COLS do for k:=1 to MAPED_TILES_ROWS do begin
        TilesTable[(i-1)*MAPED_TILES_ROWS+k] := MyControls.AddButtonFlat(Panel_Tiles,8+(i-1)*32,4+(k-1)*32,32,32,((i-1)*MAPED_TILES_ROWS+k)mod 8+2); //2..9
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].Tag := (i-1)*MAPED_TILES_ROWS+k; //Store ID
        TilesTable[(i-1)*MAPED_TILES_ROWS+k].OnClick := TerrainTiles_Change;
      end;
      TilesScroll := MyControls.AddScrollBar(Panel_Tiles, 8, 4 + 4 + MAPED_TILES_ROWS * 32, 180, 20, sa_Horizontal);
      TilesScroll.MinValue := 0;
      TilesScroll.MaxValue := 256 div MAPED_TILES_ROWS - MAPED_TILES_COLS; // 16 - 6
      TilesScroll.Position := 0;
      TilesScroll.OnChange := TerrainTiles_Change;

    Panel_Objects := MyControls.AddPanel(Panel_Terrain,0,28,196,400);
      ObjectErase := MyControls.AddButtonFlat(Panel_Objects, 8, 8,32,32,340);
      ObjectsTable[1] := MyControls.AddButtonFlat(Panel_Objects, 8, 40,90,110,1,1); //RXid=1  // 1 2
      ObjectsTable[2] := MyControls.AddButtonFlat(Panel_Objects, 8,150,90,110,1,1); //RXid=1  // 3 4
      ObjectsTable[3] := MyControls.AddButtonFlat(Panel_Objects,98, 40,90,110,1,1); //RXid=1
      ObjectsTable[4] := MyControls.AddButtonFlat(Panel_Objects,98,150,90,110,1,1); //RXid=1
      for i:=1 to 4 do begin
        ObjectsTable[i].Tag := i; //Store ID
        ObjectsTable[i].OnClick := TerrainObjects_Change;
      end;
      ObjectErase.Tag := 255; //no object
      ObjectErase.OnClick := TerrainObjects_Change;
      ObjectsScroll := MyControls.AddScrollBar(Panel_Objects, 8, 268, 180, 20, sa_Horizontal);
      ObjectsScroll.MinValue := 1;
      ObjectsScroll.MaxValue := ActualMapElemQty div 2;
      ObjectsScroll.Position := 1;
      ObjectsScroll.OnChange := TerrainObjects_Change;
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
        Button_Citizen[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,30+((i-1) div 5)*37,33,33,byte(School_Order[i])+140); //List of tiles 32x8
        Button_Citizen[i].Hint := TypeToString(School_Order[i]);
        Button_Citizen[i].Tag := byte(School_Order[i]); //Returns unit ID
        Button_Citizen[i].OnClick := Unit_ButtonClick;
      end;
      Button_UnitCancel := MyControls.AddButtonFlat(Panel_Units,8+(length(Button_Citizen) mod 5)*37,30+(length(Button_Citizen) div 5)*37,33,33,340);
      Button_UnitCancel.Hint := fTextLibrary.GetTextString(211);
      Button_UnitCancel.OnClick := Unit_ButtonClick;

      for i:=1 to length(Button_Warriors) do
      begin
        Button_Warriors[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,160+((i-1) div 5)*37,33,33,370+i);
        Button_Warriors[i].Hint := TypeToString(MapEd_Order[i]);
        Button_Warriors[i].Tag := byte(MapEd_Order[i]); //Returns unit ID
        Button_Warriors[i].OnClick := Unit_ButtonClick;
      end;

      for i:=1 to length(Button_Animals) do
      begin
        Button_Animals[i] := MyControls.AddButtonFlat(Panel_Units,8+((i-1) mod 5)*37,250+((i-1) div 5)*37,33,33,370+i);
        Button_Animals[i].Hint := TypeToString(Animal_Order[i]);
        Button_Animals[i].Tag := byte(Animal_Order[i]); //Returns animal ID
        Button_Animals[i].OnClick := Unit_ButtonClick;
      end;

    Panel_Script := MyControls.AddPanel(Panel_Village,0,28,196,400);
end;


{Statistics page}
procedure TKMapEdInterface.Create_Stats_Page;
const IncY=34; Nil_Width=10; House_Width=30; Unit_Width=26;
var i,k:integer; hc,uc,off:integer;
begin
  Panel_Stats:=MyControls.AddPanel(Panel_Main,0,412,200,400);

  hc:=1; uc:=1;
  for i:=1 to 8 do begin
    off:=8;
    case i of
    1: begin
          MyControls.AddBevel(Panel_Stats,  8,(i-1)*IncY,56,30);
          MyControls.AddBevel(Panel_Stats, 71,(i-1)*IncY,56,30);
          MyControls.AddBevel(Panel_Stats,134,(i-1)*IncY,56,30);
       end;
    2: begin
          MyControls.AddBevel(Panel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(Panel_Stats,104,(i-1)*IncY,86,30);
       end;
    3: begin
          MyControls.AddBevel(Panel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(Panel_Stats,104,(i-1)*IncY,86,30);
       end;
    4: begin
          MyControls.AddBevel(Panel_Stats,  8,(i-1)*IncY,86,30);
          MyControls.AddBevel(Panel_Stats,104,(i-1)*IncY,86,30);
       end;
    5:    MyControls.AddBevel(Panel_Stats,8,(i-1)*IncY,116,30);
    6:    MyControls.AddBevel(Panel_Stats,8,(i-1)*IncY,146,30);
    7:    MyControls.AddBevel(Panel_Stats,8,(i-1)*IncY,86,30);
    8: begin
          MyControls.AddBevel(Panel_Stats,  8,(i-1)*IncY,120,30);
          MyControls.AddBevel(Panel_Stats,138,(i-1)*IncY,52,30);
       end;
    end;

    for k:=1 to 8 do
    if StatCount[i,k]=0 then begin
      if i=1 then
        inc(off,Nil_Width-3)
      else
        inc(off,Nil_Width);
    end else
    if StatCount[i,k]=1 then begin
      Stat_HousePic[hc]:=MyControls.AddImage(Panel_Stats,off,(i-1)*IncY,House_Width,30,41{byte(StatHouse[hc])+300});
      Stat_HousePic[hc].Hint:=TypeToString(StatHouse[hc]);
      Stat_HouseQty[hc]:=MyControls.AddLabel(Panel_Stats,off+House_Width-2,(i-1)*IncY+16,37,30,'-',fnt_Grey,kaRight);
      Stat_HouseQty[hc].Hint:=TypeToString(StatHouse[hc]);
      inc(hc);
      inc(off,House_Width);
    end else
    if StatCount[i,k]=2 then begin
      Stat_UnitPic[uc]:=MyControls.AddImage(Panel_Stats,off,(i-1)*IncY,Unit_Width,30,byte(StatUnit[uc])+140);
      Stat_UnitPic[uc].Hint:=TypeToString(StatUnit[uc]);
      Stat_UnitQty[uc]:=MyControls.AddLabel(Panel_Stats,off+Unit_Width-2,(i-1)*IncY+16,33,30,'-',fnt_Grey,kaRight);
      Stat_UnitQty[uc].Hint:=TypeToString(StatUnit[uc]);
      inc(uc);
      inc(off,Unit_Width);
    end;
  end;

end;
                                           

{Menu page}
procedure TKMapEdInterface.Create_Menu_Page;
begin
  Panel_Menu:=MyControls.AddPanel(Panel_Main,0,412,196,400);
    Button_Menu_Save:=MyControls.AddButton(Panel_Menu,8,20,180,30,fTextLibrary.GetTextString(175),fnt_Metal);
    Button_Menu_Save.OnClick:=SwitchPage;
    Button_Menu_Save.Hint:=fTextLibrary.GetTextString(175);
    Button_Menu_Load:=MyControls.AddButton(Panel_Menu,8,60,180,30,fTextLibrary.GetTextString(174),fnt_Metal);
    Button_Menu_Load.OnClick:=Menu_Load;
    Button_Menu_Load.Hint:=fTextLibrary.GetTextString(174);
    Button_Menu_Settings:=MyControls.AddButton(Panel_Menu,8,100,180,30,fTextLibrary.GetTextString(179),fnt_Metal);
    Button_Menu_Settings.Hint:=fTextLibrary.GetTextString(179);
    Button_Menu_Settings.Disable;
    Button_Menu_Quit:=MyControls.AddButton(Panel_Menu,8,180,180,30,fTextLibrary.GetTextString(180),fnt_Metal);
    Button_Menu_Quit.Hint:=fTextLibrary.GetTextString(180);
    Button_Menu_Quit.OnClick:=SwitchPage;
    Button_Menu_Load.Disable;
end;


{Save page}
procedure TKMapEdInterface.Create_Save_Page;
begin
  Panel_Save := MyControls.AddPanel(Panel_Main,0,412,196,400);
    MyControls.AddLabel(Panel_Save,100,30,100,30,'Save map',fnt_Outline,kaCenter);
    TextEdit_SaveName   := MyControls.AddTextEdit(Panel_Save,8,50,180,20, fnt_Grey);
    Label_SaveExists    := MyControls.AddLabel(Panel_Save,100,80,100,30,'Map already exists',fnt_Outline,kaCenter);
    CheckBox_SaveExists := MyControls.AddCheckBox(Panel_Save,12,100,100,20,'Overwrite', fnt_Metal);
    Button_SaveSave     := MyControls.AddButton(Panel_Save,8,120,180,30,'Save',fnt_Metal);
    Button_SaveCancel   := MyControls.AddButton(Panel_Save,8,160,180,30,'Cancel',fnt_Metal);
    TextEdit_SaveName.OnChange  := Menu_Save;
    CheckBox_SaveExists.OnClick := Menu_Save;
    Button_SaveSave.OnClick     := Menu_Save;
    Button_SaveCancel.OnClick   := Menu_Save;
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
    Button_Army_RotCW.OnClickEither   := Unit_ArmyChange;
    Button_Army_RotCCW.OnClickEither  := Unit_ArmyChange;
    Button_Army_ForUp.OnClickEither   := Unit_ArmyChange;
    Button_Army_ForDown.OnClickEither := Unit_ArmyChange;

    Button_ArmyDec      := MyControls.AddButton(Panel_Army, 80,92,20,20,'-', fnt_Metal);
    Button_ArmyInc      := MyControls.AddButton(Panel_Army,160,92,20,20,'+', fnt_Metal);
    Button_ArmyDec.OnClickEither := Unit_ArmyChange;
    Button_ArmyInc.OnClickEither := Unit_ArmyChange;
end;


{House description page}
procedure TKMapEdInterface.Create_House_Page;
begin
  Panel_House:=MyControls.AddPanel(Panel_Main,0,412,200,400);
    //Thats common things
    Label_House:=MyControls.AddLabel(Panel_House,100,14,100,30,'',fnt_Outline,kaCenter);
    Image_House_Logo:=MyControls.AddImage(Panel_House,8,41,32,32,338);
    Image_House_Logo.Center;
    Image_House_Worker:=MyControls.AddImage(Panel_House,38,41,32,32,141);
    Image_House_Worker.Center;
    Label_HouseHealth:=MyControls.AddLabel(Panel_House,130,41,30,50,fTextLibrary.GetTextString(228),fnt_Mini,kaCenter,$FFFFFFFF);
    KMHealthBar_House:=MyControls.AddPercentBar(Panel_House,100,53,60,20,50,'',fnt_Mini);
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
      for i:=1 to 12 do
      begin
        Button_Barracks[i]:=MyControls.AddButtonFlat(Panel_HouseBarracks, 8+((i-1)mod 6)*31,8+((i-1)div 6)*42,28,38,366+i);
        Button_Barracks[i].TexOffsetX:=1;
        Button_Barracks[i].TexOffsetY:=1;
        Button_Barracks[i].CapOffsetY:=2;
        Button_Barracks[i].HideHighlight:=true;
        Button_Barracks[i].Hint:=TypeToString(TResourceType(16+i));
      end;
      Button_Barracks[12].TexID:=154;
      Button_Barracks[12].Hint:=TypeToString(ut_Recruit);

      Label_Barracks_Unit:=MyControls.AddLabel(Panel_HouseBarracks,100,96,100,30,'',fnt_Outline,kaCenter);

      Image_Barracks_Left :=MyControls.AddImage(Panel_HouseBarracks,  8,116,54,80,535);
      Image_Barracks_Left.Enabled := false;
      Image_Barracks_Train:=MyControls.AddImage(Panel_HouseBarracks, 70,116,54,80,536);
      Image_Barracks_Right:=MyControls.AddImage(Panel_HouseBarracks,132,116,54,80,537);
      Image_Barracks_Right.Enabled := false;

      Button_Barracks_Left :=MyControls.AddButton(Panel_HouseBarracks,  8,226,54,40,35);
      Button_Barracks_Train:=MyControls.AddButton(Panel_HouseBarracks, 70,226,54,40,42);
      Button_Barracks_Right:=MyControls.AddButton(Panel_HouseBarracks,132,226,54,40,36);
      Button_Barracks_Left.Hint :=fTextLibrary.GetTextString(237);
      Button_Barracks_Train.Hint:=fTextLibrary.GetTextString(240);
      Button_Barracks_Right.Hint:=fTextLibrary.GetTextString(238);
      Button_Barracks_Train.Disable; //Unimplemented yet
end;


{Should update any items changed by game (resource counts, hp, etc..)}
{If it ever gets a bottleneck then some static Controls may be excluded from update}
procedure TKMapEdInterface.UpdateState;
begin
  if ShownUnit<>nil then ShowUnitInfo(ShownUnit) else
  if ShownHouse<>nil then ShowHouseInfo(ShownHouse);

  if ShownHint<>nil then DisplayHint(ShownHint,[],0,0);
  if Mouse.CursorPos.X>ToolBarWidth then DisplayHint(nil,[],0,0); //Don't display hints if not over ToolBar

  Minimap_Update(nil);

  if Panel_Stats.Visible then Stats_Fill(nil);
end;


procedure TKMapEdInterface.TerrainHeight_Change(Sender: TObject);
begin
  if Sender = HeightCircle then
  begin
    HeightCircle.Down := true;
    HeightSquare.Down := false;
    CursorMode.Mode  := cm_Height;
    CursorMode.Tag1 := HeightSize.Position;
    CursorMode.Tag2 := MAPED_HEIGHT_CIRCLE;
  end;
  if Sender = HeightSquare then
  begin
    HeightSquare.Down := true;
    HeightCircle.Down := false;
    CursorMode.Mode  := cm_Height;
    CursorMode.Tag1 := HeightSize.Position;
    CursorMode.Tag2 := MAPED_HEIGHT_SQUARE;
  end;
  if Sender = HeightSize then
    CursorMode.Tag1 := HeightSize.Position;
end;


procedure TKMapEdInterface.TerrainTiles_Change(Sender: TObject);
var i,k:integer;
begin
  if Sender = TilesScroll then //Shift tiles
    for i:=1 to MAPED_TILES_COLS do
    for k:=1 to MAPED_TILES_ROWS do
      TilesTable[(i-1)*MAPED_TILES_ROWS+k].TexID := (TilesScroll.Position*MAPED_TILES_ROWS+(i-1)*MAPED_TILES_ROWS+k)mod 8+2; //icons are in 2..9
  if Sender is TKMButtonFlat then
  begin
    CursorMode.Mode := cm_Tiles;
    CursorMode.Tag1 := EnsureRange(TilesScroll.Position*MAPED_TILES_ROWS + TKMButtonFlat(Sender).Tag, 0, 247); //Offset+Tag without road overlays?
    CursorMode.Tag2 := 0;
  end;
end;


procedure TKMapEdInterface.TerrainObjects_Change(Sender: TObject);
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
      ObjectsTable[i].Down := ObjID = OriginalMapElem[CursorMode.Tag1+1]; //Mark the selected one using reverse lookup
      ObjectsTable[i].Caption := inttostr(ObjID);
    end;
    ObjectErase.Down := (CursorMode.Tag1 = 255); //or delete button
  end;

  if Sender is TKMButtonFlat then
  begin
    CursorMode.Mode := cm_Objects;
    ObjID := ObjectsScroll.Position*2-1 + (TKMButtonFlat(Sender).Tag-1); //1..n
    if TKMButtonFlat(Sender).Tag = 255 then
      CursorMode.Tag1 := 255 //erase object
    else
      CursorMode.Tag1 := ActualMapElem[ObjID]-1; //0..n
    CursorMode.Tag2 := 0;
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
  CursorMode.Mode:=cm_None;
  CursorMode.Tag1:=0;
  CursorMode.Tag2:=0;
  Label_Build.Caption := '';

  if Button_BuildCancel.Down then begin
    CursorMode.Mode:=cm_Erase;
    Label_Build.Caption := fTextLibrary.GetTextString(210);
  end;
  if Button_BuildRoad.Down then begin
    CursorMode.Mode:=cm_Road;
    Label_Build.Caption := fTextLibrary.GetTextString(212);
  end;
  if Button_BuildField.Down then begin
    CursorMode.Mode:=cm_Field;
    Label_Build.Caption := fTextLibrary.GetTextString(214);
  end;
  if Button_BuildWine.Down then begin
    CursorMode.Mode:=cm_Wine;
    Label_Build.Caption := fTextLibrary.GetTextString(218);
  end;
{  if Button_BuildWall.Down then begin
    CursorMode.Mode:=cm_Wall;
    Label_BuildCost_Wood.Caption:='1';
    //Label_Build.Caption := fTextLibrary.GetTextString(218);
  end;}

  for i:=1 to HOUSE_COUNT do
  if GUIHouseOrder[i] <> ht_None then
  if Button_Build[i].Down then begin
     CursorMode.Mode:=cm_Houses;
     CursorMode.Tag1:=byte(GUIHouseOrder[i]);
     Label_Build.Caption := TypeToString(THouseType(byte(GUIHouseOrder[i])));
  end;
end;


procedure TKMapEdInterface.Unit_ButtonClick(Sender: TObject);
var i:integer;
begin
  if Sender=nil then begin CursorMode.Mode:=cm_None; exit; end;

  //Release all buttons
  for i:=1 to Panel_Units.ChildCount do
    if Panel_Units.Childs[i] is TKMButtonFlat then
      TKMButtonFlat(Panel_Units.Childs[i]).Down := false;

  //Press the button
  TKMButtonFlat(Sender).Down:=true;

  //Reset cursor and see if it needs to be changed
  CursorMode.Mode:=cm_None;
  CursorMode.Tag1:=0;
  CursorMode.Tag2:=0;
  //Label_Build.Caption := '';

  if Button_UnitCancel.Down then begin
    CursorMode.Mode:=cm_Erase;
    //Label_Build.Caption := fTextLibrary.GetTextString(210);
  end;

  if (TKMButtonFlat(Sender).Tag in [byte(ut_Serf)..byte(ut_Duck)]) then
  begin
    CursorMode.Mode := cm_Units;
    CursorMode.Tag1 := byte(TKMButtonFlat(Sender).Tag);
    Label_Units.Caption := TypeToString(TUnitType(byte(TKMButtonFlat(Sender).Tag)));
  end;

end;


procedure TKMapEdInterface.ShowHouseInfo(Sender:TKMHouse);
var i:integer; WasShown: boolean;
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

  for i:=1 to Panel_House.ChildCount do
    Panel_House.Childs[i].Show; //show all
  Image_House_Worker.Visible := TUnitType(HouseDAT[byte(Sender.GetHouseType)].OwnerType+1) <> ut_None;
  SwitchPage(Panel_House);

  case Sender.GetHouseType of
    ht_Store: begin
          Store_Fill(nil);
          WasShown := Panel_HouseStore.Visible;
          SwitchPage(Panel_HouseStore);
          if not WasShown then
            Store_SelectWare(Button_Store[StorehouseItem]); //Reselect the ware so the display is updated
        end;

    ht_Barracks: begin
          Image_House_Worker.Enabled := true; //In the barrack the recruit icon is always enabled
          SwitchPage(Panel_HouseBarracks);
          end;
    ht_TownHall:;
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
  Label_UnitName.Caption:=TypeToString(Sender.GetUnitType);
  Image_UnitPic.TexID:=520+byte(Sender.GetUnitType);
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
    Label_UnitDescription.Caption := fTextLibrary.GetTextString(siUnitDescriptions+byte(Sender.GetUnitType));
    Label_UnitDescription.Show;
  end;
end;


procedure TKMapEdInterface.Menu_Save(Sender:TObject);
begin
  if Sender = TextEdit_SaveName then begin
    CheckBox_SaveExists.Enabled := CheckFileExists(KMRemakeMapPath(TextEdit_SaveName.Text,'dat'), true);
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
    fGame.SaveMapEditor(TextEdit_SaveName.Text, true);
    SwitchPage(Button_SaveCancel); //return to previous menu
  end;

  if Sender = Button_SaveCancel then begin
    SwitchPage(Button_SaveCancel);
  end;
end;


{Show mission loading dialogue}
procedure TKMapEdInterface.Menu_Load(Sender:TObject);
begin
  //@Lewin:
  //We have 3 options here:
  // 1. Use VCL FileOpen dialogue
  // 2. Write our own file selection control
  // 3. Use SingleMap folder scanning technique and list only those maps that are in Maps folder
  // I prefer no.1
  //@Krom: That sounds ok, most map editors for games use windows style controls so there's no point
  //       in making our own control.
  //@Lewin: Lil update, I prefer 1. until we implement 3. ;)
end;


{Quit the mission and return to main menu}
procedure TKMapEdInterface.Menu_QuitMission(Sender:TObject);
var i:integer;
begin
  Panel_Main.Hide;
  for i:=1 to Panel_Main.ChildCount do
    if Panel_Main.Childs[i] is TKMPanel then
      Panel_Main.Childs[i].Hide;

  fGame.StopGame(gr_MapEdEnd);
end;


{Virtually press BuildRoad button when changing page to BuildingPage or after house plan is placed}
procedure TKMapEdInterface.Build_SelectRoad;
begin
  Build_ButtonClick(Button_BuildRoad);
end;


//This function will be called if the user right clicks on the screen.
procedure TKMapEdInterface.RightClick_Cancel;
begin
  //We should drop the tool but don't close opened tab
  if GetShownPage = esp_Terrain then exit; //Terrain uses both buttons for relief changing
  CursorMode.Mode:=cm_None;
  CursorMode.Tag1:=0;
  CursorMode.Tag2:=0;
end;


procedure TKMapEdInterface.Store_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  if fPlayers.Selected=nil then exit;
  if not (fPlayers.Selected is TKMHouseStore) then exit;
  for i:=1 to 28 do begin
    Tmp:=TKMHouseStore(fPlayers.Selected).ResourceCount[i];
    if Tmp=0 then Button_Store[i].Caption:='-' else
    //if Tmp>999 then Button_Store[i].Caption:=float2fix(round(Tmp/10)/100,2)+'k' else
                  Button_Store[i].Caption:=inttostr(Tmp);
  end;
end;


procedure TKMapEdInterface.Stats_Fill(Sender:TObject);
var i,Tmp:integer;
begin
  for i:=low(StatHouse) to high(StatHouse) do
  begin
    Tmp:=MyPlayer.GetHouseQty(StatHouse[i]);
    if Tmp=0 then Stat_HouseQty[i].Caption:='-' else Stat_HouseQty[i].Caption:=inttostr(Tmp);
    if MyPlayer.GetCanBuild(StatHouse[i]) or (Tmp>0) then
    begin
      Stat_HousePic[i].TexID:=byte(StatHouse[i])+300;
      Stat_HousePic[i].Hint:=TypeToString(StatHouse[i]);
      Stat_HouseQty[i].Hint:=TypeToString(StatHouse[i]);
    end
    else
    begin
      Stat_HousePic[i].TexID:=41;
      Stat_HousePic[i].Hint:=fTextLibrary.GetTextString(251); //Building not available
      Stat_HouseQty[i].Hint:=fTextLibrary.GetTextString(251); //Building not available
    end;
  end;
  for i:=low(StatUnit) to high(StatUnit) do
  begin
    Tmp:=MyPlayer.GetUnitQty(StatUnit[i]);
    if Tmp=0 then Stat_UnitQty[i].Caption:='-' else Stat_UnitQty[i].Caption:=inttostr(Tmp);
    Stat_UnitPic[i].Hint:=TypeToString(StatUnit[i]);
    Stat_UnitQty[i].Hint:=TypeToString(StatUnit[i]);
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
end;


procedure TKMapEdInterface.Unit_ArmyChange(Sender:TObject; AButton:TMouseButton);
var Amt:shortint; Commander:TKMUnitWarrior;
begin
  if ShownUnit = nil then exit;
  if not (ShownUnit is TKMUnitWarrior) then exit;

  Commander := TKMUnitWarrior(ShownUnit).GetCommander;

  Amt := 0;
  if Sender = Button_ArmyDec then Amt := -1; //Decrease
  if Sender = Button_ArmyInc then Amt := 1; //Increase
  if AButton = mbLeft then Amt  := Amt * 1;
  if AButton = mbRight then Amt := Amt * 10;

  Commander.fMapEdMembersCount := EnsureRange(Commander.fMapEdMembersCount + Amt, 0, 200); //max members

  if Sender = Button_Army_ForUp then Commander.UnitsPerRow := max(Commander.UnitsPerRow-1,1);
  if Sender = Button_Army_ForDown then Commander.UnitsPerRow := min(Commander.UnitsPerRow+1,Commander.fMapEdMembersCount+1);

  ImageStack_Army.SetCount(Commander.fMapEdMembersCount + 1,Commander.UnitsPerRow);


  if Sender = Button_Army_RotCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)-1);
  if Sender = Button_Army_RotCCW then Commander.Direction := KMLoopDirection(byte(Commander.Direction)+1);

end;


procedure TKMapEdInterface.Store_SelectWare(Sender:TObject);
var i:integer;
begin
  if not Panel_HouseStore.Visible then exit;
  if not (Sender is TKMButtonFlat) then exit; //Only FlatButtons
  if TKMButtonFlat(Sender).Tag = 0 then exit; //with set Tag
  for i:=1 to length(Button_Store) do
    Button_Store[i].Down:=false;
  TKMButtonFlat(Sender).Down := true;
  StorehouseItem := TKMButtonFlat(Sender).Tag;
  Store_EditWareCount(Sender, mbLeft);
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

  Label_Store_WareCount.Caption := inttostr(Store.ResourceCount[byte(Res)]);
end;


procedure TKMapEdInterface.OnKeyUp(Key:Word; IsDown:boolean=false);
begin
  //1-5 game menu shortcuts
  if Key in [49..53] then
  begin
    Button_Main[Key-48].Down := IsDown;
    if not IsDown then SwitchPage(Button_Main[Key-48]);
  end;
end;


procedure TKMapEdInterface.ClearShownUnit;
begin
  ShownUnit := nil;
  SwitchPage(nil);
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
