unit KM_Global_Data;

interface

uses
  KM_Terrain, windows, dglOpenGL, KM_Defaults, KM_Tplayer, KM_Render, KM_RenderUI, KM_Viewport, KM_Log, KM_Controls;

const
  CellSize=40;          //Single cell size in pixels (width)
  Overlap=0.001;        //UV position overlap (to avoid edge artefacts in render)
  FPSLag=5;             //lag between frames, 1000/FPSLag = max allowed FPS
  FPS_INTERVAL=1000;    //time between FPS measurements, more=accurate

  MakeTeamColors=true; //Whenever to make team colors or not, saves RAM for debug
  MaxPlayers=8;         //Maximum players per map
  MaxHouses=255;        //Maximum houses one player can own
  MaxResInHouse=5;      //Maximum resource items allowed to be in house (it's 5, but I use 3 for testing)
  MaxTexRes=1024;       //Maximum texture resolution client can handle (should be used for packing sprites)

var
  XH:integer=32;        //Height divider
  GlobalTickCount:integer=0;

  fRender: TRender;
  fControls: TKMControlsCollection;
  //fRenderUI: TRenderUI;
  fViewport: TViewport;
  fMiniMap: TMiniMap;
  fTerrain: TTerrain;
  fLog: TKMLog;

  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  ExeDir:string;

  CursorMode:cmCursorMode;
  LandBrush:integer=0;  //Active brush
  s:string;
  MouseButton:TMouseButton2;

  CursorX,CursorY:single;    //Precise cursor position on map
  CursorXn,CursorYn:integer; //Cursor position node
  CursorXc,CursorYc:integer; //Cursor position cell
  CursorXn2,CursorYn2:integer;     //keeps previous node position
  CursorXc2,CursorYc2:integer;     //keeps previous cell position

  Mission:TMission;

  //Pallete for RX bitmaps
  Pal0:array[1..256,1..3]of byte;

  RXData:array [1..5]of record
    Title:string;
    Qty:integer;
    Pal:array[1..9500] of byte;
    Size:array[1..9500,1..2] of word;
    Pivot:array[1..9500] of record x,y:integer; end;
    Data:array[1..9500] of array of byte;
    NeedTeamColors:boolean;
  end;

  GFXData: array [1..5,1..9500] of record
    TexID,AltID: GLUint; //AltID used for team colors
    u1,v1,u2,v2: single;
    PxWidth,PxHeight:word;
  end;

  FontData:array[1..32]of record
    Title:TKMFont;
    TexID:GLUint;
    Pal:array[0..255]of byte;
    Letters:array[0..255]of record
      Width,Height:word;
      Add:array[1..4]of word;
      Data:array[1..256] of byte;
      u1,v1,u2,v2:single;
    end;
  end;

HouseDAT1:array[1..30,1..35]of smallint;
HouseDAT:array[1..30] of packed record
  Stone,Wood,WoodPal,StonePal:smallint;
  SupplyIn:array[1..4,1..5]of smallint;
  SupplyOut:array[1..4,1..5]of smallint;
  Anim:array[1..19] of record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;
  Foot:array[1..135]of smallint;
end;

UnitCarry:array[1..28] of packed record
  Dir:array[1..8]of packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;
end;

UnitStat:array[1..41]of record x1,Attack,AttackHorseBonus,x4,HitPoints,Speed,x7,Sight{?},x9,x10,x11:smallint; end;
UnitSprite2:array[1..41,1..18]of smallint;
UnitSprite:array[1..41]of packed record
  Act:array[1..14]of packed record
    Dir:array[1..8]of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;
  end;
end;

  MapElemQty:integer=254; //Default qty
  MapElem:array[1..512]of packed record
    Step:array[1..30]of smallint; //60
    Count:word;                   //62
    u1:array[1..16]of word;       //94
    u2:shortint;                  //95
    u3,u4:word;                   //99
  end;

  ActiveTileName:TObject; //Object (Brush) that was pressed last, should be released on tab change
  MiniMapSpy:boolean=false;
  MousePressed:boolean=false;

  TileMMColor:array[1..256]of record R,G,B:byte; end;

implementation

end.
