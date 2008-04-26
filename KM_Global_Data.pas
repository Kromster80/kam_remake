unit KM_Global_Data;

interface

uses
  KM_Terrain, windows, dglOpenGL, KM_Defaults, KM_Tplayer, KM_Render, KM_Viewport, KM_Log;

const
CellSize=40;            //Single cell size in pixels
Overlap=0.001;          //UV position overlap
FPSLag=5;              //lag between frames, 1000/FPSLag = max allowed FPS
FPS_INTERVAL=1000;      //time between FPS measurements, more=accurate
zz=#10+#13;             //EndOfLine

MaxPlayers=6;           //Maximum players per map
MaxHouses=255;          //Maximum houses one player can own

var
  fRender: TRender;
  fViewport: TViewport;
  fMiniMap: TMiniMap;
  fTerrain: TTerrain;
  fLog: TKMLog;


  TreeTex:array[1..256,1..3]of GLuint;     //Object textures
  HouseTex:array[1..2000,1..3]of GLuint;     //Object textures
  UnitTex:array[1..9500,1..3]of GLuint;     //Units [ID,width,height]

  OldTimeFPS,OldFrameTimes,FrameCount:cardinal;

  f,f2:file;
  ft:textfile;
  fsize,NumRead:integer;
  c:array[1..65536] of char;
  ExeDir:string;
  XH:integer=32;        //Height divider
  BrushMode:bmBrushMode;
  LandBrush:integer=0;  //Active brush
  s:string;
  Map: record X,Y:integer; end;
  ResHead: packed record x1:word; Allocated,Qty1,Qty2,x5,Len17:integer; end;
  Res:array[1..1024]of packed record X1,Y1,X2,Y2:integer; Typ:byte; end;
  MouseButton:TMouseButton2;

  MapX,MapY:single; //Precise cursor position on map
  MapXn,MapYn:integer; //Cursor position node
  MapXc,MapYc:integer; //Cursor position cell
  MapXn2,MapYn2:integer; //keeps previous node position
  MapXc2,MapYc2:integer; //keeps previous cell position

  Mission:TMission;

TreeQty:integer;
TreePal:array[1..512] of byte;
TreeSize:array[1..512,1..2] of word;
TreePivot:array[1..512] of record x,y:integer; end;
TreeData:array[1..512] of array of byte;
Pal0:array[1..256,1..3]of byte;

HouseQty:integer;
HousePal:array[1..2000] of byte;
HouseBMP:array[0..2000] of word;
HouseSize:array[1..2000,1..2] of word;
HousePivot:array[1..2000] of record x,y:integer; end;
HouseData:array[1..2000] of array of byte;
HouseID:array[1..2000] of word;

UnitQty:integer;
UnitPal:array[1..9500] of byte;
UnitSize:array[1..9500,1..2] of word;
UnitPivot:array[1..9500] of record x,y:integer; end;
UnitData:array[1..9500] of array of byte;
UnitID:array[1..9500] of word;

HouseDAT1:array[1..30,1..35]of smallint;
HouseDAT:array[1..30] of packed record
  Stone,Wood,WoodPal,StonePal:smallint;
  SupplyIn:array[1..4,1..5]of smallint;
  SupplyOut:array[1..4,1..5]of smallint;
  Anim:array[1..19] of record
    Step:array[1..30]of smallint;
    Count,MoveX,x3,MoveY,x5:smallint;
  end;
  Foot:array[1..135]of smallint;
end;

UnitCarry:array[1..28] of record
  Dir:array[1..8]of record
    Step:array[1..30]of smallint;
    Count,MoveX,x3,MoveY,x5:smallint;
  end;
end;

//1  Trunk      //2  Stone      //3  Wood       //4  IronOre
//5  GoldOre    //6  Coal       //7  Steel      //8  Gold
//9  Wine       //10 Corn       //11 Bread      //12 Flour
//13 Leather    //14 Sousages   //15 Pig        //16 Skin
//17 WoodShield //18 MetalShield//19 Armor      //20 MetalArmor
//21 Axe        //22 Sword      //23 Pike       //24 Hallebard
//25 Bow        //26 Arbalet    //27 Horse      //28 FishBucket

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

//0  Walk
//8  *Special (digging hole)
//16
//24 Death
//32
//40
//48
//56 Eating (food types)
//64 ??
//72 *Special (carrying plant)

MapElemQty:integer=254; //Default qty
MapElem:array[1..512]of packed record
Tree:array[1..30]of smallint; //60
animqty:word;                 //62
u1:array[1..16]of word;       //94
u2:shortint;                  //95
u3,u4:word;                   //99
end;

  ActiveTileName:TObject; //Object (Brush) that was pressed last, should be released on tab change
  MiniMapSpy:boolean=false;
  MousePressed:boolean=false;

implementation

end.
