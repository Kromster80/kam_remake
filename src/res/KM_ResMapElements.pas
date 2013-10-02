unit KM_ResMapElements;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, KromUtils,
  KM_CommonTypes, KM_Defaults;


type
  TKMMapElement = packed record
    Anim: TKMAnimLoop;          //Animation loop info
    CuttableTree: LongBool;     //This tree can be cut by a woodcutter
    DiagonalBlocked: LongBool;  //Can't walk diagonally accross this object (mainly trees)
    AllBlocked: LongBool;       //All passibility blocked. Can't walk, build, swim, etc.
    WineOrCorn: LongBool;       //Draw multiple (4 or 2) sprites per object (corn or grapes)
    CanGrow: LongBool;          //This object can grow (i.e. change to another object)
    DontPlantNear: LongBool;    //This object can't be planted within one tile of
    Stump: ShortInt;            //Tree stump ID
    CanBeRemoved: LongBool;     //Can be removed in favor of building house
  end;

  TKMMapElements = class
  private
    fCount: Integer;
    fCRC: Cardinal;
  public
    property Count: Integer read fCount;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure ExportToText(const FileName: string);
    property CRC: Cardinal read fCRC;
  end;


var
  //MapElem is in global access because of the recursive FloodFill algorithm
  //when it uses TKMMapElements.MapElem each call takes 8 times more memory
  //on the stack (View>Debug>CPU>Stack) for reasons unknown to me.
  MapElem: array [Byte] of TKMMapElement;

type
  TChopableAge = (caAge1, caAge2, caAge3, caAgeFull, caAgeFall, caAgeStump);

const
  //Chopable tree, Chopdown animation,
  //Age1, Age2, Age3, Age4, Falling, Stump
  ChopableTrees: array [1..13, TChopableAge] of byte = (
  //For grass
  (  88,  89,  90,  90,  91,  37), //These two are very look alike
  (  97,  98,  99, 100, 101,  41), //yet different in small detail and fall direction
  ( 102, 103, 104, 105, 106,  45),
  ( 107, 108, 109, 110, 111,  41),
  ( 112, 113, 114, 114, 115,  25), //These two are very look alike
  ( 116, 117, 118, 119, 120,  25), //yet different in small detail and fall direction
  //For grass and yellow
  (  92,  93,  94,  95,  96,  49),
  //For yellow soil only
  ( 121, 122, 123, 124, 125,  64),
  //For dirt (pine trees)
  ( 149, 150, 151, 151, 152,  29),
  ( 153, 154, 155, 155, 156,  29),
  ( 157, 158, 159, 160, 161,  33),
  ( 162, 163, 164, 165, 166,  33),
  ( 167, 168, 169, 170, 171,  33));

  //Ages at which trees/fields grow up/change sprite multiplied by TERRAIN_PACE
  TREE_AGE_1 = 2400 div TERRAIN_PACE;
  TREE_AGE_2 = 5000 div TERRAIN_PACE;
  TREE_AGE_FULL = 8000 div TERRAIN_PACE; //Tree is old enough to be chopped

  CORN_AGE_1 = 1400 div TERRAIN_PACE;    //Measured from KaM ~150sec
  CORN_AGE_2 = 2200 div TERRAIN_PACE;   //Number measured from KaM ~195sec
  CORN_AGE_3 = 4400 div TERRAIN_PACE;
  CORN_AGE_FULL = 6400 div TERRAIN_PACE; //Corn ready to be cut
  CORN_AGE_MAX = 255; //todo: Remove. We set it to this once it's fully grown

  //Wine values have been tweaked for balance. In KaM they matched corn.
  WINE_AGE_1 = 1600 div TERRAIN_PACE;
  WINE_AGE_2 = 3400 div TERRAIN_PACE;
  WINE_AGE_FULL = 5000 div TERRAIN_PACE; //Wine ready to be harvested


implementation


{ TKMMapElements }
//Reading map elements properties and animation data
procedure TKMMapElements.LoadFromFile(const FileName: string);
var
  S: TMemoryStream;
begin
  if not FileExists(FileName) then Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(FileName);
  S.Read(MapElem[0], 255 * SizeOf(TKMMapElement));
  fCount := S.Size div SizeOf(TKMMapElement); //254 by default
  fCRC := Adler32CRC(S);
  S.Free;

  MapElem[63].Anim.Count := 1;
  MapElem[63].Anim.Step[1] := 16;
  MapElem[63].CuttableTree := False;
  MapElem[63].DiagonalBlocked := False;
  MapElem[63].AllBlocked := False;
  MapElem[63].WineOrCorn := False;
  MapElem[63].CanGrow := False;
  MapElem[63].DontPlantNear := False;
  MapElem[63].Stump := -1;
  MapElem[63].CanBeRemoved := True;

  //Save ti file if we want to have it there. For now hardcoded is ok
  //SaveToFile(FileName);
end;


procedure TKMMapElements.SaveToFile(const FileName: string);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  S.Write(MapElem[0], fCount * SizeOf(TKMMapElement));
  S.SaveToFile(FileName);
  S.Free;
end;


procedure TKMMapElements.ExportToText(const FileName: string);
var I,K: Integer; ft: TextFile;
begin
  AssignFile(ft, ExeDir + 'Trees.txt');
  Rewrite(ft);
  for I := 1 to fCount do
  begin
    Writeln(ft);
    Writeln(ft, inttostr(I) + ' :' + inttostr(MapElem[I].Anim.Count));
    for K := 1 to 30 do
      if MapElem[I].Anim.Step[K] > 0 then
        Write(ft, MapElem[I].Anim.Step[K], '.')
      else
        Write(ft, '_.');

    Writeln(ft);
    // for K:=1 to 16 do
    // write(ft,MapElem[I].CuttableTree,''); //Those are 1/0 so we can ommit space between them

    Write(ft, ' =', MapElem[I].CanBeRemoved);
    Writeln(ft);
  end;
  CloseFile(ft);
end;


end.
