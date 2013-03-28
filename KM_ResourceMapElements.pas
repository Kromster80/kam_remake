unit KM_ResourceMapElements;
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
    ValidCount: Byte;
    ValidToObject: array [Byte] of Byte; //Pointers to valid MapElem's
    ObjectToValid: array [Byte] of Byte; //Pointers of valid MapElem's back to map objects. (reverse lookup to one above) 256 is no object.
    property Count: Integer read fCount;
    procedure LoadMapElements(const FileName: string);
    procedure ExportToText(const FileName: string);
    property CRC: Cardinal read fCRC;
  end;


var
  //MapElem is in global access because of the recursive FloodFill algorithm
  //when it uses TKMMapElements.MapElem each call takes 8 times more memory
  //on the stack (View>Debug>CPU>Stack) for reasons unknown to me.
  MapElem: array [Byte] of TKMMapElement;


implementation


{ TKMMapElements }
//Reading map elements properties and animation data
procedure TKMMapElements.LoadMapElements(const FileName: string);
var
  I: Integer;
  S: TMemoryStream;
begin
  if not FileExists(FileName) then Exit;

  S := TMemoryStream.Create;
  S.LoadFromFile(FileName);
  S.Read(MapElem[0], 255 * SizeOf(TKMMapElement));
  fCount := S.Size div SizeOf(TKMMapElement); //254 by default
  fCRC := Adler32CRC(S);
  S.Free;

  ValidCount := 0;
  for I := 0 to fCount - 1 do
  if (MapElem[I].Anim.Count > 0) and (MapElem[I].Anim.Step[1] > 0) then
  begin
    ValidToObject[ValidCount] := I; //pointer
    ObjectToValid[I] := ValidCount; //Reverse lookup
    Inc(ValidCount);
  end;
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
