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
