unit KM_ResourceMapElements;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Pics, KM_Render, KM_TextLibrary;


var
  //Trees and other terrain elements properties
  MapElemQty:integer=254; //Default qty
  ActualMapElemQty:integer; //Usable qty read from RX file
  ActualMapElem:array[1..254]of integer; //pointers to usable MapElem's
  OriginalMapElem:array[1..256]of integer; //pointers of usable MapElem's back to map objects. (reverse lookup to one above) 256 is no object.
  MapElem:array[1..512]of packed record
    Step:array[1..30]of smallint;           //60
    Count:word;                             //62
    MoveX,MoveY:integer;                    //70
    CuttableTree:longbool;                  //This tree can be cut by a woodcutter
    DiagonalBlocked:longbool;               //Can't walk diagonally accross this object (mainly trees)
    AllBlocked:longbool;                    //All passibility blocked. Can't walk, build, swim, etc.
    WineOrCorn:longbool;                    //Draw multiple (4 or 2) sprites per object (corn or grapes)
    CanGrow:longbool;                       //This object can grow (i.e. change to another object)
    DontPlantNear:longbool;                 //This object can't be planted within one tile of
    Stump:shortint;                         //95 Tree stump
    CanBeRemoved:longbool;                  //99 //Can be removed in favor of building house
  end;


type
  TKMMapElements = class
  private

  public

    function LoadMapElements(const FileName: string): Boolean;

  end;


implementation
uses KromUtils, KM_Log, Types, StrUtils, KM_BinPacking;



{ TKMMapElements }
//Reading map elements (has animation data)
function TKMMapElements.LoadMapElements(const FileName: string): Boolean;
var ii,kk:integer; ft:textfile; f:file;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);
  blockread(f,MapElem[1],MapElemQty*99);
  closefile(f);

  ActualMapElemQty:=0;
  for ii:=1 to MapElemQty do
  if (MapElem[ii].Count>0)and(MapElem[ii].Step[1]>0) then
  begin
    inc(ActualMapElemQty);
    ActualMapElem[ActualMapElemQty] := ii; //pointer
    OriginalMapElem[ii] := ActualMapElemQty; //Reverse lookup
  end;

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Trees.txt'); rewrite(ft);
    for ii:=1 to MapElemQty do begin
    writeln(ft);
    writeln(ft,inttostr(ii)+' :'+inttostr(MapElem[ii].Count));
      for kk:=1 to 30 do if MapElem[ii].Step[kk]>0 then
      write(ft,MapElem[ii].Step[kk],'.') else write(ft,'_.');

      writeln(ft);
//      for kk:=1 to 16 do
//      write(ft,MapElem[ii].CuttableTree,''); //Those are 1/0 so we can ommit space between them

      write(ft,' =',MapElem[ii].CanBeRemoved);
      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;

end.
