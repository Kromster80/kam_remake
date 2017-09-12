unit KM_ResCursors;
{$I KaM_Remake.inc}
interface
uses
  Classes, Forms,
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Graphics, Math, SysUtils,
  KM_Points, KM_ResSprites;

type
  TKMCursor = (
    kmc_Default, kmc_Info, kmc_Attack, kmc_JoinYes, kmc_JoinNo, kmc_Edit, kmc_DragUp,
    kmc_Dir0, kmc_Dir1, kmc_Dir2, kmc_Dir3, kmc_Dir4, kmc_Dir5, kmc_Dir6, kmc_Dir7, kmc_DirNA,
    kmc_Scroll0, kmc_Scroll1, kmc_Scroll2, kmc_Scroll3, kmc_Scroll4, kmc_Scroll5, kmc_Scroll6, kmc_Scroll7,
    kmc_Beacon, kmc_Drag, kmc_PaintBucket, kmc_Invisible);


  TKMResCursors = class
  private
    fRXData: PRXData; //Store pointer to record instead of duplicating it
    function GetCursor: TKMCursor;
    procedure SetCursor(Value: TKMCursor);
  public
    procedure MakeCursors(aSprites: TKMSpritePack);
    property Cursor: TKMCursor read GetCursor write SetCursor;
    function CursorOffset(aDir: TKMDirection): TKMPoint;
    function CursorTexID(aDir: TKMDirection): Integer;
  end;


implementation
uses
  KM_Defaults;

const
  //Screen.Cursors[0] is used by System default cursor
  COUNT_OFFSET = 1;

  //Indexes of cursor images in GUI.RX
  Cursors: array [TKMCursor] of Word = (
    1, 452, 457, 460, 450, 453, 449,
    511,  512, 513, 514, 515, 516, 517, 518, 519,
    4, 7, 3, 9, 5, 8, 2, 6,
    456, 451, 661, 999);

  //Which cursor is used for which direction
  TKMCursorDirections: array [TKMDirection] of TKMCursor = (
    kmc_DirNA, kmc_Dir0, kmc_Dir1, kmc_Dir2, kmc_Dir3, kmc_Dir4, kmc_Dir5, kmc_Dir6, kmc_Dir7);


{ TKMResCursors }
function TKMResCursors.GetCursor: TKMCursor;
begin
  if InRange(Screen.Cursor - COUNT_OFFSET, Byte(Low(TKMCursor)), Byte(High(TKMCursor))) then
    Result := TKMCursor(Screen.Cursor - COUNT_OFFSET)
  else
    Result := kmc_Default;
end;


procedure TKMResCursors.SetCursor(Value: TKMCursor);
begin
  if SKIP_LOADING_CURSOR then Exit;
  Screen.Cursor := Byte(Value) + COUNT_OFFSET;
end;


procedure TKMResCursors.MakeCursors(aSprites: TKMSpritePack);
const
  SF = 17; //Full width/height of a scroll cursor
  SH = 8; //Half width/height of a scroll cursor
  //Measured manually
  CursorOffsetsX: array [TKMCursor] of Integer = (0,0,20, 0, 0,-8, 9,0, 1,1,1,0,-1,-1,-1,0, SH,SF,SF,SF,SH, 0, 0,0, 0,0,27,0);
  CursorOffsetsY: array [TKMCursor] of Integer = (0,9,10,18,20,44,13,0,-1,0,1,1, 1, 0,-1,0, 0 ,0 ,SH,SF,SF,SF,SH,0,28,0,28,0);
var
  KMC: TKMCursor;
  sx,sy,x,y: Integer;
  bm,bm2: TBitmap;
  IconInfo: TIconInfo;
  Px: PRGBQuad;
begin
  if SKIP_RENDER then Exit;

  fRXData := @aSprites.RXData;

  bm  := TBitmap.Create; bm.HandleType  := bmDIB; bm.PixelFormat  := pf32bit;
  bm2 := TBitmap.Create; bm2.HandleType := bmDIB; bm2.PixelFormat := pf32bit;

  for KMC := Low(KMC) to High(KMC) do
  begin

    //Special case for invisible cursor
    if KMC = kmc_Invisible then
    begin
      bm.Width  := 1; bm.Height  := 1;
      bm2.Width := 1; bm2.Height := 1;
      bm2.Canvas.Pixels[0,0] := clWhite; //Invisible mask, we don't care for Image color
      IconInfo.xHotspot := 0;
      IconInfo.yHotspot := 0;
    end
    else
    begin
      sx := fRXData.Size[Cursors[KMC]].X;
      sy := fRXData.Size[Cursors[KMC]].Y;
      bm.Width  := sx; bm.Height  := sy;
      bm2.Width := sx; bm2.Height := sy;

      for y:=0 to sy-1 do
      begin
        Px := bm.ScanLine[y];
        for x:=0 to sx-1 do
        begin
          if fRXData.RGBA[Cursors[KMC],y*sx+x] and $FF000000 = 0 then
            Px.rgbReserved := $00
          else
            Px.rgbReserved := $FF;
          // Here we have BGR, not RGB
          Px.rgbBlue := (fRXData.RGBA[Cursors[KMC],y*sx+x] and $FF0000) shr 16;
          Px.rgbGreen := (fRXData.RGBA[Cursors[KMC],y*sx+x] and $FF00) shr 8;
          Px.rgbRed := fRXData.RGBA[Cursors[KMC],y*sx+x] and $FF;
          inc(Px);
        end;
      end;
      //Load hotspot offsets from RX file, adding the manual offsets (normally 0)
      IconInfo.xHotspot := Math.max(-fRXData.Pivot[Cursors[KMC]].x+CursorOffsetsX[KMC],0);
      IconInfo.yHotspot := Math.max(-fRXData.Pivot[Cursors[KMC]].y+CursorOffsetsY[KMC],0);
    end;

    //Release the Mask, otherwise there is black rect in Lazarus
    //it works only from within the loop, means mask is recreated when we access canvas or something like that
    bm2.ReleaseMaskHandle;

    IconInfo.fIcon := false; //true=Icon, false=Cursor
    IconInfo.hbmColor := bm.Handle;

    //I have a suspicion that maybe Windows could create icon delayed, at a time when bitmap data is
    //no longer valid (replaced by other bitmap or freed). Hence issues with transparency.
    {$IFDEF MSWindows}
      IconInfo.hbmMask  := bm2.Handle;
      Screen.Cursors[Byte(KMC) + COUNT_OFFSET] := CreateIconIndirect(IconInfo);
    {$ENDIF}
    {$IFDEF Unix}
      bm2.Mask(clWhite);
      IconInfo.hbmMask  := bm2.MaskHandle;
      Screen.Cursors[Byte(KMC) + COUNT_OFFSET] :=  CreateIconIndirect(@IconInfo);
    {$ENDIF}
  end;

  bm.Free;
  bm2.Free;
end;


// Return cursor offset for given direction, which is a signed(!) value
function TKMResCursors.CursorOffset(aDir: TKMDirection): TKMPoint;
begin
  Result.X := fRXData.Pivot[Cursors[TKMCursorDirections[aDir]]].X;
  Result.Y := fRXData.Pivot[Cursors[TKMCursorDirections[aDir]]].Y;
end;


// Sprite index of the cursor
function TKMResCursors.CursorTexID(aDir: TKMDirection): Integer;
begin
  Result := Cursors[TKMCursorDirections[aDir]];
end;


end.
