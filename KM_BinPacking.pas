unit KM_BinPacking;
{$I KaM_Remake.inc}
interface
uses Types;

type
  TIndexSizeArray = array of record
    ID, X, Y: Word;
  end;

  TBinItem = packed record
    Width, Height: Word;
    Sprites: array of record
      SpriteID: Word;
      PosX, PosY: Word;
    end;
  end;

  TBinArray = array of TBinItem;


  procedure BinPack(aItems: TIndexSizeArray; aPad: Byte; var aOut: TBinArray);


implementation
uses KromUtils;


procedure BinPack(aItems: TIndexSizeArray; aPad: Byte; var aOut: TBinArray);
var I: Integer;
begin
  SetLength(aOut, Length(aItems));

  for I := 0 to High(aItems) do
  begin
    aOut[I].Width := MakePOT(aItems[I].X + aPad * 2);
    aOut[I].Height := MakePOT(aItems[I].Y + aPad * 2);

    SetLength(aOut[I].Sprites, 1);

    aOut[I].Sprites[0].SpriteID := aItems[I].ID;
    aOut[I].Sprites[0].PosX := aPad;
    aOut[I].Sprites[0].PosY := aPad;
  end;
end;

end.
