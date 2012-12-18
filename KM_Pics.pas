unit KM_Pics;
{$I KaM_Remake.inc}
interface


type
  TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxGuiMainH, //Unused, I have moved all used sprites into rxGuiMain
    rxTiles //Tiles
    );

  TKMPic = record
    RX: TRXType;
    ID: Word;
  end;

  function MakePic(aRX: TRXType; aIndex: Word): TKMPic;


implementation


function MakePic(aRX: TRXType; aIndex: Word): TKMPic;
begin
  Result.RX := aRX;
  Result.ID := aIndex;
end;


end.
