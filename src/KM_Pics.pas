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
    rxCustom, //Used for loading stuff like campaign maps (there is no main RXX file)
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
