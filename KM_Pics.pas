unit KM_Pics;
{$I KaM_Remake.inc}
interface
uses
  Classes, Math, SysUtils;

type
  TRXType = (
    rxTrees,
    rxHouses,
    rxUnits,
    rxGui,
    rxGuiMain,
    rxGuiMainH,
    rxMenu, //Remake menu elements
    rxTiles, //Tiles
    rxGame); //Remake game sprites

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
