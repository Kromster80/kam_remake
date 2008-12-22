unit KM_GamePlayInterface;
interface
uses KM_Defaults, KM_Houses, Forms, Graphics, Windows, SysUtils, KromUtils;

procedure ShowHouseInfo(KMHouse:TKMHouse);

implementation
uses KM_Unit1;

procedure ShowHouseInfo(KMHouse:TKMHouse);
var i:integer;
begin
  Form1.Label6.Caption:=TypeToString(KMHouse.GetHouseType);
  case KMHouse.GetHouseType of
    ht_Store:
      begin

      

      end;
    ht_Barracks:;
    ht_School:;
  else begin
    Form1.Label3.Caption:=TypeToString(HouseOutput[byte(KMHouse.GetHouseType),1]);
    Form1.Image2.Canvas.FillRect(Form1.Image2.ClientRect);
    for i:=1 to KMHouse.CheckResOut(HouseOutput[byte(KMHouse.GetHouseType),1]) do
      Form1.IL_ResourceIcons.Draw(Form1.Image2.Canvas,4+i*20,4,byte(HouseOutput[byte(KMHouse.GetHouseType),1])-1);
  end;
  end;
end;

end.
 