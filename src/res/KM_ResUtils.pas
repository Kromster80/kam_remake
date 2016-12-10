unit KM_ResUtils;
{$I KaM_Remake.inc}
interface
uses KM_ResFonts;

  // Return maximum of the width of specified strings when printed on screen with specified font.
  function GetMaxPrintWidthOfStrings(aStrings: array of string; aFont: TKMFont): Integer;

implementation

uses KM_Resource;

function GetMaxPrintWidthOfStrings(aStrings: array of string; aFont: TKMFont): Integer;
var I, Width: Integer;
begin
  Result := 0;
  for I := Low(aStrings) to High(aStrings) do
  begin
    Width := gRes.Fonts[aFont].GetTextSize(aStrings[I]).X;
    if (Width > Result) then
      Result := Width;
  end;
end;

end.
