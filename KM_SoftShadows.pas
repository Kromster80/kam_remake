unit KM_SoftShadows;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils, Math, KM_ResourceSprites;

type
  TKMSoftShadowConverter = class
  private
    fSpritePack: TKMSpritePack;
    function ReadPixelSafe(ID, X, Y: Integer): Cardinal;

    function IsBlack(Color:Cardinal):Boolean;
    function IsTransparent(Color:Cardinal):Boolean;
    function IsObject(Color:Cardinal):Boolean;
    function IsTransparentOrObject(Color:Cardinal):Boolean;

    function IsShadow(ID, X, Y: Integer): Boolean;

    function GetRXData:TRXData;
    property RXData:TRXData read GetRXData;
  public
    constructor Create(aSpritePack: TKMSpritePack);
    procedure ConvertShadows(ID:Integer);
  end;


implementation
const
  BLUR_RADIUS = 2.0; //Amount of blurring applied on shadow edges
  SHADING_LEVEL = 150.0; //Alpha value for full shadow (0..255)

constructor TKMSoftShadowConverter.Create(aSpritePack: TKMSpritePack);
begin
  fSpritePack := aSpritePack;
end;


function TKMSoftShadowConverter.GetRXData:TRXData;
begin
  Result := fSpritePack.RXData;
end;


function TKMSoftShadowConverter.ReadPixelSafe(ID, X, Y: Integer): Cardinal;
begin
  if (X < 0) or (Y < 0) or (X >= RXData.Size[ID].X) or (Y >= RXData.Size[ID].Y) then
    Result := 0
  else
    Result := RXData.RGBA[ID, Y*RXData.Size[ID].X + X];
end;

//Maybe the definition of black will change later (to include almost black colors?)
function TKMSoftShadowConverter.IsBlack(Color:Cardinal):Boolean;
begin
  Result := (Color = $FF000000);
end;

//Maybe the definition of transparent will change later
function TKMSoftShadowConverter.IsTransparent(Color:Cardinal):Boolean;
begin
  Result := (Color = $00000000);
end;

//Pixels that are not transparent and not black are an object (part of actual sprite)
function TKMSoftShadowConverter.IsObject(Color:Cardinal):Boolean;
begin
  Result := not IsTransparent(Color) and not IsBlack(Color);
end;

function TKMSoftShadowConverter.IsTransparentOrObject(Color:Cardinal):Boolean;
begin
  Result := IsTransparent(Color) or not IsBlack(Color);
end;

function TKMSoftShadowConverter.IsShadow(ID, X, Y: Integer): Boolean;
var Color, LeftColor, RightColor, TopColor, BottomColor: Cardinal;
begin
  Color       := ReadPixelSafe(ID, X,   Y);
  LeftColor   := ReadPixelSafe(ID, X-1, Y);
  RightColor  := ReadPixelSafe(ID, X+1, Y);
  TopColor    := ReadPixelSafe(ID, X,   Y-1);
  BottomColor := ReadPixelSafe(ID, X,   Y+1);

  Result := False;

  if IsBlack(Color) then
  begin
    if IsTransparent(LeftColor) or IsTransparent(RightColor) or
       IsTransparent(TopColor)  or IsTransparent(BottomColor) then
      Result := (
                  Byte(IsTransparentOrObject(LeftColor)) +
                  Byte(IsTransparentOrObject(RightColor)) +
                  Byte(IsTransparentOrObject(TopColor)) +
                  Byte(IsTransparentOrObject(BottomColor))
                ) > 2;
  end
  else
    if IsTransparent(Color) then
    begin
      if IsBlack(LeftColor) or IsBlack(RightColor) or
         IsBlack(TopColor)  or IsBlack(BottomColor) then
        Result := (
                    Byte(not IsTransparent(LeftColor)) +
                    Byte(not IsTransparent(RightColor)) +
                    Byte(not IsTransparent(TopColor)) +
                    Byte(not IsTransparent(BottomColor))
                  ) > 2;
    end;
end;

procedure TKMSoftShadowConverter.ConvertShadows(ID:Integer);
var
  TempShadowMap: array of array of Boolean;
  ShadowMap: array of array of Boolean;

  function ReadTempShadowMapSafe(X, Y: Integer): Boolean;
  begin
    if (X < 0) or (Y < 0) or (X >= RXData.Size[ID].X) or (Y >= RXData.Size[ID].Y) then
      Result := False
    else
      Result := TempShadowMap[X, Y];
  end;

  function IsShadowOrObject(X, Y: Integer): Boolean;
  begin
    if (X < 0) or (Y < 0) or (X >= RXData.Size[ID].X) or (Y >= RXData.Size[ID].Y) then
      Result := False
    else
      Result := TempShadowMap[X, Y] or IsObject(ReadPixelSafe(ID,X,Y));
  end;

  function ShadowsNearby(X,Y: Integer):Byte;
  begin
    Result := 0;
    if ReadTempShadowMapSafe(X-1, Y  ) or
       ReadTempShadowMapSafe(X+1, Y  ) or
       ReadTempShadowMapSafe(X,   Y-1) or
       ReadTempShadowMapSafe(X,   Y+1) then
      Result := Byte(IsShadowOrObject(X-1, Y  )) +
                Byte(IsShadowOrObject(X+1, Y  )) +
                Byte(IsShadowOrObject(X,   Y-1)) +
                Byte(IsShadowOrObject(X,   Y+1));
  end;

  function GetBlurredShadow(X,Y: Integer):Single;
  var
    aX, aY, XDiff, YDiff, BlurCeil: Integer;
    Distance, Multiplier, Divisor, Ret: Single;
    Shadow, WasRealShadow: Boolean;
  begin
    WasRealShadow := False;
    Ret := 0;
    Divisor := 0;
    BlurCeil := Ceil(BLUR_RADIUS);
    for aX := X-BlurCeil to X+BlurCeil do
      for aY := Y-BlurCeil to Y+BlurCeil do
      begin
        XDiff := aX-X;
        YDiff := aY-Y;
        Distance := Sqrt(XDiff*XDiff + YDiff*YDiff);
        Multiplier := BLUR_RADIUS - Distance;

        if Multiplier > 0 then
        begin
          Divisor := Divisor + Multiplier;
          if (aX < 0) or (aY < 0) or (aX >= RXData.Size[ID].X) or (aY >= RXData.Size[ID].Y) then
            Continue;
          Shadow := ShadowMap[aX, aY];
          if Shadow then WasRealShadow := True;
          if not IsTransparent(ReadPixelSafe(ID,aX,aY)) then Shadow := True;
          if Shadow then Ret := Ret + Multiplier;
        end;
      end;
    if not WasRealShadow then
      Result := 0
    else
      Result := Ret/Divisor;
  end;

var
  X,Y: Integer;
  Shadow: Boolean;
  Color: Cardinal;
  RealShadow: Byte;
begin
  SetLength(TempShadowMap, RXData.Size[ID].X, RXData.Size[ID].Y);
  SetLength(ShadowMap,     RXData.Size[ID].X, RXData.Size[ID].Y);

  for X:=0 to RXData.Size[ID].X-1 do
    for Y:=0 to RXData.Size[ID].Y-1 do
      TempShadowMap[X, Y] := IsShadow(ID,X,Y);

  for X:=0 to RXData.Size[ID].X-1 do
    for Y:=0 to RXData.Size[ID].Y-1 do
    begin
      Shadow := TempShadowMap[X, Y];

      if not IsObject(ReadPixelSafe(ID, X, Y)) then
      begin
        if Shadow and (ShadowsNearby(X, Y) = 1) then Shadow := False
        else
          if Shadow and (ShadowsNearby(X, Y) = 3) then Shadow := True;
      end;

      ShadowMap[X, Y] := Shadow;
    end;

  for X:=0 to RXData.Size[ID].X-1 do
    for Y:=0 to RXData.Size[ID].Y-1 do
    begin
      Color := ReadPixelSafe(ID, X, Y);
      if (TempShadowMap[X, Y] or ShadowMap[X, Y] or IsTransparent(Color)) and not IsObject(Color) then
      begin
        RealShadow := Min(Round(GetBlurredShadow(X, Y)*SHADING_LEVEL), 255);
        RXData.RGBA[ID, Y*RXData.Size[ID].X + X] := RealShadow shl 24;
      end;
    end;
end;


end.
