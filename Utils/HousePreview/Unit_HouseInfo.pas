unit Unit_HouseInfo;
interface
uses Graphics, Math, PNGImage;


type
  TKMHouseInfo = class
  private
    fBuildStage:word;
    fColBuf: TBitmap;
    fDest:TCanvas;

    fHeight:word;
    fWidth:word;

    fWoodSteps:byte;
    fStoneSteps:byte;

    PNG:array[1..4] of TPNGObject;
    procedure SetBuildStage(const Value: word);
    function ShadesCount(aImage:TPNGObject):byte;
  public
    constructor Create(aDest:TCanvas);
    destructor Destroy; override;

    property BuildStage:word read fBuildStage write SetBuildStage;
    property WoodSteps:byte read fWoodSteps;
    property StoneSteps:byte read fStoneSteps;

    procedure Draw;
  end;


implementation


{ TKMHouseInfo }
constructor TKMHouseInfo.Create(aDest:TCanvas);
begin
  Inherited Create;

  fDest := aDest;

  fColBuf := TBitmap.Create;
  fColBuf.PixelFormat := pf24bit;
  fColBuf.Width := 300;
  fColBuf.Height := 200;
  fColBuf.HandleType := bmDIB;

  PNG[1] := TPNGObject.Create;
  PNG[2] := TPNGObject.Create;
  PNG[3] := TPNGObject.Create;
  PNG[4] := TPNGObject.Create;
  PNG[1].LoadFromFile('2_0151.png');
  PNG[2].LoadFromFile('2_0152.png');
  PNG[3].LoadFromFile('2_0153.png');
  PNG[4].LoadFromFile('2_0154.png');

  fWoodSteps := ShadesCount(PNG[3]);
  fStoneSteps := ShadesCount(PNG[4]);

  fHeight := PNG[1].Height;
  fWidth := PNG[1].Width;
end;


destructor TKMHouseInfo.Destroy;
begin
  PNG[1].Free;
  PNG[2].Free;
  PNG[3].Free;
  PNG[4].Free;
  fColBuf.Free;

  inherited;
end;


procedure TKMHouseInfo.Draw;
var i,k:integer;
begin

  fColBuf.Canvas.Brush.Color := PNG[2].Pixels[0,0];
  fColBuf.Canvas.FillRect(fColBuf.Canvas.ClipRect);

  for i:=0 to fHeight-1 do for k:=0 to fWidth-1 do
    if fBuildStage - fWoodSteps > (PNG[4].Pixels[k,i] AND $FF) then
      fColBuf.Canvas.Pixels[k,i] := PNG[1].Pixels[k,i]
    else
    if fBuildStage > (PNG[3].Pixels[k,i] AND $FF) then
      fColBuf.Canvas.Pixels[k,i] := PNG[2].Pixels[k,i];

  fDest.Draw(0,0,fColBuf);
end;


procedure TKMHouseInfo.SetBuildStage(const Value: word);
begin
  fBuildStage := Value;
  Draw;
end;


function TKMHouseInfo.ShadesCount(aImage: TPNGObject): byte;
var
  i,k:integer;
  Shades:array[0..255]of boolean;
begin
  FillChar(Shades, SizeOf(Shades), #0);
  for i:=0 to aImage.Height-1 do for k:=0 to aImage.Width-1 do
    Shades[aImage.Pixels[k,i] AND $FF] := True;

  Result := 0;
  for i:=0 to 255 do
    if Shades[i] then inc(Result);
end;

end.
