unit KM_Controls;
interface
uses Controls, KM_Classes, KM_RenderUI;

type
TKMControl = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  constructor Create();
  procedure Paint(); virtual; abstract;
end;


TKMPanel = class(TKMControl)
  public
    TexID: integer;
//  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
  procedure Paint(); override;
end;

TKMButton = class(TKMControl)
  public
    TexID: integer;
  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
  procedure Paint(); override;
end;


TKMControlsCollection = class(TKMList)
  private
    fControl: TKMControl;
  public
    constructor Create;
    procedure Add(aLeft,aTop,aWidth,aHeight,aTexID:integer);
    procedure Paint();
end;

var
    fRenderUI: TRenderUI;

implementation

constructor TKMControl.Create();
begin
end;



constructor TKMButton.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create;
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
  TexID:=aTexID;
end;

procedure TKMButton.Paint();
begin
  fRenderUI.Write3DButton(TexID,Left,Top,Width,Height);
end;

procedure TKMPanel.Paint();
begin
  fRenderUI.WritePic(TexID,Left,Top);
end;

constructor TKMControlsCollection.Create();
begin
  fRenderUI:= TRenderUI.Create;
end;

procedure TKMControlsCollection.Add(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Add(TKMButton.Create(aLeft,aTop,aWidth,aHeight,aTexID));
end;

procedure TKMControlsCollection.Paint();
  var i:integer;
begin
  for i:=0 to Count-1 do
    TKMControl(Items[I]).Paint;
end;

end.
