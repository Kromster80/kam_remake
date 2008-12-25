unit KM_Controls;
interface
uses Controls, KM_Classes, KM_RenderUI, KromUtils, Math, KM_Defaults;

type TNotifyEvent = procedure(Sender: TObject) of object;


{Base class for all TKM elements}
type
TKMControl = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    FOnClick:TNotifyEvent;
    CursorOver:boolean;
    CursorDown:boolean;
  constructor Create(aLeft,aTop,aWidth,aHeight:integer);
  procedure Paint(); virtual; abstract;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
end;


{Panel with image on it}
TKMPanel = class(TKMControl)
  public
    TexID: integer;
  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
  procedure Paint(); override;
end;


{3DButton}
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
    procedure Add(Sender:TKMControl);
    procedure OnMouseOver(X,Y:integer);
    procedure OnMouseDown(X,Y:integer);
    procedure OnMouseUp(X,Y:integer);
    procedure Paint();
end;

var
    fRenderUI: TRenderUI;

implementation

constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
end;

constructor TKMButton.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
end;


procedure TKMButton.Paint();
var State:T3DButtonState;
begin
  if CursorOver then State:=State+[bs_Highlight];
  if CursorDown then State:=State+[bs_Down];
  fRenderUI.Write3DButton(TexID,Left,Top,Width,Height,State);
end;


constructor TKMPanel.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
end;


procedure TKMPanel.Paint();
begin
  fRenderUI.WritePic(TexID,Left,Top);
end;


constructor TKMControlsCollection.Create();
begin
  fRenderUI:= TRenderUI.Create;
end;


procedure TKMControlsCollection.Add(Sender:TKMControl);
begin
  Inherited Add(Sender);
end;


procedure TKMControlsCollection.OnMouseOver(X,Y:integer);
var i:integer;
begin
  for i:=0 to Count-1 do
    TKMControl(Items[I]).CursorOver:=
    InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
    InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Left+TKMControl(Items[I]).Height);
end;


procedure TKMControlsCollection.OnMouseDown(X,Y:integer);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Left+TKMControl(Items[I]).Height) then
      TKMControl(Items[I]).CursorDown:=true;
end;


procedure TKMControlsCollection.OnMouseUp(X,Y:integer);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Left+TKMControl(Items[I]).Height) then
      TKMControl(Items[I]).OnClick(nil);
end;


procedure TKMControlsCollection.Paint();
  var i:integer;
begin
  for i:=0 to Count-1 do
    TKMControl(Items[I]).Paint;
end;

end.
