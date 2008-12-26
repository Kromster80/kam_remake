unit KM_Controls;
interface
uses Controls, KM_Classes, KM_RenderUI, Math, KM_Defaults, KromOGLUtils;

type TNotifyEvent = procedure(Sender: TObject) of object;

{Base class for all TKM elements}
type
TKMControl = class (TObject)
  private
    Parent: TKMControl;
    ChildCount:word;
    Childs: array of TKMControl;
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Enabled: boolean;
    Visible: boolean;
    Tag:integer;
    FOnClick:TNotifyEvent;
    CursorOver:boolean;
    CursorDown:boolean;
  constructor Create(aLeft,aTop,aWidth,aHeight:integer);
  procedure Paint(); virtual;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    procedure ParentTo (aParent:TKMControl);
end;


{Panel which should have child items on it}
TKMPanel = class(TKMControl)
  public
  constructor Create(aLeft,aTop,aWidth,aHeight:integer);
  procedure Paint(); override;
end;


{Image}
TKMImage = class(TKMControl)
  public
    TexID: integer;
  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
  procedure Paint(); override;
end;


{3DButton}
TKMButton = class(TKMControl)
  public
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer); overload;
  constructor Create(aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont); overload;
  procedure Paint(); override;
end;


{FlatButton}
TKMButtonFlat = class(TKMControl)
  public
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  constructor Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
  procedure Paint(); override;
end;


{Text Label}
TKMLabel = class(TKMControl)
  public
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  constructor Create(aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string);
  procedure Paint(); override;
end;


TKMControlsCollection = class(TKMList)
  private
    //fControl: TKMControl;
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
    TagCount:integer=1;

implementation

constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
  Enabled:=true;
  Visible:=true;
  Tag:=TagCount;
  inc(TagCount);
end;

{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMControl.ParentTo(aParent:TKMControl);
begin
  inc(aParent.ChildCount);
  setlength(aParent.Childs,aParent.ChildCount);
  aParent.Childs[aParent.ChildCount-1]:=Self;
  Self.Parent:=aParent;
  Self.Top:=aParent.Top+Self.Top;
  Self.Left:=aParent.Left+Self.Left;
end;


{One common thing - draw childs for self}
procedure TKMControl.Paint();
var i:integer;
begin
  for i:=1 to ChildCount do
    Childs[i-1].Paint;
end;


constructor TKMPanel.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
end;


procedure TKMPanel.Paint();
begin
  //fRenderUI.WritePic(1,Left,Top);
  Inherited Paint;
end;


constructor TKMButton.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
end;

{Different version of button, with caption on in instead of image}
constructor TKMButton.Create(aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption:=aCaption;
  Font:=aFont;
  TextAlign:=kaCenter; //Thats default everywhere in KaM
end;


procedure TKMButton.Paint();
var State:T3DButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if CursorDown then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.Write3DButton(TexID,Left,Top,Width,Height,State);
  if TexID=0 then
    fRenderUI.WriteText(Left + Width div 2, Top + Height div 2, TextAlign, Caption, Font);
end;


{Simple version of button, with image and nothing more}
constructor TKMButtonFlat.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
end;

{Render}
procedure TKMButtonFlat.Paint();
var State:T3DButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if CursorDown then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.WriteFlatButton(TexID,Left,Top,Width,Height,State);
end;


constructor TKMImage.Create(aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
end;


procedure TKMImage.Paint();
begin
  fRenderUI.WritePic(TexID,Left,Top);
end;


constructor TKMLabel.Create(aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Font:=aFont;
  TextAlign:=aTextAlign;
  Caption:=aCaption;
end;


procedure TKMLabel.Paint();
begin
  Width:=fRenderUI.WriteText(Left,Top, TextAlign, Caption, Font);
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
    if TKMControl(Items[I]).Visible then
    TKMControl(Items[I]).CursorOver:=
    InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
    InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height);
end;


procedure TKMControlsCollection.OnMouseDown(X,Y:integer);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).Visible then
      if TKMControl(Items[I]).Enabled then
        TKMControl(Items[I]).CursorDown:=true;
end;


procedure TKMControlsCollection.OnMouseUp(X,Y:integer);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).Visible then
      if TKMControl(Items[I]).Enabled then begin
        TKMControl(Items[I]).CursorDown:=false;
        if Assigned(TKMControl(Items[I]).OnClick) then begin
          TKMControl(Items[I]).OnClick(TKMControl(Items[I]));
          exit; //Send OnClick only to one item
        end;
      end;
end;


{Paint controls}
{Keep painting of childs to their parent control}
procedure TKMControlsCollection.Paint();
  var i:integer;
begin
  for i:=0 to Count-1 do
    if TKMControl(Items[I]).Parent=nil then
      if TKMControl(Items[I]).Visible then TKMControl(Items[I]).Paint;
end;


end.
