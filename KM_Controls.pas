unit KM_Controls;
interface
uses Controls, Math, KromOGLUtils, Classes, KM_Defaults;

type TNotifyEvent = procedure(Sender: TObject) of object;

{Base class for all TKM elements}
type
TKMControl = class
  public
    Parent: TKMControl;
    ChildCount:word;             //Those two are actually used only for TKMPanel
    Childs: array of TKMControl; //No other elements needs to be parented
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    Enabled: boolean;
    Visible: boolean;
    FOnClick:TNotifyEvent;
    CursorOver:boolean;
    Pressed:boolean;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aLeft,aTop,aWidth,aHeight:integer);
    procedure ParentTo (aParent:TKMControl);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState);
    procedure Paint(); virtual;
  public
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
end;


{Panel which should have child items on it}
TKMPanel = class(TKMControl)
  private
  public
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint(); override;
end;


{Image}
TKMImage = class(TKMControl)
  public
    TexID: integer;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer);
    procedure Paint(); override;
end;


{Percent bar}
TKMPercentBar = class(TKMControl)
  public
    Position: byte;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer);
    procedure Paint(); override;
end;


{3DButton}
TKMButton = class(TKMControl)
  public
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont); overload;
    procedure Paint(); override;
end;


{FlatButton}
TKMButtonFlat = class(TKMControl)
  public
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer);
    procedure Paint(); override;
end;


{Text Label}
TKMLabel = class(TKMControl)
  public
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string);
    procedure Paint(); override;
end;


TKMControlsCollection = class(TKMList)
  private
    procedure AddToCollection(Sender:TKMControl);
  public
    constructor Create;
    function AddPanel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
    function AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMButton; overload;
    function AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMButton; overload;
    function AddButtonFlat(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMButtonFlat;
    function AddLabel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string):TKMLabel;
    function AddPercentBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer):TKMPercentBar;
    function AddImage(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMImage;
    procedure OnMouseOver(X,Y:integer; AShift:TShiftState);
    procedure OnMouseDown(X,Y:integer; AButton:TMouseButton);
    procedure OnMouseUp(X,Y:integer; AButton:TMouseButton);
    procedure Paint();
end;

var
  fControls: TKMControlsCollection;

implementation
uses KM_RenderUI, KM_Global_Data;

constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
  Enabled:=true;
  Visible:=true;
end;

{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMControl.ParentTo(aParent:TKMControl);
begin
  if aParent=nil then exit; //Has no parent
  Assert(aParent is TKMPanel,'Let''s not parent controls to anything else except TKMPanels');
  inc(aParent.ChildCount);
  setlength(aParent.Childs,aParent.ChildCount);
  aParent.Childs[aParent.ChildCount-1]:=Self;
  Self.Parent:=aParent;
  Self.Top:=aParent.Top+Self.Top;
  Self.Left:=aParent.Left+Self.Left;
end;


procedure TKMControl.CheckCursorOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  CursorOver:=InRange(X,Left,Left+Width) and InRange(Y,Top,Top+Height);
  Pressed:=Pressed and CursorOver;
  //Now check to see if they moved their mouse back over with the left button still depressed
  if (ssLeft in AShift) and (CursorOver) then
    Pressed := true;

  for i:=1 to ChildCount do
    if Childs[i-1].Visible then  
      if Childs[i-1].Enabled then
        Childs[i-1].CheckCursorOver(X,Y,AShift);
end;


{One common thing - draw childs for self}
procedure TKMControl.Paint();
var i:integer;
begin
  for i:=1 to ChildCount do
    if Childs[i-1].Visible then
      Childs[i-1].Paint;
end;


constructor TKMPanel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


{Panel Paint means to Paint all its childs}
procedure TKMPanel.Paint();
begin
  if MakeDrawPagesOverlay then fRenderUI.WriteLayer($200000FF,Left,Top,Width,Height);
  Inherited Paint;
  if MakeDrawPagesOverlay then fRenderUI.WriteLayer($200000FF,Left,Top,Width,Height);
end;


constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
  ParentTo(aParent);
end;

{Different version of button, with caption on in instead of image}
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption:=aCaption;
  Font:=aFont;
  TextAlign:=kaCenter; //Thats default everywhere in KaM
  ParentTo(aParent);
end;


procedure TKMButton.Paint();
var State:T3DButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if Pressed then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.Write3DButton(TexID,Left,Top,Width,Height,State);
  if TexID=0 then
    fRenderUI.WriteText(Left + Width div 2, (Top + Height div 2)-6, TextAlign, Caption, Font);
end;


{Simple version of button, with image and nothing more}
constructor TKMButtonFlat.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  TexID:=aTexID;
  ParentTo(aParent);
end;

{Render}
procedure TKMButtonFlat.Paint();
var State:T3DButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if Pressed then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.WriteFlatButton(TexID,Left,Top,Width,Height,State);
end;


{Make sure image area is at least enough to fit an image, or bigger}
constructor TKMImage.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer);
begin
  aWidth:=max(aWidth,GFXData[4,aTexID].PxWidth);
  aHeight:=max(aHeight,GFXData[4,aTexID].PxHeight);
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  TexID:=aTexID;
end;


{If image area is bigger than image - do center image in it}
procedure TKMImage.Paint();
begin
  fRenderUI.WritePic(TexID, Left + (Width-GFXData[4,TexID].PxWidth) div 2,
                            Top + (Height-GFXData[4,TexID].PxHeight) div 2);
end;


constructor TKMPercentBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Position:=EnsureRange(aPos,0,100);
end;


procedure TKMPercentBar.Paint();
begin
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
end;


constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
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


procedure TKMControlsCollection.AddToCollection(Sender:TKMControl);
begin
  Inherited Add(Sender);
end;

function TKMControlsCollection.AddPanel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
begin
  Result:=TKMPanel.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButtonFlat(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMButtonFlat;
begin
  Result:=TKMButtonFlat.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddLabel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string):TKMLabel;
begin
  Result:=TKMLabel.Create(aParent, aLeft,aTop,aWidth,aHeight, aFont, aTextAlign, aCaption);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddPercentBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer):TKMPercentBar;
begin
  Result:=TKMPercentBar.Create(aParent, aLeft,aTop,aWidth,aHeight, aPos);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddImage(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer):TKMImage;
begin
  Result:=TKMImage.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID);
  AddToCollection(Result);
end;

procedure TKMControlsCollection.OnMouseOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  for i:=0 to Count-1 do
    if TKMControl(Items[I]).Parent=nil then
      if TKMControl(Items[I]).Visible then
      if TKMControl(Items[I]).Enabled then
        TKMControl(Items[I]).CheckCursorOver(X,Y,AShift);
end;


procedure TKMControlsCollection.OnMouseDown(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).Visible then   
      if AButton = mbLeft then //For now only allow pressing with the LEFT mouse button
      if TKMControl(Items[I]).Enabled then
        TKMControl(Items[I]).Pressed:=true;
end;


procedure TKMControlsCollection.OnMouseUp(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).Visible then
      if AButton = mbLeft then //For now only allow pressing with the LEFT mouse button
      if TKMControl(Items[I]).Enabled then begin
        TKMControl(Items[I]).Pressed:=false;
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
