unit KM_Controls;
interface
uses Controls, Math, KromOGLUtils, Classes, KM_Defaults, KromUtils, Graphics, SysUtils, Types;

type TNotifyEvent = procedure(Sender: TObject) of object;

{Base class for all TKM elements}
type
TKMControl = class(TObject)
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

    Tag: integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons

    CursorOver:boolean;

    FOnClick:TNotifyEvent;
    FOnChange:TNotifyEvent;
    FOnRightClick:TNotifyEvent;
    FOnMouseOver:TMouseMoveEvent;
    FOnHint:TMouseMoveEvent;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aLeft,aTop,aWidth,aHeight:integer);
    procedure ParentTo (aParent:TKMControl);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); virtual;
    procedure HintCheckCursorOver(X,Y:integer; AShift:TShiftState); virtual;
    procedure Paint(); virtual;
  public
    procedure Enable;
    procedure Disable;
    procedure Show;
    procedure Hide;
    function IsVisible():boolean;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRightClick: TNotifyEvent read FOnRightClick write FOnRightClick;
    property OnMouseOver: TMouseMoveEvent read FOnMouseOver write FOnMouseOver;
    property OnHint: TMouseMoveEvent read FOnHint write FOnHint;
end;

type TKMPanel = class;

{Text Label}
TKMLabel = class(TKMControl)
  public
    Font: TKMFont;
    FontColor: TColor4;
    TextAlign: KAlign;
    AutoWrap: boolean; //Wherever to automatically wrap text within given text area width
    Caption: string;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string; aColor:TColor4=$FFFFFFFF);
    procedure Paint(); override;
end;


{Image}
TKMImage = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    StretchImage: boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{Panel which should have child items on it, is virtual thing and is not visible}
TKMPanel = class(TKMControl)
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint(); override;
end;


{Panel which is visible}
TKMBevel = class(TKMControl)
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint(); override;
end;


{Rectangle}
TKMShape = class(TKMControl)
  public
    Color:TColor4; //color of rectangle
    LineWidth:byte;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aColor:TColor4);
    procedure Paint(); override;
end;


{3DButton}
TKMButton = class(TKMControl)
  public
    Down:boolean; //Only 3DButton can be pressed down, I rename it according to Delphi VCL Controls rules, ok?
    RXid: integer; //RX library
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont); overload;
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


{FlatButton}
TKMButtonFlat = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    TexOffsetX:shortint;
    Caption: string;
    Font: TKMFont;
    TextAlign: KAlign;
    Down:boolean;
    HideHighlight:boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{Percent bar}
TKMPercentBar = class(TKMControl)
  public
    Position: integer;
    Caption: string;
    Font: TKMFont;
    TextAlign: KAlign;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string; aFont:TKMFont);
    procedure Paint(); override;
end;


{Resource bar}
TKMResourceRow = class(TKMControl)
  public
    Resource: TResourceType;
    ResourceCount: integer;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
    procedure Paint(); override;
end;


{Resource order bar}
TKMResourceOrderRow = class(TKMControl)
  public
    Resource: TResourceType;
    ResourceCount: integer;
    OrderAdd:TKMButton;
    OrderLab:TKMLabel;
    OrderRem:TKMButton;
    OrderCount:word;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
    procedure Paint(); override;
end;


{Production cost bar}
TKMCostsRow = class(TKMControl)
  public
    CostID:byte;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte);
    procedure Paint(); override;
end;


{Ratio bar}
TKMRatioRow = class(TKMControl)
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


{Scroll bar}
TKMScrollBar = class(TKMControl)
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    ScrollVertical:boolean; //So far only vertical scrolls are working
    Thumb:word;
    ScrollUp:TKMButton;
    ScrollDown:TKMButton;
    procedure IncPosition(Sender:TObject);
    procedure DecPosition(Sender:TObject);
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure RefreshItems();
    procedure Paint(); override;
end;


{ Minimap as stand-alone control}
TKMMinimap = class(TKMControl)
  public
    MapSize:TKMPoint;
    CenteredAt:TKMPoint;
    ViewArea:TRect;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


TKMControlsCollection = class(TKMList)
  private
    procedure AddToCollection(Sender:TKMControl);
  public
    constructor Create;
    function AddLabel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: KAlign; const aColor:TColor4=$FFFFFFFF):TKMLabel;
    function AddImage           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImage;
    function AddPanel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
    function AddBevel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMBevel;
    function AddShape           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aColor:TColor4):TKMShape;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButton; overload;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMButton; overload;
    function AddButtonFlat      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButtonFlat;
    function AddPercentBar      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Minimum):TKMPercentBar;
    function AddResourceRow     (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceRow;
    function AddResourceOrderRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceOrderRow;
    function AddCostsRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte):TKMCostsRow;
    function AddRatioRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMRatioRow;
    function AddScrollBar       (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMScrollBar;
    function AddMinimap         (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
    procedure OnMouseOver       (X,Y:integer; AShift:TShiftState);
    procedure OnMouseDown       (X,Y:integer; AButton:TMouseButton);
    procedure OnMouseUp         (X,Y:integer; AButton:TMouseButton);
    procedure Paint();
end;


implementation
uses KM_RenderUI;

constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
  Enabled:=true;
  Visible:=true;
  Tag:=0;
  Hint:='';
end;

{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMControl.ParentTo(aParent:TKMControl);
begin
  if aParent=nil then exit; //Has no parent
  Assert(aParent is TKMPanel,'Let''s not parent controls to anything else except TKMPanels');
  inc(aParent.ChildCount);
  {Hereby I still try to make a rule to count starting from 1, not from zero}
  setlength(aParent.Childs,aParent.ChildCount+1);
  aParent.Childs[aParent.ChildCount]:=Self;
  Self.Parent:=aParent;
  Self.Top:=aParent.Top+Self.Top;
  Self.Left:=aParent.Left+Self.Left;
end;


procedure TKMControl.CheckCursorOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  CursorOver:=InRange(X,Left,Left+Width) and InRange(Y,Top,Top+Height);

  if (CursorOver)and(Assigned(Self.OnMouseOver)) then
    Self.OnMouseOver(Self,AShift,X,Y);

  for i:=1 to ChildCount do
    if Childs[i].Visible then
      if Childs[i].Enabled then
        Childs[i].CheckCursorOver(X,Y,AShift);
end;


procedure TKMControl.HintCheckCursorOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  CursorOver:=InRange(X,Left,Left+Width) and InRange(Y,Top,Top+Height);

  if (CursorOver)and(Assigned(Self.OnHint))and(Hint<>'') then
    Self.OnHint(Self,AShift,X,Y);

  for i:=1 to ChildCount do
    if Childs[i].Visible then //No hints for invisible controls
      Childs[i].HintCheckCursorOver(X,Y,AShift);
end;


{One common thing - draw childs for self}
procedure TKMControl.Paint();
var i:integer;
begin
  for i:=1 to ChildCount do
    if Childs[i].Visible then begin
      Childs[i].Paint;
      inc(CtrlPaintCount);
    end;
end;


{Shortcuts to Controls properties}
procedure TKMControl.Enable;  begin Enabled:=true;  end;
procedure TKMControl.Disable; begin Enabled:=false; end;
procedure TKMControl.Show;    begin Visible:=true;  end;
procedure TKMControl.Hide;    begin Visible:=false; end;


{Check Control including all its Parents to see if Control is actually displayed/visible}
function TKMControl.IsVisible():boolean;
var C:TKMControl;
begin
  Result:=Visible;
  C:=Self.Parent;
  while C<>nil do begin
    Result:=Result and C.Visible;
    C:=C.Parent;
  end;
end;


constructor TKMPanel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


{Panel Paint means to Paint all its childs}
procedure TKMPanel.Paint();
begin
  if MakeDrawPagesOverlay then fRenderUI.WriteLayer(Left, Top, Width, Height, $400000FF);
  Inherited Paint;
end;


{ TKMBevel }
constructor TKMBevel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


procedure TKMBevel.Paint();
begin
  fRenderUI.WriteBevel(Left,Top,Width,Height);
end;


{ TKMShape }
constructor TKMShape.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aColor:TColor4);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Color:=aColor;
  LineWidth:=2;
end;


procedure TKMShape.Paint();
begin
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,Color);
end;


{ TKMButton }
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  RXid:=aRXid;
  TexID:=aTexID;
  ParentTo(aParent);
end;

{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption:=aCaption;
  Font:=aFont;
  TextAlign:=kaCenter; //Thats default everywhere in KaM
  ParentTo(aParent);
end;


procedure TKMButton.CheckCursorOver(X,Y:integer; AShift:TShiftState);
begin
  Down:=Down and CursorOver;
  //Now check to see if they moved their mouse back over with the button still depressed
  if ((ssLeft in AShift) or (ssRight in AShift) or (ssMiddle in AShift)) and (CursorOver) then
    Down := true;

  Inherited CheckCursorOver(X,Y,AShift);
end;


procedure TKMButton.Paint();
var State:T3DButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if Down then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.Write3DButton(Left,Top,Width,Height,RXid,TexID,State);
  if TexID=0 then
    if Enabled then //If disabled then text should be faded
      fRenderUI.WriteText(Left + Width div 2 +byte(Down), (Top + Height div 2)-7+byte(Down), Width, Caption, Font, TextAlign, false, $FFFFFFFF)
    else
      fRenderUI.WriteText(Left + Width div 2, (Top + Height div 2)-7, Width, Caption, Font, TextAlign, false, $FF888888);
end;


{Simple version of button, with image and nothing more}
constructor TKMButtonFlat.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  RXid:=aRXid;
  TexID:=aTexID;
  TexOffsetX:=0;
  Caption:='';
  Font:=fnt_Grey;
  TextAlign:=kaLeft;
  HideHighlight:=false;
  ParentTo(aParent);
end;


{Render}
procedure TKMButtonFlat.Paint();
var State:TFlatButtonStateSet;
begin
  State:=[];
  if CursorOver and Enabled and not HideHighlight then State:=State+[fbs_Highlight];
  if Down and not HideHighlight then State:=State+[fbs_Selected];
  //if not Enabled then State:=State+[fbs_Disabled];

  fRenderUI.WriteFlatButton(Left,Top,Width,Height,RXid,TexID,TexOffsetX,Caption,State);
end;


constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Font:=aFont;
  FontColor:=aColor;
  TextAlign:=aTextAlign;
  AutoWrap:=false;
  Caption:=aCaption;
end;


{Send caption to render and recieve in result how much space did it took on screen}
procedure TKMLabel.Paint();
var Tmp:TKMPoint;
begin
  if MakeDrawPagesOverlay then
  case TextAlign of
    kaLeft:   fRenderUI.WriteLayer(Left, Top, Width, Height, $4000FFFF);
    kaCenter: fRenderUI.WriteLayer(Left - Width div 2, Top, Width, Height, $4000FFFF);
    kaRight:  fRenderUI.WriteLayer(Left - Width, Top, Width, Height, $4000FFFF);
  end;
  if Enabled then
    Tmp:=fRenderUI.WriteText(Left,Top, Width, Caption, Font, TextAlign, AutoWrap, FontColor)
  else
    Tmp:=fRenderUI.WriteText(Left,Top, Width, Caption, Font, TextAlign, AutoWrap, $FF888888);

  if not AutoWrap then
    Width:=Tmp.X;
  Height:=Tmp.Y;
end;


{Make sure image area is at least enough to fit an image, or bigger}
{if Width/Height are 0 then image gets centered around Left/Top}
{if Width/Height are smaller than actual image then adjust them to fit image}
{if Width/Height are bigger than actual image then image will be centered within bounds}
constructor TKMImage.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
begin
  RXid:=aRXid;
  TexID:=aTexID;
  StretchImage:=false;
  if aWidth=0 then aLeft:=aLeft - GFXData[RXid,aTexID].PxWidth div 2;
  if aHeight=0 then aTop:=aTop - GFXData[RXid,aTexID].PxHeight div 2;
  aWidth:=max(aWidth,GFXData[RXid,aTexID].PxWidth);
  aHeight:=max(aHeight,GFXData[RXid,aTexID].PxHeight);
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


{If image area is bigger than image - do center image in it}
procedure TKMImage.Paint();
begin
  if TexID=0 then exit;
  if MakeDrawPagesOverlay then fRenderUI.WriteLayer(Left, Top, Width, Height, $4000FF00);
  if StretchImage then
    fRenderUI.WritePicture(Left, Top, Width, Height, RXid, TexID, Enabled)
  else
    fRenderUI.WritePicture(Left + (Width-GFXData[RXid,TexID].PxWidth) div 2,
                           Top + (Height-GFXData[RXid,TexID].PxHeight) div 2, RXid,TexID, Enabled);
end;


constructor TKMPercentBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Position:=EnsureRange(aPos,0,100);
  Font:=aFont;
  TextAlign:=kaCenter;
  Caption:=aCaption;
end;


procedure TKMPercentBar.Paint();
begin
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
  if Caption <> '' then //Now draw text over bar, if required
    fRenderUI.WriteText((Left + Width div 2)+2, (Top + Height div 2)-4, Width, Caption, Font, TextAlign, false, $FF000000);
  if Caption <> '' then //Now draw text over bar, if required
    fRenderUI.WriteText((Left + Width div 2)+1, (Top + Height div 2)-5, Width, Caption, Font, TextAlign, false, $FFFFFFFF);
end;


{ TKMResourceRow }
constructor TKMResourceRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Resource:=aRes;
  ResourceCount:=aCount;
end;


procedure TKMResourceRow.Paint();
var i:integer;
begin
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFFFFFFF);
  Assert(ResourceCount<=7,'Resource count exceeded'); //4+3 for Stonecutter
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, 4,350+byte(Resource));
end;


constructor TKMResourceOrderRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
begin
  Inherited Create(aLeft+68,aTop,aWidth-68,aHeight);
  ParentTo(aParent);
  Resource:=aRes;
  ResourceCount:=aCount;
  OrderCount:=0;
end;


procedure TKMResourceOrderRow.Paint();
var i:integer;
begin
  OrderRem.Top:=Top;
  OrderLab.Top:=Top+4;
  OrderAdd.Top:=Top;

  //Otherwise they won't be rendered
  OrderRem.Visible:=Visible;
  OrderLab.Visible:=Visible;
  OrderAdd.Visible:=Visible;

  OrderLab.Caption:=inttostr(OrderCount);

  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFFFFFFF);
  Assert(ResourceCount<=7,'Resource count exceeded'); //4+3 for Stonecutter
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, 4,350+byte(Resource));
end;


{ TKMCostsRow }
constructor TKMCostsRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  CostID:=aProductionCostID;
end;


procedure TKMCostsRow.Paint();
var TexID:byte;
begin
  fRenderUI.WriteText(Left, Top + 4, Width, TypeToString(TResourceType(CostID)), fnt_Grey, kaLeft, false, $FFFFFFFF);
  if ProductionCosts[CostID,1] in [rt_Trunk..rt_Fish] then begin
    TexID:=byte(ProductionCosts[CostID,1]);
    fRenderUI.WritePicture(Left+Width-40, Top + (Height-GFXData[4,TexID].PxHeight) div 2, 4,350+TexID);
  end;
  if ProductionCosts[CostID,2] in [rt_Trunk..rt_Fish] then begin
    TexID:=byte(ProductionCosts[CostID,2]);
    fRenderUI.WritePicture(Left+Width-20, Top + (Height-GFXData[4,TexID].PxHeight) div 2, 4,350+TexID);
  end;
end;


{ TKMRatioRow }
constructor TKMRatioRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Position:=5;
  MaxValue:=10;
  MinValue:=1;
end;


procedure TKMRatioRow.CheckCursorOver(X,Y:integer; AShift:TShiftState);
var NewPos: integer;
begin
  Inherited CheckCursorOver(X,Y,AShift);
  NewPos := Position;
  if (CursorOver) and (ssLeft in AShift) then
    NewPos:=EnsureRange(round(MinValue+((X-Left-12)/(Width-28))*(MaxValue-MinValue)),MinValue,MaxValue);
  if NewPos <> Position then
  begin
    Position := NewPos;
    if Assigned(OnChange) then
      OnChange(Self);
  end
  else Position := NewPos;
end;


procedure TKMRatioRow.Paint();
var Pos:word;
begin
  if MakeDrawPagesOverlay then
    fRenderUI.WriteLayer(Left, Top, Width, Height, $4000FF00);

  fRenderUI.WriteBevel(Left+2,Top+2,Width-4,Height-4);
  Pos:= round(mix (0,Width-4-24,1-(Position-MinValue) / (MaxValue-MinValue)));
  fRenderUI.WritePicture(Left+Pos+2, Top, 4,132);
  if Enabled then
    fRenderUI.WriteText(Left+12+2+Pos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FFFFFFFF)
  else
    fRenderUI.WriteText(Left+12+2+Pos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FF888888);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  ScrollVertical:=true;
  Position:=5;
  MaxValue:=10;
  MinValue:=1;
  Thumb:=10;
end;


procedure TKMScrollBar.CheckCursorOver(X,Y:integer; AShift:TShiftState);
var NewPos: integer;
begin
  Inherited CheckCursorOver(X,Y,AShift);

  if InRange(Y,Top+Width,Top+Height-Width) then begin

    NewPos := Position;
    if (CursorOver) and (ssLeft in AShift) then
      NewPos:=EnsureRange(round(

      MinValue+((Y-Top-Width-Thumb/2)/(Height-Width*2-Thumb))*(MaxValue-MinValue)

      ),MinValue,MaxValue);

    if NewPos <> Position then begin
      Position := NewPos;
      if Assigned(OnChange) then
        OnChange(Self);
    end;

  end;
end;


{Refresh button sizes and etc.}
procedure TKMScrollBar.RefreshItems();
begin
  if ScrollVertical then begin
    ScrollUp.Top:=Top;
    ScrollDown.Top:=Top+Height-Width;
    ScrollUp.Height:=Width;
    ScrollDown.Height:=Width;
    ScrollUp.Width:=Width;
    ScrollDown.Width:=Width;
    Thumb:=(Height-2*Width) div 4;
  end;

  ScrollUp.Enabled:=Enabled;
  ScrollDown.Enabled:=Enabled;
end;


procedure TKMScrollBar.IncPosition(Sender:TObject);
begin
  Position:=EnsureRange(Position+1,MinValue,MaxValue);
  OnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender:TObject);
begin
  Position:=EnsureRange(Position-1,MinValue,MaxValue);
  OnChange(Self);
end;


procedure TKMScrollBar.Paint();
var Pos:word; State:T3DButtonStateSet;
begin
  if MakeDrawPagesOverlay then
    fRenderUI.WriteLayer(Left, Top, Width, Height, $40FFFF00);

  //Otherwise they won't be rendered
  ScrollUp.Visible:=Visible;
  ScrollDown.Visible:=Visible;

  fRenderUI.WriteBevel(Left, Top+Width, Width, Height-Width);

  if MinValue=MaxValue then begin
    Pos:=(Height-Width*2-Thumb) div 2;
    State:=[bs_Disabled];
    ScrollUp.Disable;
    ScrollDown.Disable;
  end else begin
    Pos:=(Position-MinValue)*(Height-Width*2-Thumb) div (MaxValue-MinValue);
    State:=[];   // <- looks like a smiley
    ScrollUp.Enable;
    ScrollDown.Enable;
  end;

  fRenderUI.Write3DButton(Left,Top+Width+Pos,Width,Thumb,0,0,State);
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  CenteredAt:=KMPoint(0,0);
end;


procedure TKMMinimap.CheckCursorOver(X,Y:integer; AShift:TShiftState);
begin
  Inherited CheckCursorOver(X,Y,AShift);
  if (CursorOver) and (ssLeft in AShift) then begin
    CenteredAt.X := EnsureRange(X - Left - (Width-MapSize.X) div 2,1,MapSize.X);
    CenteredAt.Y := EnsureRange(Y - Top - (Height-MapSize.Y) div 2,1,MapSize.Y);
  end;
  if Assigned(OnChange) then
    OnChange(Self);
end;


procedure TKMMinimap.Paint();
begin
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.RenderMinimap(Left,Top,Width,Height,MapSize.X,MapSize.Y);
  fRenderUI.WriteRect(Left + (Width-MapSize.X) div 2 + ViewArea.Left,
                      Top  + (Height-MapSize.Y) div 2 + ViewArea.Top,
                      ViewArea.Right-ViewArea.Left,
                      ViewArea.Bottom-ViewArea.Top, 1, $FFFFFFFF);
end;


constructor TKMControlsCollection.Create();
begin
  fRenderUI:= TRenderUI.Create;
end;


procedure TKMControlsCollection.AddToCollection(Sender:TKMControl);
begin
  Inherited Add(Sender);
end;

function TKMControlsCollection.AddLabel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string;
        aFont:TKMFont; aTextAlign: KAlign; const aColor:TColor4 = $FFFFFFFF):TKMLabel;
begin
  Result:=TKMLabel.Create(aParent, aLeft,aTop,aWidth,aHeight, aFont, aTextAlign, aCaption, aColor);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddImage(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImage;
begin
  Result:=TKMImage.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID,aRXid);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddPanel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
begin
  Result:=TKMPanel.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddBevel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMBevel;
begin
  Result:=TKMBevel.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddShape(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aColor:TColor4):TKMShape;
begin
  Result:=TKMShape.Create(aParent, aLeft,aTop,aWidth,aHeight,aColor);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID,aRXid);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButtonFlat(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButtonFlat;
begin
  Result:=TKMButtonFlat.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID,aRXid);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddPercentBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Minimum):TKMPercentBar;
begin
  Result:=TKMPercentBar.Create(aParent, aLeft,aTop,aWidth,aHeight, aPos,aCaption,aFont);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddResourceRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceRow;
begin
  Result:=TKMResourceRow.Create(aParent, aLeft,aTop,aWidth,aHeight, aRes, aCount);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddResourceOrderRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceOrderRow;
begin
  Result:=TKMResourceOrderRow.Create(aParent, aLeft,aTop,aWidth,aHeight, aRes, aCount);
  AddToCollection(Result);
  //These three will be added to collection themselfes
  Result.OrderRem :=AddButton(aParent,aLeft,aTop+2,20,aHeight,'-',fnt_Metal);
  Result.OrderLab :=AddLabel(aParent,aLeft+33,aTop+4,0,0,'',fnt_Grey,kaCenter);
  Result.OrderAdd :=AddButton(aParent,aLeft+46,aTop+2,20,aHeight,'+',fnt_Metal);
end;

function TKMControlsCollection.AddCostsRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte):TKMCostsRow;
begin
  Result:=TKMCostsRow.Create(aParent, aLeft,aTop,aWidth,aHeight, aProductionCostID);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddRatioRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMRatioRow;
begin
  Result:=TKMRatioRow.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddScrollBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMScrollBar;
begin
  Result:=TKMScrollBar.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
  //These three will be added to collection themselfes
  Result.ScrollUp :=AddButton(aParent,aLeft,aTop,aWidth,aHeight,4);
  Result.ScrollUp.OnClick:=Result.DecPosition;
  Result.ScrollDown :=AddButton(aParent,aLeft,aTop,aWidth,aHeight,5);
  Result.ScrollDown.OnClick:=Result.IncPosition;
  Result.RefreshItems();
end;

function TKMControlsCollection.AddMinimap(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
begin
  Result:=TKMMinimap.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

procedure TKMControlsCollection.OnMouseOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  for i:=0 to Count-1 do
    if TKMControl(Items[I]).Parent=nil then
      if TKMControl(Items[I]).IsVisible then
      begin
        if TKMControl(Items[I]).Enabled then
          TKMControl(Items[I]).CheckCursorOver(X,Y,AShift);
        TKMControl(Items[I]).HintCheckCursorOver(X,Y,AShift);
      end;
end;


procedure TKMControlsCollection.OnMouseDown(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).IsVisible then
      if TKMControl(Items[I]).Enabled then
      if TKMControl(Items[i]).ClassType=TKMButton then
      TKMButton(Items[I]).Down:=true;
end;


procedure TKMControlsCollection.OnMouseUp(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  for i:=0 to Count-1 do
    if InRange(X,TKMControl(Items[I]).Left,TKMControl(Items[I]).Left+TKMControl(Items[I]).Width)and
       InRange(Y,TKMControl(Items[I]).Top,TKMControl(Items[I]).Top+TKMControl(Items[I]).Height) then
      if TKMControl(Items[I]).IsVisible then
      if TKMControl(Items[I]).Enabled then begin
        if TKMControl(Items[i]).ClassType=TKMButton then
          TKMButton(Items[I]).Down:=false;
        if AButton = mbLeft then begin
          if Assigned(TKMControl(Items[I]).OnClick) then begin
            TKMControl(Items[I]).OnClick(TKMControl(Items[I]));
            exit; //Send OnClick only to one item
          end;
        end else
          if AButton = mbRight then
          if Assigned(TKMControl(Items[I]).OnRightClick) then begin
            TKMControl(Items[I]).OnRightClick(TKMControl(Items[I]));
            exit; //Send OnClick only to one item
          end;
      end;
end;


{Paint controls}
{Keep painting of childs to their parent control}
procedure TKMControlsCollection.Paint();
  var i:integer;
begin
  CtrlPaintCount:=0;
  for i:=0 to Count-1 do
    if TKMControl(Items[I]).Parent=nil then
      if TKMControl(Items[I]).IsVisible then
        TKMControl(Items[I]).Paint;
  CtrlPaintCount:=0; //Counter
end;


end.
