unit KM_Controls;
interface
uses Windows, MMSystem, Controls, Math, KromOGLUtils, Classes, KM_Defaults, KromUtils, Graphics, SysUtils, Types, KM_CommonTypes, KM_Utils;

type
  TNotifyEvent = procedure(Sender: TObject) of object;
  TNotifyEvent2 = procedure(Sender: TObject; AButton:TMouseButton) of object;


{Base class for all TKM elements}
type
TKMControl = class(TObject)
  private
    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;
    Scale: Single;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetWidth: Integer;
  public
    Parent: TKMControl;

    Enabled: boolean;
    Visible: boolean;
    HasFocus: boolean;

    Tag: integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons

    CursorOver:boolean;

    FOnChange:TNotifyEvent;
    FOnClick:TNotifyEvent;
    FOnClickEither:TNotifyEvent2;
    FOnClickRight:TNotifyEvent;
    FOnMouseOver:TMouseMoveEvent;
    FOnHint:TMouseMoveEvent;
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aLeft,aTop,aWidth,aHeight:integer);
    procedure ParentTo (aParent:TKMControl);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); virtual;
    procedure HintCheckCursorOver(X,Y:integer; AShift:TShiftState); virtual;
    procedure Paint(); virtual;
  public
    property Left: Integer read GetLeft write fLeft;
    property Top: Integer read GetTop write fTop;
    property Width: Integer read GetWidth write fWidth;
    property Height: Integer read GetHeight write fHeight;
    procedure Enable;
    procedure Disable;
    procedure Show;
    procedure Hide;
    function IsVisible():boolean;
    function HitTest(X, Y: Integer): Boolean; virtual;
    function KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnClickEither: TNotifyEvent2 read FOnClickEither write FOnClickEither;
    property OnClickRight: TNotifyEvent read FOnClickRight write FOnClickRight;
    property OnMouseOver: TMouseMoveEvent read FOnMouseOver write FOnMouseOver;
    property OnHint: TMouseMoveEvent read FOnHint write FOnHint;
end;


{Panel which should have child items on it, it's virtual and invisible}
TKMPanel = class(TKMControl)
  public
    ChildCount:word;             //Those two are actually used only for TKMPanel
    Childs: array of TKMControl; //No other elements needs to be parented
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


{Panel which is visible, beveled area}
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


{Text Label}
TKMLabel = class(TKMControl)
  public
    Font: TKMFont;
    FontColor: TColor4;
    TextAlign: KAlign;
    AutoWrap: boolean; //Wherever to automatically wrap text within given text area width
    SmoothScrollToTop: integer; //Delta between this and TimeGetTime affects vertical position
    Caption: string;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string; aColor:TColor4=$FFFFFFFF);
    function HitTest(X, Y: Integer): Boolean; override;
    procedure Paint(); override;
end;


{Image}
TKMImage = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    Anchors: TAnchors;
    Highlight:boolean;
    HighlightOnMouseOver:boolean;
    procedure Stretch;
    procedure Center;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


TKMImageStack = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    Count: integer;
    Columns: integer;
    DrawWidth: integer;
    DrawHeight: integer;
    procedure SetCount(aCount,aColumns:integer);
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{3DButton}
TKMButton = class(TKMControl)
  public
    Down:boolean; //Only 3DButton can be pressed down
    MakesSound:boolean;
    RXid: integer; //RX library
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
    Style:TButtonStyle;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer; aStyle:TButtonStyle); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle); overload;
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


{FlatButton}
TKMButtonFlat = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    TexOffsetX,TexOffsetY,CapOffsetY:shortint;
    Caption: string;
    Font: TKMFont;
    TextAlign: KAlign;
    Down:boolean;
    HideHighlight:boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{EditField}
TKMTextEdit = class(TKMControl)
  public
    Text: string;
    Font: TKMFont;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    procedure Paint(); override;
  public
    function KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean; override;
end;


{Checkbox}
TKMCheckBox = class(TKMControl)
  public
    Caption: string;
    Font: TKMFont;
    Checked:boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
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
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


type TScrollAxis = (sa_Vertical, sa_Horizontal);

{Scroll bar}
TKMScrollBar = class(TKMControl)
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    fScrollAxis:TScrollAxis;
    Thumb:word;
    ScrollDec:TKMButton;
    ScrollInc:TKMButton;
    Style:TButtonStyle;
    procedure IncPosition(Sender:TObject);
    procedure DecPosition(Sender:TObject);
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure RefreshItems();
    procedure Paint(); override;
end;


{ Minimap as stand-alone control }
TKMMinimap = class(TKMControl)
  public
    MapSize:TKMPoint;
    CenteredAt:TKMPointF;
    ViewArea:TRect;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure CheckCursorOver(X,Y:integer; AShift:TShiftState); override;
    procedure Paint(); override;
end;


TKMControlsCollection = class(TKMList) //Making list of true TKMControls involves much coding for no visible result
  private
    fFocusedControl:TKMControl;
    procedure AddToCollection(Sender:TKMControl);
    function GetControl(Index: Integer): TKMControl;
    procedure SetControl(Index: Integer; Item: TKMControl);
    property Controls[Index: Integer]: TKMControl read GetControl write SetControl; //Use instead of Items[.]
  public
    constructor Create;
    destructor Destroy; override;
    function AddPanel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
    function AddBevel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMBevel;
    function AddShape           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aColor:TColor4):TKMShape;
    function AddLabel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: KAlign; const aColor:TColor4=$FFFFFFFF):TKMLabel;
    function AddImage           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImage;
    function AddImageStack      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImageStack;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4; aStyle:TButtonStyle=bsGame):TKMButton; overload;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle=bsGame):TKMButton; overload;
    function AddButtonFlat      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButtonFlat;
    function AddTextEdit        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont):TKMTextEdit;
    function AddCheckBox        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMCheckBox;
    function AddPercentBar      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Minimum):TKMPercentBar;
    function AddResourceRow     (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceRow;
    function AddResourceOrderRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceOrderRow;
    function AddCostsRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte):TKMCostsRow;
    function AddRatioRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer):TKMRatioRow;
    function AddScrollBar       (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle=bsGame):TKMScrollBar;
    function AddMinimap         (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
    property GetFocusedControl:TKMControl read fFocusedControl;
    function KeyUp              (Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean;
    function MouseOverControl   ():TKMControl;
    procedure OnMouseOver       (X,Y:integer; AShift:TShiftState);
    procedure OnMouseDown       (X,Y:integer; AButton:TMouseButton);
    procedure OnMouseUp         (X,Y:integer; AButton:TMouseButton);
    procedure Paint();
end;


implementation
uses KM_RenderUI;

constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create;
  Left      := aLeft;
  Top       := aTop;
  Width     := aWidth;
  Height    := aHeight;
  HasFocus  := false;
  Enabled   := true;
  Visible   := true;
  Tag       := 0;
  Hint      := '';
  Scale     := 1;
end;


{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMControl.ParentTo(aParent:TKMControl);
var ParentPanel:TKMPanel;
begin
  if aParent=nil then exit; //Has no parent
  fLog.AssertToLog(aParent is TKMPanel,'Let''s not parent controls to anything else except TKMPanels');
  ParentPanel := TKMPanel(aParent);

  inc(ParentPanel.ChildCount);
  {Hereby I still try to make a rule to count starting from 1, not from zero}
  setlength(ParentPanel.Childs,ParentPanel.ChildCount+1);
  ParentPanel.Childs[ParentPanel.ChildCount] := Self;
  Self.Parent := ParentPanel;
end;


procedure TKMControl.CheckCursorOver(X,Y:integer; AShift:TShiftState);
begin
  CursorOver:=InRange(X,Left,Left+Width) and InRange(Y,Top,Top+Height);
  if (CursorOver)and(Assigned(Self.OnMouseOver)) then
    Self.OnMouseOver(Self,AShift,X,Y);
end;


procedure TKMControl.HintCheckCursorOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  CursorOver:=InRange(X,Left,Left+Width) and InRange(Y,Top,Top+Height);

  if (CursorOver)and(Assigned(Self.OnHint))and(Hint<>'') then
    Self.OnHint(Self,AShift,X,Y);

  if Self is TKMPanel then //Only Panels have childs
  for i:=1 to TKMPanel(Self).ChildCount do
    if TKMPanel(Self).Childs[i].Visible then //No hints for invisible controls
       TKMPanel(Self).Childs[i].HintCheckCursorOver(X,Y,AShift);
end;


function TKMControl.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean;
var Amt:byte;
begin
  Result := false;
  if not MODE_DESIGN_CONTORLS then exit;
  if IsDown then exit;

  Amt := 1;
  if ssCtrl in Shift then Amt := 10;
  if ssShift in Shift then Amt := 100;

  if Key = VK_LEFT then fLeft := fLeft - Amt;
  if Key = VK_RIGHT then fLeft := fLeft + Amt;
  if Key = VK_UP then fTop := fTop - Amt;
  if Key = VK_DOWN then fTop := fTop + Amt;
end;


function TKMControl.HitTest(X, Y: Integer): Boolean;
begin
  Result:= InRange(X, Left, Left + Width) and InRange(Y, Top, Top + Height) and IsVisible;
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint();
var sColor:TColor4;
begin
  if not SHOW_CONTROLS_OVERLAY then exit;

  sColor := $FFFFFFFF;

  if Self is TKMPanel then sColor := $400000FF;
  if Self is TKMBevel then ;
  if Self is TKMShape then ;

  if Self is TKMLabel then begin //Special case for aligned text
    case TKMLabel(Self).TextAlign of
      kaLeft:   fRenderUI.WriteLayer(Left, Top, Width, Height, $4000FFFF);
      kaCenter: fRenderUI.WriteLayer(Left - Width div 2, Top, Width, Height, $4000FFFF);
      kaRight:  fRenderUI.WriteLayer(Left - Width, Top, Width, Height, $4000FFFF);
    end;
    exit;
  end;

  if Self is TKMImage      then sColor := $4000FF00;
  if Self is TKMImageStack then sColor := $4080FF00;
  if Self is TKMCheckBox   then sColor := $40FF00FF;
  if Self is TKMRatioRow   then sColor := $4000FF00;
  if Self is TKMScrollBar  then sColor := $40FFFF00;

  fRenderUI.WriteLayer(Left, Top, Width, Height, sColor);
end;


{Shortcuts to Controls properties}
procedure TKMControl.Enable;  begin Enabled:=true;  end;

function TKMControl.GetHeight: Integer;
begin
  Result := Round(fHeight * Scale);
end;

function TKMControl.GetLeft: Integer;
begin
  Result := Round(fLeft * Scale);
  if Parent <> nil then Result := Result + Parent.GetLeft;
end;

function TKMControl.GetTop: Integer;
begin
  Result := Round(fTop * Scale);
  if Parent <> nil then Result := Result + Parent.GetTop;
end;

function TKMControl.GetWidth: Integer;
begin
  Result := Round(fWidth * Scale);
end;

procedure TKMControl.Disable; begin Enabled := false; end;

procedure TKMControl.Show;
begin
  if Self.Parent<>nil then Self.Parent.Show;
  Visible := true;
end;

procedure TKMControl.Hide;    begin Visible := false; end;


{Check Control including all its Parents to see if Control is actually displayed/visible}
function TKMControl.IsVisible():boolean;
var C:TKMControl;
begin
  Result := Visible;
  C := Self.Parent;
  while C<>nil do begin
    Result := Result and C.Visible;
    C := C.Parent;
  end;
end;


{ TKMPanel } //virtual panels to contain child items
procedure TKMPanel.CheckCursorOver(X, Y: integer; AShift: TShiftState);
var
  I: Integer;
begin
  inherited;
  for i:=1 to ChildCount do
    if Childs[i].Visible and Childs[i].Enabled then
       Childs[i].CheckCursorOver(X,Y,AShift);
end;

constructor TKMPanel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


{Panel Paint means to Paint all its childs}
procedure TKMPanel.Paint();
var i:integer;
begin
  Inherited;
  for i:=1 to TKMPanel(Self).ChildCount do
    if TKMPanel(Self).Childs[i].Visible then
    begin
      TKMPanel(Self).Childs[i].Paint;
      inc(CtrlPaintCount);
    end;
end;


{ TKMBevel }
constructor TKMBevel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


procedure TKMBevel.Paint();
begin
  Inherited;
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
  Inherited;
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,Color);
end;


constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aTextAlign: KAlign; aCaption:string; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Font:=aFont;
  FontColor:=aColor;
  TextAlign:=aTextAlign;
  AutoWrap:=false;
  SmoothScrollToTop:=0; //means disabled
  Caption:=aCaption;
end;


function TKMLabel.HitTest(X, Y: Integer): Boolean;
begin
  case TextAlign of
    kaLeft: Result := InRange(X, Left, Left + Width) and InRange(Y, Top, Top + Height) and IsVisible;
    kaCenter: Result := InRange(X, Left - Width div 2, Left + Width div 2) and InRange(Y, Top, Top + Height) and IsVisible;
    kaRight: Result := InRange(X, Left - Width, Left) and InRange(Y, Top, Top + Height) and IsVisible;
  end;
end;


{Send caption to render and recieve in result how much space did it took on screen}
procedure TKMLabel.Paint();
var Tmp:TKMPoint; NewTop:integer;
begin
  Inherited;
  if SmoothScrollToTop<>0 then
    NewTop := Top - (integer(TimeGetTime) - SmoothScrollToTop) div 50 //Compute delta and shift by it upwards (Credits page)
  else
    NewTop := Top;

  if Enabled then
    Tmp := fRenderUI.WriteText(Left, NewTop, Width, Caption, Font, TextAlign, AutoWrap, FontColor)
  else
    Tmp := fRenderUI.WriteText(Left, NewTop, Width, Caption, Font, TextAlign, AutoWrap, $FF888888);

  if not AutoWrap then
    Width:=Tmp.X;
  Height:=Tmp.Y;
end;


constructor TKMImage.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID, aRXid:integer);
begin
  RXid := aRXid;
  TexID := aTexID;
  Anchors := [akLeft, akTop];
  Highlight := false;
  HighlightOnMouseOver := false;
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
end;


procedure TKMImage.Stretch;
begin
  Anchors := [akLeft, akRight, akTop, akBottom]; //Stretch image to fit
end;


procedure TKMImage.Center; //Render image from center
begin
  Anchors := [];
end;


{If image area is bigger than image - do center image in it}
procedure TKMImage.Paint();
var
  OffsetX, OffsetY, DrawWidth, DrawHeight:smallint; //variable parameters
  StretchDraw:boolean; //Check if the picture should be stretched
begin
  Inherited;
  if (TexID=0)or(RXid=0) then exit; //No picture to draw

  StretchDraw := false; 
  DrawWidth   := Width;
  DrawHeight  := Height;
  OffsetX     := 0;
  OffsetY     := 0;

  if akRight in Anchors then OffsetX := Width - GFXData[RXid, TexID].PxWidth; //First check "non-zero offset" anchor incase both anchors are set
  if akLeft in Anchors then OffsetX := 0;
  if (akLeft in Anchors) and (akRight in Anchors) then //Both anchors means: stretch the image
  begin
    StretchDraw := true;
    DrawWidth := Width;
  end;
  if not ((akLeft in Anchors) or (akRight in Anchors)) then //No anchors means: draw the image in center
    OffsetX := (Width - GFXData[RXid, TexID].PxWidth) div 2;

  if akBottom in Anchors then OffsetY := Width - GFXData[RXid, TexID].PxHeight;
  if akTop in Anchors then OffsetY := 0;
  if (akTop in Anchors) and (akBottom in Anchors) then
  begin
    StretchDraw := true;
    DrawHeight := Height;
  end;
  if not ((akTop in Anchors) or (akBottom in Anchors)) then
    OffsetY := (Height - GFXData[RXid, TexID].PxHeight) div 2;

  if StretchDraw then
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, DrawWidth, DrawHeight, RXid, TexID, Enabled, (HighlightOnMouseOver AND CursorOver) OR Highlight)
  else
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, RXid, TexID, Enabled, (HighlightOnMouseOver AND CursorOver) OR Highlight);
end;


constructor TKMImageStack.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID, aRXid:integer);
begin
  RXid  := aRXid;
  TexID := aTexID;
  Count := 0;
  Columns := 0;
  DrawWidth := 0;
  DrawHeight := 0;
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
end;


procedure TKMImageStack.SetCount(aCount,aColumns:integer);
var Aspect: single;
begin
  Count := aCount;
  Columns := Math.max(1,aColumns);
  DrawWidth  := EnsureRange(Width div Columns, 8, GFXData[RXid, TexID].PxWidth);
  DrawHeight := EnsureRange(Height div ceil(Count/Columns), 6, GFXData[RXid, TexID].PxHeight);

  Aspect := GFXData[RXid, TexID].PxWidth / GFXData[RXid, TexID].PxHeight;
  if DrawHeight * Aspect <= DrawWidth then
    DrawWidth  := round(DrawHeight * Aspect)
  else
    DrawHeight := round(DrawWidth / Aspect);
end;


{If image area is bigger than image - do center image in it}
procedure TKMImageStack.Paint();
var
  i:integer;
  OffsetX, OffsetY, CenterX, CenterY:smallint; //variable parameters
begin
  Inherited;
  if (TexID=0)or(RXid=0) then exit; //No picture to draw

  OffsetX := Width div Columns;
  OffsetY := Height div ceil(Count/Columns);

  CenterX := (Width - OffsetX * (Columns-1) - DrawWidth) div 2;
  CenterY := (Height - OffsetY * (ceil(Count/Columns)-1) - DrawHeight) div 2;

  for i := 1 to Count do
    fRenderUI.WritePicture(Left + CenterX + OffsetX*((i-1) mod Columns),
                            Top + CenterY + OffsetY*((i-1) div Columns),
                            DrawWidth, DrawHeight, RXid, TexID, Enabled);
end;


{ TKMButton }
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer; aStyle:TButtonStyle);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  RXid:=aRXid;
  TexID:=aTexID;
  Caption:='';
  ParentTo(aParent);
  Style:=aStyle;
  MakesSound:=true;
end;

{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  RXid:=0;
  TexID:=0;
  Caption:=aCaption;
  Font:=aFont;
  TextAlign:=kaCenter; //Thats default everywhere in KaM
  ParentTo(aParent);
  Style:=aStyle;
  MakesSound:=true;
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
  Inherited;
  State:=[];
  if CursorOver and Enabled then State:=State+[bs_Highlight];
  if Down then State:=State+[bs_Down];
  if not Enabled then State:=State+[bs_Disabled];
  fRenderUI.Write3DButton(Left,Top,Width,Height,RXid,TexID,State,Style);
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
  TexOffsetY:=0;
  CapOffsetY:=0;
  Caption:='';
  Font:=fnt_Grey;
  TextAlign:=kaLeft;
  HideHighlight:=false;
  ParentTo(aParent);
end;


procedure TKMButtonFlat.Paint();
var State:TFlatButtonStateSet;
begin
  Inherited;
  State:=[];
  if CursorOver and Enabled and not HideHighlight then State:=State+[fbs_Highlight];
  if Down then State:=State+[fbs_Selected];
  //if not Enabled then State:=State+[fbs_Disabled];

  fRenderUI.WriteFlatButton(Left,Top,Width,Height,RXid,TexID,TexOffsetX,TexOffsetY,CapOffsetY,Caption,State);
end;


{TKMTextEdit}
constructor TKMTextEdit.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Text := 'Test';
  Font := aFont;
  ParentTo(aParent);
end;


function TKMTextEdit.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean;
begin
  Result := true;
  if Inherited KeyUp(Key, Shift, IsDown) then exit;

  if (not IsDown) and (chr(Key) in [' ', '_', '!', '(', ')', '0'..'9', 'A'..'Z']) then //Letters don't auto-repeat
    if ssShift in Shift then Text := Text + UpperCase(chr(Key))
                        else Text := Text + LowerCase(chr(Key))
  else
    if IsDown and (Key = VK_BACK) then decs(Text); //Allow fast delete if IsDown

  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMTextEdit.Paint();
var Col:TColor4;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if Enabled then Col:=$FFFFFFFF else Col:=$FF888888;
  if HasFocus and ((TimeGetTime div 500) mod 2 = 0)then
    fRenderUI.WriteText(Left+4, Top+4, Width-8, Text+'[', Font, kaLeft, false, Col)
  else
    fRenderUI.WriteText(Left+4, Top+4, Width-8, Text, Font, kaLeft, false, Col);
end;


{TKMCheckBox}
constructor TKMCheckBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption:=aCaption;
  Font:=aFont;
  ParentTo(aParent);
end;


procedure TKMCheckBox.Paint();
const BoxWidth=25;
var Tmp:TKMPoint; Col:TColor4;
begin
  Inherited;
  if Enabled then Col:=$FFFFFFFF else Col:=$FF888888;

  //We can replace it with something better later on. For now [x] fits just fine
  fRenderUI.WriteText(Left, Top, Width, '[ ]', Font, kaLeft, false, Col);
  if Checked then
    fRenderUI.WriteText(Left+4, Top, Width, 'x', Font, kaLeft, false, Col);

  Tmp:=fRenderUI.WriteText(Left+BoxWidth, Top, Width, Caption, Font, kaLeft, false, Col);
  
  Width:=Tmp.X+BoxWidth;
  Height:=Tmp.Y;
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
  Inherited;
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
  if Caption <> '' then begin //Now draw text over bar, if required
    fRenderUI.WriteText((Left + Width div 2)+2, (Top + Height div 2)-4, Width, Caption, Font, TextAlign, false, $FF000000);
    fRenderUI.WriteText((Left + Width div 2)+1, (Top + Height div 2)-5, Width, Caption, Font, TextAlign, false, $FFFFFFFF);
  end;
end;


{ TKMResourceRow }
constructor TKMResourceRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Resource := aRes;
  ResourceCount := aCount;
end;


procedure TKMResourceRow.Paint();
var i:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFFFFFFF);
  fLog.AssertToLog(ResourceCount<=7,'Resource count in this house is exceeded'); //4+3 for Stonecutter
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top, 4,350+byte(Resource));
end;


constructor TKMResourceOrderRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
begin
  Inherited Create(aLeft+68,aTop,aWidth-68,aHeight);
  ParentTo(aParent);
  Resource := aRes;
  ResourceCount := aCount;
  OrderCount := 0;
end;


procedure TKMResourceOrderRow.Paint();
var i:integer;
begin
  Inherited;
  OrderRem.Top := fTop; //Use internal fTop instead of GetTop (which will return absolute value)
  OrderLab.Top := fTop + 4;
  OrderAdd.Top := fTop;

  //Otherwise they won't be rendered
  OrderRem.Visible := Visible;
  OrderLab.Visible := Visible;
  OrderAdd.Visible := Visible;

  OrderLab.Caption := inttostr(OrderCount);

  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFFFFFFF);
  fLog.AssertToLog(ResourceCount<=7,'Resource count exceeded'); //4+3 for Stonecutter
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
  Inherited;
  fRenderUI.WriteText(Left, Top + 4, Width, TypeToString(TResourceType(CostID)), fnt_Grey, kaLeft, false, $FFFFFFFF);
  if WarfareCosts[CostID,1] in [rt_Trunk..rt_Fish] then begin
    TexID:=byte(WarfareCosts[CostID,1]);
    fRenderUI.WritePicture(Left+Width-40, Top + (Height-GFXData[4,TexID].PxHeight) div 2, 4,350+TexID);
  end;
  if WarfareCosts[CostID,2] in [rt_Trunk..rt_Fish] then begin
    TexID:=byte(WarfareCosts[CostID,2]);
    fRenderUI.WritePicture(Left+Width-20, Top + (Height-GFXData[4,TexID].PxHeight) div 2, 4,350+TexID);
  end;
end;


{ TKMRatioRow }
constructor TKMRatioRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  MinValue:=aMin;
  MaxValue:=aMax;
  Position:=(MinValue+MaxValue) div 2;
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
  Inherited;
  fRenderUI.WriteBevel(Left+2,Top+2,Width-4,Height-4);
  Pos:= round(mix (0,Width-4-24,1-(Position-MinValue) / (MaxValue-MinValue)));
  fRenderUI.WritePicture(Left+Pos+2, Top, 4,132);
  if Enabled then
    fRenderUI.WriteText(Left+12+2+Pos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FFFFFFFF)
  else
    fRenderUI.WriteText(Left+12+2+Pos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FF888888);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
begin
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
  fScrollAxis := aScrollAxis;
  Position := 5;
  MaxValue := 10;
  MinValue := 1;
  Thumb    := 10;
  Style    := aStyle;
end;


procedure TKMScrollBar.CheckCursorOver(X,Y:integer; AShift:TShiftState);
var NewPos: integer;
begin
  Inherited CheckCursorOver(X,Y,AShift);

  NewPos := Position;

  if (CursorOver) and (ssLeft in AShift) then
  begin
    if fScrollAxis = sa_Vertical then
    if InRange(Y,Top+Width,Top+Height-Width) then
      NewPos := round( MinValue+((Y-Top-Width-Thumb/2)/(Height-Width*2-Thumb))*(MaxValue-MinValue) );
    if fScrollAxis = sa_Horizontal then
    if InRange(X,Left+Height,Left+Width-Height) then
      NewPos := round( MinValue+((X-Left-Height-Thumb/2)/(Width-Height*2-Thumb))*(MaxValue-MinValue) );
  end;
  if NewPos <> Position then begin
    Position := EnsureRange(NewPos, MinValue, MaxValue);
    if Assigned(OnChange) then
      OnChange(Self);
  end;

end;


{Refresh button sizes and etc.}
procedure TKMScrollBar.RefreshItems();
begin
  case fScrollAxis of
    sa_Vertical:   Thumb := Math.max(0, (Height-2*Width)) div 4;
    sa_Horizontal: Thumb := Math.max(0, (Width-2*Height)) div 4;
  end;

  ScrollDec.Enabled := Enabled; //Copy property from TKMScrollBar
  ScrollInc.Enabled := Enabled;
end;


procedure TKMScrollBar.IncPosition(Sender:TObject);
begin
  Position := EnsureRange(Position+1, MinValue, MaxValue);
  OnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender:TObject);
begin
  Position := EnsureRange(Position-1, MinValue, MaxValue);
  OnChange(Self);
end;


procedure TKMScrollBar.Paint();
var Pos:word; State:T3DButtonStateSet;
begin
  Inherited;
  //Copy property to child buttons. Otherwise they won't be rendered
  ScrollDec.Visible := Visible;
  ScrollInc.Visible := Visible;

  Pos := 0;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.WriteBevel(Left, Top+Width, Width, Height - Width*2);
    sa_Horizontal: fRenderUI.WriteBevel(Left+Height, Top, Width - Height*2, Height);
  end;

  if MinValue = MaxValue then begin
    case fScrollAxis of
      sa_Vertical:   Pos := (Height-Width*2-Thumb) div 2;
      sa_Horizontal: Pos := (Width-Height*2-Thumb) div 2;
    end;
    State := [bs_Disabled];
    ScrollDec.Disable;
    ScrollInc.Disable;
  end else begin
    case fScrollAxis of
      sa_Vertical:   Pos := (Position-MinValue)*(Height-Width*2-Thumb) div (MaxValue-MinValue);
      sa_Horizontal: Pos := (Position-MinValue)*(Width-Height*2-Thumb) div (MaxValue-MinValue);
    end;
    State := [];
    ScrollDec.Enable;
    ScrollInc.Enable;
  end;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.Write3DButton(Left,Top+Width+Pos,Width,Thumb,0,0,State,Style);
    sa_Horizontal: fRenderUI.Write3DButton(Left+Height+Pos,Top,Thumb,Height,0,0,State,Style);
  end;
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  CenteredAt:=KMPointF(0,0);
end;


procedure TKMMinimap.CheckCursorOver(X,Y:integer; AShift:TShiftState);
begin
  Inherited CheckCursorOver(X,Y,AShift);
  if (CursorOver) and (ssLeft in AShift) then begin
    CenteredAt.X := EnsureRange(X - Left - (Width-MapSize.X) div 2,1,MapSize.X);
    CenteredAt.Y := EnsureRange(Y - Top - (Height-MapSize.Y) div 2,1,MapSize.Y);
  end;
  if Assigned(OnChange) and (ssLeft in AShift) then
    OnChange(Self);
end;


procedure TKMMinimap.Paint();
begin
  Inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.RenderMinimap(Left,Top,Width,Height);
  fRenderUI.WriteRect(Left + (Width-MapSize.X) div 2 + ViewArea.Left,
                      Top  + (Height-MapSize.Y) div 2 + ViewArea.Top,
                      ViewArea.Right-ViewArea.Left,
                      ViewArea.Bottom-ViewArea.Top, 1, $FFFFFFFF);
end;


constructor TKMControlsCollection.Create();
begin
  Inherited;
  CtrlPaintCount := 0;
  fFocusedControl := nil;
  if fRenderUI <> nil then
  fRenderUI := TRenderUI.Create;
end;


destructor TKMControlsCollection.Destroy();
begin
  if fRenderUI <> nil then
  FreeAndNil(fRenderUI);
  Inherited;
end;


procedure TKMControlsCollection.AddToCollection(Sender:TKMControl);
begin
  Inherited Add(Sender);
end;


function TKMControlsCollection.GetControl(Index: Integer): TKMControl;
begin
  Result := TKMControl(Items[Index])
end;


procedure TKMControlsCollection.SetControl(Index: Integer; Item: TKMControl);
begin
  Items[Index] := Item;
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

function TKMControlsCollection.AddImageStack(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID:integer; const aRXid:integer=4):TKMImageStack;
begin
  Result:=TKMImageStack.Create(aParent, aLeft, aTop, aWidth, aHeight, aTexID, aRXid);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4; aStyle:TButtonStyle=bsGame):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID,aRXid,aStyle);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButton(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle=bsGame):TKMButton;
begin
  Result:=TKMButton.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont,aStyle);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddButtonFlat(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButtonFlat;
begin
  Result:=TKMButtonFlat.Create(aParent, aLeft,aTop,aWidth,aHeight,aTexID,aRXid);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddTextEdit(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont):TKMTextEdit;
begin
  Result:=TKMTextEdit.Create(aParent, aLeft,aTop,aWidth,aHeight,aFont);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddCheckBox(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMCheckBox;
begin
  Result:=TKMCheckBox.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont);
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

function TKMControlsCollection.AddRatioRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer):TKMRatioRow;
begin
  Result:=TKMRatioRow.Create(aParent, aLeft,aTop,aWidth,aHeight,aMin,aMax);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddScrollBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle=bsGame):TKMScrollBar;
begin
  Result := TKMScrollBar.Create(aParent, aLeft, aTop, aWidth, aHeight, aScrollAxis, aStyle);
  AddToCollection(Result);
  //These two will be added to collection by themselfes
  if aScrollAxis = sa_Vertical then begin
    Result.ScrollDec := AddButton(aParent, aLeft, aTop, aWidth, aWidth, 4, 4, aStyle);
    Result.ScrollInc := AddButton(aParent, aLeft, aTop+aHeight-aWidth, aWidth, aWidth, 5, 4, aStyle);
  end;
  if aScrollAxis = sa_Horizontal then begin
    Result.ScrollDec := AddButton(aParent, aLeft, aTop, aHeight, aHeight, 2, 4, aStyle);
    Result.ScrollInc := AddButton(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 3, 4, aStyle);
  end;
  Result.ScrollDec.OnClick := Result.DecPosition;
  Result.ScrollInc.OnClick := Result.IncPosition;
  Result.RefreshItems();
end;

function TKMControlsCollection.AddMinimap(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
begin
  Result:=TKMMinimap.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;


function TKMControlsCollection.KeyUp(Key: Word; Shift: TShiftState; IsDown:boolean=false):boolean;
begin
  Result := false;
  if fFocusedControl = nil then exit;
  Result := fFocusedControl.KeyUp(Key, Shift, IsDown);
end;


function TKMControlsCollection.MouseOverControl():TKMControl;
var i:integer;
begin
  Result:=nil;

  for i:=Count-1 downto 1 do //This will return last created cursor
    if not (Controls[I] is TKMPanel) then //Do not check Panels
      if Controls[I].IsVisible then
          if Controls[I].CursorOver then begin
            Result := Controls[i];
            break;
          end;
end;


procedure TKMControlsCollection.OnMouseOver(X,Y:integer; AShift:TShiftState);
var i:integer;
begin
  if MODE_DESIGN_CONTORLS then exit; //Don't do
  for i:=0 to Count-1 do
    if Controls[i].Parent=nil then
      if Controls[i].IsVisible then
      begin
        if Controls[i].Enabled then
          Controls[i].CheckCursorOver(X,Y,AShift);
        Controls[i].HintCheckCursorOver(X,Y,AShift);
      end;
end;


procedure TKMControlsCollection.OnMouseDown(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  if MODE_DESIGN_CONTORLS then exit; //Don't do
  for i:=0 to Count-1 do
    if Controls[i].HitTest(X, Y) then
      if Controls[i].Enabled then
        if Controls[i] is TKMButton then
          TKMButton(Controls[i]).Down := true;
end;


{Send OnClick event to control below}
procedure TKMControlsCollection.OnMouseUp(X,Y:integer; AButton:TMouseButton);
var i:integer;
begin
  if fFocusedControl <> nil then fFocusedControl.HasFocus := false; //Release focus in any case of OnMouseUp
  for i:=0 to Count-1 do
  if Controls[i].HitTest(X, Y) then
  if Controls[i].Enabled or MODE_DESIGN_CONTORLS then //Allow selecting of disabled Controls
  begin
    if Controls[i] is TKMButton then
      TKMButton(Controls[i]).Down := false;

    if (AButton = mbLeft) then //Set focus irregardless of assigned OnClick events
    begin
      fFocusedControl := Controls[i]; //Only LMB can set focus
      fFocusedControl.HasFocus := true; //Set Focus
    end;

    if (AButton = mbLeft)
    and Assigned(Controls[i].OnClick)
    and not MODE_DESIGN_CONTORLS then //Don't do, but keep on scanning controls
    begin
      Controls[i].OnClick(Controls[i]);
      exit; //Send OnClick only to one item
    end;

    if (AButton = mbRight)
    and Assigned(Controls[i].OnClickRight) 
    and not MODE_DESIGN_CONTORLS then //Don't do, but keep on scanning controls
    begin
      Controls[i].OnClickRight(Controls[i]);
      exit; //Send OnClickRight only to one item
    end;

    if Assigned(Controls[i].OnClickEither)
    and not MODE_DESIGN_CONTORLS then //Don't do, but keep on scanning controls
    begin
      Controls[i].OnClickEither(Controls[i], AButton);
      exit; //Send OnClickRight only to one item
    end;
  end;
end;


{Paint controls}
{Leave painting of childs to their parent control}
procedure TKMControlsCollection.Paint();
  var i:integer;
begin
  CtrlPaintCount:=0;
  for i:=0 to Count-1 do
    if Controls[i].Parent=nil then
      if Controls[i].IsVisible then
        Controls[i].Paint;

  if MODE_DESIGN_CONTORLS and (fFocusedControl<>nil) then
  with fFocusedControl do
    fRenderUI.WriteText(Left, Top-14, Width, inttostr(Left)+':'+inttostr(Top), fnt_Grey, kaLeft, false, $FFFFFFFF);
end;


end.
