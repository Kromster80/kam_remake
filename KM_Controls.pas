unit KM_Controls;
{$I KaM_Remake.inc}
interface
uses Classes, Controls, Graphics, Math, MMSystem, Windows, SysUtils,
    KromUtils, KromOGLUtils,
    KM_CommonTypes, KM_Defaults;

type
  TNotifyEventMB = procedure(Sender: TObject; AButton:TMouseButton) of object;
  TNotifyEventMW = procedure(Sender: TObject; WheelDelta:integer) of object;
  TNotifyEventKey = procedure(Sender: TObject; Key: Word) of object;
  
  TKMControlState = (csDown, csFocus, csOver);
  TKMControlStateSet = set of TKMControlState;


{Base class for all TKM elements}
type
TKMControl = class
  private
    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;

    fOnClick:TNotifyEvent;
    fOnClickEither:TNotifyEventMB;
    fOnClickRight:TNotifyEvent;
    fOnMouseWheel:TNotifyEventMW;
    fOnMouseOver:TNotifyEvent;

    function GetLeft: Integer;
    function GetTop: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(aValue:Integer); virtual;
    procedure SetWidth(aValue:Integer); virtual;
  public
    Parent: TKMControl;

    Anchors: TAnchors;
    Enabled: boolean;
    Visible: boolean;
    State: TKMControlStateSet; //Each control has it localy to avoid quering Collection on each Render

    Tag: integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons
  protected //We don't want these to be accessed outside of this unit, all externals should access TKMControlsCollection instead
    constructor Create(aLeft,aTop,aWidth,aHeight:integer);
    procedure ParentTo (aParent:TKMControl);
    function HitTest(X, Y: Integer): Boolean; virtual;
    procedure Paint(); virtual;
  public
    property Left: Integer read GetLeft write fLeft;
    property Top: Integer read GetTop write fTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    procedure Enable;
    procedure Disable;
    procedure Show;
    procedure Hide;
    procedure Center;
    procedure Stretch;
    function IsVisible():boolean;

    function KeyDown(Key: Word; Shift: TShiftState):boolean; virtual;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; virtual;
    procedure MouseDown (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseMove (X,Y:integer; Shift:TShiftState); virtual;
    procedure MouseUp   (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); virtual;

    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property OnClickEither: TNotifyEventMB read fOnClickEither write fOnClickEither;
    property OnClickRight: TNotifyEvent read fOnClickRight write fOnClickRight;
    property OnMouseWheel: TNotifyEventMW read fOnMouseWheel write fOnMouseWheel;
    property OnMouseOver: TNotifyEvent read fOnMouseOver write fOnMouseOver;
end;


{ Panel which should have child items on it, it's virtual and invisible }
TKMPanel = class(TKMControl)
  private
    GetCollection:Pointer;
    procedure SetHeight(aValue:Integer); override;
    procedure SetWidth(aValue:Integer); override;
  public
    ChildCount:word;             //Those two are actually used only for TKMPanel
    Childs: array of TKMControl; //No other elements needs to be parented
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint(); override;
end;


{ Beveled area }
TKMBevel = class(TKMControl)
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint(); override;
end;


{Rectangle}
TKMShape = class(TKMControl)
  public
    Hitable:boolean;
    FillColor:TColor4; 
    LineColor:TColor4; //color of outline
    LineWidth:byte;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4);
    function HitTest(X, Y: Integer): Boolean; override;
    procedure Paint(); override;
end;


{Text Label}
TKMLabel = class(TKMControl)
  private
    fCaption: string;
    fText:string;
    procedure SetCaption(aCaption:string);
    procedure ReformatText();
  public
    Font: TKMFont;
    FontColor: TColor4;
    TextAlign: KAlign;
    AutoWrap: boolean; //Wherever to automatically wrap text within given text area width
    SmoothScrollToTop: integer; //Delta between this and TimeGetTime affects vertical position
    property Caption:string read fCaption write SetCaption;
    function TextHeight():integer;
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
    ImageAnchors: TAnchors;
    Highlight:boolean;
    HighlightOnMouseOver:boolean;
    procedure ImageStretch;
    procedure ImageCenter;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{Image stack - for army formation view}
TKMImageStack = class(TKMControl)
  private
    fCount: integer;
    fColumns: integer;
    fDrawWidth: integer;
    fDrawHeight: integer;
  public
    RXid: integer; //RX library
    TexID: integer;
    procedure SetCount(aCount,aColumns:integer);
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
end;


{ Color swatch - to select a color from given samples/palette }
TKMColorSwatch = class(TKMControl)
  private
    CellSize:byte; //in pixels
    SelectedColor:byte; //Index 0..255
    Columns:byte;
    Rows:byte;
    Colors:array of TColor4; //Range is 0..255
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer);
    procedure Paint(); override;
  public
    function GetColor():TColor4;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
end;


{3DButton}
TKMButton = class(TKMControl)
  public
    MakesSound:boolean;
    RXid: integer; //RX library
    TexID: integer;
    Font: TKMFont;
    TextAlign: KAlign;
    Caption: string;
    Style:TButtonStyle;
    function DoClick:boolean; //Try to click a button and return TRUE if succeded
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer; aStyle:TButtonStyle); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle); overload;
    procedure Paint(); override;
  public
    procedure MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
end;


{FlatButton}
TKMButtonFlat = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    TexOffsetX,TexOffsetY,CapOffsetY:shortint;
    Caption: string;
    Font: TKMFont;
    FontColor:TColor4;
    TextAlign: KAlign;
    Down:boolean;
    HideHighlight:boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID,aRXid:integer);
    procedure Paint(); override;
  public
    procedure MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
end;


{FlatButton with Shape on it}
TKMFlatButtonShape = class(TKMControl)
  public
    CapOffsetY:shortint;
    Caption: string;
    Font: TKMFont;
    ShapeColor:TColor4;
    Down:boolean;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4);
    procedure Paint(); override;
end;


{EditField}
TKMEdit = class(TKMControl)
  private
    fText:string;
    fOnChange:TNotifyEvent;
    fOnKeyDown:TNotifyEventKey;
    procedure SetText(aText:string);
  public
    Font: TKMFont;
    Masked:boolean;
    CursorPos:integer;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean);
    property Text:string read fText write SetText;
    property OnChange: TNotifyEvent write FOnChange;
    property OnKeyDown: TNotifyEventKey write fOnKeyDown;
    function KeyDown(Key: Word; Shift: TShiftState):boolean; override;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; override;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint(); override;
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
    FontColor:TColor4;
    TextAlign: KAlign;
  protected
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string; aFont:TKMFont; aColor:TColor4=$FFFFFFFF);
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
  private
    FOnChange:TNotifyEvent;
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer);
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Paint(); override;
end;


type TScrollAxis = (sa_Vertical, sa_Horizontal);

{Scroll bar}
TKMScrollBar = class(TKMControl)
  private
    fScrollAxis:TScrollAxis;
    FOnChange:TNotifyEvent;
    procedure IncPosition(Sender:TObject);
    procedure DecPosition(Sender:TObject);
    procedure RefreshItems();
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    Thumb:word;
    ScrollDec:TKMButton;
    ScrollInc:TKMButton;
    Style:TButtonStyle;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Paint(); override;
end;


TKMListBox = class(TKMControl)
  private
    ItemHeight:byte;
    fItems:TStringList;
    ScrollBar:TKMScrollBar;
    FOnChange:TNotifyEvent;
    procedure ChangeScrollPosition (Sender:TObject);
  public
    TopIndex:smallint; //up to 32k files
    ItemIndex:smallint;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    destructor Destroy; override;

    procedure RefreshList();
    property Items:TStringList read fItems;

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected

    procedure Paint(); override;
end;


{ Minimap as stand-alone control }
TKMMinimap = class(TKMControl)
  private
    FOnChange:TNotifyEvent;
  public
    MapSize:TKMPoint;
    BoundRectAt:TKMPoint;
    ViewArea:TRect;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    function GetMapCoords(X,Y:integer; const Inset:shortint=0):TKMPoint;
    function InMapCoords(X,Y:integer):boolean;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure Paint(); override;
end;


{ Files list } //Possible solution for MapEd and Save/Load
TKMFileList = class(TKMControl)
  private
    ItemHeight:byte;
    fPath:string;
    ScrollBar:TKMScrollBar;
    FOnChange:TNotifyEvent;
    procedure ChangeScrollPosition (Sender:TObject);
  public
    TopIndex:smallint; //up to 32k files
    ItemIndex:smallint;
    fPaths:TStringList;
    fFiles:TStringList;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    destructor Destroy; override;

    procedure RefreshList(aPath,aExtension:string; ScanSubFolders:boolean=false);
    function FileName:string;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  protected

    procedure Paint(); override;
end;


{ TKMControlsCollection }
TKMControlsCollection = class(TKMList) //Making list of true TKMControls involves much coding for no visible result
  private
    fCtrlDown:TKMControl; //Control that was pressed Down
    fCtrlFocus:TKMControl; //Control which has input Focus
    fCtrlOver:TKMControl; //Control which has cursor Over it
    fCtrlUp:TKMControl; //Control above which cursor was released
    procedure AddToCollection(Sender:TKMControl);
    function  GetControl(Index: Integer): TKMControl;
    procedure SetControl(Index: Integer; Item: TKMControl);
    property  Controls[Index: Integer]: TKMControl read GetControl write SetControl; //Use instead of Items[.]

    function HitControl(X,Y:integer):TKMControl;
    procedure SetCtrlDown(aCtrl:TKMControl);
    procedure SetCtrlFocus(aCtrl:TKMControl);
    procedure SetCtrlOver(aCtrl:TKMControl);
    procedure SetCtrlUp(aCtrl:TKMControl);
  public
    constructor Create;
    destructor Destroy; override;

    property CtrlDown:TKMControl read fCtrlDown write SetCtrlDown;
    property CtrlFocus:TKMControl read fCtrlFocus write SetCtrlFocus;
    property CtrlOver:TKMControl read fCtrlOver write SetCtrlOver;
    property CtrlUp:TKMControl read fCtrlUp write SetCtrlUp;

    function AddPanel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
    function AddBevel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMBevel;
    function AddShape           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4):TKMShape;
    function AddLabel           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: KAlign; const aColor:TColor4=$FFFFFFFF):TKMLabel;
    function AddImage           (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImage;
    function AddImageStack      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMImageStack;
    function AddColorSwatch     (aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer):TKMColorSwatch;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4; aStyle:TButtonStyle=bsGame):TKMButton; overload;
    function AddButton          (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle=bsGame):TKMButton; overload;
    function AddButtonFlat      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; const aRXid:integer=4):TKMButtonFlat;
    function AddFlatButtonShape (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4):TKMFlatButtonShape;
    function AddEdit            (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean=false):TKMEdit;
    function AddCheckBox        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMCheckBox;
    function AddPercentBar      (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini):TKMPercentBar;
    function AddResourceRow     (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceRow;
    function AddResourceOrderRow(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer):TKMResourceOrderRow;
    function AddCostsRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte):TKMCostsRow;
    function AddRatioRow        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer):TKMRatioRow;
    function AddScrollBar       (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle=bsGame):TKMScrollBar;
    function AddListBox         (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMListBox;
    function AddMinimap         (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
    function AddFileList        (aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMFileList;

    function KeyDown            (Key: Word; Shift: TShiftState):boolean;
    function KeyUp              (Key: Word; Shift: TShiftState):boolean;
    procedure MouseDown         (X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
    procedure MouseMove         (X,Y:integer; Shift:TShiftState);
    procedure MouseUp           (X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
    procedure MouseWheel        (X,Y:integer; WheelDelta:integer);

    procedure Paint();

    procedure SaveToFile(aFileName:string);
end;


implementation
uses KM_RenderUI, KM_ResourceGFX, KM_Sound, KM_Utils;


{ TKMControl }
constructor TKMControl.Create(aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create;
  Left      := aLeft;
  Top       := aTop;
  Width     := aWidth;
  Height    := aHeight;
  Anchors   := [akLeft, akTop];
  State     := [];
  Enabled   := true;
  Visible   := true;
  Tag       := 0;
  Hint      := '';
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
  Parent := ParentPanel;
end;


function TKMControl.KeyDown(Key: Word; Shift: TShiftState):boolean;
var Amt:byte;
begin
  Result := false;
  if not MODE_DESIGN_CONTORLS then exit;

  Amt := 1;
  if ssCtrl  in Shift then Amt := 10;
  if ssShift in Shift then Amt := 100;

  if Key = VK_LEFT  then fLeft := fLeft - Amt;
  if Key = VK_RIGHT then fLeft := fLeft + Amt;
  if Key = VK_UP    then fTop  := fTop  - Amt;
  if Key = VK_DOWN  then fTop  := fTop  + Amt;
end;


function TKMControl.KeyUp(Key: Word; Shift: TShiftState):boolean;
begin
  Result := false;
  if not MODE_DESIGN_CONTORLS then exit;
  //nothing yet
end;


procedure TKMControl.MouseDown(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  //if Assigned(fOnMouseDown) then fOnMouseDown(Self); { Unused }
end;


procedure TKMControl.MouseMove(X,Y:integer; Shift:TShiftState);
begin
  if Assigned(fOnMouseOver) then fOnMouseOver(Self);
end;


{ Send Click events }
procedure TKMControl.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  //if Assigned(fOnMouseUp) then OnMouseUp(Self); { Unused }
  if (csDown in State) then begin
    State := State - [csDown];
    if ((Button = mbLeft)or(Button = mbRight)) and Assigned(fOnClickEither) then OnClickEither(Self, Button)
    else
    if (Button = mbLeft) and Assigned(fOnClick) then OnClick(Self)
    else
    if (Button = mbRight) and Assigned(fOnClickRight) then OnClickRight(Self)
  end;
end;


procedure TKMControl.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  if Assigned(OnMouseWheel) then OnMouseWheel(Self, WheelDelta);
end;


function TKMControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := IsVisible and Enabled and InRange(X, Left, Left + Width) and InRange(Y, Top, Top + Height);
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint();
var sColor:TColor4;
begin
  inc(CtrlPaintCount);
  if not SHOW_CONTROLS_OVERLAY then exit;

  sColor := $FFFFFFFF;

  if Self is TKMPanel then exit;//sColor := $000000FF;
  if Self is TKMBevel then sColor := $20FFFFFF;
  if Self is TKMShape then sColor := $20FFFFFF;

  if Self is TKMLabel then begin //Special case for aligned text
    case TKMLabel(Self).TextAlign of
      kaLeft:   fRenderUI.WriteLayer(Left, Top, Width, Height, $2000FFFF);
      kaCenter: fRenderUI.WriteLayer(Left - Width div 2, Top, Width, Height, $2000FFFF);
      kaRight:  fRenderUI.WriteLayer(Left - Width, Top, Width, Height, $2000FFFF);
    end;
    fRenderUI.WriteLayer(Left-3, Top-3, 6, 6, sColor or $FF000000);
    exit;
  end;

  if Self is TKMImage      then sColor := $2000FF00;
  if Self is TKMImageStack then sColor := $2080FF00;
  if Self is TKMCheckBox   then sColor := $20FF00FF;
  if Self is TKMRatioRow   then sColor := $2000FF00;
  if Self is TKMScrollBar  then sColor := $20FFFF00;
  if Self is TKMFileList   then sColor := $200080FF;

  if csOver in State then sColor := sColor OR $30000000; //Highlight on mouse over

  fRenderUI.WriteLayer(Left, Top, Width, Height, sColor);
  fRenderUI.WriteLayer(Left-3, Top-3, 6, 6, sColor or $FF000000);
end;


{Shortcuts to Controls properties}
function TKMControl.GetLeft: Integer;
begin
  Result := fLeft;
  if Parent <> nil then Result := Result + Parent.GetLeft;
end;

function TKMControl.GetTop: Integer;
begin
  Result := fTop;
  if Parent <> nil then Result := Result + Parent.GetTop;
end;

function TKMControl.GetHeight: Integer;
begin
  Result := fHeight;
end;

function TKMControl.GetWidth: Integer;
begin
  Result := fWidth;
end;

procedure TKMControl.SetHeight(aValue:Integer);
begin
  fHeight := aValue;
end;

procedure TKMControl.SetWidth(aValue:Integer);
begin
  fWidth := aValue;
end;

procedure TKMControl.Enable;  begin Enabled := true;  end;
procedure TKMControl.Disable; begin Enabled := false; end;

{Will show up entire branch in which control resides}
procedure TKMControl.Show;
begin
  if Parent<>nil then Parent.Show;
  Visible := true;
end;

procedure TKMControl.Hide;    begin Visible := false; end;
procedure TKMControl.Center;  begin Anchors := []; end;
procedure TKMControl.Stretch; begin Anchors := [akLeft, akTop, akRight, akBottom]; end;


{Check Control including all its Parents to see if Control is actually displayed/visible}
function TKMControl.IsVisible():boolean;
var C:TKMControl;
begin
  Result := Visible;
  C := Parent;
  while C<>nil do begin
    Result := Result and C.Visible;
    C := C.Parent;
  end;
end;


{ TKMPanel } //virtual panels to contain child items
constructor TKMPanel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
end;


procedure TKMPanel.SetHeight(aValue:Integer);
var i:integer;
begin
  for i:=1 to ChildCount do
    if (akTop in TKMControl(Childs[i]).Anchors)and(akBottom in TKMControl(Childs[i]).Anchors) then
      TKMControl(Childs[i]).Height := TKMControl(Childs[i]).Height + (aValue-fHeight)
    else
    if akBottom in TKMControl(Childs[i]).Anchors then
      TKMControl(Childs[i]).Top := TKMControl(Childs[i]).Top + (aValue-fHeight)
    else
    if not (akTop in TKMControl(Childs[i]).Anchors)and not(akBottom in TKMControl(Childs[i]).Anchors) then
      TKMControl(Childs[i]).Top := TKMControl(Childs[i]).Top + (aValue-fHeight) div 2;

  Inherited;
end;

procedure TKMPanel.SetWidth(aValue:Integer);
var i:integer;
begin
  for i:=1 to ChildCount do
    if (akLeft in TKMControl(Childs[i]).Anchors)and(akRight in TKMControl(Childs[i]).Anchors) then
      TKMControl(Childs[i]).Width := TKMControl(Childs[i]).Width + (aValue-fWidth)
    else
    if akRight in TKMControl(Childs[i]).Anchors then
      TKMControl(Childs[i]).Left := TKMControl(Childs[i]).Left + (aValue-fWidth)
    else
    if not (akLeft in TKMControl(Childs[i]).Anchors)and not(akRight in TKMControl(Childs[i]).Anchors) then
      TKMControl(Childs[i]).Left := TKMControl(Childs[i]).Left + (aValue-fWidth) div 2;
  Inherited;
end;



{Panel Paint means to Paint all its childs}
procedure TKMPanel.Paint();
var i:integer;
begin
  Inherited;
  for i:=1 to ChildCount do
    if Childs[i].Visible then
      Childs[i].Paint;
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
constructor TKMShape.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Hitable   := true;
  FillColor := $00000000;
  LineColor := aLineColor;
  LineWidth := 2;
end;


{If Fill color alpha is transparent, then treat the thing as non-hitable}
function TKMShape.HitTest(X,Y:Integer): Boolean;
begin
  if Hitable then
    Result := Inherited HitTest(X,Y)
  else
    Result := false;
end;


procedure TKMShape.Paint();
begin
  Inherited;
  fRenderUI.WriteLayer(Left,Top,Width,Height,FillColor,$00000000);
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,LineColor);
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
  SetCaption(aCaption);
end;


procedure TKMLabel.SetCaption(aCaption:string);
begin
  fCaption := aCaption;
  ReformatText()
end;


function TKMLabel.HitTest(X, Y: Integer): Boolean;
begin
  case TextAlign of
    kaLeft: Result := InRange(X, Left, Left + Width) and InRange(Y, Top, Top + Height) and IsVisible;
    kaCenter: Result := InRange(X, Left - Width div 2, Left + Width div 2) and InRange(Y, Top, Top + Height) and IsVisible;
    kaRight: Result := InRange(X, Left - Width, Left) and InRange(Y, Top, Top + Height) and IsVisible;
    else Result := false;
  end;
end;


function TKMLabel.TextHeight():integer;
begin
  ReformatText();
  Result := fRenderUI.WriteText(0, 0, Width, fText, Font, TextAlign, AutoWrap, $00000000).Y;
end;


//Existing EOLs should be preserved, and new ones added where needed.
procedure TKMLabel.ReformatText();
var i,CharSpacing,AdvX,PrevX,LastSpace:integer;
begin
  fText := fCaption; //Keep original intact incase we need to Reformat text again
  if not AutoWrap then exit;

  AdvX := 0;
  PrevX := 0;
  LastSpace := -1;
  CharSpacing := FontData[Font].CharSpacing; //Spacing between letters, this varies between fonts

  for i:=1 to length(fText) do
  begin
    if (fText[i]=#32) or (fText[i]=#124) then begin
      LastSpace := i;
      PrevX := AdvX;
    end;

    if fText[i]=#32 then inc(AdvX, FontData[Font].WordSpacing)
                    else inc(AdvX, FontData[Font].Letters[ord(fText[i])].Width + CharSpacing);

    //This algorithm is not perfect, somehow line width is not within SizeX, but very rare
    if ((AdvX>Width)and(LastSpace<>-1))or(fText[i]=#124) then
    begin
      fText[LastSpace] := #124; //Replace last whitespace with EOL
      dec(AdvX, PrevX); //Subtract width since replaced whitespace
    end;
  end;
end;


{Send caption to render and recieve in result how much space did it took on screen}
procedure TKMLabel.Paint();
var Tmp:TKMPoint; NewTop:integer;
begin
  Inherited;
  if SmoothScrollToTop<>0 then
    NewTop := Top + Height - (integer(TimeGetTime) - SmoothScrollToTop) div 50 //Compute delta and shift by it upwards (Credits page)
  else
    NewTop := Top;

  if Enabled then
    Tmp := fRenderUI.WriteText(Left, NewTop, Width, fText, Font, TextAlign, AutoWrap, FontColor)
  else
    Tmp := fRenderUI.WriteText(Left, NewTop, Width, fText, Font, TextAlign, AutoWrap, $FF888888);

  if not AutoWrap then Width := Tmp.X;
  if SmoothScrollToTop=0 then Height := Tmp.Y;
end;


constructor TKMImage.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID, aRXid:integer);
begin
  RXid := aRXid;
  TexID := aTexID;
  ImageAnchors := [akLeft, akTop];
  Highlight := false;
  HighlightOnMouseOver := false;
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
end;


procedure TKMImage.ImageStretch;
begin
  ImageAnchors := [akLeft, akRight, akTop, akBottom]; //Stretch image to fit
end;


procedure TKMImage.ImageCenter; //Render image from center
begin
  ImageAnchors := [];
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

  if akRight in ImageAnchors then OffsetX := Width - GFXData[RXid, TexID].PxWidth; //First check "non-zero offset" anchor incase both ImageAnchors are set
  if akLeft in ImageAnchors then OffsetX := 0;
  if (akLeft in ImageAnchors) and (akRight in ImageAnchors) then //Both ImageAnchors means: stretch the image
  begin
    StretchDraw := true;
    DrawWidth := Width;
  end;
  if not ((akLeft in ImageAnchors) or (akRight in ImageAnchors)) then //No ImageAnchors means: draw the image in center
    OffsetX := (Width - GFXData[RXid, TexID].PxWidth) div 2;

  if akBottom in ImageAnchors then OffsetY := Width - GFXData[RXid, TexID].PxHeight;
  if akTop in ImageAnchors then OffsetY := 0;
  if (akTop in ImageAnchors) and (akBottom in ImageAnchors) then
  begin
    StretchDraw := true;
    DrawHeight := Height;
  end;
  if not ((akTop in ImageAnchors) or (akBottom in ImageAnchors)) then
    OffsetY := (Height - GFXData[RXid, TexID].PxHeight) div 2;

  if StretchDraw then
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, DrawWidth, DrawHeight, RXid, TexID, Enabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight)
  else
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, RXid, TexID, Enabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight);
end;


{ TKMImageStack }
constructor TKMImageStack.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID, aRXid:integer);
begin
  RXid  := aRXid;
  TexID := aTexID;
  fCount := 0;
  fColumns := 0;
  fDrawWidth := 0;
  fDrawHeight := 0;
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
end;


procedure TKMImageStack.SetCount(aCount,aColumns:integer);
var Aspect: single;
begin
  fCount := aCount;
  fColumns := Math.max(1,aColumns);
  fDrawWidth  := EnsureRange(Width div fColumns, 8, GFXData[RXid, TexID].PxWidth);
  fDrawHeight := EnsureRange(Height div ceil(fCount/fColumns), 6, GFXData[RXid, TexID].PxHeight);

  Aspect := GFXData[RXid, TexID].PxWidth / GFXData[RXid, TexID].PxHeight;
  if fDrawHeight * Aspect <= fDrawWidth then
    fDrawWidth  := round(fDrawHeight * Aspect)
  else
    fDrawHeight := round(fDrawWidth / Aspect);
end;


{If image area is bigger than image - do center image in it}
procedure TKMImageStack.Paint();
var
  i:integer;
  OffsetX, OffsetY, CenterX, CenterY:smallint; //variable parameters
begin
  Inherited;
  if (TexID=0)or(RXid=0) then exit; //No picture to draw

  OffsetX := Width div fColumns;
  OffsetY := Height div ceil(fCount/fColumns);

  CenterX := (Width - OffsetX * (fColumns-1) - fDrawWidth) div 2;
  CenterY := (Height - OffsetY * (ceil(fCount/fColumns)-1) - fDrawHeight) div 2;

  for i := 1 to fCount do
    fRenderUI.WritePicture(Left + CenterX + OffsetX*((i-1) mod fColumns),
                            Top + CenterY + OffsetY*((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, RXid, TexID, Enabled);
end;


{ TKMColorSwatch }
constructor TKMColorSwatch.Create(aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer);
var i,k:integer;
begin
  Inherited Create(aLeft, aTop, 0, 0);

  Columns := aColumns;
  Rows := aRows;
  CellSize := 11;

  Width := Columns*CellSize;
  Height := Rows*CellSize;

  setlength(Colors, Columns*Rows);
  for i:=0 to Rows-1 do
  for k:=0 to Columns-1 do
  if i*Columns+k<=255 then //Rows*Columns could be >255
    Colors[i*Columns+k] := fResource.GetColor32(i*Columns+k, DEF_PAL);
  ParentTo(aParent);
end;


function TKMColorSwatch.GetColor():TColor4;
begin
  Result := fResource.GetColor32(SelectedColor, DEF_PAL);
end;


procedure TKMColorSwatch.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  SelectedColor := EnsureRange(
                     EnsureRange((Y-Top)div CellSize,0,Rows-1)*Columns +
                     EnsureRange((X-Left)div CellSize,0,Columns-1),
                     0,255);
  Inherited;
end;


procedure TKMColorSwatch.Paint();
var i,k:integer;
begin
  Inherited;
  for i:=0 to Rows-1 do
  for k:=0 to Columns-1 do
  if i*Columns+k<=255 then
    fRenderUI.WriteLayer(Left+k*CellSize, Top+i*CellSize, CellSize, CellSize, Colors[i*Columns+k], $00);

  //fRenderUI.WriteLayer(Left, Top+Rows*CellSize, CellSize, CellSize, Colors[SelectedColor], $00);
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


//DoClick is called by keyboard shortcuts
//It's important that Control must be:
// IsVisible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.DoClick:boolean;
begin
  if IsVisible and Enabled then begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    TKMControlsCollection(TKMPanel(Parent).GetCollection).CtrlOver := Self;
    TKMControlsCollection(TKMPanel(Parent).GetCollection).CtrlUp := Self;
    if Assigned(OnClick) then OnClick(Self);
    Result := true; //Click has happened
  end else
    Result := false; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  if Enabled and MakesSound and (csDown in State) then fSoundLib.Play(sfx_Click);
  Inherited;
end;


procedure TKMButton.Paint();
var StateSet:T3DButtonStateSet;
begin
  Inherited;
  StateSet:=[];
  if (csOver in State) and Enabled then StateSet:=StateSet+[bs_Highlight];
  if (csDown in State) then StateSet:=StateSet+[bs_Down];
  if not Enabled then StateSet:=StateSet+[bs_Disabled];
  fRenderUI.Write3DButton(Left,Top,Width,Height,RXid,TexID,StateSet,Style);
  if TexID=0 then
    if Enabled then //If disabled then text should be faded
      fRenderUI.WriteText(Left + Width div 2 +byte(csDown in State), (Top + Height div 2)-7+byte(csDown in State), Width, Caption, Font, TextAlign, false, $FFFFFFFF)
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
  Down:=false;
  HideHighlight:=false;
  ParentTo(aParent);
end;


procedure TKMButtonFlat.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  if Enabled and (csDown in State) then fSoundLib.Play(sfx_Click);
  Inherited;
end;


procedure TKMButtonFlat.Paint();
var StateSet:TFlatButtonStateSet;
begin
  Inherited;
  StateSet:=[];
  if (csOver in State) and Enabled and not HideHighlight then StateSet:=StateSet+[fbs_Highlight];
  if (csDown in State) or Down then StateSet:=StateSet+[fbs_Selected];
  //if not Enabled then StateSet:=StateSet+[fbs_Disabled];

  fRenderUI.WriteFlatButton(Left,Top,Width,Height,RXid,TexID,TexOffsetX,TexOffsetY,CapOffsetY,Caption,StateSet);
end;


constructor TKMFlatButtonShape.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption     := aCaption;
  CapOffsetY  := 0;
  ShapeColor  := aShapeColor;
  Font        := aFont;
  Down        := false;
  ParentTo(aParent);
end;


procedure TKMFlatButtonShape.Paint();
begin
  Inherited;  
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteLayer(Left+1,Top+1,Width-2,Width-2, ShapeColor, $00000000);
  fRenderUI.WriteText(Left+(Width div 2),Top+(Height div 2)+4+CapOffsetY,Width, Caption, Font, kaCenter, false, $FFFFFFFF);
  if (csOver in State) and Enabled then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $40FFFFFF, $00);
  if (csDown in State) or Down then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $00000000, $FFFFFFFF);
end;


{TKMEdit}
constructor TKMEdit.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  fText := '<<<LEER>>>';
  Font := aFont;
  Masked := aMasked;
  CursorPos := length(fText);
  ParentTo(aParent);
end;


procedure TKMEdit.SetText(aText:string);
begin
  fText := aText;
  CursorPos := min(CursorPos, length(fText));
end;


function TKMEdit.KeyDown(Key: Word; Shift: TShiftState):boolean;
  function ValidKey(aKey:word):boolean;
  begin //Utility, Numbers, NumPad numbers, Letters
    Result := chr(aKey) in [' ', '_', '!', '(', ')', '0'..'9', #96..#105, 'A'..'Z'];
  end;
var s:string;
begin
  Result := true;
  if Inherited KeyDown(Key, Shift) then exit;

  if ValidKey(Key) then begin
    s := GetCharFromVirtualKey(Key);
    Insert(s, fText, CursorPos+1);
    inc(CursorPos,length(s)); //GetCharFromVirtualKey might be 1 or 2 chars
  end;

  case Key of
    VK_BACK:    begin Delete(fText, CursorPos, 1); dec(CursorPos); end;
    VK_DELETE:  Delete(fText, CursorPos+1, 1);
    VK_LEFT:    dec(CursorPos);
    VK_RIGHT:   inc(CursorPos);
  end;
  CursorPos := EnsureRange(CursorPos, 0, length(fText));

  if Assigned(fOnKeyDown) then fOnKeyDown(Self, Key);
end;


function TKMEdit.KeyUp(Key: Word; Shift: TShiftState):boolean;
begin
  Result := true;
  if Inherited KeyUp(Key, Shift) then exit;

  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMEdit.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  CursorPos := length(fText);
end;


procedure TKMEdit.Paint();
var Col:TColor4; RText:String; OffX:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if Enabled then Col:=$FFFFFFFF else Col:=$FF888888;
  if Masked then RText := StringOfChar('*', Length(fText)) else RText := fText;

  fRenderUI.WriteText(Left+4, Top+4, Width-8, RText, Font, kaLeft, false, Col);

  if (csFocus in State) and ((TimeGetTime div 500) mod 2 = 0)then begin
    setlength(RText,CursorPos);
    OffX := Left + 3 + fRenderUI.WriteText(Left+4, Top+4, Width-8, RText, Font, kaLeft, false, Col).X;
    fRenderUI.WriteLayer(OffX-1, Top+2, 3, Height-4, Col, $FF000000);
  end;
end;


{TKMCheckBox}
constructor TKMCheckBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  Caption:=aCaption;
  Font:=aFont;
  ParentTo(aParent);
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMCheckBox.Paint();
var Box,Tmp:TKMPoint; Col:TColor4;
begin
  Inherited;
  if Enabled then Col:=$FFFFFFFF else Col:=$FF888888;

  Box := fRenderUI.WriteText(Left, Top, Width, '[ ]', Font, kaLeft, false, Col);
  if Checked then
    fRenderUI.WriteText(Left+3, Top-1, Width, 'x', Font, kaLeft, false, Col);

  if Caption <> '' then
    Tmp := fRenderUI.WriteText(Left+Box.X, Top, Width, ' '+Caption, Font, kaLeft, false, Col)
  else
    Tmp := KMPoint(0,0);

  Width  := Tmp.X + Box.X;
  Height := Math.max(Tmp.Y, Box.Y);
end;


constructor TKMPercentBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string; aFont:TKMFont; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  Position:=EnsureRange(aPos,0,100);
  Font:=aFont;
  FontColor:=aColor;
  TextAlign:=kaCenter;
  Caption:=aCaption;
end;


procedure TKMPercentBar.Paint();
begin
  Inherited;
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
  if Caption <> '' then begin //Now draw text over bar, if required
    fRenderUI.WriteText((Left + Width div 2)+2, (Top + Height div 2)-4, Width, Caption, Font, TextAlign, false, $FF000000);
    fRenderUI.WriteText((Left + Width div 2)+1, (Top + Height div 2)-5, Width, Caption, Font, TextAlign, false, FontColor);
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
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFE0E0E0);
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
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFE0E0E0);
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


procedure TKMRatioRow.MouseMove(X,Y:Integer; Shift:TShiftState);
var NewPos: integer;
begin
  Inherited;

  NewPos := Position;
  if (ssLeft in Shift) then   
    NewPos:=EnsureRange(round(MinValue+((X-Left-12)/(Width-28))*(MaxValue-MinValue)),MinValue,MaxValue);
  if NewPos <> Position then
  begin
    Position := NewPos;
    if Assigned(OnChange) then
      OnChange(Self);
  end
  else
    Position := NewPos;
end;


procedure TKMRatioRow.Paint();
var ThumbPos:word;
begin
  Inherited;
  fRenderUI.WriteBevel(Left+2,Top+2,Width-4,Height-4);
  ThumbPos:= round(mix (0,Width-4-24,1-(Position-MinValue) / (MaxValue-MinValue)));
  fRenderUI.WritePicture(Left+ThumbPos+2, Top, 4,132);
  if Enabled then
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FFFFFFFF)
  else
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FF888888);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
begin
  Inherited Create(aLeft, aTop, aWidth, aHeight);
  ParentTo(aParent);
  fScrollAxis := aScrollAxis;
  Position := 1;
  MaxValue := 10;
  MinValue := 1;
  Thumb    := 10;
  Style    := aStyle;
end;


procedure TKMScrollBar.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMScrollBar.MouseMove(X,Y:integer; Shift:TShiftState);
var NewPos: integer;
begin
  Inherited;

  NewPos := Position;
  if (ssLeft in Shift) then begin

    if fScrollAxis = sa_Vertical then
      if InRange(Y,Top+Width,Top+Height-Width) then
        NewPos := round( MinValue+((Y-Top-Width-Thumb/2)/(Height-Width*2-Thumb))*(MaxValue-MinValue) );

    if fScrollAxis = sa_Horizontal then
      if InRange(X,Left+Height,Left+Width-Height) then
        NewPos := round( MinValue+((X-Left-Height-Thumb/2)/(Width-Height*2-Thumb))*(MaxValue-MinValue) );

    if NewPos <> Position then begin
      Position := EnsureRange(NewPos, MinValue, MaxValue);
      if Assigned(OnChange) then
        OnChange(Self);
    end;

  end;
end;


procedure TKMScrollBar.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  if WheelDelta < 0 then IncPosition(Self);
  if WheelDelta > 0 then DecPosition(Self);
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
  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender:TObject);
begin
  Position := EnsureRange(Position-1, MinValue, MaxValue);
  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMScrollBar.Paint();
var ThumbPos:word; State:T3DButtonStateSet;
begin
  Inherited;
  //Copy property to child buttons. Otherwise they won't be rendered
  ScrollDec.Visible := Visible;
  ScrollInc.Visible := Visible;

  ThumbPos := 0;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.WriteBevel(Left, Top+Width, Width, Height - Width*2);
    sa_Horizontal: fRenderUI.WriteBevel(Left+Height, Top, Width - Height*2, Height);
  end;

  if MinValue = MaxValue then begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := (Height-Width*2-Thumb) div 2;
      sa_Horizontal: ThumbPos := (Width-Height*2-Thumb) div 2;
    end;
    State := [bs_Disabled];
    ScrollDec.Disable;
    ScrollInc.Disable;
  end else begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := (Position-MinValue)*(Height-Width*2-Thumb) div (MaxValue-MinValue);
      sa_Horizontal: ThumbPos := (Position-MinValue)*(Width-Height*2-Thumb) div (MaxValue-MinValue);
    end;
    State := [];
    ScrollDec.Enable;
    ScrollInc.Enable;
  end;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.Write3DButton(Left,Top+Width+ThumbPos,Width,Thumb,0,0,State,Style);
    sa_Horizontal: fRenderUI.Write3DButton(Left+Height+ThumbPos,Top,Thumb,Height,0,0,State,Style);
  end;
end;


{ List }
constructor TKMListBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  ItemHeight := 20;
  TopIndex := 0;
  ItemIndex := -1;
  fItems := TStringList.Create;
end;


destructor TKMListBox.Destroy();
begin
  FreeAndNil(fItems);
  Inherited;
end;


procedure TKMListBox.ChangeScrollPosition(Sender:TObject);
begin
  TopIndex := TKMScrollBar(Sender).Position;
end;


procedure TKMListBox.RefreshList();
begin
  ScrollBar.MinValue := 0;
  ScrollBar.MaxValue := Math.max(fItems.Count - (fHeight div ItemHeight),0);
  ScrollBar.Position := EnsureRange(TopIndex, ScrollBar.MinValue, ScrollBar.MaxValue);
  ScrollBar.Enabled := ScrollBar.MaxValue > ScrollBar.MinValue;
end;


procedure TKMListBox.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  TopIndex := EnsureRange(TopIndex - sign(WheelDelta), 0, ScrollBar.MaxValue);
  ScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMListBox.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMListBox.MouseMove(X,Y:integer; Shift:TShiftState);
var NewIndex:integer;
begin
  Inherited;

  if (ssLeft in Shift) and
     (InRange(X, Left, Left+Width-ScrollBar.Width)) and
     (InRange(Y, Top, Top+Height div ItemHeight * ItemHeight))
  then begin
    NewIndex := TopIndex + (Y-Top) div ItemHeight;

    if NewIndex > fItems.Count-1 then NewIndex := -1;

    if (NewIndex<>ItemIndex) then begin
      ItemIndex := NewIndex;
      if Assigned(OnChange) and (ssLeft in Shift) then
        OnChange(Self);
    end;
  end;
end;


procedure TKMListBox.Paint();
var i:integer;
begin
  Inherited;

  RefreshList();

  fRenderUI.WriteBevel(Left, Top, Width-ScrollBar.Width, Height);

  if (ItemIndex <> -1) and (ItemIndex >= TopIndex) and (ItemIndex <= TopIndex+(fHeight div ItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+ItemHeight*(ItemIndex-TopIndex), Width-ScrollBar.Width, ItemHeight, $88888888);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div ItemHeight)-1) do
    fRenderUI.WriteText(Left+8, Top+i*ItemHeight+3, Width, fItems.Strings[TopIndex+i] , fnt_Metal, kaLeft, false, $FFFFFFFF);
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  BoundRectAt := KMPoint(0,0);
end;


procedure TKMMinimap.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift);
end;


procedure TKMMinimap.MouseMove(X,Y:integer; Shift:TShiftState);
begin
  Inherited;
  if ssLeft in Shift then begin
    BoundRectAt := GetMapCoords(X,Y);
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


function TKMMinimap.GetMapCoords(X,Y:integer; const Inset:shortint=0):TKMPoint;
begin
  Assert(Inset>=-1, 'Min allowed inset is -1, to be within TKMPoint range of 0..n');
  Result.X := EnsureRange(X - (Left+(Width -MapSize.X) div 2), 1-Inset, MapSize.X+Inset);
  Result.Y := EnsureRange(Y - (Top +(Height-MapSize.Y) div 2), 1-Inset, MapSize.Y+Inset);
end;


function TKMMinimap.InMapCoords(X,Y:integer):boolean;
begin
  Result := InRange(X, 1, MapSize.X) and InRange(Y, 1, MapSize.Y);
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


{ Files list } //Possible solution for MapEd and Save/Load
constructor TKMFileList.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aLeft,aTop,aWidth,aHeight);
  ParentTo(aParent);
  ItemHeight := 20;
  TopIndex := 0;
  ItemIndex := -1;
  fPaths := TStringList.Create;
  fFiles := TStringList.Create;
  fPath := ExeDir;
end;


destructor TKMFileList.Destroy();
begin
  FreeAndNil(fFiles);
  FreeAndNil(fPaths);
  Inherited;
end;


procedure TKMFileList.ChangeScrollPosition(Sender:TObject);
begin
  TopIndex := TKMScrollBar(Sender).Position;
end;


procedure TKMFileList.RefreshList(aPath,aExtension:string; ScanSubFolders:boolean=false);
var
  DirID:integer;
  SearchRec:TSearchRec;
  DirList:TStringList;
begin
  if not DirectoryExists(aPath) then begin
    fFiles.Clear;
    exit;
  end;

  fPath := aPath;
  fPaths.Clear;
  fFiles.Clear;
  DirList := TStringList.Create;

  DirList.Add(''); //Initialize
  DirID := 0;

  repeat
    ChDir(fPath+DirList[DirID]);
    FindFirst('*', faAnyFile, SearchRec);
    repeat
      if (SearchRec.Name<>'.')and(SearchRec.Name<>'..') then //Exclude parent folders
        if SearchRec.Attr and faDirectory = faDirectory then begin
          if ScanSubFolders then
            DirList.Add(DirList[DirID]+SearchRec.Name+'\')
        end else
        if (aExtension='') or (GetFileExt(SearchRec.Name) = UpperCase(aExtension)) then begin
          fPaths.Add(DirList[DirID]);
          fFiles.Add(SearchRec.Name);
        end;
    until (FindNext(SearchRec)<>0);
    inc(DirID);
    FindClose(SearchRec);
  until(DirID = DirList.Count);

  DirList.Free;

  ScrollBar.MinValue := 0;
  ScrollBar.MaxValue := Math.max(fFiles.Count - (fHeight div ItemHeight),0);
  ScrollBar.Position := 0;
  ScrollBar.Enabled := ScrollBar.MaxValue > ScrollBar.MinValue;
end;


procedure TKMFileList.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  TopIndex := EnsureRange(TopIndex - sign(WheelDelta), 0, ScrollBar.MaxValue);
  ScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


function TKMFileList.FileName():string;
begin
  if InRange(ItemIndex, 0, fFiles.Count) then
    Result := fPath + fPaths.Strings[ItemIndex] + fFiles.Strings[ItemIndex]
  else
    Result := '';
end;


procedure TKMFileList.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMFileList.MouseMove(X,Y:integer; Shift:TShiftState);
var NewIndex:integer;
begin
  Inherited;

  if (ssLeft in Shift) and
     (InRange(X, Left, Left+Width-ScrollBar.Width)) and
     (InRange(Y, Top, Top+Height div ItemHeight * ItemHeight))
  then begin
    NewIndex := TopIndex + (Y-Top) div ItemHeight;

    if NewIndex > fFiles.Count-1 then NewIndex := -1;

    if (NewIndex<>ItemIndex) then begin
      ItemIndex := NewIndex;
      if Assigned(OnChange) and (ssLeft in Shift) then
        OnChange(Self);
    end;
  end;
end;


procedure TKMFileList.Paint();
var i:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width-ScrollBar.Width, Height);

  if (ItemIndex <> -1) and (ItemIndex >= TopIndex) and (ItemIndex <= TopIndex+(fHeight div ItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+ItemHeight*(ItemIndex-TopIndex), Width-ScrollBar.Width, ItemHeight, $88888888);

  for i:=0 to Math.min(fFiles.Count-1, (fHeight div ItemHeight)-1) do
    fRenderUI.WriteText(Left+8, Top+i*ItemHeight+3, Width, TruncateExt(fFiles.Strings[TopIndex+i]) , fnt_Metal, kaLeft, false, $FFFFFFFF);

  if (ItemIndex <> -1) then
  fRenderUI.WriteText(Left+8, Top+Height+4, Width, '\'+fPaths.Strings[ItemIndex] + fFiles.Strings[ItemIndex], fnt_Grey, kaLeft, false, $FFFFFFFF);
end;


{ TKMControlsCollection }
constructor TKMControlsCollection.Create();
begin
  Inherited;
  CtrlPaintCount := 0;
  fCtrlDown  := nil;
  fCtrlFocus := nil;
  fCtrlOver  := nil;
  fCtrlUp    := nil;
  if fRenderUI <> nil then
    fRenderUI := TRenderUI.Create;
end;


destructor TKMControlsCollection.Destroy();
begin
  if fRenderUI <> nil then
    FreeAndNil(fRenderUI);
  Inherited;
end;

procedure TKMControlsCollection.SetCtrlDown(aCtrl:TKMControl);
begin
  if fCtrlDown <> nil then fCtrlDown.State := fCtrlDown.State - [csDown]; //Release previous
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csDown];             //Press new
  fCtrlDown := aCtrl;                                                     //Update info
end;


procedure TKMControlsCollection.SetCtrlFocus(aCtrl:TKMControl);
begin
  if fCtrlFocus <> nil then fCtrlFocus.State := fCtrlFocus.State - [csFocus];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csFocus];
  fCtrlFocus := aCtrl;
end;


procedure TKMControlsCollection.SetCtrlOver(aCtrl:TKMControl);
begin
  if fCtrlOver <> nil then fCtrlOver.State := fCtrlOver.State - [csOver];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csOver];
  fCtrlOver := aCtrl;
end;


procedure TKMControlsCollection.SetCtrlUp(aCtrl:TKMControl);
begin
  fCtrlUp := aCtrl;
  if fCtrlDown = fCtrlUp then CtrlFocus := fCtrlUp else CtrlFocus := nil;
end;


procedure TKMControlsCollection.AddToCollection(Sender:TKMControl);
begin
  Inherited Add(Sender);
end;


function TKMControlsCollection.GetControl(Index: Integer): TKMControl;
begin
  Result := TKMControl(Items[Index]);
end;


procedure TKMControlsCollection.SetControl(Index: Integer; Item: TKMControl);
begin
  Items[Index] := Item;
end;


function TKMControlsCollection.AddPanel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMPanel;
begin
  Result:=TKMPanel.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
  Result.GetCollection := Self;
end;

function TKMControlsCollection.AddBevel(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMBevel;
begin
  Result:=TKMBevel.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddShape(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4):TKMShape;
begin
  Result:=TKMShape.Create(aParent, aLeft,aTop,aWidth,aHeight,aLineColor);
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

function TKMControlsCollection.AddColorSwatch(aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer):TKMColorSwatch;
begin
  Result:=TKMColorSwatch.Create(aParent,aLeft,aTop,aColumns,aRows);
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

function TKMControlsCollection.AddFlatButtonShape(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4):TKMFlatButtonShape;
begin
  Result:=TKMFlatButtonShape.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont,aShapeColor);
  AddToCollection(Result);
end;


function TKMControlsCollection.AddEdit(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean=false):TKMEdit;
begin
  Result:=TKMEdit.Create(aParent, aLeft,aTop,aWidth,aHeight,aFont,aMasked);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddCheckBox(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont):TKMCheckBox;
begin
  Result:=TKMCheckBox.Create(aParent, aLeft,aTop,aWidth,aHeight,aCaption,aFont);
  AddToCollection(Result);
end;

function TKMControlsCollection.AddPercentBar(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini):TKMPercentBar;
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


function TKMControlsCollection.AddListBox(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMListBox;
const ScrollWidth = 20;
begin
  Result := TKMListBox.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);

  Result.ScrollBar := AddScrollBar(aParent, aLeft+aWidth-ScrollWidth, aTop, ScrollWidth, aHeight, sa_Vertical);
  Result.ScrollBar.OnChange := Result.ChangeScrollPosition;
end;


function TKMControlsCollection.AddMinimap(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMMinimap;
begin
  Result:=TKMMinimap.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);
end;


function TKMControlsCollection.AddFileList(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer):TKMFileList;
const ScrollWidth = 20;
begin
  Result := TKMFileList.Create(aParent, aLeft,aTop,aWidth,aHeight);
  AddToCollection(Result);

  Result.ScrollBar := AddScrollBar(aParent, aLeft+aWidth-ScrollWidth, aTop, ScrollWidth, aHeight, sa_Vertical);
  Result.ScrollBar.OnChange := Result.ChangeScrollPosition;
end;


{ Recursing function to find topmost control (excl. Panels)}
function TKMControlsCollection.HitControl(X,Y:integer):TKMControl;
  function ScanChild(P:TKMPanel; aX,aY:integer):TKMControl;
  var i:integer;
  begin
    Result := nil;
    for i:=P.ChildCount downto 1 do
      if (P.Childs[i] is TKMPanel) then begin
        Result := ScanChild(TKMPanel(P.Childs[i]),aX,aY);
        if Result <> nil then exit;
      end else
      if P.Childs[i].HitTest(aX,aY) then begin
        Result := P.Childs[i];
        exit;
      end;
  end;
begin
  Result := ScanChild(TKMPanel(Controls[0]), X, Y);
end;


function TKMControlsCollection.KeyDown(Key: Word; Shift: TShiftState):boolean;
begin
  if CtrlFocus <> nil then
    Result := CtrlFocus.KeyDown(Key, Shift)
  else
    Result := false;
end;


function TKMControlsCollection.KeyUp(Key: Word; Shift: TShiftState):boolean;
begin
  if CtrlFocus <> nil then
    Result := CtrlFocus.KeyUp(Key, Shift)
  else
    Result := false;
end;


procedure TKMControlsCollection.MouseDown(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  CtrlDown := HitControl(X,Y);
  if CtrlDown <> nil then CtrlDown.MouseDown(X,Y,Shift,Button);
end;


procedure TKMControlsCollection.MouseMove(X,Y:Integer; Shift:TShiftState);
begin
  if CtrlDown=nil then //User is dragging some Ctrl (e.g. scrollbar) and went away from Ctrl bounds
    CtrlOver := HitControl(X,Y);
  if CtrlOver <> nil then CtrlOver.MouseMove(X,Y,Shift);
  if (CtrlDown <> nil) and (CtrlOver <> CtrlDown) then CtrlDown := nil;
end;


procedure TKMControlsCollection.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  CtrlUp := HitControl(X,Y);

  //Here comes tricky part, we can't do anything after calling an event (it might Destroy everything,
  //e.g. Exit button, or Resolution change). We need to release CtrlDown (otherwise it remains
  //pressed), but we need to keep csDown state until it's registered by Control.MouseUp
  //to call OnClick. So, we nil fCtrlDown here and Control.MouseUp will reset ControlState
  //Other case, if we don't care for OnClick (CtrlDown<>CtrlUp) - just release the CtrDown as usual
  if CtrlDown <> CtrlUp then
    CtrlDown := nil
  else
    fCtrlDown := nil;

  if CtrlUp <> nil then CtrlUp.MouseUp(X,Y,Shift,Button);
end;


procedure TKMControlsCollection.MouseWheel(X,Y:integer; WheelDelta:integer);
var C:TKMControl;
begin
  C := HitControl(X,Y);
  if C <> nil then C.MouseWheel(C,WheelDelta);
end;


{Paint controls}
{Leave painting of childs to their parent control}
procedure TKMControlsCollection.Paint();
begin
  CtrlPaintCount := 0;
  Controls[0].Paint;

  if MODE_DESIGN_CONTORLS and (CtrlFocus<>nil) then
    with CtrlFocus do
      fRenderUI.WriteText(Left, Top-14, Width, inttostr(Left)+':'+inttostr(Top), fnt_Grey, kaLeft, false, $FFFFFFFF);
end;


procedure TKMControlsCollection.SaveToFile(aFileName:string);
var i:integer; ft:textfile;
begin
  assignfile(ft,aFileName); rewrite (ft);

  for i:=0 to Count-1 do
  with Controls[i] do
  {if Parent = nil then} begin
    writeln(ft, i, '.');
    writeln(ft, ClassName);
    writeln(ft, Format('{%d %d %d %d}', [fLeft, fTop, fWidth, fHeight]));
    writeln(ft);
  end;

  closefile(ft);
end;


end.
