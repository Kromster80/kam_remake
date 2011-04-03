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

  TKMControl = class;
  TKMPanel = class;


{ TKMMaster }
TKMMasterControl = class
  private
    fCtrl:TKMPanel; //Parentmost control (TKMPanel with all its childs)
    fCtrlDown:TKMControl; //Control that was pressed Down
    fCtrlFocus:TKMControl; //Control which has input Focus
    fCtrlOver:TKMControl; //Control which has cursor Over it
    fCtrlUp:TKMControl; //Control above which cursor was released

    fOnHint:TNotifyEvent; //Comes along with OnMouseOver

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

    property OnHint: TNotifyEvent write fOnHint;

    function KeyDown    (Key: Word; Shift: TShiftState):boolean;
    function KeyUp      (Key: Word; Shift: TShiftState):boolean;
    procedure MouseDown (X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
    procedure MouseMove (X,Y:integer; Shift:TShiftState);
    procedure MouseUp   (X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
    procedure MouseWheel(X,Y:integer; WheelDelta:integer);

    procedure Paint;

    procedure SaveToFile(aFileName:string);
end;


{Base class for all TKM elements}
TKMControl = class
  private
    fParent: TKMPanel;

    fLeft: Integer;
    fTop: Integer;
    fWidth: Integer;
    fHeight: Integer;

    fEnabled: boolean;
    fVisible: boolean;

    fOnClick:TNotifyEvent;
    fOnClickEither:TNotifyEventMB;
    fOnClickRight:TNotifyEvent;
    fOnMouseWheel:TNotifyEventMW;
    //fOnMouseOver:TNotifyEvent;

    function GetLeft: Integer;
    function GetTop: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHeight(aValue:Integer); virtual;
    procedure SetWidth(aValue:Integer); virtual;

    function GetVisible:boolean;
    procedure SetVisible(aValue:boolean); virtual;
    procedure SetEnabled(aValue:boolean); virtual;
  public
    Anchors: TAnchors;
    State: TKMControlStateSet; //Each control has it localy to avoid quering Collection on each Render

    Tag: integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    function HitTest(X, Y: Integer): Boolean; virtual;

    property Parent: TKMPanel read fParent;
    property Left: Integer read GetLeft write fLeft;
    property Top: Integer read GetTop write fTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Enabled:boolean read fEnabled write SetEnabled;
    property Visible:boolean read GetVisible write SetVisible;
    procedure Enable;
    procedure Disable;
    procedure Show;
    procedure Hide;
    procedure Center;
    procedure Stretch;
    function MasterParent:TKMPanel;

    function KeyDown(Key: Word; Shift: TShiftState):boolean; virtual;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; virtual;
    procedure MouseDown (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseMove (X,Y:integer; Shift:TShiftState); virtual;
    procedure MouseUp   (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); virtual;

    property OnClick: TNotifyEvent write fOnClick;
    property OnClickEither: TNotifyEventMB write fOnClickEither;
    property OnClickRight: TNotifyEvent write fOnClickRight;
    property OnMouseWheel: TNotifyEventMW write fOnMouseWheel;
    //property OnMouseOver: TNotifyEvent write fOnMouseOver;

    procedure Paint; virtual;
end;


{ Panel which keeps child items in it, it's virtual and invisible }
TKMPanel = class(TKMControl)
  private
    GetCollection:TKMMasterControl;
    procedure SetHeight(aValue:Integer); override;
    procedure SetWidth(aValue:Integer); override;
  public
    ChildCount:word;             //Those two are actually used only for TKMPanel
    Childs: array of TKMControl; //No other elements needs to be parented
    constructor Create(aParent:TKMMasterControl; aLeft,aTop,aWidth,aHeight:integer); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer); overload;
    destructor Destroy; override;
    procedure AddChild(aChild:TKMControl);
    procedure Paint; override;
end;


{ Beveled area }
TKMBevel = class(TKMControl)
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    procedure Paint; override;
end;


{Rectangle}
TKMShape = class(TKMControl)
  public
    Hitable:boolean;
    FillColor:TColor4;
    LineColor:TColor4; //color of outline
    LineWidth:byte;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4);
    function HitTest(X, Y: Integer): Boolean; override;
    procedure Paint; override;
end;


{Text Label}
TKMLabel = class(TKMControl)
  private
    fCaption: string;
    fText:string;
    procedure SetCaption(aCaption:string);
    procedure ReformatText;
  public
    Font: TKMFont;
    FontColor: TColor4;
    TextAlign: KAlign;
    AutoWrap: boolean; //Wherever to automatically wrap text within given text area width
    SmoothScrollToTop: integer; //Delta between this and TimeGetTime affects vertical position
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: KAlign; aColor:TColor4=$FFFFFFFF);
    function HitTest(X, Y: Integer): Boolean; override;
    property Caption:string read fCaption write SetCaption;
    function TextHeight:integer;
    procedure Paint; override;
end;


{Image}
TKMImage = class(TKMControl)
  public
    RXid: integer; //RX library
    TexID: integer;
    ImageAnchors: TAnchors;
    Highlight:boolean;
    HighlightOnMouseOver:boolean;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4);
    procedure ImageStretch;
    procedure ImageCenter;
    procedure Paint; override;
end;


{Image stack - for army formation view}
TKMImageStack = class(TKMControl)
  private
    fRXid: integer; //RX library
    fTexID: integer;
    fCount: integer;
    fColumns: integer;
    fDrawWidth: integer;
    fDrawHeight: integer;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4);
    procedure SetCount(aCount,aColumns:integer);
    procedure Paint; override;
end;


{ Color swatch - to select a color from given samples/palette }
TKMColorSwatch = class(TKMControl)
  private
    CellSize:byte; //in pixels
    SelectedColor:byte; //Index 0..255
    Columns:byte;
    Rows:byte;
    Colors:array of TColor4; //Range is 0..255
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer);
    function GetColor:TColor4;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
end;


{3DButton}
TKMButton = class(TKMControl)
  private
    fCaption: string;
    fFont: TKMFont;
    fTextAlign: KAlign;
    fStyle: TButtonStyle;
    fMakesSound: boolean;
    fRXid: integer; //RX library
    fTexID: integer;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4; aStyle:TButtonStyle=bsGame); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle=bsGame); overload;
    property Caption:string read fCaption write fCaption;
    property MakesSound:boolean read fMakesSound write fMakesSound;
    property TexID:integer read fTexID write fTexID;
    function DoPress:boolean;
    function DoClick:boolean; //Try to click a button and return TRUE if succeded
    procedure MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
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
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4);
    procedure MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
end;


{FlatButton with Shape on it}
TKMFlatButtonShape = class(TKMControl)
  public
    CapOffsetY:shortint;
    Caption: string;
    Font: TKMFont;
    ShapeColor:TColor4;
    Down:boolean;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4);
    procedure Paint; override;
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
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean=false);
    property Text:string read fText write SetText;
    property OnChange: TNotifyEvent write fOnChange;
    property OnKeyDown: TNotifyEventKey write fOnKeyDown;
    function KeyDown(Key: Word; Shift: TShiftState):boolean; override;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; override;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
end;


{ Checkbox }
TKMCheckBox = class(TKMControl)
  private
    fCaption:string;
    fChecked:boolean;
    fFlatStyle:boolean; //Render the check as a rectangle ()modern style
    fFont:TKMFont;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
    property Caption:string read fCaption;
    property Checked:boolean read fChecked write fChecked;
    property FlatStyle:boolean read fFlatStyle write fFlatStyle;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
end;


{ TKMRadioGroup }
TKMRadioGroup = class(TKMControl)
  private
    fItemIndex:integer;
    fItems:TStringList;
    fFont:TKMFont;
    function GetItemCount:integer;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    destructor Destroy; override;
    property ItemCount:integer read GetItemCount;
    property ItemIndex:integer read fItemIndex write fItemIndex;
    property Items:TStringList read fItems;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
end;


{Percent bar}
TKMPercentBar = class(TKMControl)
  public
    Position: integer;
    Caption: string;
    Font: TKMFont;
    FontColor:TColor4;
    TextAlign: KAlign;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini; aColor:TColor4=$FFFFFFFF);
    procedure Paint; override;
end;


{Resource bar}
TKMResourceRow = class(TKMControl)
  public
    Resource: TResourceType;
    ResourceCount: integer;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);
    procedure Paint; override;
end;


{Resource order bar}
TKMResourceOrderRow = class(TKMControl)
  private
    fOrderAdd:TKMButton;
    fOrderLab:TKMLabel;
    fOrderRem:TKMButton;
    procedure SetVisible(aValue:boolean); override;
  public
    Resource: TResourceType;
    ResourceCount: integer;
    OrderCount:word;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aRes:TResourceType; aCount:integer);

    property OrderAdd:TKMButton read fOrderAdd;
    property OrderRem:TKMButton read fOrderRem;

    procedure Paint; override;
end;


{Production cost bar}
TKMCostsRow = class(TKMControl)
  public
    CostID:byte;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte);
    procedure Paint; override;
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
    procedure Paint; override;
end;


TScrollAxis = (sa_Vertical, sa_Horizontal);

{Scroll bar}
TKMScrollBar = class(TKMControl)
  private
    fBackAlpha:single; //Alpha of background (usually 0.5, dropbox 1)
    fScrollAxis:TScrollAxis;
    fOnChange:TNotifyEvent;
    procedure IncPosition(Sender:TObject);
    procedure DecPosition(Sender:TObject);
    procedure RefreshItems;
    procedure SetHeight(aValue:Integer); override;
    procedure SetEnabled(aValue:boolean); override;
    procedure SetVisible(aValue:boolean); override;
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    Thumb:word;
    ScrollDec:TKMButton;
    ScrollInc:TKMButton;
    Style:TButtonStyle;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
    destructor Destroy; override;

    property BackAlpha:single write fBackAlpha;

    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
end;


TKMListBox = class(TKMControl)
  private
    fBackAlpha:single; //Alpha of background (usually 0.5, dropbox 1)
    fItemHeight:byte;
    fItemIndex:smallint;
    fItems:TStringList;
    fTopIndex:smallint; //up to 32k files
    fScrollBar:TKMScrollBar;
    fOnChange:TNotifyEvent;
    procedure ChangeScrollPosition (Sender:TObject);
    procedure SetHeight(aValue:Integer); override;
    procedure SetVisible(aValue:boolean); override;
    procedure SetTopIndex(aIndex:smallint);
    procedure SetBackAlpha(aValue:single);
    procedure SetEnabled(aValue:boolean); override;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    destructor Destroy; override;

    procedure RefreshList;

    property BackAlpha:single write SetBackAlpha;
    property ItemHeight:byte read fItemHeight;
    property ItemIndex:smallint read fItemIndex write fItemIndex;
    property Items:TStringList read fItems;
    property TopIndex:smallint read fTopIndex write SetTopIndex;

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent write fOnChange;

    procedure Paint; override;
end;


TKMDropBox = class(TKMControl)
  private
    fCaption:string;
    fDropCount:byte;
    fFont: TKMFont;
    fButton:TKMButton;
    fList:TKMListBox;
    fShape:TKMShape;
    fOnChange:TNotifyEvent;
    procedure ListShow(Sender:TObject);
    procedure ListClick(Sender:TObject);
    procedure ListHide(Sender:TObject);
    function GetItems:TStringList;
    function GetItemIndex:smallint;
    procedure SetItemIndex(aIndex:smallint);
    procedure SetEnabled(aValue:boolean); override;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    destructor Destroy; override;
    property DropCount:byte write fDropCount;
    property ItemIndex:smallint read GetItemIndex write SetItemIndex;
    property Items:TStringList read GetItems;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
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
    procedure Paint; override;
end;


{ Files list } //Possible solution for MapEd and Save/Load
TKMFileList = class(TKMControl)
  private
    ItemHeight:byte;
    fPath:string;
    fScrollBar:TKMScrollBar;
    fOnChange:TNotifyEvent;
    procedure ChangeScrollPosition (Sender:TObject);
    procedure SetVisible(aValue:boolean); override;
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
    property OnChange: TNotifyEvent write fOnChange;

    procedure Paint; override;
end;


implementation
uses KM_RenderUI, KM_ResourceGFX, KM_Sound, KM_Utils;


{ TKMControl }
constructor TKMControl.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create;
  fLeft     := aLeft;
  fTop      := aTop;
  fWidth    := aWidth;
  fHeight   := aHeight;
  Anchors   := [akLeft, akTop];
  State     := [];
  fEnabled  := true;
  fVisible  := true;
  Tag       := 0;
  Hint      := '';

  //Parent will be Nil only for master Panel which contains all the controls in it
  fParent   := aParent;
  if aParent<>nil then TKMPanel(aParent).AddChild(Self);
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
  //if Assigned(fOnMouseOver) then fOnMouseOver(Self);
end;


{ Send Click events }
procedure TKMControl.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  //if Assigned(fOnMouseUp) then OnMouseUp(Self); { Unused }
  if (csDown in State) then begin
    State := State - [csDown];
    if ((Button = mbLeft)or(Button = mbRight)) and Assigned(fOnClickEither) then fOnClickEither(Self, Button)
    else
    if (Button = mbLeft) and Assigned(fOnClick) then fOnClick(Self)
    else
    if (Button = mbRight) and Assigned(fOnClickRight) then fOnClickRight(Self)
  end;
end;


procedure TKMControl.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  if Assigned(fOnMouseWheel) then fOnMouseWheel(Self, WheelDelta);
end;


//fVisible is checked earlier
function TKMControl.HitTest(X, Y: Integer): Boolean;
begin
  Result := fEnabled and InRange(X, Left, Left + fWidth) and InRange(Y, Top, Top + fHeight);
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint;
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
      kaLeft:   fRenderUI.WriteLayer(Left, Top, fWidth, fHeight, $2000FFFF);
      kaCenter: fRenderUI.WriteLayer(Left - fWidth div 2, Top, fWidth, fHeight, $2000FFFF);
      kaRight:  fRenderUI.WriteLayer(Left - fWidth, Top, fWidth, fHeight, $2000FFFF);
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

  fRenderUI.WriteLayer(Left, Top, fWidth, fHeight, sColor);
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

//Overriden in child classes
procedure TKMControl.SetHeight(aValue:Integer);
begin
  fHeight := aValue;
end;

//Overriden in child classes
procedure TKMControl.SetWidth(aValue:Integer);
begin
  fWidth := aValue;
end;


{Check Control including all its Parents to see if Control is actually displayed/visible}
function TKMControl.GetVisible:boolean;
var C:TKMControl;
begin
  Result := fVisible;
  C := Parent;
  while C<>nil do begin
    Result := Result and C.fVisible;
    C := C.Parent;
  end;
end;


procedure TKMControl.SetEnabled(aValue:boolean);
begin
  fEnabled := aValue;
end;


procedure TKMControl.SetVisible(aValue:boolean);
begin
  fVisible := aValue;
end;


procedure TKMControl.Enable;  begin SetEnabled(true);  end; //Overrides will be set too
procedure TKMControl.Disable; begin SetEnabled(false); end;


{Will show up entire branch in which control resides}
procedure TKMControl.Show;
begin
  if Parent<>nil then Parent.Show;
  Visible := true;
end;


procedure TKMControl.Hide;    begin Visible := false; end;
procedure TKMControl.Center;  begin Anchors := []; end;
procedure TKMControl.Stretch; begin Anchors := [akLeft, akTop, akRight, akBottom]; end;


function TKMControl.MasterParent:TKMPanel;
var P:TKMPanel;
begin
  P := Parent;
  while P.Parent <> nil do
    P := P.Parent;
  Result := P;
end;



{ TKMPanel } //virtual panels to contain child items
constructor TKMPanel.Create(aParent:TKMMasterControl; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(nil, aLeft,aTop,aWidth,aHeight);
  GetCollection := aParent;
  aParent.fCtrl := Self;
end;


constructor TKMPanel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  GetCollection := aParent.GetCollection;
end;


destructor TKMPanel.Destroy;
var i:integer;
begin
  for i:=1 to ChildCount do
    Childs[i].Free;
  Inherited;
end;


{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMPanel.AddChild(aChild:TKMControl);
begin
  inc(ChildCount);
  {Hereby I still try to make a rule to count starting from 1, not from zero}
  SetLength(Childs, ChildCount+1);
  Childs[ChildCount] := aChild;
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
procedure TKMPanel.Paint;
var i:integer;
begin
  Inherited;
  for i:=1 to ChildCount do
    if Childs[i].fVisible then
      Childs[i].Paint;
end;


{ TKMBevel }
constructor TKMBevel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
end;


procedure TKMBevel.Paint;
begin
  Inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
end;


{ TKMShape }
constructor TKMShape.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
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


procedure TKMShape.Paint;
begin
  Inherited;
  fRenderUI.WriteLayer(Left,Top,Width,Height,FillColor,$00000000);
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,LineColor);
end;


constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: KAlign; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
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
  ReformatText
end;


function TKMLabel.HitTest(X, Y: Integer): Boolean;
begin
  case TextAlign of
    kaLeft: Result := InRange(X, Left, Left + Width) and InRange(Y, Top, Top + Height);
    kaCenter: Result := InRange(X, Left - Width div 2, Left + Width div 2) and InRange(Y, Top, Top + Height);
    kaRight: Result := InRange(X, Left - Width, Left) and InRange(Y, Top, Top + Height);
    else Result := false;
  end;
end;


function TKMLabel.TextHeight:integer;
begin
  ReformatText;
  Result := fRenderUI.WriteText(0, 0, Width, fText, Font, TextAlign, AutoWrap, $00000000).Y;
end;


//Existing EOLs should be preserved, and new ones added where needed.
procedure TKMLabel.ReformatText;
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
                    else inc(AdvX, FontData[Font].Letters[byte(fText[i])].Width + CharSpacing);

    //This algorithm is not perfect, somehow line width is not within SizeX, but very rare
    if ((AdvX>Width)and(LastSpace<>-1))or(fText[i]=#124) then
    begin
      fText[LastSpace] := #124; //Replace last whitespace with EOL
      dec(AdvX, PrevX); //Subtract width since replaced whitespace
    end;
  end;
end;


{Send caption to render and recieve in result how much space did it took on screen}
procedure TKMLabel.Paint;
var Tmp:TKMPoint; NewTop:integer; Col:cardinal;
begin
  Inherited;

  if SmoothScrollToTop = 0 then
    NewTop := Top
  else
  begin //Setup clipping planes
    fRenderUI.SetupClip(Top, Top+Height);
    NewTop := Top + Height - (integer(TimeGetTime) - SmoothScrollToTop) div 50 //Compute delta and shift by it upwards (Credits page)
  end;

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  Tmp := fRenderUI.WriteText(Left, NewTop, Width, fText, Font, TextAlign, AutoWrap, Col);

  if not AutoWrap then 
    Width := Tmp.X;

  if SmoothScrollToTop = 0 then
    Height := Tmp.Y
  else
    fRenderUI.ReleaseClip;
end;


constructor TKMImage.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID:integer; aRXid:integer=4);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  RXid := aRXid;
  TexID := aTexID;
  ImageAnchors := [akLeft, akTop];
  Highlight := false;
  HighlightOnMouseOver := false;
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
procedure TKMImage.Paint;
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
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, DrawWidth, DrawHeight, RXid, TexID, fEnabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight)
  else
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, RXid, TexID, fEnabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight);
end;


{ TKMImageStack }
constructor TKMImageStack.Create(aParent:TKMPanel; aLeft, aTop, aWidth, aHeight, aTexID:integer; aRXid:integer=4);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fRXid  := aRXid;
  fTexID := aTexID;
  fCount := 0;
  fColumns := 0;
  fDrawWidth := 0;
  fDrawHeight := 0;
end;


procedure TKMImageStack.SetCount(aCount,aColumns:integer);
var Aspect: single;
begin
  fCount := aCount;
  fColumns := Math.max(1,aColumns);
  fDrawWidth  := EnsureRange(Width div fColumns, 8, GFXData[fRXid, fTexID].PxWidth);
  fDrawHeight := EnsureRange(Height div ceil(fCount/fColumns), 6, GFXData[fRXid, fTexID].PxHeight);

  Aspect := GFXData[fRXid, fTexID].PxWidth / GFXData[fRXid, fTexID].PxHeight;
  if fDrawHeight * Aspect <= fDrawWidth then
    fDrawWidth  := round(fDrawHeight * Aspect)
  else
    fDrawHeight := round(fDrawWidth / Aspect);
end;


{If image area is bigger than image - do center image in it}
procedure TKMImageStack.Paint;
var
  i:integer;
  OffsetX, OffsetY, CenterX, CenterY:smallint; //variable parameters
begin
  Inherited;
  if (fTexID=0)or(fRXid=0) then exit; //No picture to draw

  OffsetX := Width div fColumns;
  OffsetY := Height div ceil(fCount/fColumns);

  CenterX := (Width - OffsetX * (fColumns-1) - fDrawWidth) div 2;
  CenterY := (Height - OffsetY * (ceil(fCount/fColumns)-1) - fDrawHeight) div 2;

  for i := 1 to fCount do
    fRenderUI.WritePicture(Left + CenterX + OffsetX*((i-1) mod fColumns),
                            Top + CenterY + OffsetY*((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, fRXid, fTexID, fEnabled);
end;


{ TKMColorSwatch }
constructor TKMColorSwatch.Create(aParent:TKMPanel; aLeft,aTop,aColumns,aRows:integer);
var i,k:integer;
begin
  Inherited Create(aParent, aLeft, aTop, 0, 0);

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
end;


function TKMColorSwatch.GetColor:TColor4;
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


procedure TKMColorSwatch.Paint;
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
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4; aStyle:TButtonStyle=bsGame);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fRXid       := aRXid;
  fTexID      := aTexID;
  fCaption    := '';
  fStyle      := aStyle;
  fMakesSound := true;
end;


{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aStyle:TButtonStyle=bsGame);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fRXid       := 0;
  fTexID      := 0;
  fCaption    := aCaption;
  fFont       := aFont;
  fTextAlign  := kaCenter; //Thats default everywhere in KaM
  fStyle      := aStyle;
  fMakesSound := true;
end;


//DoPress is called by keyboard shortcuts
//It's important that Control must be:
// IsVisible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.DoPress:boolean;
begin
  //Mark self as CtrlDown
  if Visible and fEnabled then begin
    TKMPanel(Parent).GetCollection.CtrlDown := Self;
    Result := true;
  end else
    Result := false;
end;


//DoClick is called by keyboard shortcuts
//It's important that Control must be:
// IsVisible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.DoClick:boolean;
begin
  if Visible and fEnabled then begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    TKMPanel(Parent).GetCollection.CtrlOver := Self;
    TKMPanel(Parent).GetCollection.CtrlUp := Self;
    if Assigned(fOnClick) then fOnClick(Self);
    Result := true; //Click has happened
  end else
    Result := false; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  if fEnabled and fMakesSound and (csDown in State) then fSoundLib.Play(sfx_Click);
  Inherited;
end;


procedure TKMButton.Paint;
var StateSet:T3DButtonStateSet;
begin
  Inherited;
  StateSet:=[];
  if (csOver in State) and fEnabled then StateSet:=StateSet+[bs_Highlight];
  if (csDown in State) then StateSet:=StateSet+[bs_Down];
  if not fEnabled then StateSet:=StateSet+[bs_Disabled];
  fRenderUI.Write3DButton(Left,Top,Width,Height,fRXid,fTexID,StateSet,fStyle);
  if fTexID=0 then
    if fEnabled then //If disabled then text should be faded
      fRenderUI.WriteText(Left + Width div 2 +byte(csDown in State), (Top + Height div 2)-7+byte(csDown in State), Width, fCaption, fFont, fTextAlign, false, $FFFFFFFF)
    else
      fRenderUI.WriteText(Left + Width div 2, (Top + Height div 2)-7, Width, fCaption, fFont, fTextAlign, false, $FF888888);
end;


{Simple version of button, with image and nothing more}
constructor TKMButtonFlat.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID:integer; aRXid:integer=4);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
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
end;


procedure TKMButtonFlat.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  if fEnabled and (csDown in State) then fSoundLib.Play(sfx_Click);
  Inherited;
end;


procedure TKMButtonFlat.Paint;
var StateSet:TFlatButtonStateSet;
begin
  Inherited;
  StateSet:=[];
  if (csOver in State) and fEnabled and not HideHighlight then StateSet:=StateSet+[fbs_Highlight];
  if (csDown in State) or Down then StateSet:=StateSet+[fbs_Selected];
  //if not Enabled then StateSet:=StateSet+[fbs_Disabled];

  fRenderUI.WriteFlatButton(Left,Top,Width,Height,RXid,TexID,TexOffsetX,TexOffsetY,CapOffsetY,Caption,StateSet);
end;


constructor TKMFlatButtonShape.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aShapeColor:TColor4);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Caption     := aCaption;
  CapOffsetY  := 0;
  ShapeColor  := aShapeColor;
  Font        := aFont;
  Down        := false;
end;


procedure TKMFlatButtonShape.Paint;
begin
  Inherited;  
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteLayer(Left+1,Top+1,Width-2,Width-2, ShapeColor, $00000000);
  fRenderUI.WriteText(Left+(Width div 2),Top+(Height div 2)+4+CapOffsetY,Width, Caption, Font, kaCenter, false, $FFFFFFFF);
  if (csOver in State) and fEnabled then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $40FFFFFF, $00);
  if (csDown in State) or Down then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $00000000, $FFFFFFFF);
end;


{TKMEdit}
constructor TKMEdit.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aMasked:boolean=false);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fText := '<<<LEER>>>';
  Font := aFont;
  Masked := aMasked;
  CursorPos := length(fText);
end;


procedure TKMEdit.SetText(aText:string);
begin
  fText := aText;
  CursorPos := math.min(CursorPos, length(fText));
end;


function TKMEdit.KeyDown(Key: Word; Shift: TShiftState):boolean;
  function ValidKey(aKey:word):boolean;
  begin //Utility, Numbers, NumPad numbers, Letters
    Result := chr(aKey) in [' ', '_', '.', '!', '(', ')', '0'..'9', #96..#105, 'A'..'Z'];
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


procedure TKMEdit.Paint;
var Col:TColor4; RText:String; OffX:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;
  if Masked then RText := StringOfChar('*', Length(fText)) else RText := fText;

  fRenderUI.WriteText(Left+4, Top+4, Width-8, RText, Font, kaLeft, false, Col);

  if (csFocus in State) and ((TimeGetTime div 500) mod 2 = 0)then begin
    setlength(RText,CursorPos);
    OffX := Left + 3 + fRenderUI.WriteText(Left+4, Top+4, Width-8, RText, Font, kaLeft, false, Col).X;
    fRenderUI.WriteLayer(OffX-1, Top+2, 3, Height-4, Col, $FF000000);
  end;
end;


{ TKMCheckBox }
constructor TKMCheckBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont     := aFont;
  fCaption  := aCaption;
end;


procedure TKMCheckBox.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  if (csDown in State) and (Button = mbLeft) then
    fChecked := not fChecked;
  Inherited; //There are OnMouseUp and OnClick events there
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMCheckBox.Paint;
var Box,Tmp:TKMPoint; Col:TColor4;
begin
  Inherited;
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if fFlatStyle then begin
    fRenderUI.WriteBevel(Left, Top, Width, Height, true);
    Box.X := Width;
    Box.Y := Height;
    if fChecked then
      fRenderUI.WriteLayer(Left+4, Top+4, Width-8, Height-8, $C0A0A0A0, $D0A0A0A0);
  end else begin
    Box := fRenderUI.WriteText(Left, Top, Width, '[ ]', fFont, kaLeft, false, Col);
    if fChecked then
      fRenderUI.WriteText(Left+3, Top-1, Width, 'x', fFont, kaLeft, false, Col);
  end;

  if fCaption <> '' then
    Tmp := fRenderUI.WriteText(Left+Box.X, Top, Width, ' '+fCaption, fFont, kaLeft, false, Col)
  else
    Tmp := KMPoint(0,0);

  Width  := Tmp.X + Box.X;
  Height := Math.max(Tmp.Y, Box.Y);
end;


{ TKMRadioGroup }
constructor TKMRadioGroup.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fItemIndex := -1;
  fItems := TStringList.Create;
end;


destructor TKMRadioGroup.Destroy;
begin
  fItems.Free;
  Inherited;
end;


function TKMRadioGroup.GetItemCount:integer;
begin
  Result := fItems.Count;
end;


procedure TKMRadioGroup.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  if (csDown in State) and (Button = mbLeft) and (X-Left < 20) then
    fItemIndex := (Y-Top) div round(Height/ItemCount);
  Inherited; //There are OnMouseUp and OnClick events there
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMRadioGroup.Paint;
var Box:TKMPoint; Col:TColor4; LineHeight:integer; i:integer;
begin
  Inherited;
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  LineHeight := round(fHeight / ItemCount);
  for i:=0 to ItemCount-1 do
  begin
    Box := fRenderUI.WriteText(Left, Top + i*LineHeight, Width, '[ ]', fFont, kaLeft, false, Col);
    if fItemIndex = i then
      fRenderUI.WriteText(Left+3, Top + i*LineHeight - 1, Width, 'x', fFont, kaLeft, false, Col);

    fRenderUI.WriteText(Left+Box.X, Top + i*LineHeight, Width, ' '+fItems.Strings[i], fFont, kaLeft, false, Col);
  end;
end;


{ TKMPercentBar }
constructor TKMPercentBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Position:=EnsureRange(aPos,0,100);
  Font:=aFont;
  FontColor:=aColor;
  TextAlign:=kaCenter;
  Caption:=aCaption;
end;


procedure TKMPercentBar.Paint;
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
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Resource := aRes;
  ResourceCount := aCount;
end;


procedure TKMResourceRow.Paint;
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
  Inherited Create(aParent, aLeft+68,aTop,aWidth-68,aHeight);
  Resource := aRes;
  ResourceCount := aCount;
  OrderCount := 0;

  fOrderRem := TKMButton.Create(aParent,aLeft,aTop+2,20,aHeight,'-',fnt_Metal, bsGame);
  fOrderLab := TKMLabel.Create(aParent,aLeft+33,aTop+4,0,0,'',fnt_Grey,kaCenter);
  fOrderAdd := TKMButton.Create(aParent,aLeft+46,aTop+2,20,aHeight,'+',fnt_Metal, bsGame);
end;


//Copy property to buttons. Otherwise they won't be rendered
procedure TKMResourceOrderRow.SetVisible(aValue:boolean);
begin
  Inherited;
  fOrderRem.Visible := fVisible;
  fOrderLab.Visible := fVisible;
  fOrderAdd.Visible := fVisible;
end;


procedure TKMResourceOrderRow.Paint;
var i:integer;
begin
  Inherited;
  fOrderRem.Top := fTop; //Use internal fTop instead of GetTop (which will return absolute value)
  fOrderLab.Top := fTop + 4;
  fOrderAdd.Top := fTop;

  fOrderLab.Caption := inttostr(OrderCount);

  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width, TypeToString(Resource), fnt_Game, kaLeft, false, $FFE0E0E0);
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, 4,350+byte(Resource));
end;


{ TKMCostsRow }
constructor TKMCostsRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aProductionCostID:byte);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  CostID:=aProductionCostID;
end;


procedure TKMCostsRow.Paint;
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
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
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


procedure TKMRatioRow.Paint;
var ThumbPos:word;
begin
  Inherited;
  fRenderUI.WriteBevel(Left+2,Top+2,Width-4,Height-4);
  ThumbPos:= round(mix (0,Width-4-24,1-(Position-MinValue) / (MaxValue-MinValue)));
  fRenderUI.WritePicture(Left+ThumbPos+2, Top, 4,132);
  if fEnabled then
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FFFFFFFF)
  else
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, Width, inttostr(Position), fnt_Metal, kaCenter, false, $FF888888);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fBackAlpha := 0.5;
  fScrollAxis := aScrollAxis;
  Position := 0;
  MinValue := 0;
  MaxValue := 10;
  Thumb    := 10;
  Style    := aStyle;

  if aScrollAxis = sa_Vertical then begin
    ScrollDec := TKMButton.Create(aParent, aLeft, aTop, aWidth, aWidth, 4, 4, aStyle);
    ScrollInc := TKMButton.Create(aParent, aLeft, aTop+aHeight-aWidth, aWidth, aWidth, 5, 4, aStyle);
  end;
  if aScrollAxis = sa_Horizontal then begin
    ScrollDec := TKMButton.Create(aParent, aLeft, aTop, aHeight, aHeight, 2, 4, aStyle);
    ScrollInc := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 3, 4, aStyle);
  end;
  ScrollDec.OnClick := DecPosition;
  ScrollInc.OnClick := IncPosition;
  RefreshItems;
end;


destructor TKMScrollBar.Destroy;
begin
  Inherited;
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
      if Assigned(fOnChange) then
        fOnChange(Self);
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
procedure TKMScrollBar.RefreshItems;
begin
  case fScrollAxis of
    sa_Vertical:   Thumb := Math.max(0, (Height-2*Width)) div 4;
    sa_Horizontal: Thumb := Math.max(0, (Width-2*Height)) div 4;
  end;
end;


procedure TKMScrollBar.SetHeight(aValue:Integer);
begin
  Inherited;
  if fScrollAxis = sa_Vertical then
    ScrollInc.Top := fTop+fHeight-fWidth;
end;


procedure TKMScrollBar.SetEnabled(aValue:boolean);
begin
  Inherited;
  ScrollDec.Enabled := aValue;
  ScrollInc.Enabled := aValue;
end;


//Copy property to child buttons. Otherwise they won't be rendered
procedure TKMScrollBar.SetVisible(aValue:boolean);
begin
  Inherited;
  ScrollDec.Visible := fVisible;
  ScrollInc.Visible := fVisible;
end;


procedure TKMScrollBar.IncPosition(Sender:TObject);
begin
  Position := EnsureRange(Position+1, MinValue, MaxValue);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender:TObject);
begin
  Position := EnsureRange(Position-1, MinValue, MaxValue);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.Paint;
var ThumbPos:word; State:T3DButtonStateSet;
begin
  Inherited;
  ThumbPos := 0;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.WriteBevel(Left, Top+Width, Width, Height - Width*2, false, fBackAlpha);
    sa_Horizontal: fRenderUI.WriteBevel(Left+Height, Top, Width - Height*2, Height, false, fBackAlpha);
  end;

  if MinValue = MaxValue then begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := max((Height-Width*2-Thumb),0) div 2;
      sa_Horizontal: ThumbPos := max((Width-Height*2-Thumb),0) div 2;
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
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fBackAlpha := 0.5;
  fItemHeight := 20;
  fTopIndex := 0;
  fItemIndex := -1;
  fItems := TStringList.Create;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-fItemHeight, aTop, fItemHeight, aHeight, sa_Vertical, bsGame);
  fScrollBar.fOnChange := ChangeScrollPosition;
end;


destructor TKMListBox.Destroy;
begin
  fItems.Free;
  Inherited;
end;


procedure TKMListBox.ChangeScrollPosition(Sender:TObject);
begin
  fTopIndex := fScrollBar.Position;
end;


procedure TKMListBox.SetHeight(aValue:Integer);
begin
  Inherited;
  fScrollBar.Height := fHeight;
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMListBox.SetVisible(aValue:boolean);
begin
  Inherited;
  fScrollBar.Visible := fVisible; //Hide scrollbar and its buttons
end;


procedure TKMListBox.SetTopIndex(aIndex:smallint);
begin
  fTopIndex := EnsureRange(aIndex, 0, max(fItems.Count - (fHeight div fItemHeight),0));
end;


procedure TKMListBox.SetBackAlpha(aValue:single);
begin
  fBackAlpha := aValue;
  fScrollBar.BackAlpha := aValue;
end;


procedure TKMListBox.SetEnabled(aValue:boolean);
begin
  Inherited;
  fScrollBar.Enabled := aValue;
end;


//fItems.Count has changed
procedure TKMListBox.RefreshList;
begin
  fScrollBar.MaxValue := Math.max(fItems.Count - (fHeight div fItemHeight),0);
  fScrollBar.Position := fTopIndex;
  fScrollBar.Enabled := fScrollBar.MaxValue > fScrollBar.MinValue;
end;


procedure TKMListBox.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  fTopIndex := EnsureRange(TopIndex - sign(WheelDelta), 0, fScrollBar.MaxValue);
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
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
     (InRange(X, Left, Left+Width-fScrollBar.Width)) and
     (InRange(Y, Top, Top+Height div fItemHeight * fItemHeight))
  then begin
    NewIndex := TopIndex + (Y-Top) div fItemHeight;

    if NewIndex > fItems.Count-1 then NewIndex := -1;

    if (NewIndex<>fItemIndex) then begin
      fItemIndex := NewIndex;
      if Assigned(fOnChange) and (ssLeft in Shift) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMListBox.Paint;
var i:integer;
begin
  Inherited;
  RefreshList;

  fRenderUI.WriteBevel(Left, Top, Width-fScrollBar.Width, Height, false, fBackAlpha);

  if (fItemIndex <> -1) and (fItemIndex >= fTopIndex) and (fItemIndex <= fTopIndex+(fHeight div fItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+fItemHeight*(fItemIndex-fTopIndex), Width-fScrollBar.Width, fItemHeight, $88888888);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    fRenderUI.WriteText(Left+8, Top+i*fItemHeight+3, Width, fItems.Strings[TopIndex+i] , fnt_Metal, kaLeft, false, $FFFFFFFF);
end;


constructor TKMDropBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
var P:TKMPanel;
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fDropCount := 10;
  fFont := aFont;

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, 4, bsMenu);
  fButton.fOnClick := ListShow;

  P := MasterParent;                                     
  fShape := TKMShape.Create(P, P.Left, P.Top, P.Width, P.Height, $00000000);
  fShape.fOnClick := ListHide;

  fList := TKMListBox.Create(P, Left, Top+aHeight, aWidth, 0);
  fList.BackAlpha := 0.75;
  fList.fOnClick := ListClick;

  ListHide(nil);
end;


destructor TKMDropBox.Destroy;
begin
  Inherited;
end;


procedure TKMDropBox.ListShow(Sender:TObject);
begin
  if fList.Visible then
  begin
    ListHide(nil);
    exit;
  end;

  if Items.Count < 1 then exit;

  fList.Height := min(fDropCount, Items.Count)*fList.ItemHeight;
  fList.TopIndex := ItemIndex - fDropCount div 2;
  fList.RefreshList;

  fList.Show;
  fShape.Show;
end;


procedure TKMDropBox.ListClick(Sender:TObject);
begin
  if (ItemIndex <> -1) then
  begin
    fCaption := Items[ItemIndex];
    if Assigned(fOnChange) then fOnChange(Self);
  end;
  ListHide(nil);
end;


procedure TKMDropBox.ListHide(Sender:TObject);
begin
  fList.Hide;
  fShape.Hide;
end;


function TKMDropBox.GetItems:TStringList;
begin
  Result := fList.Items;
end;


function TKMDropBox.GetItemIndex:smallint;
begin
  Result := fList.ItemIndex;
end;


procedure TKMDropBox.SetItemIndex(aIndex:smallint);
begin
  fList.ItemIndex := aIndex;
  fCaption := Items[ItemIndex];
end;


procedure TKMDropBox.SetEnabled(aValue:boolean);
begin
  Inherited;
  fButton.Enabled := aValue;
  fList.Enabled := aValue;  
end;


procedure TKMDropBox.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  //MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMDropBox.Paint;
var Col:TColor4;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  fRenderUI.WriteText(Left+4, Top+4, Width-8, fCaption, fFont, kaLeft, false, Col);
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
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


procedure TKMMinimap.Paint;
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
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  ItemHeight := 20;
  TopIndex := 0;
  ItemIndex := -1;
  fPaths := TStringList.Create;
  fFiles := TStringList.Create;
  fPath := ExeDir;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-ItemHeight, aTop, ItemHeight, aHeight, sa_Vertical, bsGame);
  fScrollBar.OnChange := ChangeScrollPosition;
end;


destructor TKMFileList.Destroy;
begin
  fFiles.Free;
  fPaths.Free;
  Inherited;
end;


procedure TKMFileList.ChangeScrollPosition(Sender:TObject);
begin
  TopIndex := fScrollBar.Position;
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMFileList.SetVisible(aValue:boolean);
begin
  Inherited;
  fScrollBar.Visible := fVisible; //Hide scrollbar and its buttons
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

  fScrollBar.MaxValue := Math.max(fFiles.Count - (fHeight div ItemHeight),0);
  fScrollBar.Position := 0;
  fScrollBar.Enabled  := fScrollBar.MaxValue > fScrollBar.MinValue;
end;


procedure TKMFileList.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  TopIndex := EnsureRange(TopIndex - sign(WheelDelta), 0, fScrollBar.MaxValue);
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


function TKMFileList.FileName:string;
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
     (InRange(X, Left, Left+Width-fScrollBar.Width)) and
     (InRange(Y, Top, Top+Height div ItemHeight * ItemHeight))
  then begin
    NewIndex := TopIndex + (Y-Top) div ItemHeight;

    if NewIndex > fFiles.Count-1 then NewIndex := -1;

    if (NewIndex<>ItemIndex) then begin
      ItemIndex := NewIndex;
      if Assigned(fOnChange) and (ssLeft in Shift) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMFileList.Paint;
var i:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width-fScrollBar.Width, Height);

  if (ItemIndex <> -1) and (ItemIndex >= TopIndex) and (ItemIndex <= TopIndex+(fHeight div ItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+ItemHeight*(ItemIndex-TopIndex), Width-fScrollBar.Width, ItemHeight, $88888888);

  for i:=0 to Math.min(fFiles.Count-1, (fHeight div ItemHeight)-1) do
    fRenderUI.WriteText(Left+8, Top+i*ItemHeight+3, Width, TruncateExt(fFiles.Strings[TopIndex+i]) , fnt_Metal, kaLeft, false, $FFFFFFFF);

  if (ItemIndex <> -1) then
  fRenderUI.WriteText(Left+8, Top+Height+4, Width, '\'+fPaths.Strings[ItemIndex] + fFiles.Strings[ItemIndex], fnt_Grey, kaLeft, false, $FFFFFFFF);
end;


{ TKMControlsCollection }
constructor TKMMasterControl.Create;
begin
  Inherited;
  CtrlPaintCount := 0;
  fCtrl      := nil;
  fCtrlDown  := nil;
  fCtrlFocus := nil;
  fCtrlOver  := nil;
  fCtrlUp    := nil;
  if fRenderUI <> nil then
    fRenderUI := TRenderUI.Create;
end;


destructor TKMMasterControl.Destroy;
begin
  fCtrl.Free; //Will destroy all its childs as well
  if fRenderUI <> nil then
    FreeAndNil(fRenderUI);
  Inherited;
end;

procedure TKMMasterControl.SetCtrlDown(aCtrl:TKMControl);
begin
  if fCtrlDown <> nil then fCtrlDown.State := fCtrlDown.State - [csDown]; //Release previous
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csDown];             //Press new
  fCtrlDown := aCtrl;                                                     //Update info
end;


procedure TKMMasterControl.SetCtrlFocus(aCtrl:TKMControl);
begin
  if fCtrlFocus <> nil then fCtrlFocus.State := fCtrlFocus.State - [csFocus];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csFocus];
  fCtrlFocus := aCtrl;
end;


procedure TKMMasterControl.SetCtrlOver(aCtrl:TKMControl);
begin
  if fCtrlOver <> nil then fCtrlOver.State := fCtrlOver.State - [csOver];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csOver];
  fCtrlOver := aCtrl;
end;


procedure TKMMasterControl.SetCtrlUp(aCtrl:TKMControl);
begin
  fCtrlUp := aCtrl;
  if fCtrlDown = fCtrlUp then CtrlFocus := fCtrlUp else CtrlFocus := nil;
end;


{ Recursing function to find topmost control (excl. Panels)}
function TKMMasterControl.HitControl(X,Y:integer):TKMControl;
  function ScanChild(P:TKMPanel; aX,aY:integer):TKMControl;
  var i:integer;
  begin
    Result := nil;
    for i:=P.ChildCount downto 1 do
    if P.Childs[i].fVisible then //ignore invisible controls
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
  Result := ScanChild(fCtrl, X, Y);
end;


function TKMMasterControl.KeyDown(Key: Word; Shift: TShiftState):boolean;
begin
  if CtrlFocus <> nil then
    Result := CtrlFocus.KeyDown(Key, Shift)
  else
    Result := false;
end;


function TKMMasterControl.KeyUp(Key: Word; Shift: TShiftState):boolean;
begin
  if CtrlFocus <> nil then
    Result := CtrlFocus.KeyUp(Key, Shift)
  else
    Result := false;
end;


procedure TKMMasterControl.MouseDown(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  CtrlDown := HitControl(X,Y);
  if CtrlDown <> nil then CtrlDown.MouseDown(X,Y,Shift,Button);
end;


procedure TKMMasterControl.MouseMove(X,Y:Integer; Shift:TShiftState);
begin
  if CtrlDown=nil then CtrlOver := HitControl(X,Y); //User is dragging some Ctrl (e.g. scrollbar) and went away from Ctrl bounds
  if CtrlOver <> nil then begin
    CtrlOver.MouseMove(X,Y,Shift);
    if Assigned(fOnHint) then fOnHint(CtrlOver)
  end;
  if (CtrlDown <> nil) and (CtrlOver <> CtrlDown) then CtrlDown := nil;
end;


procedure TKMMasterControl.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
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


procedure TKMMasterControl.MouseWheel(X,Y:integer; WheelDelta:integer);
var C:TKMControl;
begin
  C := HitControl(X,Y);
  if C <> nil then C.MouseWheel(C,WheelDelta);
end;


{Paint controls}
{Leave painting of childs to their parent control}
procedure TKMMasterControl.Paint;
begin
  CtrlPaintCount := 0;
  fCtrl.Paint;

  if MODE_DESIGN_CONTORLS and (CtrlFocus<>nil) then
    with CtrlFocus do
      fRenderUI.WriteText(Left, Top-14, Width, inttostr(Left)+':'+inttostr(Top), fnt_Grey, kaLeft, false, $FFFFFFFF);
end;


procedure TKMMasterControl.SaveToFile(aFileName:string);
var ft:textfile;
begin
  AssignFile(ft,aFileName); 
  Rewrite(ft);

  //fCtrl.SaveToFile; //Will save all the childs as well, recursively alike Paint or HitControl
  //writeln(ft, ClassName);
  //writeln(ft, Format('[%d %d %d %d]', [fLeft, fTop, fWidth, fHeight]));

  CloseFile(ft);
end;


end.
