unit KM_Controls;
{$I KaM_Remake.inc}
interface
uses
    {$IFDEF MSWindows} Windows, {$ENDIF}
    {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
    Classes, Controls, Graphics, Math, SysUtils, Clipbrd, Forms,
    KromUtils, KromOGLUtils, KM_Defaults, KM_Points, KM_CommonTypes;

type
  TNotifyEventMB = procedure(Sender: TObject; AButton:TMouseButton) of object;
  TNotifyEventMW = procedure(Sender: TObject; WheelDelta:integer) of object;
  TNotifyEventKey = procedure(Sender: TObject; Key: Word) of object;
  TNotifyEventXY = procedure(Sender: TObject; X, Y: Integer) of object;

  TTextAlign = (taLeft, taCenter, taRight);
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

    function HitControl(X,Y:integer; aIncludeDisabled:boolean=false):TKMControl;
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
    procedure KeyPress  (Key: Char);
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
    fOnClickRight:TPointEvent;
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
    Hitable:boolean; //Can this control be hit with the cursor?
    Anchors: TAnchors;
    State: TKMControlStateSet; //Each control has it localy to avoid quering Collection on each Render

    Tag: integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    function HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean; virtual;

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
    procedure KeyPress(Key: Char); virtual;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; virtual;
    procedure MouseDown (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseMove (X,Y:integer; Shift:TShiftState); virtual;
    procedure MouseUp   (X,Y:integer; Shift:TShiftState; Button:TMouseButton); virtual;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); virtual;

    property OnClick: TNotifyEvent write fOnClick;
    property OnClickEither: TNotifyEventMB write fOnClickEither;
    property OnClickRight: TPointEvent write fOnClickRight;
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
    FillColor:TColor4;
    LineColor:TColor4; //color of outline
    LineWidth:byte;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aLineColor:TColor4);
    procedure Paint; override;
  end;


  {Text Label}
  TKMLabel = class(TKMControl)
  private
    fAutoWrap: Boolean;
    fFont: TKMFont;
    fFontColor: TColor4;
    fCaption: string; //Original text
    fText: string; //Reformatted text
    fTextAlign: TTextAlign;
    fTextSize: TKMPoint;
    fStrikethrough: Boolean;
    function AreaLeft: Integer;
    function TextLeft: Integer;
    procedure SetCaption(aCaption: string);
    procedure SetAutoWrap(aValue: boolean);
    procedure ReformatText;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF); overload;
    function HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean; override;
    property AutoWrap: boolean read fAutoWrap write SetAutoWrap; //Whether to automatically wrap text within given text area width
    property Caption: string read fCaption write SetCaption;
    property FontColor: TColor4 read fFontColor write fFontColor;
    property Strikethrough: Boolean read fStrikethrough write fStrikethrough;
    property TextSize: TKMPoint read fTextSize;
    procedure Paint; override;
  end;


  TKMLabelScroll = class(TKMLabel)
  public
    SmoothScrollToTop: cardinal; //Delta between this and TimeGetTime affects vertical position
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF);
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
    fBackAlpha:single; //Alpha of background (usually 0.5, dropbox 1)
    fCellSize:byte; //Size of the square in pixels
    fColumnCount:byte;
    fRowCount:byte;
    fColorIndex:byte; //Index 0..255 should be enough
    Colors:array of TColor4; //Range is 0..255
    fOnChange:TNotifyEvent;
    fInclRandom:boolean;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize:integer);
    procedure SetColors(aColors:array of TColor4; aInclRandom:boolean=false);
    property BackAlpha:single write fBackAlpha;
    property ColorIndex:Byte read fColorIndex write fColorIndex;
    function GetColor:TColor4;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


  {3DButton}
  TKMButton = class(TKMControl)
  private
    fCaption: string;
    fFont: TKMFont;
    fTextAlign: TTextAlign;
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
  private
    fFont: TKMFont;
    TextAlign: TTextAlign;
  public
    RXid: integer; //RX library
    TexID: integer;
    TexOffsetX,TexOffsetY,CapOffsetY:shortint;
    Caption: string;
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
    fFont: TKMFont;
    fText: string;
    fCursorPos: Integer;
    fLeftIndex: Integer; //The position of the character shown left-most when text does not fit
    procedure SetCursorPos(aPos: integer);
    procedure SetText(aText:string);
  public
    Masked: Boolean;
    ReadOnly: Boolean;
    OnChange: TNotifyEvent;
    OnKeyDown: TNotifyEventKey;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    property CursorPos: integer read fCursorPos write SetCursorPos;
    property Text:string read fText write SetText;
    
    function HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean; override;
    function KeyDown(Key: Word; Shift: TShiftState):boolean; override;
    procedure KeyPress(Key: Char); override;
    function KeyUp(Key: Word; Shift: TShiftState):boolean; override;
    procedure MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure Paint; override;
  end;


  { Checkbox }
  TKMCheckBox = class(TKMControl)
  private
    fCaption:string;
    fChecked:boolean;
    fFlatStyle:boolean; //Render the check as a rectangle (modern style)
    fFont:TKMFont;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont); overload;
    constructor Create(aParent:TKMPanel; aLeft,aTop:integer; aCaption:string; aFont:TKMFont); overload;
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
    fOnChange:TNotifyEvent;
    function GetItemCount:integer;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    destructor Destroy; override;
    property ItemCount:integer read GetItemCount;
    property ItemIndex:integer read fItemIndex write fItemIndex;
    property Items:TStringList read fItems;
    property OnChange: TNotifyEvent write fOnChange;
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
    TextAlign: TTextAlign;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini; aColor:TColor4=$FFFFFFFF);
    procedure Paint; override;
  end;


  {Resource bar}
  TKMResourceRow = class(TKMControl)
  public
    RxID: Byte;
    TexID: Word;
    Caption: String;
    ResourceCount: integer;
    procedure Paint; override;
  end;


  {Resource order bar}
  TKMResourceOrderRow = class(TKMControl)
  private
    fOrderAdd:TKMButton;
    fOrderLab:TKMLabel;
    fOrderRem:TKMButton;
    procedure SetEnabled(aValue:boolean); override;
    procedure SetVisible(aValue:boolean); override;
  public
    RxID: Byte;
    TexID: Word;
    Caption: String;
    ResourceCount: integer;
    OrderCount:word;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
    property OrderAdd:TKMButton read fOrderAdd;
    property OrderRem:TKMButton read fOrderRem;
    procedure Paint; override;
  end;


  {Production cost bar}
  TKMCostsRow = class(TKMControl)
  public
    RxID: Byte;
    TexID1,TexID2: Word;
    Caption: String;
    procedure Paint; override;
  end;


  {Ratio bar}
  TKMRatioRow = class(TKMControl)
  private
    fOnChange:TNotifyEvent;
  public
    Position:byte;
    MinValue:byte;
    MaxValue:byte;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer);
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


  TScrollAxis = (sa_Vertical, sa_Horizontal);

  { Scroll bar }
  TKMScrollBar = class(TKMControl)
  private
    fMinValue: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fBackAlpha: Single; //Alpha of background (usually 0.5, dropbox 1)
    fScrollAxis: TScrollAxis;
    fOnChange: TNotifyEvent;
    procedure SetHeight(aValue:Integer); override;
    procedure SetEnabled(aValue:boolean); override;
    procedure SetVisible(aValue:boolean); override;
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure IncPosition(Sender:TObject);
    procedure DecPosition(Sender:TObject);
    procedure UpdateThumbSize;
  public
    Thumb: Word;
    ScrollDec: TKMButton;
    ScrollInc: TKMButton;
    Style: TButtonStyle;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
    property BackAlpha: Single read fBackAlpha write fBackAlpha;
    property MinValue: Integer read fPosition write SetMinValue;
    property MaxValue: Integer read fPosition write SetMaxValue;
    property Position: Integer read fPosition write SetPosition;
    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Paint; override;
  end;


  TKMListBox = class(TKMControl)
  private
    fFont: TKMFont; //Can't be changed from inital value, it will mess up the word wrapping
    fAutoHideScrollBar: boolean;
    fBackAlpha:single; //Alpha of background (usually 0.5, dropbox 1)
    fItemHeight:byte;
    fItemIndex:smallint;
    fItems:TStringList;
    fScrollBar:TKMScrollBar;
    fOnChange:TNotifyEvent;
    procedure SetHeight(aValue:Integer); override;
    procedure SetVisible(aValue:boolean); override;
    function GetTopIndex: Integer;
    procedure SetTopIndex(aIndex: Integer);
    procedure SetBackAlpha(aValue:single);
    procedure SetItemHeight(const Value: byte);
    procedure SetEnabled(aValue:boolean); override;
    procedure SetAutoHideScrollBar(Value: boolean);
    procedure UpdateScrollBar;
    function GetItem(aIndex:integer):string;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    destructor Destroy; override;

    procedure Add(aItem:string);
    procedure Clear;
    function Count:integer;
    procedure SetItems(aText:string);
    property AutoHideScrollBar: boolean read fAutoHideScrollBar write SetAutoHideScrollBar;

    property BackAlpha:single write SetBackAlpha;

    property Item[aIndex:integer]:string read GetItem;
    property ItemHeight:byte read fItemHeight write SetItemHeight;
    property ItemIndex:smallint read fItemIndex write fItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TKMColumnListBox = class(TKMControl)
  private
    fFont: TKMFont;
    fHeaderFont: TKMFont;
    fBackAlpha:single; //Alpha of background
    fItemHeight:byte;
    fItemTop:smallint;
    fItemIndex:smallint;
    fColumns: TStringList;
    fItems:array of TStringList;
    fItemColors:array of array of TColor4;
    fItemOffsets:array of integer;
    fTags:array of integer;
    fTags2:array of integer;
    fScrollBar:TKMScrollBar;
    fOnChange:TNotifyEvent;
    procedure SetHeight(aValue:Integer); override;
    procedure SetVisible(aValue:boolean); override;
    function GetTopIndex: Integer;
    procedure SetTopIndex(aIndex: Integer);
    procedure SetBackAlpha(aValue:single);
    procedure SetEnabled(aValue:boolean); override;
    procedure UpdateScrollBar;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aHeaderFont:TKMFont; aColumns:array of string; aItemOffsets:array of integer);
    destructor Destroy; override;

    procedure AddItem(aItem:array of string; aItemColor:array of TColor4; aTag:integer=0; aTag2:integer=0);
    procedure Clear;
    function GetItemTag:integer;
    function GetItemTag2:integer;

    property BackAlpha:single write SetBackAlpha;
    function Count:integer;
    property ItemHeight:byte read fItemHeight;
    property ItemIndex:smallint read fItemIndex write fItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TKMDropBox = class(TKMControl)
  private
    fCaption:string;
    fDefaultCaption:string;
    fDropCount:byte;
    fDropUp:boolean;
    fFont: TKMFont;
    fButton:TKMButton;
    fList:TKMListBox;
    fShape:TKMShape;
    fOnChange:TNotifyEvent;
    procedure ListShow(Sender:TObject);
    procedure ListClick(Sender:TObject);
    procedure ListHide(Sender:TObject);
    function GetItem(aIndex: integer):string;
    function GetItemIndex:smallint;
    procedure SetItemIndex(aIndex:smallint);
    procedure SetEnabled(aValue:boolean); override;
    procedure SetVisible(aValue:boolean); override;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aDefaultCaption:string);
    procedure Add(aItem:string);
    procedure SetItems(aText:string);
    property DropCount:byte write fDropCount;
    property DefaultCaption:string read fDefaultCaption write fDefaultCaption;
    property DropUp:boolean write fDropUp;
    property ItemIndex:smallint read GetItemIndex write SetItemIndex;
    property Item[aIndex: integer]:string read GetItem;
    procedure SelectByName(aText:string);
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


  TKMDropColorBox = class(TKMControl)
  private
    fColorIndex:integer;
    fRandomCaption:string;
    fButton:TKMButton;
    fSwatch:TKMColorSwatch;
    fShape:TKMShape;
    fOnChange:TNotifyEvent;
    procedure ListShow(Sender:TObject);
    procedure ListClick(Sender:TObject);
    procedure ListHide(Sender:TObject);
    procedure SetEnabled(aValue:boolean); override;
    procedure SetColorIndex(aIndex:integer);
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aCount:integer);
    property ColorIndex:integer read fColorIndex write SetColorIndex;
    procedure SetColors(aColors:array of TColor4; aRandomCaption:string='');
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


  TKMMemo = class(TKMControl)
  private
    fFont: TKMFont; //Can't be changed from inital value, it will mess up the word wrapping
    fItemHeight: Byte;
    fItems: TStringList;
    fAutoWrap: Boolean;
    fText: string;
    fScrollDown: Boolean;
    fScrollBar: TKMScrollBar;
    fOnChange: TNotifyEvent;
    procedure SetHeight(aValue:Integer); override;
    procedure SetWidth(aValue:Integer); override;
    procedure SetVisible(aValue:boolean); override;
    procedure SetEnabled(aValue:boolean); override;

    procedure SetAutoWrap(const Value: boolean);
    function GetText: string;
    procedure SetText(const aText: string);
    function GetTopIndex: smallint;
    procedure SetTopIndex(aIndex:smallint);
    procedure ReformatText;
    procedure UpdateScrollBar;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
    destructor Destroy; override;

    procedure Add(const aItem:string);
    procedure Clear;
    procedure ScrollToBottom;
    property AutoWrap: boolean read fAutoWrap write SetAutoWrap; //Whether to automatically wrap text within given text area width
    property Text: string read GetText write SetText;
    property ItemHeight:byte read fItemHeight write fItemHeight;
    property TopIndex:smallint read GetTopIndex write SetTopIndex;
    property ScrollDown:boolean read fScrollDown write fScrollDown;

    procedure MouseWheel(Sender: TObject; WheelDelta:integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TDragAxis = (daHoriz, daVertic, daAll);

  TKMDragger = class(TKMControl)
  private
    fMinusX, fMinusY, fPlusX, fPlusY: Integer; //Restrictions
    fPositionX: Integer;
    fPositionY: Integer;
    fStartDragX: Integer;
    fStartDragY: Integer;
  public
    OnMove: TNotifyEventXY;
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);

    procedure SetBounds(aMinusX, aMinusY, aPlusX, aPlusY: Integer);

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;

    procedure Paint; override;
  end;

  { Minimap as stand-alone control }
  TKMMinimap = class(TKMControl)
  private
    fMapSize:TKMPoint;
    fViewArea:TRect;
    fOnChange:TPointEvent;
  public
    constructor Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);

    function LocalToMapCoords(X,Y:integer; const Inset:shortint=0):TKMPoint;
    property MapSize: TKMPoint read fMapSize write fMapSize;
    property ViewArea: TRect read fViewArea write fViewArea;
    property OnChange: TPointEvent write fOnChange;

    procedure MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton); override;
    procedure MouseMove(X,Y:Integer; Shift:TShiftState); override;
    procedure Paint; override;
  end;


implementation
uses KM_RenderUI, KM_ResourceGFX, KM_Sound, KM_Utils;


{ TKMControl }
constructor TKMControl.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create;
  Hitable   := true; //All controls can be clicked by default
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
  if aParent<>nil then aParent.AddChild(Self);
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


procedure TKMControl.KeyPress(Key: Char);
begin
  //Could be something common
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
    if (Button = mbRight) and Assigned(fOnClickRight) then fOnClickRight(Self, X, Y);
  end;
end;


procedure TKMControl.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  if Assigned(fOnMouseWheel) then fOnMouseWheel(Self, WheelDelta);
end;


//fVisible is checked earlier
function TKMControl.HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean;
begin
  Result := Hitable and (fEnabled or aIncludeDisabled) and InRange(X, Left, Left + fWidth) and InRange(Y, Top, Top + fHeight);
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint;
var sColor:TColor4; Tmp: TKMPoint;
begin
  inc(CtrlPaintCount);
  if not SHOW_CONTROLS_OVERLAY then exit;

  sColor := $00000000;

  if Self is TKMPanel then sColor := $200000FF;

  if Self is TKMLabel then begin //Special case for aligned text
    Tmp := TKMLabel(Self).TextSize;
    fRenderUI.WriteLayer(TKMLabel(Self).TextLeft, Top, Tmp.X, Tmp.Y, $4000FFFF, $80FFFFFF);
    fRenderUI.WriteRect(TKMLabel(Self).AreaLeft, Top, fWidth, fHeight, 1, $FFFFFFFF);
    fRenderUI.WriteLayer(Left-3, Top-3, 6, 6, sColor or $FF000000);
    Exit;
  end;

  if Self is TKMImage      then sColor := $2000FF00;
  if Self is TKMImageStack then sColor := $2080FF00;
  if Self is TKMCheckBox   then sColor := $20FF00FF;
  if Self is TKMRatioRow   then sColor := $2000FF00;
  if Self is TKMCostsRow   then sColor := $2000FFFF;
  if Self is TKMRadioGroup then sColor := $20FFFF00;

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
    if (akTop in Childs[i].Anchors) and (akBottom in Childs[i].Anchors) then
      Childs[i].Height := Childs[i].Height + (aValue - fHeight)
    else
    if akTop in Childs[i].Anchors then
      //Do nothing
    else
    if akBottom in Childs[i].Anchors then
      Childs[i].fTop := Childs[i].fTop + (aValue - fHeight)
    else
      Childs[i].fTop := Childs[i].fTop + (aValue - fHeight) div 2;

  Inherited;
end;

procedure TKMPanel.SetWidth(aValue:Integer);
var i:integer;
begin
  for i:=1 to ChildCount do
    if (akLeft in Childs[i].Anchors) and (akRight in Childs[i].Anchors) then
      Childs[i].Width := Childs[i].Width + (aValue - fWidth)
    else
    if akLeft in Childs[i].Anchors then
      //Do nothing
    else
    if akRight in Childs[i].Anchors then
      Childs[i].fLeft := Childs[i].fLeft + (aValue - fWidth)
    else
      Childs[i].fLeft := Childs[i].fLeft + (aValue - fWidth) div 2;

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
  FillColor := $00000000;
  LineColor := aLineColor;
  LineWidth := 2;
end;


procedure TKMShape.Paint;
begin
  Inherited;
  fRenderUI.WriteLayer(Left,Top,Width,Height,FillColor,$00000000);
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,LineColor);
end;


{ TKMLabel }
constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fFontColor := aColor;
  fTextAlign := aTextAlign;
  fAutoWrap := false;
  SetCaption(aCaption);
end;

//Same as above but with width/height ommitted, as in most cases we don't know/don't care
constructor TKMLabel.Create(aParent:TKMPanel; aLeft,aTop:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF);
begin
  Create(aParent,aLeft,aTop,0,0,aCaption,aFont,aTextAlign,aColor);
end;


function TKMLabel.AreaLeft: Integer;
begin
  case fTextAlign of
    taCenter: Result := Left - Width div 2;
    taRight:  Result := Left - Width;
    else      Result := Left;
  end;
end;


function TKMLabel.TextLeft: Integer;
begin
  case fTextAlign of
    taCenter: Result := Left - fTextSize.X div 2;
    taRight:  Result := Left - fTextSize.X;
    else      Result := Left;
  end;
end;


procedure TKMLabel.SetCaption(aCaption:string);
begin
  fCaption := aCaption;
  ReformatText;
end;


procedure TKMLabel.SetAutoWrap(aValue:boolean);
begin
  fAutoWrap := aValue;
  ReformatText;
end;


function TKMLabel.HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean;
begin
  Result := Hitable and InRange(X, AreaLeft, AreaLeft + Width) and InRange(Y, Top, Top + Height);
end;


//Existing EOLs should be preserved, and new ones added where needed
//Keep original intact incase we need to Reformat text once again
procedure TKMLabel.ReformatText;
begin
  if fAutoWrap then
    fText := fResource.ResourceFont.WordWrap(fCaption, fFont, Width, true)
  else
    fText := fCaption;

  fTextSize := fResource.ResourceFont.GetTextSize(fText, fFont);
end;


{Send caption to render}
procedure TKMLabel.Paint;
var Col:cardinal;
begin
  Inherited;

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  fRenderUI.WriteText(Left, Top, Width, Height, fText, fFont, fTextAlign, Col);

  if fStrikethrough then
    fRenderUI.WriteLayer(TextLeft, Top + fTextSize.Y div 2 - 2, fTextSize.X, 3, Col, $FF000000);
end;


{ TKMLabelScroll }
constructor TKMLabelScroll.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont; aTextAlign: TTextAlign; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aCaption, aFont, aTextAlign, aColor);
  SmoothScrollToTop := 0; //Disabled by default
end;


procedure TKMLabelScroll.Paint;
var NewTop:integer; Col:cardinal;
begin
  fRenderUI.SetupClipY(Top, Top+Height);
  NewTop := Top + Height - integer(TimeGet - SmoothScrollToTop) div 50; //Compute delta and shift by it upwards (Credits page)

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  fRenderUI.WriteText(Left, NewTop, Width, Height, fCaption, fFont, fTextAlign, Col);
  fRenderUI.ReleaseClip;
end;


{ TKMImage }
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
  OffsetX, OffsetY, DrawWidth, DrawHeight: SmallInt; //variable parameters
  StretchDraw: Boolean; //Check if the picture should be stretched
begin
  Inherited;
  if (TexID = 0) or (RXid = 0) then exit; //No picture to draw

  StretchDraw := False;
  DrawWidth   := fWidth;
  DrawHeight  := fHeight;
  OffsetX     := 0;
  OffsetY     := 0;

  //Both ImageAnchors means that we will need to stretch the image
  if (akLeft in ImageAnchors) and (akRight in ImageAnchors) then
    StretchDraw := True
  else
  if akLeft in ImageAnchors then
    //Use defaults
  else
  if akRight in ImageAnchors then
    OffsetX := fWidth - GFXData[RXid, TexID].PxWidth
  else
    //No ImageAnchors means: draw the image in center
    OffsetX := (fWidth - GFXData[RXid, TexID].PxWidth) div 2;

  if (akTop in ImageAnchors) and (akBottom in ImageAnchors) then
    StretchDraw := True
  else
  if akTop in ImageAnchors then
    //Use defaults
  else
  if akBottom in ImageAnchors then
    OffsetY := fHeight - GFXData[RXid, TexID].PxHeight
  else
    OffsetY := (fHeight - GFXData[RXid, TexID].PxHeight) div 2;

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
constructor TKMColorSwatch.Create(aParent:TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize:integer);
begin
  Inherited Create(aParent, aLeft, aTop, 0, 0);

  fBackAlpha    := 0.5;
  fColumnCount  := aColumnCount;
  fRowCount     := aRowCount;
  fCellSize     := aSize;
  fInclRandom   := false;

  Width  := fColumnCount*fCellSize;
  Height := fRowCount*fCellSize;
end;


procedure TKMColorSwatch.SetColors(aColors:array of TColor4; aInclRandom:boolean=false);
begin
  fInclRandom := aInclRandom;
  if fInclRandom then
  begin
    SetLength(Colors, Length(aColors)+SizeOf(TColor4));
    Colors[0] := $00000000; //This one is reserved for random
    Move((@aColors[0])^, (@Colors[1])^, SizeOf(aColors));
  end
  else
  begin
    SetLength(Colors, Length(aColors));
    Move((@aColors[0])^, (@Colors[0])^, SizeOf(aColors));
  end;
end;


function TKMColorSwatch.GetColor:TColor4;
begin
  Result := Colors[fColorIndex];
end;


procedure TKMColorSwatch.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
var NewColor:integer;
begin
  if Button = mbLeft then begin
    NewColor := EnsureRange((Y-Top) div fCellSize, 0, fRowCount-1)*fColumnCount +
                EnsureRange((X-Left) div fCellSize, 0, fColumnCount-1);
    if InRange(NewColor, 0, Length(Colors)-1) then
    begin
      fColorIndex := NewColor;
      if Assigned(fOnChange) then fOnChange(Self);
    end;
  end;
  Inherited;
end;


procedure TKMColorSwatch.Paint;
var i,Start:integer;
begin
  Inherited;

  fRenderUI.WriteBevel(Left, Top, Width, Height, false, fBackAlpha);

  Start := 0;
  if fInclRandom then
  begin
    //Render miniature copy of all available colors with '?' on top
    for i:=0 to Length(Colors)-1 do
      fRenderUI.WriteLayer(Left+(i mod fColumnCount)*(fCellSize div fColumnCount)+2, Top+(i div fColumnCount)*(fCellSize div fColumnCount)+2, (fCellSize div fColumnCount), (fCellSize div fColumnCount), Colors[i], $00);
    fRenderUI.WriteText(Left + fCellSize div 2, Top + fCellSize div 4, 0, 0, '?', fnt_Metal, taCenter, $FFFFFFFF);
    Start := 1;
  end;

  for i:=Start to Length(Colors)-1 do
    fRenderUI.WriteLayer(Left+(i mod fColumnCount)*fCellSize, Top+(i div fColumnCount)*fCellSize, fCellSize, fCellSize, Colors[i], $00);

  //Paint selection
  fRenderUI.WriteLayer(Left+(fColorIndex mod fColumnCount)*fCellSize, Top+(fColorIndex div fColumnCount)*fCellSize, fCellSize, fCellSize, $00, $FFFFFFFF);
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
  fTextAlign  := taCenter; //Thats default everywhere in KaM
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
    Parent.GetCollection.CtrlDown := Self;
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
    Parent.GetCollection.CtrlOver := Self;
    Parent.GetCollection.CtrlUp := Self;
    if Assigned(fOnClick) then fOnClick(Self);
    Result := true; //Click has happened
  end else
    Result := false; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  if fEnabled and fMakesSound and (csDown in State) then fSoundLib.Play(sfxn_ButtonClick);
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
      fRenderUI.WriteText(Left + Width div 2 +byte(csDown in State), (Top + Height div 2)-7+byte(csDown in State), Width, 0, fCaption, fFont, fTextAlign, $FFFFFFFF)
    else
      fRenderUI.WriteText(Left + Width div 2, (Top + Height div 2)-7, Width, 0, fCaption, fFont, fTextAlign, $FF888888);
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
  fFont := fnt_Grey;
  TextAlign:=taLeft;
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
  if Down then StateSet:=StateSet+[fbs_Selected];
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
  fRenderUI.WriteText(Left+(Width div 2),Top+(Height div 2)+4+CapOffsetY, Width, 0, Caption, Font, taCenter, $FFFFFFFF);
  if (csOver in State) and fEnabled then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $40FFFFFF, $00);
  if (csDown in State) or Down then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $00000000, $FFFFFFFF);
end;


{ TKMEdit }
constructor TKMEdit.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fText := '<<<LEER>>>';
  fFont := aFont;
  CursorPos := 0;
end;


function TKMEdit.HitTest(X, Y: Integer; aIncludeDisabled:boolean=false): Boolean;
begin
  //When control is read-only we don't want to recieve Focus event
  Result := Inherited HitTest(X,Y) and not ReadOnly;
end;


//
procedure TKMEdit.SetCursorPos(aPos: Integer);
var RText: string;
begin
  fCursorPos := EnsureRange(aPos, 0, Length(fText));
  if fCursorPos < fLeftIndex then
    fLeftIndex := fCursorPos
  else
  begin
    //Remove characters to the left of fLeftIndex
    RText := Copy(fText, fLeftIndex+1, length(fText));
    while fCursorPos-fLeftIndex > fResource.ResourceFont.CharsThatFit(RText, fFont, Width-8) do
    begin
      inc(fLeftIndex);
      //Remove characters to the left of fLeftIndex
      RText := Copy(fText, fLeftIndex+1, length(fText));
    end;
  end;
end;


procedure TKMEdit.SetText(aText:string);
begin
  fText := aText;
  CursorPos := math.min(CursorPos, Length(fText));
  //Setting the text should place cursor to the end
  fLeftIndex := 0;
  SetCursorPos(Length(Text));
end;


function TKMEdit.KeyDown(Key: Word; Shift: TShiftState):boolean;
begin
  Result := true;
  if Inherited KeyDown(Key, Shift) or ReadOnly then exit;

  //Clipboard operations
  if (Shift = [ssCtrl]) and (Key <> VK_CONTROL) then
  begin
    case Key of
      Ord('C'):    Clipboard.AsText := fText;
      Ord('X'):    begin Clipboard.AsText := fText; Text := ''; end;
      Ord('V'):    begin Insert(Clipboard.AsText, fText, CursorPos+1);
                         CursorPos := CursorPos + Length(Clipboard.AsText); end;
    end;
  end;

  case Key of
    VK_BACK:    begin Delete(fText, CursorPos, 1); CursorPos := CursorPos-1; end;
    VK_DELETE:  Delete(fText, CursorPos+1, 1);
    VK_LEFT:    CursorPos := CursorPos-1;
    VK_RIGHT:   CursorPos := CursorPos+1;
    VK_HOME:    CursorPos := 0;
    VK_END:     CursorPos := length(fText);
  end;

  if Assigned(OnKeyDown) then OnKeyDown(Self, Key);
end;


procedure TKMEdit.KeyPress(Key: Char);
begin
  if ReadOnly or (Key < #32) or (Key = #124) then //Not allowed to write EOL in edit field
    Exit;

  Insert(Key, fText, CursorPos+1);
  CursorPos := CursorPos+1;
end;


function TKMEdit.KeyUp(Key: Word; Shift: TShiftState):boolean;
begin
  Result := true;
  if Inherited KeyUp(Key, Shift) or ReadOnly then exit;

  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMEdit.MouseUp(X,Y:Integer; Shift:TShiftState; Button:TMouseButton);
begin
  if ReadOnly then exit;
  Inherited;
  CursorPos := length(fText);
end;


procedure TKMEdit.Paint;
var Col:TColor4; RText:String; OffX:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if Masked then
    RText := StringOfChar('*', Length(fText))
  else
    RText := fText;

  RText := Copy(RText, fLeftIndex+1, length(RText)); //Remove characters to the left of fLeftIndex

  fRenderUI.WriteText(Left+4, Top+3, Width-8, 0, RText, fFont, taLeft, Col); //Characters that do not fit are trimmed

  //Render text cursor
  if (csFocus in State) and ((TimeGet div 500) mod 2 = 0) then
  begin
    SetLength(RText, CursorPos-fLeftIndex);
    OffX := Left + 2 + fResource.ResourceFont.GetTextSize(RText, fFont).X;
    fRenderUI.WriteLayer(OffX, Top+2, 3, Height-4, Col, $FF000000);
  end;
end;


{ TKMCheckBox }
constructor TKMCheckBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aCaption:string; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont     := aFont;
  fCaption  := aCaption;
end;


constructor TKMCheckBox.Create(aParent:TKMPanel; aLeft,aTop:integer; aCaption:string; aFont:TKMFont);
begin
  Create(aParent,aLeft,aTop,0,0,aCaption,aFont);
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
var Col:TColor4;
begin
  Inherited;
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if fFlatStyle then begin
    fRenderUI.WriteBevel(Left, Top, Width, Height, true);
    if fChecked then
      fRenderUI.WriteLayer(Left+4, Top+4, Width-8, Height-8, $C0A0A0A0, $D0A0A0A0);
  end else
  begin
    fRenderUI.WriteText(Left, Top, Width, 0, '[ ] '+fCaption, fFont, taLeft, Col);
    if fChecked then
      fRenderUI.WriteText(Left+3, Top-1, 0, 0, 'x', fFont, taLeft, Col);
  end;
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
var NewIndex:integer;
begin
  if (csDown in State) and (Button = mbLeft) then
  begin
    NewIndex := EnsureRange((Y-Top) div round(Height/ItemCount), 0, ItemCount-1); //Clicking at wrong place can select invalid ID
    if NewIndex <> fItemIndex then
    begin
      fItemIndex := NewIndex;
      if Assigned(fOnChange) then
      begin
        fOnChange(Self);
        Exit; //Don't generate OnClick after OnChanged event (esp. when reloading Game on local change)
      end;
    end;
  end;
  Inherited; //There are OnMouseUp and OnClick events there
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMRadioGroup.Paint;
var Col:TColor4; LineHeight:integer; i:integer;
begin
  Inherited;
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  LineHeight := round(fHeight / ItemCount);
  for i:=0 to ItemCount-1 do
  begin
    fRenderUI.WriteText(Left, Top + i*LineHeight, Width, 0, '[ ] '+fItems.Strings[i], fFont, taLeft, Col);
    if fItemIndex = i then
      fRenderUI.WriteText(Left+3, Top + i*LineHeight - 1, 0, 0, 'x', fFont, taLeft, Col);
  end;
end;


{ TKMPercentBar }
constructor TKMPercentBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aPos:integer; aCaption:string=''; aFont:TKMFont=fnt_Mini; aColor:TColor4=$FFFFFFFF);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Position:=EnsureRange(aPos,0,100);
  Font:=aFont;
  FontColor:=aColor;
  TextAlign:=taCenter;
  Caption:=aCaption;
end;


procedure TKMPercentBar.Paint;
begin
  Inherited;
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
  if Caption <> '' then begin //Now draw text over bar, if required
    fRenderUI.WriteText((Left + Width div 2)+2, (Top + Height div 2)-4, Width-4, 0, Caption, Font, TextAlign, $FF000000);
    fRenderUI.WriteText((Left + Width div 2)+1, (Top + Height div 2)-5, Width-4, 0, Caption, Font, TextAlign, FontColor);
  end;
end;


{ TKMResourceRow }
procedure TKMResourceRow.Paint;
var i:integer;
begin
  Inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width-8, 0, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, RxID, TexID);
end;


{ TKMResourceOrderRow }
constructor TKMResourceOrderRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft+68,aTop,aWidth-68,aHeight);

  fOrderRem := TKMButton.Create(aParent,aLeft,aTop+2,20,aHeight,'-',fnt_Metal, bsGame);
  fOrderLab := TKMLabel.Create(aParent,aLeft+33,aTop+4,'',fnt_Grey,taCenter);
  fOrderAdd := TKMButton.Create(aParent,aLeft+46,aTop+2,20,aHeight,'+',fnt_Metal, bsGame);
end;


//Copy property to buttons
procedure TKMResourceOrderRow.SetEnabled(aValue:boolean);
begin
  Inherited;
  fOrderRem.Enabled := fEnabled;
  fOrderLab.Enabled := fEnabled;
  fOrderAdd.Enabled := fEnabled;
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
  fRenderUI.WriteText(Left + 4, Top + 3, Width - 8, 0, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, RxId, TexID);
end;


{ TKMCostsRow }
procedure TKMCostsRow.Paint;
begin
  Inherited;
  fRenderUI.WriteText(Left, Top + 4, Width-20, 0, Caption, fnt_Grey, taLeft, $FFFFFFFF);
  if TexID1 <> 0 then fRenderUI.WritePicture(Left+Width-40, Top + (Height-GFXData[RxId,TexID1].PxHeight) div 2, RxId, TexID1);
  if TexID2 <> 0 then fRenderUI.WritePicture(Left+Width-20, Top + (Height-GFXData[RxId,TexID2].PxHeight) div 2, RxId, TexID2);
end;


{ TKMRatioRow }
constructor TKMRatioRow.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aMin,aMax:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  MinValue := aMin;
  MaxValue := aMax;
  Position := (MinValue + MaxValue) div 2;
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
    if Assigned(fOnChange) then
      fOnChange(Self);
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
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, 0, 0, inttostr(Position), fnt_Metal, taCenter, $FFFFFFFF)
  else
    fRenderUI.WriteText(Left+12+2+ThumbPos, Top+3, 0, 0, inttostr(Position), fnt_Metal, taCenter, $FF888888);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aScrollAxis:TScrollAxis; aStyle:TButtonStyle);
begin
  Inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fBackAlpha := 0.5;
  fScrollAxis := aScrollAxis;
  fMinValue := 0;
  fMaxValue := 10;
  fPosition := 0;
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
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetHeight(aValue:Integer);
begin
  Inherited;
  if fScrollAxis = sa_Vertical then
    ScrollInc.Top := fTop+fHeight-fWidth;

  UpdateThumbSize; //Update Thumb size
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


procedure TKMScrollBar.SetMinValue(Value: Integer);
begin
  fMinValue := Max(0, Value);
  SetEnabled(fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetMaxValue(Value: Integer);
begin
  fMaxValue := Max(0, Value);
  SetEnabled(fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetPosition(Value: Integer);
begin
  fPosition := EnsureRange(Value, fMinValue, fMaxValue);
end;


procedure TKMScrollBar.IncPosition(Sender:TObject);
begin
  SetPosition(fPosition + 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender:TObject);
begin
  SetPosition(fPosition - 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.UpdateThumbSize;
begin
  case fScrollAxis of
    sa_Vertical:   Thumb := Math.max(0, (Height-2*Width)) div 4;
    sa_Horizontal: Thumb := Math.max(0, (Width-2*Height)) div 4;
  end;
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

  NewPos := fPosition;
  if (ssLeft in Shift) then
  begin

    if fScrollAxis = sa_Vertical then
      if InRange(Y,Top+Width,Top+Height-Width) then
        NewPos := Round(fMinValue+((Y-Top-Width-Thumb/2)/(Height-Width*2-Thumb)) * (fMaxValue - fMinValue) );

    if fScrollAxis = sa_Horizontal then
      if InRange(X,Left+Height,Left+Width-Height) then
        NewPos := Round(fMinValue+((X-Left-Height-Thumb/2)/(Width-Height*2-Thumb)) * (fMaxValue - fMinValue) );

    if NewPos <> fPosition then begin
      SetPosition(NewPos);
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


procedure TKMScrollBar.Paint;
var ThumbPos:word; State:T3DButtonStateSet;
begin
  Inherited;
  ThumbPos := 0;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.WriteBevel(Left, Top+Width, Width, Height - Width*2, false, fBackAlpha);
    sa_Horizontal: fRenderUI.WriteBevel(Left+Height, Top, Width - Height*2, Height, false, fBackAlpha);
  end;

  if fMaxValue > fMinValue then begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := (fPosition-fMinValue)*(Height-Width*2-Thumb) div (fMaxValue-fMinValue);
      sa_Horizontal: ThumbPos := (fPosition-fMinValue)*(Width-Height*2-Thumb) div (fMaxValue-fMinValue);
    end;
    State := [];
  end else begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := Math.max((Height-Width*2-Thumb),0) div 2;
      sa_Horizontal: ThumbPos := Math.max((Width-Height*2-Thumb),0) div 2;
    end;
    State := [bs_Disabled];
  end;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.Write3DButton(Left,Top+Width+ThumbPos,Width,Thumb,0,0,State,Style);
    sa_Horizontal: fRenderUI.Write3DButton(Left+Height+ThumbPos,Top,Thumb,Height,0,0,State,Style);
  end;
end;


{ TKMMemo }
constructor TKMMemo.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fItemHeight := 20;
  fItems := TStringList.Create;
  fFont := aFont;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar
end;


destructor TKMMemo.Destroy;
begin
  fItems.Free;
  Inherited;
end;


procedure TKMMemo.SetHeight(aValue:Integer);
begin
  Inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


procedure TKMMemo.SetWidth(aValue:Integer);
begin
  Inherited;
  fScrollBar.Left := fLeft + fWidth - 20;
  ReformatText; //Repositions the scroll bar as well
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMMemo.SetVisible(aValue:boolean);
begin
  Inherited;
  fScrollBar.Visible := fVisible; //Hide scrollbar and its buttons
end;


function TKMMemo.GetTopIndex: smallint;
begin
  Result := fScrollBar.Position;
end;


procedure TKMMemo.SetTopIndex(aIndex: smallint);
begin
  fScrollBar.Position := aIndex;
end;


procedure TKMMemo.SetEnabled(aValue:boolean);
begin
  Inherited;
  fScrollBar.Enabled := aValue;
end;


procedure TKMMemo.SetAutoWrap(const Value: boolean);
begin
  fAutoWrap := Value;
  ReformatText;
end;


procedure TKMListBox.SetItemHeight(const Value: byte);
begin
  fItemHeight := Value;
  UpdateScrollBar;
end;


function TKMMemo.GetText: string;
begin
  Result := fText;
end;


procedure TKMMemo.SetText(const aText: string);
begin
  fText := aText;
  ReformatText;
end;


procedure TKMMemo.ReformatText;
var NewText:string;
begin
  if fAutoWrap then
    NewText := fResource.ResourceFont.WordWrap(fText, fFont, fWidth - fScrollBar.Width - 8, True)
  else
    NewText := fText;

  //KaM uses | for new line, fItems.Text:= uses standard eol to parse each item from the string
  fItems.Text := StringReplace(NewText,'|',eol,[rfReplaceAll]);
  UpdateScrollBar;
end;


//fItems.Count or Height has changed
procedure TKMMemo.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fItems.Count - (fHeight div fItemHeight);

  if fScrollDown then
  begin
    if fScrollBar.fMaxValue-fScrollBar.Position <= 2 then
      SetTopIndex(fItems.Count) //This puts it at the bottom because of the EnsureRange in SetTopIndex
  end
  else
    SetTopIndex(0);
end;


procedure TKMMemo.Add(const aItem:string);
begin
  if fText <> '' then
    fText := fText + '|';

  fText := fText + aItem; //Append the new string

  SetText(fText); //Updates the text in fItems
  UpdateScrollBar; //Scroll down with each item that is added.
end;


procedure TKMMemo.Clear;
begin
  fText := '';
  fItems.Clear;
  UpdateScrollBar;
end;


procedure TKMMemo.ScrollToBottom;
begin
  SetTopIndex(fItems.Count);
end;


procedure TKMMemo.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
end;


procedure TKMMemo.Paint;
var i,PaintWidth:integer;
begin
  Inherited;
  if fScrollBar.Visible then
    PaintWidth := Width-fScrollBar.Width //Leave space for scrollbar
  else
    PaintWidth := Width; //List takes up the entire width

  fRenderUI.WriteBevel(Left, Top, PaintWidth, Height, false, 0.5);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    fRenderUI.WriteText(Left+4, Top+i*fItemHeight+3, Width-8, 0, fItems.Strings[TopIndex+i] , fFont, taLeft, $FFFFFFFF);
end;


{ TKMListBox }
constructor TKMListBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fBackAlpha := 0.5;
  fItemHeight := 20;
  fItemIndex := -1;
  fItems := TStringList.Create;
  fFont := aFont;
  fAutoHideScrollBar := False; //Always show the scrollbar by default, then it can be turned off if required

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar
end;


destructor TKMListBox.Destroy;
begin
  fItems.Free;
  Inherited;
end;


procedure TKMListBox.SetHeight(aValue:Integer);
begin
  Inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMListBox.SetVisible(aValue:boolean);
begin
  Inherited;
  fScrollBar.Visible := fVisible and (not fAutoHideScrollBar or fScrollBar.Enabled); //Hide scrollbar and its buttons
end;


function TKMListBox.GetTopIndex: Integer;
begin
  Result := fScrollBar.Position;
end;


procedure TKMListBox.SetTopIndex(aIndex: Integer);
begin
  fScrollBar.Position := aIndex;
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
procedure TKMListBox.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fItems.Count - (fHeight div fItemHeight);
  fScrollBar.Visible := fVisible and (not fAutoHideScrollBar or fScrollBar.Enabled);
end;


procedure TKMListBox.Add(aItem:string);
begin
  fItems.Add(aItem);
  UpdateScrollBar;
end;


procedure TKMListBox.Clear;
begin
  fItems.Clear;
  fItemIndex := -1;
  UpdateScrollBar;
end;


procedure TKMListBox.SetItems(aText:string);
begin
  fItems.Text := aText;
  fItemIndex := -1;
  UpdateScrollBar;
end;


//Hide the scrollbar if it is not required (disabled) This is used for drop boxes.
procedure TKMListBox.SetAutoHideScrollBar(Value: boolean);
begin
  fAutoHideScrollBar := Value;
  UpdateScrollBar;
end;


function TKMListBox.Count:integer;
begin
  Result := fItems.Count;
end;


function TKMListBox.GetItem(aIndex:integer):string;
begin
  Result := fItems[aIndex];
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
     (InRange(X, Left, Left+Width-( fScrollBar.Width*byte(fScrollBar.Visible) ))) and
     (InRange(Y, Top, Top+Height div fItemHeight * fItemHeight))
  then begin
    NewIndex := TopIndex + (Y-Top) div fItemHeight;

    if NewIndex > fItems.Count-1 then NewIndex := -1;

    if (NewIndex<>fItemIndex) then begin
      fItemIndex := NewIndex;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMListBox.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMListBox.Paint;
var i,PaintWidth:integer;
begin
  Inherited;
  if fScrollBar.Visible then
    PaintWidth := Width-fScrollBar.Width //Leave space for scrollbar
  else    
    PaintWidth := Width; //List takes up the entire width

  fRenderUI.WriteBevel(Left, Top, PaintWidth, Height, false, fBackAlpha);

  if (fItemIndex <> -1) and InRange(ItemIndex - TopIndex, 0, (fHeight div ItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+fItemHeight*(fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    fRenderUI.WriteText(Left+4, Top+i*fItemHeight+3, PaintWidth-8, 0, fItems.Strings[TopIndex+i] , fFont, taLeft, $FFFFFFFF);
end;


{ TKMColumnListBox }
constructor TKMColumnListBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aHeaderFont:TKMFont; aColumns:array of string; aItemOffsets:array of integer);
var i:integer;
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fBackAlpha := 0.5;
  fItemHeight := 20;
  fItemIndex := -1;
  fItemTop := 24; //Allow 20px for the header
  fColumns := TStringList.Create;
  SetLength(fItems,Length(aColumns));
  SetLength(fItemOffsets,Length(aColumns));
  SetLength(fTags,0);
  SetLength(fTags2,0);
  SetLength(fItemColors,Length(aColumns));
  assert(Length(aColumns) = Length(aItemOffsets));
  for i:=0 to Length(aColumns)-1 do
  begin
    fItems[i] := TStringList.Create;
    fColumns.Add(aColumns[i]);
    fItemOffsets[i] := aItemOffsets[i];
  end;
  fFont := aFont;
  fHeaderFont := aHeaderFont;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-fItemHeight, aTop, fItemHeight, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar
end;


destructor TKMColumnListBox.Destroy;
var i:integer;
begin
  for i:=0 to Length(fItems)-1 do
    fItems[i].Free;
  fColumns.Free;
  Inherited;
end;


procedure TKMColumnListBox.SetHeight(aValue:Integer);
begin
  Inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMColumnListBox.SetVisible(aValue:boolean);
begin
  Inherited;
  fScrollBar.Visible := fVisible; //Hide scrollbar and its buttons
end;


function TKMColumnListBox.GetTopIndex: Integer;
begin
  Result := fScrollBar.Position;
end;


procedure TKMColumnListBox.SetTopIndex(aIndex: Integer);
begin
  fScrollBar.Position := aIndex;
end;


procedure TKMColumnListBox.SetBackAlpha(aValue:single);
begin
  fBackAlpha := aValue;
  fScrollBar.BackAlpha := aValue;
end;


procedure TKMColumnListBox.SetEnabled(aValue:boolean);
begin
  Inherited;
  fScrollBar.Enabled := aValue;
end;


//fItems.Count or Height has changed
procedure TKMColumnListBox.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fItems[0].Count - ((fHeight - fItemTop) div fItemHeight);
end;


procedure TKMColumnListBox.AddItem(aItem:array of string; aItemColor:array of TColor4; aTag:integer=0; aTag2:integer=0);
var i, NewCount: integer;
begin
  Assert(Length(aItem) = Length(fItems));
  Assert(Length(aItemColor) = Length(fItems));
  NewCount := fItems[0].Count + 1;
  SetLength(fTags, NewCount);
  SetLength(fTags2, NewCount);
  fTags[NewCount-1] := aTag;
  fTags2[NewCount-1] := aTag2;
  for i:=0 to Length(fItems)-1 do
  begin
    fItems[i].Add(aItem[i]);
    SetLength(fItemColors[i],NewCount);
    fItemColors[i, NewCount-1] := aItemColor[i];
  end;
  UpdateScrollBar;
end;


procedure TKMColumnListBox.Clear;
var i: integer;
begin
  for i:=0 to Length(fItems)-1 do
    fItems[i].Clear;
  fItemIndex := -1;
  UpdateScrollBar;
end;


function TKMColumnListBox.GetItemTag:integer;
begin
  if InRange(ItemIndex, 0, Count-1) then
    Result := fTags[ItemIndex]
  else
    Result := -1;
end;


function TKMColumnListBox.GetItemTag2:integer;
begin
  if InRange(ItemIndex, 0, Count-1) then
    Result := fTags2[ItemIndex]
  else
    Result := -1;
end;


function TKMColumnListBox.Count:integer;
begin
  Result := fItems[0].Count;
end;


procedure TKMColumnListBox.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMColumnListBox.MouseMove(X,Y:integer; Shift:TShiftState);
var NewIndex:integer;
begin
  Inherited;

  if (ssLeft in Shift) and
     (InRange(X, Left, Left+Width-( fScrollBar.Width*byte(fScrollBar.Visible) ))) and
     (InRange(Y, Top+fItemTop, Top+Height div fItemHeight * fItemHeight))
  then begin
    NewIndex := TopIndex + (Y-(Top+fItemTop)) div fItemHeight;

    if NewIndex > fItems[0].Count-1 then NewIndex := -1;

    if (NewIndex<>fItemIndex) then begin
      fItemIndex := NewIndex;
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMColumnListBox.MouseWheel(Sender: TObject; WheelDelta:integer);
begin
  Inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMColumnListBox.Paint;
var i,k,PaintWidth,ItemWidth:integer;
begin
  Inherited;
  if fScrollBar.Visible then
    PaintWidth := Width-fScrollBar.Width //Leave space for scrollbar
  else    
    PaintWidth := Width; //List takes up the entire width

  fRenderUI.WriteBevel(Left, Top, PaintWidth, Height, false, fBackAlpha);

  if (fItemIndex <> -1) and InRange(ItemIndex - TopIndex, 0, ((fHeight-fItemTop) div ItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+fItemTop+fItemHeight*(fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888);

  for i:=0 to Length(fItems)-1 do
    fRenderUI.WriteText(Left+4+fItemOffsets[i], 4+Top, 0, 0, fColumns[i] , fHeaderFont, taLeft, $FFFFFFFF);

  for i:=0 to Math.min(fItems[0].Count-1, ((fHeight-fItemTop) div fItemHeight)-1) do
    for k:=0 to Length(fItems)-1 do
    begin
      if k = Length(fItems)-1 then ItemWidth := (PaintWidth-4) else ItemWidth := fItemOffsets[k+1];
      ItemWidth := ItemWidth-fItemOffsets[k]-4;
      fRenderUI.WriteText(Left+4+fItemOffsets[k], Top+fItemTop+i*fItemHeight+3, ItemWidth, 0, fItems[k].Strings[TopIndex+i] , fFont, taLeft, fItemColors[k,i]);
    end;
end;


{ TKMDropBox }
constructor TKMDropBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer; aFont:TKMFont; aDefaultCaption:string);
var P:TKMPanel;
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fDropCount := 10;
  fDropUp := false;
  fFont := aFont;
  fDefaultCaption := aDefaultCaption;
  fOnClick := ListShow; //It's common behavior when click on dropbox will show the list

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, 4, bsMenu);
  fButton.fOnClick := ListShow;
  fButton.MakesSound := false;

  P := MasterParent;
  fShape := TKMShape.Create(P, 0, 0, P.Width, P.Height, $00000000);
  fShape.fOnClick := ListHide;

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fList := TKMListBox.Create(P, Left-P.Left, Top+aHeight-P.Top, aWidth, 0, fFont);
  fList.AutoHideScrollBar := True; //A drop box should only have a scrollbar if required
  fList.BackAlpha := 0.85;
  fList.fOnClick := ListClick;

  ListHide(nil);
end;


procedure TKMDropBox.ListShow(Sender:TObject);
begin
  if fList.Visible then
  begin
    ListHide(nil);
    exit;
  end;

  if fList.Count < 1 then Exit;

  fList.Height := Math.min(fDropCount, fList.Count)*fList.ItemHeight;
  if fDropUp then
    fList.Top := Top-fList.Height-MasterParent.Top
  else
    fList.Top := Top+Height-MasterParent.Top;
  fList.TopIndex := ItemIndex - fDropCount div 2;

  fList.Show;
  fShape.Show;
end;


procedure TKMDropBox.ListClick(Sender:TObject);
begin
  if (fList.ItemIndex <> -1) then
  begin
    fCaption := fList.Item[fList.ItemIndex];
    if Assigned(fOnChange) then fOnChange(Self);
  end;
  ListHide(nil);
end;


procedure TKMDropBox.ListHide(Sender:TObject);
begin
  fList.Hide;
  fShape.Hide;
end;


function TKMDropBox.GetItemIndex:smallint;
begin
  Result := fList.ItemIndex;
end;


procedure TKMDropBox.SetItemIndex(aIndex:smallint);
begin
  fList.ItemIndex := aIndex;
  if aIndex <> -1 then
    fCaption := fList.Item[fList.ItemIndex]
  else
    fCaption := fDefaultCaption;
end;


procedure TKMDropBox.SetEnabled(aValue:boolean);
begin
  Inherited;
  fButton.Enabled := aValue;
  fList.Enabled := aValue;
end;


procedure TKMDropBox.SetVisible(aValue:boolean);
begin
  Inherited;
  fButton.Visible := aValue;
  if not aValue then ListHide(Self);
end;


procedure TKMDropBox.Add(aItem:string);
begin
  fList.Add(aItem);
end;


procedure TKMDropBox.SetItems(aText:string);
begin
  fList.SetItems(aText);
  fCaption := fDefaultCaption;
end;


procedure TKMDropBox.SelectByName(aText:string);
var i:integer;
begin
  fList.ItemIndex := -1;
  for i:=0 to fList.Count-1 do
    if fList.Item[i] = aText then
      SetItemIndex(i);
end;


function TKMDropBox.GetItem(aIndex: integer):string;
begin
  Result := fList.Item[aIndex];
end;


procedure TKMDropBox.Paint;
var Col:TColor4;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  fRenderUI.WriteText(Left+4, Top+4, Width-8, 0, fCaption, fFont, taLeft, Col);
end;


{ TKMDropColorBox }
constructor TKMDropColorBox.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight,aCount:integer);
var P:TKMPanel; Size:integer;
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fColorIndex := 0;
  fRandomCaption := ''; //Disable random by default
  fOnClick := ListShow; //It's common behavior when click on dropbox will show the list

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, 4, bsMenu);
  fButton.fOnClick := ListShow;
  fButton.MakesSound := false;

  P := MasterParent;
  fShape := TKMShape.Create(P, 0, 0, P.Width, P.Height, $00000000);
  fShape.fOnClick := ListHide;

  Size := Round(Sqrt(aCount)+0.5); //Round up

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fSwatch := TKMColorSwatch.Create(P, Left-P.Left, Top+aHeight-P.Top, Size, Size, aWidth div Size);
  fSwatch.BackAlpha := 0.75;
  fSwatch.fOnClick := ListClick;

  ListHide(nil);
end;


procedure TKMDropColorBox.ListShow(Sender:TObject);
begin
  if fSwatch.Visible then
  begin
    ListHide(nil);
    exit;
  end;

  fSwatch.Show;
  fShape.Show;
end;


procedure TKMDropColorBox.ListClick(Sender:TObject);
begin
  fColorIndex := fSwatch.ColorIndex;
  if Assigned(fOnChange) then fOnChange(Self);
  ListHide(nil);
end;


procedure TKMDropColorBox.ListHide(Sender:TObject);
begin
  fSwatch.Hide;
  fShape.Hide;
end;


procedure TKMDropColorBox.SetEnabled(aValue:boolean);
begin
  Inherited;
  fButton.Enabled := aValue;
  fSwatch.Enabled := aValue;
end;


//Set ColorIndex to fSwatch as well since it holds the actual color that we use on Paint
procedure TKMDropColorBox.SetColorIndex(aIndex:integer);
begin
  fColorIndex := aIndex;
  fSwatch.ColorIndex := aIndex;
end;


procedure TKMDropColorBox.SetColors(aColors:array of TColor4; aRandomCaption:string='');
begin
  //Store local copy of flag to substitute 0 color with "Random" text
  fRandomCaption := aRandomCaption;
  fSwatch.SetColors(aColors, (fRandomCaption <> ''));
end;


procedure TKMDropColorBox.Paint;
var Col:TColor4;
begin
  Inherited;
  fRenderUI.WriteBevel(Left, Top, Width-fButton.Width, Height);
  fRenderUI.WriteLayer(Left+2, Top+1, Width-fButton.Width-3, Height-2, fSwatch.GetColor, $00);
  if (fRandomCaption <> '') and (fSwatch.ColorIndex = 0) then
  begin
    if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;
    fRenderUI.WriteText(Left+4, Top+3, 0, 0, fRandomCaption, fnt_Metal, taLeft, Col);
  end;
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
end;


function TKMMinimap.LocalToMapCoords(X,Y:integer; const Inset:shortint=0):TKMPoint;
begin
  Assert(Inset>=-1, 'Min allowed inset is -1, to be within TKMPoint range of 0..n');
  Result.X := EnsureRange(X - (Left+(Width -fMapSize.X) div 2), 1+Inset, fMapSize.X-Inset);
  Result.Y := EnsureRange(Y - (Top +(Height-fMapSize.Y) div 2), 1+Inset, fMapSize.Y-Inset);
end;


procedure TKMMinimap.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift);
end;


procedure TKMMinimap.MouseMove(X,Y:integer; Shift:TShiftState);
var ViewPos:TKMPoint;
begin
  Inherited;

  if ssLeft in Shift then 
  begin
    ViewPos := LocalToMapCoords(X,Y);
    if Assigned(fOnChange) then
      fOnChange(Self, ViewPos.X, ViewPos.Y);
  end;
end;


procedure TKMMinimap.Paint;
begin
  Inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.RenderMinimap(Left,Top,Width,Height);
  fRenderUI.WriteRect(Left + (Width-fMapSize.X) div 2 + fViewArea.Left,
                      Top  + (Height-fMapSize.Y) div 2 + fViewArea.Top,
                      fViewArea.Right-fViewArea.Left,
                      fViewArea.Bottom-fViewArea.Top, 1, $FFFFFFFF);
end;


{ TKMDragger }
constructor TKMDragger.Create(aParent:TKMPanel; aLeft,aTop,aWidth,aHeight:integer);
begin
  Inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  //Original position is used to resrict movement
  fPositionX := 0;
  fPositionY := 0;
end;


procedure TKMDragger.SetBounds(aMinusX, aMinusY, aPlusX, aPlusY: Integer);
begin
  fMinusX := aMinusX;
  fMinusY := aMinusY;
  fPlusX  := aPlusX;
  fPlusY  := aPlusY;
end;


procedure TKMDragger.MouseDown(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  fStartDragX := X - fPositionX;
  fStartDragY := Y - fPositionY;

  MouseMove(X,Y,Shift);
end;


procedure TKMDragger.MouseMove(X,Y:integer; Shift:TShiftState);
begin
  Screen.Cursor := c_DragUp; //Always use the dragger cursor while the mouse is over it
  Inherited;

  if csDown in State then
  begin
    //Bounds are signed numbers, set them properly
    fPositionX := EnsureRange((X - fStartDragX), fMinusX, fPlusX);
    fPositionY := EnsureRange((Y - fStartDragY), fMinusY, fPlusY);

    if Assigned(OnMove) then OnMove(Self, fPositionX, fPositionY);
  end;
end;


procedure TKMDragger.MouseUp(X,Y:integer; Shift:TShiftState; Button:TMouseButton);
begin
  Inherited;
  MouseMove(X,Y,Shift);
end;


procedure TKMDragger.Paint;
var StateSet:T3DButtonStateSet;
begin
  Inherited;
  StateSet:=[];
  if (csOver in State) and fEnabled then StateSet:=StateSet+[bs_Highlight];
  if (csDown in State) then StateSet:=StateSet+[bs_Down];
  if not fEnabled then StateSet:=StateSet+[bs_Disabled];

  fRenderUI.Write3DButton(Left,Top, Width, Height, 0, 0, StateSet, bsGame);
end;


{ TKMMasterControl }
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
  //Only TKMEdit can have focus so far,
  //when we need more focusable controls we might invent a better solution than this
  if fCtrlUp is TKMEdit then
    if fCtrlDown = fCtrlUp then
      CtrlFocus := fCtrlUp
    else
      CtrlFocus := nil;
end;


{ Recursing function to find topmost control (excl. Panels)}
function TKMMasterControl.HitControl(X,Y:integer; aIncludeDisabled:boolean=false):TKMControl;
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
      if P.Childs[i].HitTest(aX,aY,aIncludeDisabled) then begin
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


procedure TKMMasterControl.KeyPress(Key: Char);
begin
  if CtrlFocus <> nil then
    CtrlFocus.KeyPress(Key);
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
var HintControl:TKMControl;
begin
  if CtrlDown=nil then CtrlOver := HitControl(X,Y); //User is dragging some Ctrl (e.g. scrollbar) and went away from Ctrl bounds
  if CtrlOver <> nil then
    CtrlOver.MouseMove(X,Y,Shift);

  HintControl := HitControl(X,Y,true); //Include disabled controls
  if (HintControl <> nil) and Assigned(fOnHint) then fOnHint(HintControl);
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

  if MODE_DESIGN_CONTORLS and (CtrlFocus <> nil) then
    fRenderUI.WriteText(CtrlFocus.Left, CtrlFocus.Top-14, 0, 0, inttostr(CtrlFocus.Left)+':'+inttostr(CtrlFocus.Top), fnt_Grey, taLeft, $FFFFFFFF);
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
