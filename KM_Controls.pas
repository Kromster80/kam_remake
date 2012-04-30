unit KM_Controls;
{$I KaM_Remake.inc}
interface
uses
    {$IFDEF MSWindows} Windows, {$ENDIF}
    {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
    Classes, Controls,  Math, SysUtils, Clipbrd,
    KromUtils, KromOGLUtils, KM_Defaults, KM_Points, KM_CommonEvents, KM_Pics,
    KM_ResourceSprites, KM_MapView;

type
  TNotifyEventMB = procedure(Sender: TObject; AButton: TMouseButton) of object;
  TNotifyEventMW = procedure(Sender: TObject; WheelDelta: Integer) of object;
  TNotifyEventKey = procedure(Sender: TObject; Key: Word) of object;
  TNotifyEventXY = procedure(Sender: TObject; X, Y: Integer) of object;

  TTextAlign = (taLeft, taCenter, taRight);
  TKMControlState = (csDown, csFocus, csOver);
  TKMControlStateSet = set of TKMControlState;

  TButtonStyle = (bsMenu, bsGame); //Menu buttons are metal, game buttons are stone
  TButtonStateSet = set of (bsOver, bsDown, bsDisabled);


  TKMControl = class;
  TKMPanel = class;

  { TKMMaster }
  TKMMasterControl = class
  private
    fCtrl: TKMPanel; //Parentmost control (TKMPanel with all its childs)
    fCtrlDown: TKMControl; //Control that was pressed Down
    fCtrlFocus: TKMControl; //Control which has input Focus
    fCtrlOver: TKMControl; //Control which has cursor Over it
    fCtrlUp: TKMControl; //Control above which cursor was released

    fOnHint: TNotifyEvent; //Comes along with OnMouseOver

    function HitControl(X,Y: Integer; aIncludeDisabled: Boolean=false): TKMControl;
    procedure SetCtrlDown(aCtrl: TKMControl);
    procedure SetCtrlFocus(aCtrl: TKMControl);
    procedure SetCtrlOver(aCtrl: TKMControl);
    procedure SetCtrlUp(aCtrl: TKMControl);
  public
    constructor Create;
    destructor Destroy; override;

    property CtrlDown: TKMControl read fCtrlDown write SetCtrlDown;
    property CtrlFocus: TKMControl read fCtrlFocus write SetCtrlFocus;
    property CtrlOver: TKMControl read fCtrlOver write SetCtrlOver;
    property CtrlUp: TKMControl read fCtrlUp write SetCtrlUp;

    property OnHint: TNotifyEvent write fOnHint;

    function KeyDown    (Key: Word; Shift: TShiftState): Boolean;
    procedure KeyPress  (Key: Char);
    function KeyUp      (Key: Word; Shift: TShiftState): Boolean;
    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
    procedure MouseMove (X,Y: Integer; Shift: TShiftState);
    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
    procedure MouseWheel(X,Y: Integer; WheelDelta: Integer);

    procedure Paint;

    procedure SaveToFile(aFileName: string);
  end;


  {Base class for all TKM elements}
  TKMControl = class
  private
    fParent: TKMPanel;
    fAnchors: TAnchors;

    //Left and Top are floating-point to allow to precisely store controls position
    //when Anchors [] are used. Cos that means that control must be centered
    //even if the Parent resized by 1px. Otherwise error quickly accumylates on
    //multiple 1px resizes
    //Everywhere else Top and Left are accessed through Get/Set and treated as Integers
    fLeft: Single;
    fTop: Single;
    fWidth: Integer;
    fHeight: Integer;

    fEnabled: Boolean;
    fVisible: Boolean;

    fTimeOfLastClick: Cardinal; //Required to handle double-clicks

    fOnClick: TNotifyEvent;
    fOnClickEither: TNotifyEventMB;
    fOnClickRight: TPointEvent;
    fOnDoubleClick: TNotifyEvent;
    fOnMouseWheel: TNotifyEventMW;
    //fOnMouseOver: TNotifyEvent;

    function GetLeft: Integer;
    function GetTop: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;

    //Let the control know that it was clicked to do its internal magic
    procedure DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); virtual;

    function GetVisible: Boolean;
  protected
    procedure SetLeft(aValue: Integer); virtual;
    procedure SetTop(aValue: Integer); virtual;
    procedure SetHeight(aValue: Integer); virtual;
    procedure SetWidth(aValue: Integer); virtual;
    procedure SetVisible(aValue: Boolean); virtual;
    procedure SetEnabled(aValue: Boolean); virtual;
  public
    Hitable: Boolean; //Can this control be hit with the cursor?
    Focusable: Boolean; //Can this control have focus (e.g. TKMEdit sets this true)
    State: TKMControlStateSet; //Each control has it localy to avoid quering Collection on each Render
    Scale: Single; //Child controls position is scaled

    Tag: Integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
    function HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean; virtual;

    property Parent: TKMPanel read fParent;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Anchors: TAnchors read fAnchors write fAnchors;
    property Enabled: Boolean read fEnabled write SetEnabled;
    property Visible: Boolean read GetVisible write SetVisible;
    procedure Enable;
    procedure Disable;
    procedure Show;
    procedure Hide;
    procedure Center;
    procedure Stretch;
    function MasterParent: TKMPanel;

    function KeyDown(Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure KeyPress(Key: Char); virtual;
    function KeyUp(Key: Word; Shift: TShiftState): Boolean; virtual;
    procedure MouseDown (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); virtual;
    procedure MouseMove (X,Y: Integer; Shift: TShiftState); virtual;
    procedure MouseUp   (X,Y: Integer; Shift: TShiftState; Button: TMouseButton); virtual;
    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); virtual;

    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property OnClickEither: TNotifyEventMB read fOnClickEither write fOnClickEither;
    property OnClickRight: TPointEvent read fOnClickRight write fOnClickRight;
    property OnDoubleClick: TNotifyEvent read fOnDoubleClick write fOnDoubleClick;
    property OnMouseWheel: TNotifyEventMW read fOnMouseWheel write fOnMouseWheel;
    //property OnMouseOver: TNotifyEvent write fOnMouseOver;

    procedure Paint; virtual;
  end;


  { Panel which keeps child items in it, it's virtual and invisible }
  TKMPanel = class(TKMControl)
  private
    GetCollection: TKMMasterControl;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;
  public
    ChildCount: Word;             //Those two are actually used only for TKMPanel
    Childs: array of TKMControl; //No other elements needs to be parented
    constructor Create(aParent: TKMMasterControl; aLeft,aTop,aWidth,aHeight: Integer); overload;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer); overload;
    destructor Destroy; override;
    procedure AddChild(aChild: TKMControl);
    procedure Paint; override;
  end;


  { Beveled area }
  TKMBevel = class(TKMControl)
  public
    BackAlpha: Single;
    HalfBright: Boolean;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
    procedure Paint; override;
  end;


  {Rectangle}
  TKMShape = class(TKMControl)
  public
    FillColor: TColor4;
    LineColor: TColor4; //color of outline
    LineWidth: Byte;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aLineColor: TColor4);
    procedure Paint; override;
  end;


  {Text Label}
  TKMLabel = class(TKMControl)
  private
    fAutoWrap: Boolean;
    fFont: TKMFont;
    fFontColor: TColor4; //Usually white (self-colored)
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
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign); overload;
    constructor Create(aParent: TKMPanel; aLeft,aTop: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign); overload;
    function HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean; override;
    property AutoWrap: boolean read fAutoWrap write SetAutoWrap; //Whether to automatically wrap text within given text area width
    property Caption: string read fCaption write SetCaption;
    property FontColor: TColor4 read fFontColor write fFontColor;
    property Strikethrough: Boolean read fStrikethrough write fStrikethrough;
    property TextSize: TKMPoint read fTextSize;
    procedure Paint; override;
  end;


  //Label that is scrolled within an area. Used in Credits
  TKMLabelScroll = class(TKMLabel)
  public
    SmoothScrollToTop: cardinal; //Delta between this and TimeGetTime affects vertical position
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign);
    procedure Paint; override;
  end;


  {Image}
  TKMImage = class(TKMControl)
  private
    fRX: TRXType;
    fTexID: Word;
    fFlagColor: TColor4;
  public
    ImageAnchors: TAnchors;
    Highlight: Boolean;
    HighlightOnMouseOver: Boolean;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui);
    property RX: TRXType read fRX write fRX;
    property TexID: Word read fTexID write fTexID;
    property FlagColor: TColor4 read fFlagColor write fFlagColor;
    function Click: Boolean;
    procedure ImageStretch;
    procedure ImageCenter;
    procedure Paint; override;
  end;


  {Image stack - for army formation view}
  TKMImageStack = class(TKMControl)
  private
    fRX: TRXType;
    fTexID1, fTexID2: Word; //Normal and commander
    fCount: Integer;
    fColumns: Integer;
    fDrawWidth: Integer;
    fDrawHeight: Integer;
    fHighlightID: Integer;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID1, aTexID2: Word; aRX: TRXType = rxGui);
    procedure SetCount(aCount, aColumns, aHighlightID: Integer);
    procedure Paint; override;
  end;


  { Color swatch - to select a color from given samples/palette }
  TKMColorSwatch = class(TKMControl)
  private
    fBackAlpha: single; //Alpha of background (usually 0.5, dropbox 1)
    fCellSize: Byte; //Size of the square in pixels
    fColumnCount: Byte;
    fRowCount: Byte;
    fColorIndex: Byte; //Index 0..255 should be enough
    Colors:array of TColor4; //Range is 0..255
    fOnChange: TNotifyEvent;
    fInclRandom: Boolean;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize: Integer);
    procedure SetColors(const aColors: array of TColor4; aInclRandom: Boolean = False);
    property BackAlpha: single read fBackAlpha write fBackAlpha;
    property ColorIndex: Byte read fColorIndex write fColorIndex;
    function GetColor: TColor4;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
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
    fMakesSound: Boolean;
    fRX: TRXType;
    fTexID: Word;
    fFlagColor: TColor4; //When using an image
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui; aStyle: TButtonStyle=bsGame); overload;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aStyle: TButtonStyle=bsGame); overload;
    property Caption: string read fCaption write fCaption;
    property FlagColor: TColor4 read fFlagColor write fFlagColor;
    property MakesSound: Boolean read fMakesSound write fMakesSound;
    property TexID: Word read fTexID write fTexID;
    function Click: Boolean; //Try to click a button and return TRUE if succeded
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  {FlatButton}
  TKMButtonFlat = class(TKMControl)
  private
    fFont: TKMFont;
    fFlagColor: TColor4;
    TextAlign: TTextAlign;
  public
    RX: TRXType;
    TexID: Word;
    TexOffsetX: Shortint;
    TexOffsetY: Shortint;
    CapOffsetY: Shortint;
    Caption: string;
    Down: Boolean;
    HideHighlight: Boolean;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID: Integer; aRX: TRXType = rxGui);
    property FlagColor: TColor4 read fFlagColor write fFlagColor;

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;


  {FlatButton with Shape on it}
  TKMFlatButtonShape = class(TKMControl)
  private
    fCaption: string;
    fFont: TKMFont;
  public
    CapOffsetY: Shortint;
    ShapeColor: TColor4;
    Down: Boolean;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aShapeColor: TColor4);
    procedure Paint; override;
  end;

  TAllowedChars = (acDigits, acFileName, acText);

  {EditField}
  TKMEdit = class(TKMControl)
  private
    fFont: TKMFont;
    fText: string;
    fAllowedChars: TAllowedChars;
    fCursorPos: Integer;
    fLeftIndex: Integer; //The position of the character shown left-most when text does not fit
    procedure SetCursorPos(aPos: Integer);
    procedure SetText(aText: string);
    function KeyEventHandled(Key: Word): Boolean;
  public
    Masked: Boolean; //Mask entered text as *s
    ReadOnly: Boolean;
    ShowColors: Boolean;
    OnChange: TNotifyEvent;
    OnKeyDown: TNotifyEventKey;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);

    property AllowedChars: TAllowedChars read fAllowedChars write fAllowedChars;
    property CursorPos: Integer read fCursorPos write SetCursorPos;
    property Text: string read fText write SetText;

    function HitTest(X,Y: Integer; aIncludeDisabled: Boolean=false): Boolean; override;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure KeyPress(Key: Char); override;
    function KeyUp(Key: Word; Shift: TShiftState): Boolean; override;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  { Checkbox }
  TKMCheckBox = class(TKMControl)
  private
    fCaption: string;
    fChecked: Boolean;
    fFlatStyle: Boolean; //Render the check as a rectangle (modern style)
    fFont: TKMFont;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont); overload;
    property Caption: string read fCaption write fCaption;
    property Checked: Boolean read fChecked write fChecked;
    property FlatStyle: Boolean read fFlatStyle write fFlatStyle;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  { TKMRadioGroup }
  TKMRadioGroup = class(TKMControl)
  private
    fItemIndex: Integer;
    fItems: TStringList;
    fFont: TKMFont;
    fOnChange: TNotifyEvent;
    function GetItemCount: Integer;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
    destructor Destroy; override;
    property ItemCount: Integer read GetItemCount;
    property ItemIndex: Integer read fItemIndex write fItemIndex;
    property Items: TStringList read fItems;
    property OnChange: TNotifyEvent write fOnChange;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  {Percent bar}
  TKMPercentBar = class(TKMControl)
  private
    fFont: TKMFont;
  public
    Position: Integer;
    Caption: string;
    FontColor: TColor4;
    TextAlign: TTextAlign;
    TextYOffset: Integer;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont = fnt_Mini);
    procedure Paint; override;
  end;


  {Row with resource name and icons}
  TKMResourceRow = class(TKMControl)
  public
    RX: TRXType;
    TexID: Word;
    Caption: String;
    ResourceCount: Byte;
    procedure Paint; override;
  end;


  {Resource order bar}
  TKMResourceOrderRow = class(TKMControl)
  private
    fOrderAdd: TKMButton;
    fOrderLab: TKMLabel;
    fOrderRem: TKMButton;
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    RX: TRXType;
    TexID: Word;
    Caption: String;
    ResourceCount: Byte;
    OrderCount: Word;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
    property OrderAdd: TKMButton read fOrderAdd; //UI sets and handles OnClickEither itself
    property OrderRem: TKMButton read fOrderRem;
    procedure Paint; override;
  end;


  {Production cost bar}
  TKMCostsRow = class(TKMControl)
  public
    RX: TRXType;
    TexID1, TexID2: Word;
    Caption: String;
    procedure Paint; override;
  end;


  {Ratio bar}
  TKMTrackBar = class(TKMControl)
  private
    fTrackTop: Byte; //Offset trackbar from top (if Caption <> '')
    fTrackHeight: Byte; //Trackbar height
    fMinValue: Word;
    fMaxValue: Word;
    fOnChange: TNotifyEvent;
    fCaption: string;
    function ThumbWidth: Word;
    procedure SetCaption(const aValue: string);
  public
    Position: Word;
    Step: Byte; //Change Position by this amount each time
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aMin, aMax: Word);

    property Caption: string read fCaption write SetCaption;
    property MinValue: Word read fMinValue;
    property MaxValue: Word read fMaxValue;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;


  TScrollAxis = (sa_Vertical, sa_Horizontal);

  { Scroll bar }
  TKMScrollBar = class(TKMControl)
  private
    fBackAlpha: Single; //Alpha of background (usually 0.5, dropbox 1)
    fScrollAxis: TScrollAxis;
    fStyle: TButtonStyle;
    fMinValue: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fThumb: Word; //Length of the thumb
    fScrollDec: TKMButton;
    fScrollInc: TKMButton;
    fOnChange: TNotifyEvent;
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure IncPosition(Sender: TObject);
    procedure DecPosition(Sender: TObject);
    procedure UpdateThumbSize;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aScrollAxis: TScrollAxis; aStyle: TButtonStyle);
    property BackAlpha: Single read fBackAlpha write fBackAlpha;
    property MinValue: Integer read fMinValue write SetMinValue;
    property MaxValue: Integer read fMaxValue write SetMaxValue;
    property Position: Integer read fPosition write SetPosition;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Paint; override;
  end;


  TKMListBox = class(TKMControl)
  private
    fAutoHideScrollBar: Boolean;
    fBackAlpha: Single; //Alpha of background (usually 0.5, dropbox 1)
    fFont: TKMFont; //Should not be changed from inital value, it will mess up the word wrapping
    fItemHeight: Byte;
    fItemIndex: Smallint;
    fItems: TStringList;
    fScrollBar: TKMScrollBar;
    fOnChange: TNotifyEvent;
    function GetTopIndex: Integer;
    procedure SetTopIndex(aIndex: Integer);
    procedure SetBackAlpha(aValue: single);
    procedure SetItemHeight(const Value: byte);
    procedure SetAutoHideScrollBar(Value: boolean);
    procedure UpdateScrollBar;
    function GetItem(aIndex: Integer): string;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
    destructor Destroy; override;

    property AutoHideScrollBar: boolean read fAutoHideScrollBar write SetAutoHideScrollBar;
    property BackAlpha: Single write SetBackAlpha;

    procedure Add(aItem: string);
    procedure Clear;
    function Count: Integer;
    procedure SetItems(aText: string);

    property Item[aIndex: Integer]: string read GetItem;
    property ItemHeight: Byte read fItemHeight write SetItemHeight; //Accessed by DropBox
    property ItemIndex: Smallint read fItemIndex write fItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;

    function KeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;

  TSortDirection = (sdNone, sdUp, sdDown);

  TKMListHeader = class (TKMControl)
  private
    fFont: TKMFont;
    fBackAlpha: Single; //Alpha of background
    fCount: Integer;
    fColumns: array of string;
    fColumnHighlight: Integer;
    fColumnOffsets: array of Word; //Offsets are easier to handle than widths
    fSortIndex: Integer;
    fSortDirection: TSortDirection;
    function GetColumnIndex(X: Integer): Integer;
    function GetColumnOffset(aIndex: Integer): Word;
    procedure DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
  public
    OnColumnClick: TIntegerEvent;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);

    property BackAlpha: Single read fBackAlpha write fBackAlpha;
    property Font: TKMFont read fFont write fFont;
    property ColumnCount: Integer read fCount write fCount;
    property ColumnOffset[aIndex: Integer]: Word read GetColumnOffset;
    property SortIndex: Integer read fSortIndex write fSortIndex;
    property SortDirection: TSortDirection read fSortDirection write fSortDirection;

    procedure AddColumn(aCaption: string; aOffset: Word);
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;

  TKMListRow = record
    Cells: array of record
      Caption: string;
      Color: TColor4;
      Pic: TKMPic;
    end;
    Tag: Integer;
  end;

  TKMColumnListBox = class(TKMControl)
  private
    fFont: TKMFont;
    fBackAlpha: Single; //Alpha of background
    fItemHeight: Byte;
    fItemIndex: Smallint;
    fRowCount: Integer;
    fRows: array of TKMListRow;
    fHeader: TKMListHeader;
    fShowHeader: Boolean;
    fScrollBar: TKMScrollBar;
    fOnChange: TNotifyEvent;
    function GetTopIndex: Integer;
    procedure SetTopIndex(aIndex: Integer);
    procedure SetBackAlpha(aValue: single);
    function GetSortIndex: Integer;
    procedure SetSortIndex(aIndex: Integer);
    function GetSortDirection: TSortDirection;
    procedure SetSortDirection(aDirection: TSortDirection);
    function GetRow(aIndex: Integer): TKMListRow;
    procedure ColumnClick(aValue: Integer);
    procedure UpdateScrollBar;
    procedure SetHeaderVisible(aValue: Boolean);
  protected
    procedure SetTop(aValue: Integer); override;
    procedure SetHeight(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    OnColumnClick: TIntegerEvent;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);

    procedure SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);
    procedure AddItem(aItem: TKMListRow);
    procedure Clear;
    function HeaderHeight: Integer;
    function GetVisibleRows:Integer;
    property ShowHeader: Boolean read fShowHeader write SetHeaderVisible;

    property Rows[aIndex: Integer]: TKMListRow read GetRow;

    property BackAlpha: single read fBackAlpha write SetBackAlpha;
    property RowCount: Integer read fRowCount;
    property ItemHeight: Byte read fItemHeight;
    property ItemIndex: Smallint read fItemIndex write fItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property SortIndex: Integer read GetSortIndex write SetSortIndex;
    property SortDirection: TSortDirection read GetSortDirection write SetSortDirection;

    function KeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TKMDropCommon = class(TKMControl)
  private
    fDropCount: Byte;
    fDropUp: Boolean;
    fFont: TKMFont;
    fButton: TKMButton;
    fShape: TKMShape;
    fOnChange: TNotifyEvent;
    procedure UpdateDropPosition; virtual; abstract;
    procedure ListShow(Sender: TObject); virtual;
    procedure ListClick(Sender: TObject); virtual;
    procedure ListHide(Sender: TObject); virtual;
    function ListVisible: Boolean; virtual; abstract;
    function GetItemIndex: smallint; virtual; abstract;
    procedure SetItemIndex(aIndex: smallint); virtual; abstract;
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);

    procedure Clear; virtual; abstract;
    function Count: Integer; virtual; abstract;

    property DropCount: Byte read fDropCount write fDropCount;
    property DropUp: Boolean read fDropUp write fDropUp;
    property ItemIndex: smallint read GetItemIndex write SetItemIndex;

    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Paint; override;
  end;


  TKMDropList = class(TKMDropCommon)
  private
    fCaption: string; //Current caption (Default or from list)
    fDefaultCaption: string;
    fList: TKMListBox;
    procedure UpdateDropPosition; override;
    procedure ListShow(Sender: TObject); override;
    procedure ListClick(Sender: TObject); override;
    procedure ListHide(Sender: TObject); override;
    function ListVisible: Boolean; override;
    function GetItem(aIndex: Integer): string;
    function GetItemIndex: smallint; override;
    procedure SetItemIndex(aIndex: smallint); override;
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string);
    procedure Clear; override;
    function Count: Integer; override;
    procedure Add(aItem: string);
    procedure SetItems(aText: string);
    procedure SelectByName(aText: string);
    property DefaultCaption: string read fDefaultCaption write fDefaultCaption;
    property Item[aIndex: Integer]: string read GetItem;

    procedure Paint; override;
  end;


  TKMDropColumns = class(TKMDropCommon)
  private
    fList: TKMColumnListBox;
    procedure UpdateDropPosition; override;
    procedure ListShow(Sender: TObject); override;
    procedure ListClick(Sender: TObject); override;
    procedure ListHide(Sender: TObject); override;
    function ListVisible: Boolean; override;
    function GetItem(aIndex: Integer): TKMListRow;
    function GetItemIndex: smallint; override;
    procedure SetItemIndex(aIndex: smallint); override;
    function GetShowHeader: Boolean;
    procedure SetShowHeader(aValue: Boolean);
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    FadeImageWhenDisabled: Boolean;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
    procedure Add(aItem: TKMListRow);
    procedure Clear; override;
    function Count: Integer; override;
    property ShowHeader: Boolean read GetShowHeader write SetShowHeader;
    property Item[aIndex: Integer]: TKMListRow read GetItem;
    procedure SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);

    procedure Paint; override;
  end;


  TKMDropColorBox = class(TKMControl)
  private
    fColorIndex: Integer;
    fRandomCaption: string;
    fButton: TKMButton;
    fSwatch: TKMColorSwatch;
    fShape: TKMShape;
    fOnChange: TNotifyEvent;
    procedure ListShow(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure ListHide(Sender: TObject);
    procedure SetColorIndex(aIndex: Integer);
  protected
    procedure SetEnabled(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aCount: Integer);
    property ColorIndex: Integer read fColorIndex write SetColorIndex;
    procedure SetColors(const aColors: array of TColor4; aRandomCaption: string = '');
    property OnChange: TNotifyEvent write fOnChange;
    procedure Paint; override;
  end;


  TKMMemo = class(TKMControl)
  private
    fFont: TKMFont; //Should not be changed from inital value, it will mess up the word wrapping
    fItemHeight: Byte;
    fItems: TStringList;
    fAutoWrap: Boolean;
    fText: string;
    fScrollDown: Boolean;
    fScrollBar: TKMScrollBar;
    fOnChange: TNotifyEvent;

    procedure SetAutoWrap(const Value: boolean);
    function GetText: string;
    procedure SetText(const aText: string);
    function GetTopIndex: smallint;
    procedure SetTopIndex(aIndex: smallint);
    procedure ReformatText;
    procedure UpdateScrollBar;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;
    procedure SetVisible(aValue: Boolean); override;
    procedure SetEnabled(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
    destructor Destroy; override;

    procedure Add(const aItem: string);
    procedure Clear;
    procedure ScrollToBottom;
    property AutoWrap: boolean read fAutoWrap write SetAutoWrap; //Whether to automatically wrap text within given text area width
    property Text: string read GetText write SetText;
    property ItemHeight: Byte read fItemHeight write fItemHeight;
    property TopIndex: Smallint read GetTopIndex write SetTopIndex;
    property ScrollDown: Boolean read fScrollDown write fScrollDown;

    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TDragAxis = (daHoriz, daVertic, daAll);

  //Element that player can drag within allowed bounds
  TKMDragger = class(TKMControl)
  private
    fMinusX, fMinusY, fPlusX, fPlusY: Integer; //Restrictions
    fPositionX: Integer;
    fPositionY: Integer;
    fStartDragX: Integer;
    fStartDragY: Integer;
  public
    OnMove: TNotifyEventXY;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);

    procedure SetBounds(aMinusX, aMinusY, aPlusX, aPlusY: Integer);

    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;


  TKMGraph = class(TKMControl)
  private
    fCaption: string;
    fCount: Integer;
    fItemHeight: Byte;
    fLines: array of record
      Title: string;
      Color: TColor4;
      Visible: Boolean;
      Values: array of Word;
    end;
    fMaxLength: Word;
    fMaxValue: Word;
    procedure UpdateMaxValue;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);

    procedure AddLine(aTitle: string; aColor: TColor4; const aValues: array of Word);
    property Caption: string read fCaption write fCaption;
    procedure Clear;
    property MaxLength: Word read fMaxLength write fMaxLength;

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;

  { Minimap as stand-alone control }
  TKMMinimap = class(TKMControl)
  private
    fMapTex: TTexture;
    fMapSize: TKMPoint;
    fViewArea: TKMRect;
    fOnChange: TPointEvent;
    fPlayerLocs: array[1..MAX_PLAYERS] of TKMPoint;
    fPlayerColors:array[1..MAX_PLAYERS] of Cardinal;
    fShowLocs: Boolean;
    function GetPlayerLoc(aIndex:byte):TKMPoint;
    procedure SetPlayerLoc(aIndex:byte; aLoc:TKMPoint);
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);

    procedure UpdateFrom(aMapView:TKMMapView);
    function LocalToMapCoords(X,Y: Integer; const Inset: shortint=0): TKMPoint;
    property MapSize: TKMPoint read fMapSize write fMapSize;
    property ViewArea: TKMRect read fViewArea write fViewArea;
    property PlayerLocs[Index:byte]: TKMPoint read GetPlayerLoc write SetPlayerLoc;
    property ShowLocs: Boolean read fShowLocs write fShowLocs;
    property OnChange: TPointEvent write fOnChange;

    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;


  function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; aTag: Integer = 0): TKMListRow; overload;
  function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; const aPic: array of TKMPic; aTag: Integer = 0): TKMListRow; overload;

implementation
uses KM_RenderUI, KM_Resource, KM_ResourceCursors, KM_Sound;


var
  fRenderUI: TRenderUI;


function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; aTag: Integer = 0): TKMListRow;
var I: Integer;
begin
  Assert(Length(aCaption) = Length(aColor));

  SetLength(Result.Cells, Length(aCaption));

  for I := 0 to High(aCaption) do
  begin
    Result.Cells[I].Caption := aCaption[I];
    Result.Cells[I].Color := aColor[I];
  end;
  Result.Tag := aTag;
end;


function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; const aPic: array of TKMPic; aTag: Integer = 0): TKMListRow;
var I: Integer;
begin
  Assert(Length(aCaption) = Length(aColor));
  Assert(Length(aCaption) = Length(aPic));

  SetLength(Result.Cells, Length(aCaption));

  for I := 0 to High(aCaption) do
  begin
    Result.Cells[I].Caption := aCaption[I];
    Result.Cells[I].Color := aColor[I];
    Result.Cells[I].Pic := aPic[I];
  end;
  Result.Tag := aTag;
end;


{ TKMControl }
constructor TKMControl.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create;
  Scale     := 1;
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
  if aParent <> nil then
    aParent.AddChild(Self);
end;


function TKMControl.KeyDown(Key: Word; Shift: TShiftState): Boolean;
var Amt: Byte;
begin
  Result := MODE_DESIGN_CONTORLS;

  if MODE_DESIGN_CONTORLS then
  begin
    Amt := 1;
    if ssCtrl  in Shift then Amt := 10;
    if ssShift in Shift then Amt := 100;

    if Key = VK_LEFT  then fLeft := fLeft - Amt;
    if Key = VK_RIGHT then fLeft := fLeft + Amt;
    if Key = VK_UP    then fTop  := fTop  - Amt;
    if Key = VK_DOWN  then fTop  := fTop  + Amt;
  end
end;


procedure TKMControl.KeyPress(Key: Char);
begin
  //Could be something common
end;


function TKMControl.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := false;
  if not MODE_DESIGN_CONTORLS then exit;
  //nothing yet
end;


procedure TKMControl.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  //if Assigned(fOnMouseDown) then fOnMouseDown(Self); { Unused }
end;


procedure TKMControl.MouseMove(X,Y: Integer; Shift: TShiftState);
begin
  //if Assigned(fOnMouseOver) then fOnMouseOver(Self);
end;


procedure TKMControl.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  //if Assigned(fOnMouseUp) then OnMouseUp(Self); { Unused }
  if (csDown in State) then
  begin
    State := State - [csDown];

    //Send Click events
    DoClick(X, Y, Shift, Button);
  end;
end;


procedure TKMControl.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  if Assigned(fOnMouseWheel) then fOnMouseWheel(Self, WheelDelta);
end;


//fVisible is checked earlier
function TKMControl.HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean;
begin
  Result := Hitable and (fEnabled or aIncludeDisabled) and InRange(X, Left, Left + fWidth) and InRange(Y, Top, Top + fHeight);
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint;
var sColor: TColor4; Tmp: TKMPoint;
begin
  inc(CtrlPaintCount);
  if not SHOW_CONTROLS_OVERLAY then exit;

  sColor := $00000000;

  if Self is TKMPanel then sColor := $200000FF;

  if Self is TKMLabel then begin //Special case for aligned text
    Tmp := TKMLabel(Self).TextSize;
    fRenderUI.WriteLayer(TKMLabel(Self).TextLeft, Top, Tmp.X, Tmp.Y, $4000FFFF, $80FFFFFF);
    fRenderUI.WriteRect(TKMLabel(Self).AreaLeft, Top, fWidth, fHeight, 1, $FFFFFFFF);
    fRenderUI.WriteLayer(Left-3, Top-3, 6, 6, sColor or $FF000000, $FFFFFFFF);
    Exit;
  end;

  if Self is TKMImage      then sColor := $2000FF00;
  if Self is TKMImageStack then sColor := $2080FF00;
  if Self is TKMCheckBox   then sColor := $20FF00FF;
  if Self is TKMTrackBar   then sColor := $2000FF00;
  if Self is TKMCostsRow   then sColor := $2000FFFF;
  if Self is TKMRadioGroup then sColor := $20FFFF00;

  if csOver in State then sColor := sColor OR $30000000; //Highlight on mouse over

  fRenderUI.WriteLayer(Left, Top, fWidth, fHeight, sColor, $FFFFFFFF);
  fRenderUI.WriteLayer(Left-3, Top-3, 6, 6, sColor or $FF000000, $FFFFFFFF);
end;


{Shortcuts to Controls properties}
function TKMControl.GetLeft: Integer;
begin
  if Parent = nil then
    Result := Round(fLeft)
  else
    Result := Round(fLeft*Parent.Scale) + Parent.GetLeft;
end;

function TKMControl.GetTop: Integer;
begin
  if Parent = nil then
    Result := Round(fTop)
  else
    Result := Round(fTop*Parent.Scale) + Parent.GetTop;
end;

function TKMControl.GetHeight: Integer;
begin
  Result := fHeight;
end;

function TKMControl.GetWidth: Integer;
begin
  Result := fWidth;
end;

procedure TKMControl.SetLeft(aValue: Integer);
begin
  fLeft := aValue;
end;

procedure TKMControl.SetTop(aValue: Integer);
begin
  fTop := aValue;
end;

//Overriden in child classes
procedure TKMControl.SetHeight(aValue: Integer);
begin
  fHeight := aValue;
end;

//Overriden in child classes
procedure TKMControl.SetWidth(aValue: Integer);
begin
  fWidth := aValue;
end;


//Let the control know that it was clicked to do its internal magic
procedure TKMControl.DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  //Note that we process double-click separately (actual sequence is Click + Double-Click)
  //because we would not like to delay Click just to make sure it is single.
  //On the ther hand it does no harm to call Click first
  if (Button = mbLeft)
  and Assigned(fOnDoubleClick)
  and (TimeGet - fTimeOfLastClick <= GetDoubleClickTime) then
  begin
    fTimeOfLastClick := 0;
    fOnDoubleClick(Self);
  end
  else
  begin
    if (Button = mbLeft) and Assigned(fOnDoubleClick) then
      fTimeOfLastClick := TimeGet;

    if ((Button = mbLeft) or (Button = mbRight)) and Assigned(fOnClickEither) then
      fOnClickEither(Self, Button)
    else
    if (Button = mbLeft) and Assigned(fOnClick) then
      fOnClick(Self)
    else
    if (Button = mbRight) and Assigned(fOnClickRight) then
      fOnClickRight(Self, X, Y);
  end;
end;


{Check Control including all its Parents to see if Control is actually displayed/visible}
function TKMControl.GetVisible: Boolean;
var C: TKMControl;
begin
  Result := fVisible;
  C := Parent;
  while C <> nil do
  begin
    Result := Result and C.fVisible;
    C := C.Parent;
  end;
end;


procedure TKMControl.SetEnabled(aValue: Boolean);
begin
  fEnabled := aValue;
end;


procedure TKMControl.SetVisible(aValue: Boolean);
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


function TKMControl.MasterParent: TKMPanel;
var P: TKMPanel;
begin
  P := Parent;
  while P.Parent <> nil do
    P := P.Parent;
  Result := P;
end;



{ TKMPanel } //virtual panels to contain child items
constructor TKMPanel.Create(aParent: TKMMasterControl; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(nil, aLeft,aTop,aWidth,aHeight);
  GetCollection := aParent;
  aParent.fCtrl := Self;
end;


constructor TKMPanel.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  GetCollection := aParent.GetCollection;
end;


destructor TKMPanel.Destroy;
var i: Integer;
begin
  for i:=1 to ChildCount do
    Childs[i].Free;
  inherited;
end;


{Parentize control to another control}
{Parent control has a list of all it's child controls}
{Also transform child according to parent position}
procedure TKMPanel.AddChild(aChild: TKMControl);
begin
  inc(ChildCount);
  {Hereby I still try to make a rule to count starting from 1, not from zero}
  SetLength(Childs, ChildCount+1);
  Childs[ChildCount] := aChild;
end;


procedure TKMPanel.SetHeight(aValue: Integer);
var I: Integer;
begin
  for I := 1 to ChildCount do
    if (akTop in Childs[I].Anchors) and (akBottom in Childs[I].Anchors) then
      Childs[I].Height := Childs[I].Height + (aValue - fHeight)
    else
    if akTop in Childs[I].Anchors then
      //Do nothing
    else
    if akBottom in Childs[I].Anchors then
      Childs[I].fTop := Childs[I].fTop + (aValue - fHeight)
    else
      Childs[I].fTop := Childs[I].fTop + (aValue - fHeight) / 2;

  inherited;
end;


procedure TKMPanel.SetWidth(aValue: Integer);
var I: Integer;
begin
  for I := 1 to ChildCount do
    if (akLeft in Childs[I].Anchors) and (akRight in Childs[I].Anchors) then
      Childs[I].Width := Childs[I].Width + (aValue - fWidth)
    else
    if akLeft in Childs[I].Anchors then
      //Do nothing
    else
    if akRight in Childs[I].Anchors then
      Childs[I].fLeft := Childs[I].fLeft + (aValue - fWidth)
    else
      Childs[I].fLeft := Childs[I].fLeft + (aValue - fWidth) / 2;

  inherited;
end;


{Panel Paint means to Paint all its childs}
procedure TKMPanel.Paint;
var i: Integer;
begin
  inherited;
  for i:=1 to ChildCount do
    if Childs[i].fVisible then
      Childs[i].Paint;
end;


{ TKMBevel }
constructor TKMBevel.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  BackAlpha := 0.4; //Default value
end;


procedure TKMBevel.Paint;
begin
  inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height, HalfBright, BackAlpha);
end;


{ TKMShape }
constructor TKMShape.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aLineColor: TColor4);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  FillColor := $00000000;
  LineColor := aLineColor;
  LineWidth := 2;
end;


procedure TKMShape.Paint;
begin
  inherited;
  fRenderUI.WriteLayer(Left,Top,Width,Height,FillColor,$00000000);
  fRenderUI.WriteRect(Left,Top,Width,Height,LineWidth,LineColor);
end;


{ TKMLabel }
constructor TKMLabel.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign{; aColor: TColor4=$FFFFFFFF});
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fFontColor := $FFFFFFFF;
  fTextAlign := aTextAlign;
  fAutoWrap := False;
  SetCaption(aCaption);
end;


//Same as above but with width/height ommitted, as in most cases we don't know/don't care
constructor TKMLabel.Create(aParent: TKMPanel; aLeft,aTop: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign);
begin
  inherited Create(aParent, aLeft,aTop,0,0);
  fFont := aFont;
  fFontColor := $FFFFFFFF;
  fTextAlign := aTextAlign;
  fAutoWrap := False;
  SetCaption(aCaption);
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


procedure TKMLabel.SetCaption(aCaption: string);
begin
  fCaption := aCaption;
  ReformatText;
end;


procedure TKMLabel.SetAutoWrap(aValue: Boolean);
begin
  fAutoWrap := aValue;
  ReformatText;
end;


//Override usual hittest with regard to text alignment
function TKMLabel.HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean;
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
var Col: Cardinal;
begin
  inherited;

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  fRenderUI.WriteText(Left, Top, Width, Height, fText, fFont, fTextAlign, Col);

  if fStrikethrough then
    fRenderUI.WriteLayer(TextLeft, Top + fTextSize.Y div 2 - 2, fTextSize.X, 3, Col, $FF000000);
end;


{ TKMLabelScroll }
constructor TKMLabelScroll.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aCaption, aFont, aTextAlign);
  SmoothScrollToTop := 0; //Disabled by default
end;


procedure TKMLabelScroll.Paint;
var NewTop: Integer; Col: Cardinal;
begin
  fRenderUI.SetupClipY(Top, Top+Height);
  NewTop := Top + Height - Integer(TimeGet - SmoothScrollToTop) div 50; //Compute delta and shift by it upwards (Credits page)

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  fRenderUI.WriteText(Left, NewTop, Width, Height, fCaption, fFont, fTextAlign, Col);
  fRenderUI.ReleaseClip;
end;


{ TKMImage }
constructor TKMImage.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fRX := aRX;
  fTexID := aTexID;
  fFlagColor := $FFFF00FF;
  ImageAnchors := [akLeft, akTop];
  Highlight := false;
  HighlightOnMouseOver := false;
end;


//DoClick is called by keyboard shortcuts
//It's important that Control must be:
// IsVisible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMImage.Click: Boolean;
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
  inherited;
  if fTexID = 0 then Exit; //No picture to draw

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
    OffsetX := fWidth - GFXData[fRX, fTexID].PxWidth
  else
    //No ImageAnchors means: draw the image in center
    OffsetX := (fWidth - GFXData[fRX, fTexID].PxWidth) div 2;

  if (akTop in ImageAnchors) and (akBottom in ImageAnchors) then
    StretchDraw := True
  else
  if akTop in ImageAnchors then
    //Use defaults
  else
  if akBottom in ImageAnchors then
    OffsetY := fHeight - GFXData[fRX, fTexID].PxHeight
  else
    OffsetY := (fHeight - GFXData[fRX, fTexID].PxHeight) div 2;

  if StretchDraw then
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, DrawWidth, DrawHeight, fRX, fTexID, fEnabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight)
  else
    fRenderUI.WritePicture(Left + OffsetX, Top + OffsetY, fRX, fTexID, fFlagColor, fEnabled, (HighlightOnMouseOver AND (csOver in State)) OR Highlight);
end;


{ TKMImageStack }
constructor TKMImageStack.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer; aTexID1, aTexID2: Word; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fRX  := aRX;
  fTexID1 := aTexID1;
  fTexID2 := aTexID2;
end;


procedure TKMImageStack.SetCount(aCount, aColumns, aHighlightID: Integer);
var Aspect: Single;
begin
  fCount := aCount;
  fColumns := Math.max(1, aColumns);
  fHighlightID := aHighlightID;

  fDrawWidth  := EnsureRange(Width div fColumns, 8, GFXData[fRX, fTexID1].PxWidth);
  fDrawHeight := EnsureRange(Height div ceil(fCount/fColumns), 6, GFXData[fRX, fTexID1].PxHeight);

  Aspect := GFXData[fRX, fTexID1].PxWidth / GFXData[fRX, fTexID1].PxHeight;
  if fDrawHeight * Aspect <= fDrawWidth then
    fDrawWidth  := round(fDrawHeight * Aspect)
  else
    fDrawHeight := round(fDrawWidth / Aspect);
end;


{If image area is bigger than image - do center image in it}
procedure TKMImageStack.Paint;
var
  i: Integer;
  OffsetX, OffsetY, CenterX, CenterY: Smallint; //variable parameters
begin
  inherited;
  if fTexID1 = 0 then Exit; //No picture to draw

  OffsetX := Width div fColumns;
  OffsetY := Height div Ceil(fCount / fColumns);

  CenterX := (Width - OffsetX * (fColumns-1) - fDrawWidth) div 2;
  CenterY := (Height - OffsetY * (Ceil(fCount/fColumns) - 1) - fDrawHeight) div 2;

  for i := 1 to fCount do
  if i <> fHighlightID then
    fRenderUI.WritePicture(Left + CenterX + OffsetX * ((i-1) mod fColumns),
                            Top + CenterY + OffsetY * ((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, fRX, fTexID1, fEnabled)
  else
    fRenderUI.WritePicture(Left + CenterX + OffsetX * ((i-1) mod fColumns),
                            Top + CenterY + OffsetY * ((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, fRX, fTexID2, fEnabled);
end;


{ TKMColorSwatch }
constructor TKMColorSwatch.Create(aParent: TKMPanel; aLeft,aTop,aColumnCount,aRowCount,aSize: Integer);
begin
  inherited Create(aParent, aLeft, aTop, 0, 0);

  fBackAlpha    := 0.5;
  fColumnCount  := aColumnCount;
  fRowCount     := aRowCount;
  fCellSize     := aSize;
  fInclRandom   := false;

  Width  := fColumnCount * fCellSize;
  Height := fRowCount * fCellSize;
end;


procedure TKMColorSwatch.SetColors(const aColors: array of TColor4; aInclRandom: Boolean = False);
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


function TKMColorSwatch.GetColor: TColor4;
begin
  Result := Colors[fColorIndex];
end;


procedure TKMColorSwatch.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var NewColor: Integer;
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
  inherited;
end;


procedure TKMColorSwatch.Paint;
var i,Start: Integer;
begin
  inherited;

  fRenderUI.WriteBevel(Left, Top, Width, Height, false, fBackAlpha);

  Start := 0;
  if fInclRandom then
  begin
    //Render miniature copy of all available colors with '?' on top
    for i:=0 to Length(Colors)-1 do
      fRenderUI.WriteLayer(Left+(i mod fColumnCount)*(fCellSize div fColumnCount)+2, Top+(i div fColumnCount)*(fCellSize div fColumnCount)+2, (fCellSize div fColumnCount), (fCellSize div fColumnCount), Colors[i], $00);
    fRenderUI.WriteText(Left + fCellSize div 2, Top + fCellSize div 4, 0, 0, '?', fnt_Metal, taCenter);
    Start := 1;
  end;

  for i:=Start to Length(Colors)-1 do
    fRenderUI.WriteLayer(Left+(i mod fColumnCount)*fCellSize, Top+(i div fColumnCount)*fCellSize, fCellSize, fCellSize, Colors[i], $00);

  //Paint selection
  fRenderUI.WriteLayer(Left+(fColorIndex mod fColumnCount)*fCellSize, Top+(fColorIndex div fColumnCount)*fCellSize, fCellSize, fCellSize, $00, $FFFFFFFF);
end;


{ TKMButton }
constructor TKMButton.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID: Word; aRX: TRXType = rxGui; aStyle: TButtonStyle = bsGame);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fRX         := aRX;
  fTexID      := aTexID;
  fCaption    := '';
  fFlagColor := $FFFF00FF;
  fStyle      := aStyle;
  fMakesSound := true;
end;


{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aStyle: TButtonStyle = bsGame);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fTexID      := 0;
  fCaption    := aCaption;
  fFlagColor := $FFFF00FF;
  fFont       := aFont;
  fTextAlign  := taCenter; //Thats default everywhere in KaM
  fStyle      := aStyle;
  fMakesSound := True;
end;


//DoClick is called by keyboard shortcuts
//It puts a focus on the button and depresses it if it was DoPress'ed
//It's important that Control must be:
// Visible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.Click: Boolean;
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


procedure TKMButton.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if fEnabled and fMakesSound and (csDown in State) then fSoundLib.Play(sfxn_ButtonClick);
  inherited;
end;


procedure TKMButton.Paint;
var
  Col: TColor4;
  StateSet: TButtonStateSet;
begin
  inherited;
  StateSet := [];
  if (csOver in State) and fEnabled then
    StateSet := StateSet + [bsOver];
  if (csDown in State) then
    StateSet := StateSet + [bsDown];
  if not fEnabled then
    StateSet := StateSet + [bsDisabled];

  fRenderUI.Write3DButton(Left, Top, Width, Height, fRX, fTexID, fFlagColor, StateSet, fStyle);

  if fTexID <> 0 then Exit;

  //If disabled then text should be faded
  if fEnabled then Col := $FFFFFFFF
              else Col := $FF888888;

  fRenderUI.WriteText(Left + Width div 2, (Top + Height div 2)-7, Width, 0, fCaption, fFont, fTextAlign, Col);
end;


{Simple version of button, with a caption and image}
constructor TKMButtonFlat.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID: Integer; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  RX    := aRX;
  TexID := aTexID;
  fFlagColor := $FFFF00FF;
  fFont := fnt_Grey;
  TextAlign := taLeft;
end;


procedure TKMButtonFlat.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if fEnabled and (csDown in State) then fSoundLib.Play(sfx_Click);
  inherited;
end;


procedure TKMButtonFlat.Paint;
var StateSet: TButtonStateSet;
begin
  inherited;
  StateSet:=[];
  if (csOver in State) and fEnabled and not HideHighlight then
    StateSet := StateSet+[bsOver];
  if Down then
    StateSet:=StateSet + [bsDown];
  //if not Enabled then StateSet:=StateSet+[fbs_Disabled];

  fRenderUI.WriteFlatButton(Left,Top,Width,Height,RX,TexID,fFlagColor,TexOffsetX,TexOffsetY,CapOffsetY,Caption,StateSet);
end;


{ TKMFlatButtonShape }
constructor TKMFlatButtonShape.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aShapeColor: TColor4);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fCaption    := aCaption;
  CapOffsetY  := 0;
  ShapeColor  := aShapeColor;
  fFont       := aFont;
end;


procedure TKMFlatButtonShape.Paint;
begin
  inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteLayer(Left+1,Top+1,Width-2,Width-2, ShapeColor, $00000000);
  fRenderUI.WriteText(Left+(Width div 2),Top+(Height div 2)+4+CapOffsetY, Width, 0, fCaption, fFont, taCenter);
  if (csOver in State) and fEnabled then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $40FFFFFF, $00000000);
  if (csDown in State) or Down then fRenderUI.WriteLayer(Left,Top,Width-1,Height-1, $00000000, $FFFFFFFF);
end;


{ TKMEdit }
constructor TKMEdit.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fText := '<<<LEER>>>';
  fFont := aFont;
  fAllowedChars := acText; //Set to the widest by default
  CursorPos := 0;
  Focusable := True;
end;


function TKMEdit.HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean;
begin
  //When control is read-only we don't want to recieve Focus event
  Result := inherited HitTest(X,Y) and not ReadOnly;
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


procedure TKMEdit.SetText(aText: string);
begin
  fText := aText;
  CursorPos := math.min(CursorPos, Length(fText));
  //Setting the text should place cursor to the end
  fLeftIndex := 0;
  SetCursorPos(Length(Text));
end;


//Key events which have no effect should not be handled (allows scrolling while chat window open with no text entered)
function TKMEdit.KeyEventHandled(Key: Word): Boolean;
begin
  Result := True;
  if fText = '' then
    case Key of
      VK_BACK,
      VK_DELETE,
      VK_UP,
      VK_DOWN,
      VK_LEFT,
      VK_RIGHT,
      VK_HOME,
      VK_END: Result := False; //These keys have no effect when text is blank
    end;
end;


function TKMEdit.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key);
  if inherited KeyDown(Key, Shift) or ReadOnly then exit;

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
const
  DigitChars: set of Char = ['1'..'9', '0'];
  NonFileChars: set of Char = [#0..#31, '<', '>', '|', '"', '\', '/', ':', '*', '?'];
  NonTextChars: set of Char = [#0..#31, '|'];
begin
  if ReadOnly then Exit;

  case fAllowedChars of
    acDigits:   if not (Key in DigitChars) then Exit;
    acFileName: if Key in NonFileChars then Exit;
    acText:     if Key in NonTextChars then Exit;
  end;

  Insert(Key, fText, CursorPos + 1);
  CursorPos := CursorPos + 1;
end;


function TKMEdit.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key);
  if inherited KeyUp(Key, Shift) or ReadOnly then exit;

  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMEdit.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if ReadOnly then Exit;
  inherited;
  CursorPos := length(fText);
end;


procedure TKMEdit.Paint;
var Col: TColor4; RText: string; OffX: Integer;
begin
  inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if Masked then
    RText := StringOfChar('*', Length(fText))
  else
    RText := fText;

  RText := Copy(RText, fLeftIndex+1, length(RText)); //Remove characters to the left of fLeftIndex

  fRenderUI.WriteText(Left+4, Top+3, Width-8, 0, RText, fFont, taLeft, Col, not ShowColors, True); //Characters that do not fit are trimmed

  //Render text cursor
  if (csFocus in State) and ((TimeGet div 500) mod 2 = 0) then
  begin
    SetLength(RText, CursorPos-fLeftIndex);
    OffX := Left + 2 + fResource.ResourceFont.GetTextSize(RText, fFont).X;
    fRenderUI.WriteLayer(OffX, Top+2, 3, Height-4, Col, $FF000000);
  end;
end;


{ TKMCheckBox }
constructor TKMCheckBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont     := aFont;
  fCaption  := aCaption;
end;


procedure TKMCheckBox.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if (csDown in State) and (Button = mbLeft) then
    fChecked := not fChecked;
  inherited; //There are OnMouseUp and OnClick events there
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMCheckBox.Paint;
var Col: TColor4;
begin
  inherited;
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
constructor TKMRadioGroup.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fItemIndex := -1;
  fItems := TStringList.Create;
end;


destructor TKMRadioGroup.Destroy;
begin
  fItems.Free;
  inherited;
end;


function TKMRadioGroup.GetItemCount: Integer;
begin
  Result := fItems.Count;
end;


procedure TKMRadioGroup.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var NewIndex: Integer;
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
  inherited; //There are OnMouseUp and OnClick events there
end;


//We can replace it with something better later on. For now [x] fits just fine
//Might need additional graphics to be added to gui.rx
//Some kind of box with an outline, darkened background and shadow maybe, similar to other controls.
procedure TKMRadioGroup.Paint;
var Col: TColor4; LineHeight: Integer; i: Integer;
begin
  inherited;
  if ItemCount = 0 then Exit; //Avoid dividing by zero
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
constructor TKMPercentBar.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont=fnt_Mini);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  FontColor := $FFFFFFFF;
  TextAlign := taCenter;
end;


procedure TKMPercentBar.Paint;
begin
  inherited;
  fRenderUI.WritePercentBar(Left,Top,Width,Height,Position);
  if Caption <> '' then begin //Now draw text over bar, if required
    //Shadow
    fRenderUI.WriteText((Left + Width div 2)+2, (Top + Height div 2)+TextYOffset-4, Width-4, 0, Caption, fFont, TextAlign, $FF000000);
    //Text
    fRenderUI.WriteText((Left + Width div 2)+1, (Top + Height div 2)+TextYOffset-5, Width-4, 0, Caption, fFont, TextAlign, FontColor);
  end;
end;


{ TKMResourceRow }
procedure TKMResourceRow.Paint;
var i: Integer;
begin
  inherited;
  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width-8, 0, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, RX, TexID, $00000000);
end;


{ TKMResourceOrderRow }
constructor TKMResourceOrderRow.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft+68,aTop,aWidth-68,aHeight);

  fOrderRem := TKMButton.Create(aParent,aLeft,aTop+2,20,aHeight,'-',fnt_Metal, bsGame);
  fOrderLab := TKMLabel.Create(aParent,aLeft+33,aTop+4,'',fnt_Grey,taCenter);
  fOrderAdd := TKMButton.Create(aParent,aLeft+46,aTop+2,20,aHeight,'+',fnt_Metal, bsGame);
end;


//Copy property to buttons
procedure TKMResourceOrderRow.SetEnabled(aValue: Boolean);
begin
  inherited;
  fOrderRem.Enabled := fEnabled;
  fOrderLab.Enabled := fEnabled;
  fOrderAdd.Enabled := fEnabled;
end;


//Copy property to buttons. Otherwise they won't be rendered
procedure TKMResourceOrderRow.SetVisible(aValue: Boolean);
begin
  inherited;
  fOrderRem.Visible := fVisible;
  fOrderLab.Visible := fVisible;
  fOrderAdd.Visible := fVisible;
end;


procedure TKMResourceOrderRow.Paint;
var i: Integer;
begin
  inherited;
  fOrderRem.Top := Round(fTop); //Use internal fTop instead of GetTop (which will return absolute value)
  fOrderLab.Top := Round(fTop + 4);
  fOrderAdd.Top := Round(fTop);

  fOrderLab.Caption := inttostr(OrderCount);

  fRenderUI.WriteBevel(Left,Top,Width,Height);
  fRenderUI.WriteText(Left + 4, Top + 3, Width - 8, 0, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for i:=1 to ResourceCount do
    fRenderUI.WritePicture((Left+Width-2-20)-(ResourceCount-i)*14, Top+1, RX, TexID, $00000000);
end;


{ TKMCostsRow }
procedure TKMCostsRow.Paint;
begin
  inherited;
  fRenderUI.WriteText(Left, Top + 4, Width-20, 0, Caption, fnt_Grey, taLeft, $FFFFFFFF);
  if TexID1 <> 0 then fRenderUI.WritePicture(Left+Width-40, Top + (Height-GFXData[RX,TexID1].PxHeight) div 2, RX, TexID1, $00000000);
  if TexID2 <> 0 then fRenderUI.WritePicture(Left+Width-20, Top + (Height-GFXData[RX,TexID2].PxHeight) div 2, RX, TexID2, $00000000);
end;


{ TKMTrackBar }
constructor TKMTrackBar.Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aMin, aMax: Word);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,0);
  fMinValue := aMin;
  fMaxValue := aMax;
  fTrackHeight := 20;
  Position := (fMinValue + fMaxValue) div 2;
  Caption := '';
  Step := 1;
end;


//Calculating it each time is not necessary, but doing it properly with setters is more hassle for no gain
function TKMTrackBar.ThumbWidth: Word;
begin
  //If the maximum allowed number of digits is more than 2 - use wider field to fit them
  Result := fResource.Sprites[rxGui].RXData.Size[132].X;
  if fMaxValue > 99 then
    Result := Round(Result * 1.5);
end;


procedure TKMTrackBar.SetCaption(const aValue: string);
begin
  fCaption := aValue;

  if Trim(fCaption) <> '' then
  begin
    fHeight := 20 + fTrackHeight;
    fTrackTop := 20;
  end
  else
  begin
    fHeight := fTrackHeight;
    fTrackTop := 0;
  end;
end;


procedure TKMTrackBar.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X, Y, Shift);
end;


procedure TKMTrackBar.MouseMove(X,Y: Integer; Shift: TShiftState);
var
  NewPos: Integer;
begin
  inherited;

  NewPos := Position;
  if (ssLeft in Shift) and InRange(Y - Top - fTrackTop, 0, fTrackHeight) then
    NewPos := EnsureRange(fMinValue + Round(((X-Left-ThumbWidth div 2) / (Width - ThumbWidth - 4))*(fMaxValue - fMinValue)/Step)*Step, fMinValue, fMaxValue);
  if NewPos <> Position then
  begin
    Position := NewPos;
    if Assigned(fOnChange) then
      fOnChange(Self);
  end
  else
    Position := NewPos;
end;


procedure TKMTrackBar.Paint;
const //Text color for disabled and enabled control
  TextColor: array [Boolean] of TColor4 = ($FF888888, $FFFFFFFF);
var
  ThumbPos, ThumbHeight: Word;
begin
  inherited;

  if fCaption <> '' then
    fRenderUI.WriteText(Left + 2, Top, Width - 8, 20, fCaption, fnt_Metal, taLeft, TextColor[fEnabled]);

  fRenderUI.WriteBevel(Left+2,Top+fTrackTop+2,Width-4,fTrackHeight-4);
  ThumbPos := Round(mix (0, Width - ThumbWidth - 4, 1-(Position-fMinValue) / (fMaxValue - fMinValue)));

  ThumbHeight := fResource.Sprites[rxGui].RXData.Size[132].Y;

  fRenderUI.WritePicture(Left + ThumbPos + 2, Top+fTrackTop, ThumbWidth, ThumbHeight, rxGui, 132, True);
  fRenderUI.WriteText(Left + ThumbPos + ThumbWidth div 2 + 2, Top+fTrackTop+3, 0, 0, IntToStr(Position), fnt_Metal, taCenter, TextColor[fEnabled]);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aScrollAxis: TScrollAxis; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fBackAlpha := 0.5;
  fScrollAxis := aScrollAxis;
  fMinValue := 0;
  fMaxValue := 10;
  fPosition := 0;
  fStyle    := aStyle;

  if aScrollAxis = sa_Vertical then begin
    fScrollDec := TKMButton.Create(aParent, aLeft, aTop, aWidth, aWidth, 4, rxGui, aStyle);
    fScrollInc := TKMButton.Create(aParent, aLeft, aTop+aHeight-aWidth, aWidth, aWidth, 5, rxGui, aStyle);
  end;
  if aScrollAxis = sa_Horizontal then begin
    fScrollDec := TKMButton.Create(aParent, aLeft, aTop, aHeight, aHeight, 2, rxGui, aStyle);
    fScrollInc := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 3, rxGui, aStyle);
  end;
  fScrollDec.OnClick := DecPosition;
  fScrollDec.OnMouseWheel := MouseWheel;
  fScrollInc.OnClick := IncPosition;
  fScrollInc.OnMouseWheel := MouseWheel;
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetHeight(aValue: Integer);
begin
  inherited;
  if fScrollAxis = sa_Vertical then
    fScrollInc.Top := Round(fTop+fHeight-fWidth);

  UpdateThumbSize; //Update Thumb size
end;


procedure TKMScrollBar.SetEnabled(aValue: Boolean);
begin
  inherited;
  fScrollDec.Enabled := aValue;
  fScrollInc.Enabled := aValue;
end;


//Copy property to child buttons. Otherwise they won't be rendered
procedure TKMScrollBar.SetVisible(aValue: Boolean);
begin
  inherited;
  fScrollDec.Visible := fVisible;
  fScrollInc.Visible := fVisible;
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


procedure TKMScrollBar.IncPosition(Sender: TObject);
begin
  SetPosition(fPosition + 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender: TObject);
begin
  SetPosition(fPosition - 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMScrollBar.UpdateThumbSize;
begin
  case fScrollAxis of
    sa_Vertical:   fThumb := Math.max(0, (Height-2*Width)) div 4;
    sa_Horizontal: fThumb := Math.max(0, (Width-2*Height)) div 4;
  end;
end;


procedure TKMScrollBar.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMScrollBar.MouseMove(X,Y: Integer; Shift: TShiftState);
var NewPos: Integer;
begin
  inherited;

  NewPos := fPosition;
  if (ssLeft in Shift) then
  begin

    if fScrollAxis = sa_Vertical then
      if InRange(Y,Top+Width,Top+Height-Width) then
        NewPos := Round(fMinValue+((Y-Top-Width-fThumb/2)/(Height-Width*2-fThumb)) * (fMaxValue - fMinValue) );

    if fScrollAxis = sa_Horizontal then
      if InRange(X,Left+Height,Left+Width-Height) then
        NewPos := Round(fMinValue+((X-Left-Height-fThumb/2)/(Width-Height*2-fThumb)) * (fMaxValue - fMinValue) );

    if NewPos <> fPosition then begin
      SetPosition(NewPos);
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;

  end;
end;


procedure TKMScrollBar.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  inherited;
  if WheelDelta < 0 then IncPosition(Self);
  if WheelDelta > 0 then DecPosition(Self);
end;


procedure TKMScrollBar.Paint;
var
  ThumbPos: Word;
  ButtonState: TButtonStateSet;
begin
  inherited;
  ThumbPos := 0;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.WriteBevel(Left, Top+Width, Width, Height - Width*2, false, fBackAlpha);
    sa_Horizontal: fRenderUI.WriteBevel(Left+Height, Top, Width - Height*2, Height, false, fBackAlpha);
  end;

  if fMaxValue > fMinValue then begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := (fPosition-fMinValue)*(Height-Width*2-fThumb) div (fMaxValue-fMinValue);
      sa_Horizontal: ThumbPos := (fPosition-fMinValue)*(Width-Height*2-fThumb) div (fMaxValue-fMinValue);
    end;
    ButtonState := [];
  end else begin
    case fScrollAxis of
      sa_Vertical:   ThumbPos := Math.max((Height-Width*2-fThumb),0) div 2;
      sa_Horizontal: ThumbPos := Math.max((Width-Height*2-fThumb),0) div 2;
    end;
    ButtonState := [bsDisabled];
  end;

  case fScrollAxis of
    sa_Vertical:   fRenderUI.Write3DButton(Left,Top+Width+ThumbPos,Width,fThumb,rxGui,0,$FFFF00FF,ButtonState,fStyle);
    sa_Horizontal: fRenderUI.Write3DButton(Left+Height+ThumbPos,Top,fThumb,Height,rxGui,0,$FFFF00FF,ButtonState,fStyle);
  end;
end;


{ TKMMemo }
constructor TKMMemo.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fItemHeight := 20;
  fItems := TStringList.Create;
  fFont := aFont;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar
end;


destructor TKMMemo.Destroy;
begin
  fItems.Free;
  inherited;
end;


procedure TKMMemo.SetHeight(aValue: Integer);
begin
  inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


procedure TKMMemo.SetWidth(aValue: Integer);
begin
  inherited;
  fScrollBar.Left := Round(fLeft + fWidth) - 20;
  ReformatText; //Repositions the scroll bar as well
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMMemo.SetVisible(aValue: Boolean);
begin
  inherited;

  //Hide scrollbar and its buttons
  fScrollBar.Visible := fVisible and (fScrollBar.MaxValue <> fScrollBar.MinValue);
end;


function TKMMemo.GetTopIndex: smallint;
begin
  Result := fScrollBar.Position;
end;


procedure TKMMemo.SetTopIndex(aIndex: smallint);
begin
  fScrollBar.Position := aIndex;
end;


procedure TKMMemo.SetEnabled(aValue: Boolean);
begin
  inherited;
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
var NewText: string;
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
var OldMax: Integer;
begin
  OldMax := fScrollBar.MaxValue;
  fScrollBar.MaxValue := fItems.Count - (fHeight div fItemHeight);
  fScrollBar.Visible := fVisible and (fScrollBar.MaxValue <> fScrollBar.MinValue);

  if fScrollDown then
  begin
    if OldMax-fScrollBar.Position <= 2 then //If they were near the bottom BEFORE updating, keep them at the bottom
      SetTopIndex(fItems.Count) //This puts it at the bottom because of the EnsureRange in SetTopIndex
  end
  else
    SetTopIndex(0);
end;


procedure TKMMemo.Add(const aItem: string);
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


procedure TKMMemo.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
end;


procedure TKMMemo.Paint;
var i,PaintWidth: Integer;
begin
  inherited;
  if fScrollBar.Visible then
    PaintWidth := Width - fScrollBar.Width //Leave space for scrollbar
  else
    PaintWidth := Width; //List takes up the entire width

  fRenderUI.WriteBevel(Left, Top, PaintWidth, Height, false, 0.5);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    fRenderUI.WriteText(Left+4, Top+i*fItemHeight+3, Width-8, 0, fItems.Strings[TopIndex+i] , fFont, taLeft);
end;


{ TKMListBox }
constructor TKMListBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fBackAlpha := 0.5;
  fItemHeight := 20;
  fItemIndex := -1;
  fItems := TStringList.Create;
  fFont := aFont;
  fAutoHideScrollBar := False; //Always show the scrollbar by default, then it can be turned off if required
  Focusable := True; //For up/down keys

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar
end;


destructor TKMListBox.Destroy;
begin
  fItems.Free;
  inherited;
end;


procedure TKMListBox.SetHeight(aValue: Integer);
begin
  inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMListBox.SetVisible(aValue: Boolean);
begin
  inherited;
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


procedure TKMListBox.SetBackAlpha(aValue: single);
begin
  fBackAlpha := aValue;
  fScrollBar.BackAlpha := aValue;
end;


procedure TKMListBox.SetEnabled(aValue: Boolean);
begin
  inherited;
  fScrollBar.Enabled := aValue;
end;


//fItems.Count has changed
procedure TKMListBox.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fItems.Count - (fHeight div fItemHeight);
  fScrollBar.Visible := fVisible and (not fAutoHideScrollBar or fScrollBar.Enabled);
end;


procedure TKMListBox.Add(aItem: string);
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


//When you want to reset ItemIndex do so separately
procedure TKMListBox.SetItems(aText: string);
begin
  fItems.Text := aText;
  UpdateScrollBar;
end;


//Hide the scrollbar if it is not required (disabled) This is used for drop boxes.
procedure TKMListBox.SetAutoHideScrollBar(Value: boolean);
begin
  fAutoHideScrollBar := Value;
  UpdateScrollBar;
end;


function TKMListBox.Count: Integer;
begin
  Result := fItems.Count;
end;


function TKMListBox.GetItem(aIndex: Integer): string;
begin
  Result := fItems[aIndex];
end;


function TKMListBox.KeyDown(Key: Word; Shift: TShiftState): Boolean;
var
  NewIndex: Integer;
begin
  Result := (Key = VK_UP) or (Key = VK_DOWN);
  if inherited KeyDown(Key, Shift) then Exit;

  case Key of
    VK_UP:    NewIndex := fItemIndex - 1;
    VK_DOWN:  NewIndex := fItemIndex + 1;
    else      Exit;
  end;

  if InRange(NewIndex, 0, Count - 1) then
  begin
    fItemIndex := NewIndex;
    if TopIndex < fItemIndex - (Height div fItemHeight) + 1 then //Moving down
      TopIndex := fItemIndex - (Height div fItemHeight) + 1
    else
    if TopIndex > fItemIndex then //Moving up
      TopIndex := fItemIndex;
  end;
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMListBox.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMListBox.MouseMove(X,Y: Integer; Shift: TShiftState);
var NewIndex: Integer;
begin
  inherited;

  if (ssLeft in Shift)
  and InRange(X, Left, Left + Width - (fScrollBar.Width*byte(fScrollBar.Visible)))
  and InRange(Y, Top, Top + Height div fItemHeight * fItemHeight)
  then begin
    NewIndex := TopIndex + (Y-Top) div fItemHeight;

    if NewIndex > fItems.Count - 1 then
    begin
      //Double clicking not allowed if we are clicking past the end of the list, but keep last item selected
      fTimeOfLastClick := 0;
      NewIndex := fItems.Count - 1;
    end;

    if (NewIndex<>fItemIndex) then begin
      fItemIndex := NewIndex;
      fTimeOfLastClick := 0; //Double click shouldn't happen if you click on one server A, then server B
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMListBox.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMListBox.Paint;
var i,PaintWidth: Integer;
begin
  inherited;
  if fScrollBar.Visible then
    PaintWidth := Width - fScrollBar.Width //Leave space for scrollbar
  else
    PaintWidth := Width; //List takes up the entire width

  fRenderUI.WriteBevel(Left, Top, PaintWidth, Height, false, fBackAlpha);

  if (fItemIndex <> -1) and InRange(fItemIndex - TopIndex, 0, (fHeight div fItemHeight)-1) then
    fRenderUI.WriteLayer(Left, Top+fItemHeight*(fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888, $FFFFFFFF);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    fRenderUI.WriteText(Left+4, Top+i*fItemHeight+3, PaintWidth-8, 0, fItems.Strings[TopIndex+i] , fFont, taLeft);
end;


{ TKMListHeader }
constructor TKMListHeader.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fSortDirection := sdNone;
  fSortIndex := -1;
end;


function TKMListHeader.GetColumnIndex(X: Integer): Integer;
var i: Integer;
begin
  Result := -1;

  for i := 0 to fCount - 1 do
    if X - Left > fColumnOffsets[i] then
      Result := i;
end;


function TKMListHeader.GetColumnOffset(aIndex: Integer): Word;
begin
  Assert(InRange(aIndex, 0, fCount - 1));
  Result := fColumnOffsets[aIndex];
end;


//We know we were clicked and now we can decide what to do
procedure TKMListHeader.DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var ColumnID: Integer;
begin
  ColumnID := GetColumnIndex(X);
  if (ColumnID <> -1) and Assigned(OnColumnClick) then
  begin
    //Apply sorting to the column, toggling the state, sdDown is default
    if fSortIndex = ColumnID then
      if fSortDirection = sdDown then
        fSortDirection := sdUp
      else
        fSortDirection := sdDown
    else
      fSortDirection := sdDown;
    fSortIndex := ColumnID;
    OnColumnClick(ColumnID);
  end
  else
    inherited; //Process the usual clicks if e.g. there are no columns
end;


procedure TKMListHeader.AddColumn(aCaption: string; aOffset: Word);
begin
  if fCount >= Length(fColumns) then
  begin
    SetLength(fColumns, fCount + 8);
    SetLength(fColumnOffsets, fCount + 8);
  end;

  fColumns[fCount] := aCaption;
  fColumnOffsets[fCount] := aOffset;

  inc(fCount);
end;


procedure TKMListHeader.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited;
  fColumnHighlight := GetColumnIndex(X);
end;


procedure TKMListHeader.Paint;
var
  I: Integer;
  ColumnWidth: Integer;
begin
  inherited;

  for I := 0 to fCount - 1 do
  begin
    if I < fCount - 1 then
      ColumnWidth := fColumnOffsets[I+1] - fColumnOffsets[I]
    else
      ColumnWidth := Width - fColumnOffsets[I];

    fRenderUI.WriteBevel(Left + fColumnOffsets[I], Top, ColumnWidth, Height, True, fBackAlpha);
    if Assigned(OnColumnClick) and (csOver in State) and (fColumnHighlight = I) then
      fRenderUI.WriteLayer(Left + fColumnOffsets[I], Top, ColumnWidth, Height, $20FFFFFF, $00000000);
    fRenderUI.WriteText(Left + fColumnOffsets[I] + 4, Top + 4, 0, 0, fColumns[I], fFont, taLeft);

    if fSortIndex = I then
      case fSortDirection of
        sdDown: fRenderUI.WritePicture(Left + fColumnOffsets[I] + ColumnWidth - 4-10, Top + 6, 10, 11, rxGui, 60, True);
        sdUp:   fRenderUI.WritePicture(Left + fColumnOffsets[I] + ColumnWidth - 4-10, Top + 6, 10, 11, rxGui, 59, True);
      end;
  end;
end;


{ TKMColumnListBox }
constructor TKMColumnListBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
const DEF_HEADER_HEIGHT = 24;
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fFont       := aFont;
  fItemHeight := 20;
  fItemIndex  := -1;
  fShowHeader := True;
  Focusable := True;

  fHeader := TKMListHeader.Create(aParent, aLeft, aTop, aWidth - fItemHeight, DEF_HEADER_HEIGHT);
  fHeader.OnColumnClick := ColumnClick;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-fItemHeight, aTop, fItemHeight, aHeight, sa_Vertical, bsGame);
  UpdateScrollBar; //Initialise the scrollbar

  SetBackAlpha(0.5);
end;


procedure TKMColumnListBox.SetTop(aValue: Integer);
begin
  inherited;
  fHeader.Top := aValue;
  fScrollBar.Top := aValue;
end;


procedure TKMColumnListBox.SetHeaderVisible(aValue: Boolean);
begin
  fHeader.Visible := aValue;
  fShowHeader := aValue;
end;


procedure TKMColumnListBox.SetHeight(aValue: Integer);
begin
  inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


procedure TKMColumnListBox.SetSortDirection(aDirection: TSortDirection);
begin
  fHeader.SortDirection := aDirection;
end;


procedure TKMColumnListBox.SetSortIndex(aIndex: Integer);
begin
  fHeader.SortIndex := aIndex;
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMColumnListBox.SetVisible(aValue: Boolean);
begin
  inherited;
  fHeader.Visible := fVisible and fShowHeader;
  fScrollBar.Visible := fVisible and (fScrollBar.Enabled); //Hide scrollbar and its buttons
end;


function TKMColumnListBox.GetTopIndex: Integer;
begin
  Result := fScrollBar.Position;
end;


function TKMColumnListBox.HeaderHeight: Integer;
begin
  Result := fHeader.Height;
end;


function TKMColumnListBox.GetVisibleRows: Integer;
begin
  Result := (fHeight - fHeader.Height * Byte(fShowHeader)) div fItemHeight;
end;


procedure TKMColumnListBox.SetTopIndex(aIndex: Integer);
begin
  fScrollBar.Position := aIndex;
end;


procedure TKMColumnListBox.SetBackAlpha(aValue: Single);
begin
  fBackAlpha := aValue;
  fHeader.BackAlpha := aValue;
  fScrollBar.BackAlpha := aValue;
end;


procedure TKMColumnListBox.SetEnabled(aValue: Boolean);
begin
  inherited;
  fHeader.Enabled := aValue;
  fScrollBar.Enabled := aValue;
end;


function TKMColumnListBox.GetRow(aIndex: Integer): TKMListRow;
begin
  Assert(InRange(aIndex, 0, fRowCount - 1));
  Result := fRows[aIndex];
end;


function TKMColumnListBox.GetSortDirection: TSortDirection;
begin
  Result := fHeader.SortDirection;
end;


function TKMColumnListBox.GetSortIndex: Integer;
begin
  Result := fHeader.SortIndex;
end;


//We could process the clicks here (i.e. do the sorting inplace)
//but there are various circumstances where plain string sorting will look wrong
//and the listbox just misses the knowledge to do it right:
//MP game status (sort by type), ping (sort 1>9), playercount (sort 9>1), dates (sort by TDateTime)
//Let the UI communicate to Game and do it right
procedure TKMColumnListBox.ColumnClick(aValue: Integer);
begin
  if Assigned(OnColumnClick) then
    OnColumnClick(aValue);
end;


//fRowCount or Height has changed
procedure TKMColumnListBox.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fRowCount - (fHeight - fHeader.Height * Byte(fShowHeader)) div fItemHeight;
  Assert(fScrollBar.MaxValue >= fScrollBar.MinValue);
  fScrollBar.Visible := fVisible and (fScrollBar.MaxValue <> fScrollBar.MinValue);
end;


//If we don't add columns there will be Assert on items add
procedure TKMColumnListBox.SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);
var I: Integer;
begin
  Assert(Length(aColumns) = Length(aColumnOffsets));

  Clear; //We don't want to conflict with already added rows elements

  fHeader.Font := aFont;

  for I := 0 to Length(aColumns) - 1 do
    fHeader.AddColumn(aColumns[I], aColumnOffsets[I]);
end;


procedure TKMColumnListBox.AddItem(aItem: TKMListRow);
begin
  Assert(fHeader.ColumnCount > 0);
  Assert(Length(aItem.Cells) = fHeader.ColumnCount);

  if fRowCount >= Length(fRows) then
    SetLength(fRows, fRowCount + 16);

  fRows[fRowCount] := aItem;

  Inc(fRowCount);
  UpdateScrollBar;
end;


procedure TKMColumnListBox.Clear;
begin
  fRowCount := 0;
  fItemIndex := -1;
  UpdateScrollBar;
end;


function TKMColumnListBox.KeyDown(Key: Word; Shift: TShiftState): Boolean;
var
  NewIndex: Integer;
begin
  Result := (Key = VK_UP) or (Key = VK_DOWN);
  if inherited KeyDown(Key, Shift) then Exit;

  case Key of
    VK_UP:    NewIndex := fItemIndex - 1;
    VK_DOWN:  NewIndex := fItemIndex + 1;
    else      Exit;
  end;

  if InRange(NewIndex, 0, fRowCount - 1) then
  begin
    fItemIndex := NewIndex;
    if TopIndex < fItemIndex - (Height div fItemHeight) + 1 then //Moving down
      TopIndex := fItemIndex - (Height div fItemHeight) + 1
    else
    if TopIndex > fItemIndex then //Moving up
      TopIndex := fItemIndex;
  end;
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TKMColumnListBox.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMColumnListBox.MouseMove(X,Y: Integer; Shift: TShiftState);
var NewIndex: Integer;
begin
  inherited;

  if (ssLeft in Shift)
  and (InRange(X, Left, Left + Width - fScrollBar.Width * Byte(fScrollBar.Visible)))
  then begin
    NewIndex := TopIndex + (Y - Top - fHeader.Height * Byte(fShowHeader)) div fItemHeight;

    if NewIndex >= fRowCount then
    begin
      //Double clicking not allowed if we are clicking past the end of the list, but keep last item selected
      fTimeOfLastClick := 0;
      NewIndex := -1;
    end;

    if InRange(NewIndex, 0, fRowCount - 1) and (NewIndex <> fItemIndex) then
    begin
      fItemIndex := NewIndex;
      fTimeOfLastClick := 0; //Double click shouldn't happen if you click on one server A, then server B
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;


procedure TKMColumnListBox.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  inherited;
  SetTopIndex(TopIndex - sign(WheelDelta));
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMColumnListBox.Paint;
var
  I, K, PaintWidth, ItemWidth, MaxItem, Y: Integer;
begin
  inherited;

  if fScrollBar.Visible then
    PaintWidth := Width - fScrollBar.Width //Leave space for scrollbar
  else
    PaintWidth := Width; //List takes up the entire width

  fHeader.Width := PaintWidth;

  Y := Top + fHeader.Height * Byte(fShowHeader);
  MaxItem := GetVisibleRows-1;

  fRenderUI.WriteBevel(Left, Y, PaintWidth, Height - fHeader.Height * Byte(fShowHeader), false, fBackAlpha);

  //Selected item highlight on background
  if (fItemIndex <> -1) and InRange(ItemIndex - TopIndex, 0, MaxItem) then
    fRenderUI.WriteLayer(Left, Y + fItemHeight * (fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888, $FFFFFFFF);

  for I := 0 to Math.min(fRowCount - 1, MaxItem) do
    for K := 0 to fHeader.ColumnCount - 1 do
    begin

      if K = fHeader.ColumnCount - 1 then
        ItemWidth := PaintWidth - 4 - fHeader.ColumnOffset[K] - 4
      else
        ItemWidth := fHeader.ColumnOffset[K+1] - fHeader.ColumnOffset[K] - 4;

      if fRows[TopIndex+I].Cells[K].Pic.ID <> 0 then
        fRenderUI.WritePicture(Left + 4 + fHeader.ColumnOffset[K],
                               Y + I*fItemHeight + 1,
                               fRows[TopIndex+I].Cells[K].Pic.RX,
                               fRows[TopIndex+I].Cells[K].Pic.ID,
                               fRows[TopIndex+I].Cells[K].Color);
      if fRows[TopIndex+I].Cells[K].Caption <> '' then
        fRenderUI.WriteText(Left + 4 + fHeader.ColumnOffset[K],
                            Y + I*fItemHeight + 3,
                            ItemWidth,
                            0,
                            fRows[TopIndex+I].Cells[K].Caption,
                            fFont,
                            taLeft,
                            fRows[TopIndex+I].Cells[K].Color);
    end;
end;


{ TKMDropCommon }
constructor TKMDropCommon.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fDropCount := 10;
  fDropUp := False;
  fFont := aFont;
  fOnClick := ListShow; //It's common behavior when click on dropbox will show the list

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, rxGui, bsMenu);
  fButton.fOnClick := ListShow;
  fButton.MakesSound := False;

  P := MasterParent;
  fShape := TKMShape.Create(P, 0, 0, P.Width, P.Height, $00000000);
  fShape.fOnClick := ListHide;
end;


procedure TKMDropCommon.ListShow(Sender: TObject);
begin
  if ListVisible then
  begin
    ListHide(nil);
    Exit;
  end;

  if Count > 0 then
    fShape.Show;
end;


procedure TKMDropCommon.ListClick(Sender: TObject);
begin
  if (ItemIndex <> -1) then
    if Assigned(fOnChange) then fOnChange(Self);
  ListHide(nil);
end;


procedure TKMDropCommon.ListHide(Sender: TObject);
begin
  fShape.Hide;
end;


procedure TKMDropCommon.SetEnabled(aValue: Boolean);
begin
  inherited;
  fButton.Enabled := aValue;
end;


procedure TKMDropCommon.SetVisible(aValue: Boolean);
begin
  inherited;
  fButton.Visible := aValue;
  if not aValue then ListHide(Self);
end;


procedure TKMDropCommon.Paint;
begin
  inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);

end;


{ TKMDropList }
constructor TKMDropList.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aFont);

  fDefaultCaption := aDefaultCaption;

  P := MasterParent;

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fList := TKMListBox.Create(P, Left-P.Left, Top+aHeight-P.Top, aWidth, 0, fFont);
  fList.Height := fList.ItemHeight * fDropCount;
  fList.AutoHideScrollBar := True; //A drop box should only have a scrollbar if required
  fList.BackAlpha := 0.85;
  fList.fOnClick := ListClick;
  fList.Focusable := False; //For drop downs we don't want the list to be focusable

  ListHide(nil);
end;


procedure TKMDropList.ListShow(Sender: TObject);
begin
  inherited;
  if ListVisible or (Count < 1) then Exit;

  UpdateDropPosition;
  fList.Show;
end;


procedure TKMDropList.ListClick(Sender: TObject);
begin
  fCaption := fList.Item[ItemIndex];

  inherited;
end;


procedure TKMDropList.ListHide(Sender: TObject);
begin
  inherited;
  fList.Hide;
end;


function TKMDropList.ListVisible: Boolean;
begin
  Result := fList.Visible;
end;


function TKMDropList.GetItemIndex: smallint;
begin
  Result := fList.ItemIndex;
end;


procedure TKMDropList.SetItemIndex(aIndex: smallint);
begin
  fList.ItemIndex := aIndex;
  if aIndex <> -1 then
    fCaption := fList.Item[fList.ItemIndex]
  else
    fCaption := fDefaultCaption;
end;


procedure TKMDropList.SetEnabled(aValue: Boolean);
begin
  inherited;
  fList.Enabled := aValue;
end;


procedure TKMDropList.SetVisible(aValue: Boolean);
begin
  inherited;
  if not aValue then ListHide(Self);
end;


function TKMDropList.Count: Integer;
begin
  Result := fList.Count;
end;


//When new items are added to the list we must update the drop height and position
procedure TKMDropList.UpdateDropPosition;
begin
  if (Count > 0) then
  begin
    fList.Height := Math.min(fDropCount, fList.Count)*fList.ItemHeight;
    if fDropUp then
      fList.Top := Top-fList.Height-MasterParent.Top
    else
      fList.Top := Top+Height-MasterParent.Top;
  end;
end;


procedure TKMDropList.Add(aItem: string);
begin
  fList.Add(aItem);
  UpdateDropPosition;
end;


procedure TKMDropList.SetItems(aText: string);
begin
  fList.SetItems(aText);
  UpdateDropPosition;
end;


procedure TKMDropList.SelectByName(aText: string);
var i: Integer;
begin
  fList.ItemIndex := -1;
  for i:=0 to fList.Count-1 do
    if fList.Item[i] = aText then
      SetItemIndex(i);
end;


function TKMDropList.GetItem(aIndex: Integer): string;
begin
  Result := fList.Item[aIndex];
end;


procedure TKMDropList.Clear;
begin
  fList.Clear;
end;


procedure TKMDropList.Paint;
var Col: TColor4;
begin
  inherited;

  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;
  fRenderUI.WriteText(Left+4, Top+4, Width-8, 0, fCaption, fFont, taLeft, Col);
end;


{ TKMDropColumns }
constructor TKMDropColumns.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aFont);

  P := MasterParent;

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fList := TKMColumnListBox.Create(P, Left-P.Left, Top+aHeight-P.Top, aWidth, 0, fFont);
  fList.BackAlpha := 0.85;
  fList.OnClick := ListClick;
  fList.Focusable := False; //For drop downs we don't want the list to be focusable

  ListHide(nil);
end;


procedure TKMDropColumns.ListShow(Sender: TObject);
begin
  inherited;
  if ListVisible or (Count < 1) then Exit;

  UpdateDropPosition;
  fList.Show;
end;


procedure TKMDropColumns.ListClick(Sender: TObject);
begin
  inherited;
end;


procedure TKMDropColumns.ListHide(Sender: TObject);
begin
  inherited;
  fList.Hide;
end;


function TKMDropColumns.ListVisible: Boolean;
begin
  Result := fList.Visible;
end;


function TKMDropColumns.GetItemIndex: smallint;
begin
  Result := fList.ItemIndex;
end;


procedure TKMDropColumns.SetItemIndex(aIndex: smallint);
begin
  fList.ItemIndex := aIndex;
end;


procedure TKMDropColumns.SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);
begin
  fList.SetColumns(aFont, aColumns, aColumnOffsets);
end;


procedure TKMDropColumns.SetEnabled(aValue: Boolean);
begin
  inherited;
  fList.Enabled := aValue;
end;


function TKMDropColumns.GetShowHeader: Boolean;
begin
  Result := fList.ShowHeader;
end;


procedure TKMDropColumns.SetShowHeader(aValue: Boolean);
begin
  fList.ShowHeader := aValue;
end;


procedure TKMDropColumns.SetVisible(aValue: Boolean);
begin
  inherited;
  if not aValue then ListHide(Self);
end;


function TKMDropColumns.Count: Integer;
begin
  Result := fList.RowCount;
end;


//When new items are added to the list we must update the drop height and position
procedure TKMDropColumns.UpdateDropPosition;
begin
  if (Count > 0) then
  begin
    fList.Height := Math.min(fDropCount, fList.RowCount) * fList.ItemHeight + fList.HeaderHeight * Byte(fList.ShowHeader);
    if fDropUp then
      fList.Top := Top - fList.Height - MasterParent.Top
    else
      fList.Top := Top + Height - MasterParent.Top;
  end;
end;


procedure TKMDropColumns.Add(aItem: TKMListRow);
begin
  fList.AddItem(aItem);
  UpdateDropPosition;
end;


function TKMDropColumns.GetItem(aIndex: Integer): TKMListRow;
begin
  Result := fList.Rows[aIndex];
end;


procedure TKMDropColumns.Clear;
begin
  fList.Clear;
end;


procedure TKMDropColumns.Paint;
var Col: TColor4;
begin
  inherited;
  fRenderUI.WriteBevel(Left, Top, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if ItemIndex <> -1 then
    fRenderUI.WritePicture(Left + 4, Top + 1,
                            fList.Rows[ItemIndex].Cells[0].Pic.RX,
                            fList.Rows[ItemIndex].Cells[0].Pic.ID,
                            fList.Rows[ItemIndex].Cells[0].Color,
                            fEnabled or not FadeImageWhenDisabled)
  else
    fRenderUI.WriteText(Left + 4, Top + 4, Width - 8, 0, '<<?>>', fFont, taLeft, Col);
end;


{ TKMDropColorBox }
constructor TKMDropColorBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aCount: Integer);
var P: TKMPanel; Size: Integer;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fColorIndex := 0;
  fRandomCaption := ''; //Disable random by default
  fOnClick := ListShow; //It's common behavior when click on dropbox will show the list

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 5, rxGui, bsMenu);
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


procedure TKMDropColorBox.ListShow(Sender: TObject);
begin
  if fSwatch.Visible then
  begin
    ListHide(nil);
    exit;
  end;

  fSwatch.Show;
  fShape.Show;
end;


procedure TKMDropColorBox.ListClick(Sender: TObject);
begin
  fColorIndex := fSwatch.ColorIndex;
  if Assigned(fOnChange) then fOnChange(Self);
  ListHide(nil);
end;


procedure TKMDropColorBox.ListHide(Sender: TObject);
begin
  fSwatch.Hide;
  fShape.Hide;
end;


procedure TKMDropColorBox.SetEnabled(aValue: Boolean);
begin
  inherited;
  fButton.Enabled := aValue;
  fSwatch.Enabled := aValue;
end;


//Set ColorIndex to fSwatch as well since it holds the actual color that we use on Paint
procedure TKMDropColorBox.SetColorIndex(aIndex: Integer);
begin
  fColorIndex := aIndex;
  fSwatch.ColorIndex := aIndex;
end;


procedure TKMDropColorBox.SetColors(const aColors: array of TColor4; aRandomCaption: string = '');
begin
  //Store local copy of flag to substitute 0 color with "Random" text
  fRandomCaption := aRandomCaption;
  fSwatch.SetColors(aColors, (fRandomCaption <> ''));
end;


procedure TKMDropColorBox.Paint;
var Col: TColor4;
begin
  inherited;
  fRenderUI.WriteBevel(Left, Top, Width-fButton.Width, Height);
  fRenderUI.WriteLayer(Left+2, Top+1, Width-fButton.Width-3, Height-2, fSwatch.GetColor, $00);
  if (fRandomCaption <> '') and (fSwatch.ColorIndex = 0) then
  begin
    if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;
    fRenderUI.WriteText(Left+4, Top+3, 0, 0, fRandomCaption, fnt_Metal, taLeft, Col);
  end;
end;


{ TKMMinimap }
constructor TKMMinimap.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
end;


function TKMMinimap.GetPlayerLoc(aIndex:byte):TKMPoint;
begin
  Result := fPlayerLocs[aIndex];
end;


procedure TKMMinimap.SetPlayerLoc(aIndex:byte; aLoc:TKMPoint);
begin
  fPlayerLocs[aIndex] := aLoc;
end;


procedure TKMMinimap.UpdateFrom(aMapView: TKMMapView);
var i: Integer;
begin
  fMapSize.X := aMapView.MapX;
  fMapSize.Y := aMapView.MapY;
  fMapTex := aMapView.MapTex;
  if fShowLocs then
  begin
    for i:=1 to MAX_PLAYERS do
    begin
      fPlayerLocs[i] := aMapView.GetPlayerLoc(i);
      fPlayerColors[i] := aMapView.PlayerColors[i];
    end;
  end
  else
    FillChar(fPlayerLocs, SizeOf(fPlayerLocs), #0);
end;


function TKMMinimap.LocalToMapCoords(X,Y: Integer; const Inset: shortint=0): TKMPoint;
var PaintWidth, PaintHeight, NewLeft, NewTop:integer;
begin
  if fMapSize.X > fMapSize.Y then
  begin
    PaintWidth := Width;
    PaintHeight := Round(Height*fMapSize.Y/Max(fMapSize.X,1)); //X could = 0
    NewLeft := Left;
    NewTop := Top + (Height - PaintHeight) div 2;
  end
  else
  begin
    PaintWidth := Round(Width*fMapSize.X/Max(fMapSize.Y,1)); //Y could = 0
    PaintHeight := Height;
    NewLeft := Left + (Width - PaintWidth) div 2;
    NewTop := Top;
  end;

  Assert(Inset>=-1, 'Min allowed inset is -1, to be within TKMPoint range of 0..n');
  Result.X := EnsureRange(Round((X - NewLeft)*fMapSize.X/PaintWidth), 1+Inset, fMapSize.X-Inset);
  Result.Y := EnsureRange(Round((Y - NewTop)*fMapSize.Y/PaintHeight), 1+Inset, fMapSize.Y-Inset);
end;


procedure TKMMinimap.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift);
end;


procedure TKMMinimap.MouseMove(X,Y: Integer; Shift: TShiftState);
var ViewPos: TKMPoint;
begin
  inherited;

  if ssLeft in Shift then
  begin
    ViewPos := LocalToMapCoords(X,Y);
    if Assigned(fOnChange) then
      fOnChange(Self, ViewPos.X, ViewPos.Y);
  end;
end;


procedure TKMMinimap.Paint;
var i, PaintWidth, PaintHeight, NewLeft, NewTop:integer;
const LOC_RAD = 8; //Radius of circle around player location
begin
  inherited;

  if fMapSize.X * fMapSize.Y = 0 then
  begin
    PaintWidth := 0;
    PaintHeight := 0;
    NewLeft := Left;
    NewTop := Top;
  end
  else
    if fMapSize.X > fMapSize.Y then
    begin
      PaintWidth := Width;
      PaintHeight := Round(Height*fMapSize.Y/Max(fMapSize.X,1)); //X could = 0
      NewLeft := Left;
      NewTop := Top + (Height - PaintHeight) div 2;
    end
    else
    begin
      PaintWidth := Round(Width*fMapSize.X/Max(fMapSize.Y,1)); //Y could = 0
      PaintHeight := Height;
      NewLeft := Left + (Width - PaintWidth) div 2;
      NewTop := Top;
    end;

  if fMapTex.Tex <> 0 then
    fRenderUI.WriteTexture(NewLeft, NewTop, PaintWidth, PaintHeight, fMapTex, $FFFFFFFF)
  else
    fRenderUI.WriteBevel(NewLeft, NewTop, PaintWidth, PaintHeight);

  if (fViewArea.Right - fViewArea.Left) * (fViewArea.Bottom - fViewArea.Top) > 0 then
    fRenderUI.WriteRect(NewLeft + Round(fViewArea.Left*PaintWidth / fMapSize.X),
                        NewTop  + Round(fViewArea.Top*PaintHeight / fMapSize.Y),
                        Round((fViewArea.Right - fViewArea.Left)*PaintWidth / fMapSize.X),
                        Round((fViewArea.Bottom - fViewArea.Top)*PaintHeight / fMapSize.Y), 1, $FFFFFFFF);
  //Draw all the circles, THEN all the numbers so the numbers are not covered by circles when they are close
  for i:=1 to MAX_PLAYERS do
    if not KMSamePoint(fPlayerLocs[i], KMPoint(0,0)) then
      fRenderUI.WriteCircle(NewLeft+EnsureRange(Round(fPlayerLocs[i].X*PaintWidth / fMapSize.X),LOC_RAD,PaintWidth-LOC_RAD),
                            NewTop +EnsureRange(Round(fPlayerLocs[i].Y*PaintHeight / fMapSize.Y),LOC_RAD,PaintHeight-LOC_RAD),
                            LOC_RAD, fPlayerColors[i]);
  for i:=1 to MAX_PLAYERS do
    if not KMSamePoint(fPlayerLocs[i], KMPoint(0,0)) then
      fRenderUI.WriteText(NewLeft+EnsureRange(Round(fPlayerLocs[i].X*PaintWidth / fMapSize.X),LOC_RAD,PaintWidth-LOC_RAD),
                          NewTop +EnsureRange(Round(fPlayerLocs[i].Y*PaintHeight / fMapSize.Y),LOC_RAD,PaintHeight-LOC_RAD)-6,
                          16, 16, IntToStr(i), fnt_Outline, taCenter);
end;


{ TKMDragger }
constructor TKMDragger.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

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


procedure TKMDragger.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  fStartDragX := X - fPositionX;
  fStartDragY := Y - fPositionY;

  MouseMove(X,Y,Shift);
end;


procedure TKMDragger.MouseMove(X,Y: Integer; Shift: TShiftState);
begin
  inherited;

  if csDown in State then
  begin
    //Bounds are signed numbers, set them properly
    fPositionX := EnsureRange((X - fStartDragX), fMinusX, fPlusX);
    fPositionY := EnsureRange((Y - fStartDragY), fMinusY, fPlusY);

    if Assigned(OnMove) then OnMove(Self, fPositionX, fPositionY);
  end;
end;


procedure TKMDragger.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift);
end;


procedure TKMDragger.Paint;
var
  StateSet: TButtonStateSet;
begin
  inherited;
  StateSet := [];
  if (csOver in State) and fEnabled then
    StateSet := StateSet + [bsOver];
  if (csDown in State) then
    StateSet := StateSet + [bsDown];
  if not fEnabled then
    StateSet := StateSet + [bsDisabled];

  fRenderUI.Write3DButton(Left, Top, Width, Height, rxGui, 0, $FFFF00FF, StateSet, bsGame);
end;


{ TKMGraph }
constructor TKMGraph.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fItemHeight := 13;
end;


procedure TKMGraph.AddLine(aTitle: string; aColor: TColor4; const aValues: array of Word);
var K: Integer;
begin
  Assert(Length(aValues) >= fMaxLength);

  SetLength(fLines, fCount + 1);

  fLines[fCount].Color := aColor;
  fLines[fCount].Title := aTitle;
  fLines[fCount].Visible := True;
  SetLength(fLines[fCount].Values, fMaxLength);
  if SizeOf(aValues) <> 0 then
    Move(aValues[0], fLines[fCount].Values[0], SizeOf(aValues[0]) * fMaxLength);

  for K := 0 to fMaxLength - 1 do
    if fLines[fCount].Values[K] > fMaxValue then
      fMaxValue := fLines[fCount].Values[K];

  Inc(fCount);
end;


procedure TKMGraph.Clear;
begin
  fCount := 0;
  SetLength(fLines, 0);
  fMaxValue := 0;
end;


procedure TKMGraph.UpdateMaxValue;
var I, K: Integer;
begin
  fMaxValue := 0;
  for I := 0 to fCount - 1 do
    if fLines[I].Visible then
      for K := 0 to fMaxLength - 1 do
        if fLines[I].Values[K] > fMaxValue then
          fMaxValue := fLines[I].Values[K];
end;


procedure TKMGraph.MouseUp(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
var I: Integer;
begin
  inherited;

  if X < Left + Width-55 then Exit;

  I := (Y - Top - 20) div fItemHeight;
  if not InRange(I, 0, fCount - 1) then Exit;

  fLines[I].Visible := not fLines[I].Visible;

  UpdateMaxValue;
end;


procedure TKMGraph.Paint;
const Intervals: array [0..9] of Word = (1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000);
var I, Best: Integer;
begin
  inherited;
  fRenderUI.WriteText(Left+Width div 2, Top, 0, 20, fCaption, fnt_Outline, taCenter);
  fRenderUI.WriteRect(Left+25, Top+20, Width-25-60, Height-20, 1, $FFFFFFFF);

  for I := 0 to fCount - 1 do
  begin
    if fLines[I].Visible then
      fRenderUI.WritePlot(Left+25, Top+20, Width-25-60, Height-20, fLines[I].Values, fMaxValue, fLines[I].Color);

    fRenderUI.WriteLayer(Left+Width-55, Top + 20 + I*fItemHeight+2, 11, 11, fLines[I].Color, $00000000);

    if fLines[I].Visible then
      fRenderUI.WriteText(Left+Width-55, Top + 20 + I*fItemHeight - 1, 0, 0, 'v', fnt_Game, taLeft);

    fRenderUI.WriteText(Left+Width-43, Top + 20 + I*fItemHeight, 0, 0, fLines[I].Title, fnt_Game, taLeft);
  end;

  fRenderUI.WriteText(Left+20, Top + Height, 0, 0, IntToStr(0), fnt_Game, taRight);
  //fRenderUI.WriteText(Left+20, Top + 20, 0, 0, IntToStr(fMaxValue), fnt_Game, taRight);

  Best := 0;
  for I := 0 to 9 do
    if fMaxValue div Intervals[I] < 10 then
    begin
      Best := Intervals[I];
      Break;
    end;

  if Best <> 0 then
  for I := 1 to (fMaxValue div Best) do
  begin
    fRenderUI.WriteText(Left+20, Top + Height - Round(I * Best / fMaxValue * (Height-20)) - 6, 0, 0, IntToStr(I * Best), fnt_Game, taRight);
    fRenderUI.WriteLayer(Left+22, Top + Height - Round(I * Best / fMaxValue * (Height-20)), 5, 2, $FFFFFFFF, $00000000);
  end;
end;


{ TKMMasterControl }
constructor TKMMasterControl.Create;
begin
  inherited;
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
  inherited;
end;


procedure TKMMasterControl.SetCtrlDown(aCtrl: TKMControl);
begin
  if fCtrlDown <> nil then fCtrlDown.State := fCtrlDown.State - [csDown]; //Release previous
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csDown];             //Press new
  fCtrlDown := aCtrl;                                                     //Update info
end;


procedure TKMMasterControl.SetCtrlFocus(aCtrl: TKMControl);
begin
  if fCtrlFocus <> nil then fCtrlFocus.State := fCtrlFocus.State - [csFocus];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csFocus];
  fCtrlFocus := aCtrl;
end;


procedure TKMMasterControl.SetCtrlOver(aCtrl: TKMControl);
begin
  if fCtrlOver <> nil then fCtrlOver.State := fCtrlOver.State - [csOver];
  if aCtrl <> nil then aCtrl.State := aCtrl.State + [csOver];
  fCtrlOver := aCtrl;
end;


procedure TKMMasterControl.SetCtrlUp(aCtrl: TKMControl);
begin
  fCtrlUp := aCtrl;
  //Give focus only to controls with Focusable=True
  if (fCtrlUp <> nil) and fCtrlUp.Focusable then
    if fCtrlDown = fCtrlUp then
      CtrlFocus := fCtrlUp
    else
      CtrlFocus := nil;
end;


{ Recursing function to find topmost control (excl. Panels)}
function TKMMasterControl.HitControl(X,Y: Integer; aIncludeDisabled: Boolean=false): TKMControl;
  function ScanChild(P: TKMPanel; aX,aY: Integer): TKMControl;
  var i: Integer;
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


function TKMMasterControl.KeyDown(Key: Word; Shift: TShiftState): Boolean;
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


function TKMMasterControl.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  if CtrlFocus <> nil then
    Result := CtrlFocus.KeyUp(Key, Shift)
  else
    Result := false;
end;


procedure TKMMasterControl.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  CtrlDown := HitControl(X,Y);
  if CtrlDown <> nil then CtrlDown.MouseDown(X,Y,Shift,Button);
end;


procedure TKMMasterControl.MouseMove(X,Y: Integer; Shift: TShiftState);
var HintControl: TKMControl;
begin
  if CtrlDown = nil then CtrlOver := HitControl(X,Y); //User is dragging some Ctrl (e.g. scrollbar) and went away from Ctrl bounds
  if CtrlOver <> nil then
    CtrlOver.MouseMove(X,Y,Shift);

  //The Game hides cursor when using DirectionSelector, don't spoil it
  if fResource.Cursors.Cursor <> kmc_Invisible then
    if CtrlOver is TKMEdit then
      fResource.Cursors.Cursor := kmc_Edit
    else
    if CtrlOver is TKMDragger then
      fResource.Cursors.Cursor := kmc_DragUp
    else
      if fResource.Cursors.Cursor in [kmc_Edit,kmc_DragUp] then
        fResource.Cursors.Cursor := kmc_Default; //Reset the cursor from these two special cursors

  HintControl := HitControl(X,Y,true); //Include disabled controls
  if (HintControl <> nil) and Assigned(fOnHint) then fOnHint(HintControl);
  if (CtrlDown <> nil) and (CtrlOver <> CtrlDown) then CtrlDown := nil;
end;


procedure TKMMasterControl.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
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


procedure TKMMasterControl.MouseWheel(X,Y: Integer; WheelDelta: Integer);
var C: TKMControl;
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
    fRenderUI.WriteText(CtrlFocus.Left, CtrlFocus.Top-14, 0, 0, inttostr(CtrlFocus.Left)+':'+inttostr(CtrlFocus.Top), fnt_Grey, taLeft);
end;


procedure TKMMasterControl.SaveToFile(aFileName: string);
var ft: Textfile;
begin
  AssignFile(ft,aFileName);
  Rewrite(ft);

  //fCtrl.SaveToFile; //Will save all the childs as well, recursively alike Paint or HitControl
  //writeln(ft, ClassName);
  //writeln(ft, Format('[%d %d %d %d]', [fLeft, fTop, fWidth, fHeight]));

  CloseFile(ft);
end;


end.
