unit KM_Controls;
{$I KaM_Remake.inc}
interface
uses
    {$IFDEF MSWindows} Windows, {$ENDIF}
    {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
    Classes, Controls,  Math, SysUtils, StrUtils, Clipbrd,
    KromUtils, KromOGLUtils, KM_Defaults, KM_Points, KM_CommonTypes, KM_Pics,
    KM_RenderUI, KM_ResourceSprites, KM_Minimap, KM_Viewport;

type
  TNotifyEventMB = procedure(Sender: TObject; AButton: TMouseButton) of object;
  TNotifyEventMW = procedure(Sender: TObject; WheelDelta: Integer) of object;
  TNotifyEventKey = procedure(Sender: TObject; Key: Word) of object;
  TNotifyEventXY = procedure(Sender: TObject; X, Y: Integer) of object;

  TKMControlState = (csDown, csFocus, csOver);
  TKMControlStateSet = set of TKMControlState;

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

    property MainPanel: TKMPanel read fCtrl;
    procedure UpdateFocus(aSender: TKMControl);

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
    //even if the Parent resized by 1px. Otherwise error quickly accumulates on
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

    function GetAbsLeft: Integer;
    function GetAbsTop: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;

    //Let the control know that it was clicked to do its internal magic
    procedure DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); virtual;

    function GetVisible: Boolean;
    procedure SetAbsLeft(aValue: Integer);
    procedure SetAbsTop(aValue: Integer);
    procedure SetTopF(aValue: Single);
    procedure SetLeftF(aValue: Single);
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
    DrawOutline: Boolean;
    OutlineColor: Cardinal;

    Tag: Integer; //Some tag which can be used for various needs
    Hint: string; //Text that shows up when cursor is over that control, mainly for Buttons
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
    function HitTest(X, Y: Integer; aIncludeDisabled: Boolean=false): Boolean; virtual;

    property Parent: TKMPanel read fParent;
    property AbsLeft: Integer read GetAbsLeft write SetAbsLeft;
    property AbsTop: Integer read GetAbsTop write SetAbsTop;
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
    //Do not propogate SetEnabled and SetVisible because that would show/enable ALL child controls
    //e.g. scrollbar on a listbox
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
    EdgeAlpha: Single;
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
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
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
    property Font: TKMFont read fFont write fFont;
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
    Lightness: Single;
    ClipToBounds: Boolean;
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
    fTextAlign: TTextAlign;
    fStyle: TButtonStyle;
    fRX: TRXType;
  public
    Caption: string;
    FlagColor: TColor4; //When using an image
    Font: TKMFont;
    MakesSound: Boolean;
    TexID: Word;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID: Word; aRX: TRXType; aStyle: TButtonStyle); overload;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aStyle: TButtonStyle); overload;
    function Click: Boolean; //Try to click a button and return TRUE if succeded
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  {FlatButton}
  TKMButtonFlat = class(TKMControl)
  private
  public
    RX: TRXType;
    TexID: Word;
    TexOffsetX: Shortint;
    TexOffsetY: Shortint;
    CapOffsetY: Shortint;
    Caption: string;
    Down: Boolean;
    FlagColor: TColor4;
    Font: TKMFont;
    HideHighlight: Boolean;
    Clickable: Boolean; //Disables clicking without dimming

    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID: Integer; aRX: TRXType = rxGui);

    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;


  {FlatButton with Shape on it}
  TKMFlatButtonShape = class(TKMControl)
  private
    fCaption: string;
    fFont: TKMFont;
    fFontHeight: Byte;
  public
    Down: Boolean;
    FontColor: TColor4;
    ShapeColor: TColor4;
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
    procedure ValidateText;
    function KeyEventHandled(Key: Word; Shift: TShiftState): Boolean;
  public
    Masked: Boolean; //Mask entered text as *s
    ReadOnly: Boolean;
    ShowColors: Boolean;
    MaxLen: Word;
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
    fFont: TKMFont;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont); overload;
    property Caption: string read fCaption write fCaption;
    property Checked: Boolean read fChecked write fChecked;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  { TKMRadioGroup }
  TKMRadioGroup = class(TKMControl)
  private
    fItemIndex: Integer;
    fCount: Integer;
    fItems: array of record
      Text: string;
      Enabled: Boolean;
    end;
    fFont: TKMFont;
    fOnChange: TNotifyEvent;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);

    procedure Add(aText: string; aEnabled: Boolean = True);
    procedure Clear;
    property Count: Integer read fCount;
    property ItemIndex: Integer read fItemIndex write fItemIndex;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure Paint; override;
  end;


  {Percent bar}
  TKMPercentBar = class(TKMControl)
  private
    fFont: TKMFont;
    fPosition: Single;
    fTextAlign: TTextAlign;
    fSeam: Single;
    procedure SetPosition(aValue: Single);
    procedure SetSeam(aValue: Single);
  public
    Caption: string;
    FontColor: TColor4;
    TextYOffset: Integer;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont = fnt_Mini);
    property Seam: Single read fSeam write SetSeam;
    property Position: Single read fPosition write SetPosition;
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


  TKMNumericEdit = class(TKMControl)
  private
    fButtonInc: TKMButton;
    fButtonDec: TKMButton;
    fFont: TKMFont;
    fText: string;
    fValue: Integer;
    fCursorPos: Integer;
    fLeftIndex: Integer; //The position of the character shown left-most when text does not fit
    procedure ButtonClick(Sender: TObject; AButton: TMouseButton);
    procedure SetCursorPos(aPos: Integer);
    function MaxLength: Byte;
    procedure SetValue(aValue: Integer);
    procedure ValidateText;
    function KeyEventHandled(Key: Word; Shift: TShiftState): Boolean;
    procedure SetSharedHint(aHint: string);
  protected
    procedure SetLeft(aValue: Integer); override;
    procedure SetTop(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    ValueMin: Integer;
    ValueMax: Integer;
    OnChange: TNotifyEvent;
    constructor Create(aParent: TKMPanel; aLeft,aTop: Integer; aValueMin, aValueMax: SmallInt);
    property CursorPos: Integer read fCursorPos write SetCursorPos;
    property Value: Integer read fValue write SetValue;
    property SharedHint: string read Hint write SetSharedHint;
    function KeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure KeyPress(Key: Char); override;
    function KeyUp(Key: Word; Shift: TShiftState): Boolean; override;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
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


  TKMTrackBar = class(TKMControl)
  private
    fTrackTop: Byte; //Offset trackbar from top (if Caption <> '')
    fTrackHeight: Byte; //Trackbar height
    fMinValue: Word;
    fMaxValue: Word;
    fOnChange: TNotifyEvent;
    fCaption: string;
    fPosition: Word;
    fFont: TKMFont;
    procedure SetCaption(const aValue: string);
    procedure SetPosition(aValue: Word);
  public
    Step: Byte; //Change Position by this amount each time
    ThumbText: string;
    ThumbWidth: Word;

    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth: Integer; aMin, aMax: Word);

    property Caption: string read fCaption write SetCaption;
    property Position: Word read fPosition write SetPosition;
    property Font: TKMFont read fFont write fFont;
    property MinValue: Word read fMinValue;
    property MaxValue: Word read fMaxValue;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;


  TScrollAxis = (sa_Vertical, sa_Horizontal);

  TKMScrollBar = class(TKMPanel)
  private
    fScrollAxis: TScrollAxis;
    fStyle: TButtonStyle;
    fMinValue: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fThumbPos: Integer; //Position of the thumb
    fThumbSize: Word; //Length of the thumb
    fOffset: Integer;
    fScrollDec: TKMButton;
    fScrollInc: TKMButton;
    fOnChange: TNotifyEvent;
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure IncPosition(Sender: TObject);
    procedure DecPosition(Sender: TObject);
    procedure UpdateThumbPos;
    procedure UpdateThumbSize;
  protected
    procedure SetHeight(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
  public
    BackAlpha: Single; //Alpha of background (usually 0.5, dropbox 1)
    EdgeAlpha: Single; //Alpha of background outline (usually 1)

    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aScrollAxis: TScrollAxis; aStyle: TButtonStyle);
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
    ItemTags: array of Integer;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
    destructor Destroy; override;

    property AutoHideScrollBar: boolean read fAutoHideScrollBar write SetAutoHideScrollBar;
    property BackAlpha: Single write SetBackAlpha;

    procedure Add(aItem: string; aTag: Integer=0);
    procedure Clear;
    function Count: Integer;

    property Item[aIndex: Integer]: string read GetItem; default;
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

  TKMListHeaderColumn = class
    Caption: string;
    Glyph: TKMPic;
    Offset: Word; //Offsets are easier to handle than widths
  end;

  TKMListHeader = class (TKMControl)
  private
    fFont: TKMFont;
    fCount: Integer;
    fColumns: array of TKMListHeaderColumn;
    fColumnHighlight: Integer;
    fSortIndex: Integer;
    fSortDirection: TSortDirection;
    fTextAlign: TTextAlign;
    function GetColumnIndex(X: Integer): Integer;
    function GetColumn(aIndex: Integer): TKMListHeaderColumn;
    procedure ClearColumns;
    procedure DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
  public
    BackAlpha: Single; //Alpha of background
    EdgeAlpha: Single; //Alpha of background outline

    OnColumnClick: TIntegerEvent;
    constructor Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
    destructor Destroy; override;

    procedure SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);

    property Font: TKMFont read fFont write fFont;
    property ColumnCount: Integer read fCount;
    property Columns[aIndex: Integer]: TKMListHeaderColumn read GetColumn;
    property SortIndex: Integer read fSortIndex write fSortIndex;
    property SortDirection: TSortDirection read fSortDirection write fSortDirection;
    property TextAlign: TTextAlign read fTextAlign write fTextAlign;

    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;

  TKMListRow = record
    Cells: array of record
      Caption: string;
      Hint: string;
      Color: TColor4;
      HighlightColor: TColor4;
      Pic: TKMPic;
    end;
    Tag: Integer;
  end;

  TKMListColumn = class
    Font: TKMFont;
    HintFont: TKMFont;
    TextAlign: TTextAlign;
  end;

  TKMColumnBox = class(TKMControl)
  private
    fFont: TKMFont;
    fBackAlpha: Single; //Alpha of background
    fEdgeAlpha: Single; //Alpha of outline
    fItemHeight: Byte;
    fItemIndex: Smallint;
    fSearchColumn: ShortInt; //which columns text we should search, -1 for disabled
    fSearch: string; //Contains user input characters we should search for
    fLastKeyTime: Cardinal;
    fRowCount: Integer;
    fColumns: array of TKMListColumn;
    fHeader: TKMListHeader;
    fShowHeader: Boolean;
    fShowLines: Boolean;
    fMouseOverRow: Smallint;
    fScrollBar: TKMScrollBar;
    fOnChange: TNotifyEvent;
    function GetTopIndex: Integer;
    procedure SetTopIndex(aIndex: Integer);
    procedure SetBackAlpha(aValue: single);
    procedure SetEdgeAlpha(aValue: Single);
    function GetSortIndex: Integer;
    procedure SetSortIndex(aIndex: Integer);
    function GetSortDirection: TSortDirection;
    procedure SetSortDirection(aDirection: TSortDirection);
    procedure UpdateScrollBar;
    procedure SetShowHeader(aValue: Boolean);
    function GetOnColumnClick: TIntegerEvent;
    procedure SetOnColumnClick(const Value: TIntegerEvent);
    function GetColumn(aIndex: Integer): TKMListColumn;
    procedure ClearColumns;
    procedure SetSearchColumn(aValue: ShortInt);
    procedure SetItemIndex(const Value: Smallint);
  protected
    procedure SetLeft(aValue: Integer); override;
    procedure SetTop(aValue: Integer); override;
    procedure SetWidth(aValue: Integer); override;
    procedure SetHeight(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
    procedure DoPaintLine(aIndex: Integer; X,Y: Integer; PaintWidth: Integer);
  public
    HideSelection: Boolean;
    HighlightOnMouseOver: Boolean;
    Rows: array of TKMListRow; //Exposed to public since we need to edit sub-fields

    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
    destructor Destroy; override;

    procedure SetColumns(aHeaderFont: TKMFont; aCaptions: array of string; aOffsets: array of Word);
    procedure AddItem(aItem: TKMListRow);
    procedure Clear;
    function GetVisibleRows: Integer;
    function GetVisibleRowsExact: Single;
    property ShowHeader: Boolean read fShowHeader write SetShowHeader;
    property ShowLines: Boolean read fShowLines write fShowLines;
    property SearchColumn: ShortInt read fSearchColumn write SetSearchColumn;

    property Columns[aIndex: Integer]: TKMListColumn read GetColumn;
    property BackAlpha: Single read fBackAlpha write SetBackAlpha;
    property EdgeAlpha: Single read fEdgeAlpha write SetEdgeAlpha;
    property RowCount: Integer read fRowCount;
    property ItemHeight: Byte read fItemHeight write fItemHeight;
    property ItemIndex: Smallint read fItemIndex write SetItemIndex;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property Header: TKMListHeader read fHeader;

    //Sort properties are just hints to render Up/Down arrows. Actual sorting is done by client
    property OnColumnClick: TIntegerEvent read GetOnColumnClick write SetOnColumnClick;
    property SortIndex: Integer read GetSortIndex write SetSortIndex;
    property SortDirection: TSortDirection read GetSortDirection write SetSortDirection;

    function KeyDown(Key: Word; Shift: TShiftState): Boolean; override;
    procedure KeyPress(Key: Char); override;
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
    procedure ButtonClick(Sender: TObject);
    procedure ListShow(Sender: TObject); virtual;
    procedure ListClick(Sender: TObject); virtual;
    procedure ListChange(Sender: TObject); virtual;
    procedure ListHide(Sender: TObject); virtual;
    function ListVisible: Boolean; virtual; abstract;
    function GetItemIndex: SmallInt; virtual; abstract;
    procedure SetItemIndex(aIndex: SmallInt); virtual; abstract;
  protected
    procedure DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure SetTop(aValue: Integer); override;
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);

    procedure Clear; virtual; abstract;
    function Count: Integer; virtual; abstract;

    property DropCount: Byte read fDropCount write fDropCount;
    property DropUp: Boolean read fDropUp write fDropUp;
    property ItemIndex: SmallInt read GetItemIndex write SetItemIndex;

    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Paint; override;
  end;


  //DropBox with a ListBox
  TKMDropList = class(TKMDropCommon)
  private
    fCaption: string; //Current caption (Default or from list)
    fDefaultCaption: string;
    fList: TKMListBox;
    procedure UpdateDropPosition; override;
    procedure ListShow(Sender: TObject); override;
    procedure ListClick(Sender: TObject); override;
    procedure ListChange(Sender: TObject); override;
    procedure ListHide(Sender: TObject); override;
    function ListVisible: Boolean; override;
    function GetItem(aIndex: Integer): string;
    function GetItemIndex: smallint; override;
    procedure SetItemIndex(aIndex: smallint); override;
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string; aStyle: TButtonStyle);
    procedure Clear; override;
    function Count: Integer; override;
    procedure Add(aItem: string; aTag: Integer=0);
    procedure SelectByName(aText: string);
    procedure SelectByTag(aTag: Integer);
    function GetTag(aIndex: Integer): Integer;
    function GetSelectedTag: Integer;
    property DefaultCaption: string read fDefaultCaption write fDefaultCaption;
    property Item[aIndex: Integer]: string read GetItem;

    procedure Paint; override;
  end;


  //DropBox with a ColumnBox
  TKMDropColumns = class(TKMDropCommon)
  private
    fDefaultCaption: string;
    fDropWidth: Integer;
    fList: TKMColumnBox;
    procedure UpdateDropPosition; override;
    procedure ListShow(Sender: TObject); override;
    procedure ListClick(Sender: TObject); override;
    procedure ListChange(Sender: TObject); override;
    procedure ListHide(Sender: TObject); override;
    function ListVisible: Boolean; override;
    function GetItem(aIndex: Integer): TKMListRow;
    function GetItemIndex: smallint; override;
    procedure SetItemIndex(aIndex: smallint); override;
    procedure SetDropWidth(aDropWidth:Integer);
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    FadeImageWhenDisabled: Boolean;
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string; aStyle: TButtonStyle);
    procedure Add(aItem: TKMListRow);
    procedure Clear; override;
    function Count: Integer; override;
    property List: TKMColumnBox read fList;
    property Item[aIndex: Integer]: TKMListRow read GetItem;
    procedure SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);
    property DefaultCaption: string read fDefaultCaption write fDefaultCaption;
    property DropWidth: Integer read fDropWidth write SetDropWidth;

    procedure Paint; override;
  end;


  //DropBox with a ColorSwatch
  TKMDropColors = class(TKMControl)
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
    fIndentAfterNL: Boolean;
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
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
    destructor Destroy; override;

    procedure Add(const aItem: string);
    procedure Clear;
    procedure ScrollToBottom;
    property AutoWrap: Boolean read fAutoWrap write SetAutoWrap; //Whether to automatically wrap text within given text area width
    property IndentAfterNL: Boolean read fIndentAfterNL write fIndentAfterNL;
    property Text: string read GetText write SetText;
    property ItemHeight: Byte read fItemHeight write fItemHeight;
    property TopIndex: Smallint read GetTopIndex write SetTopIndex;
    property ScrollDown: Boolean read fScrollDown write fScrollDown;

    procedure MouseWheel(Sender: TObject; WheelDelta: Integer); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    procedure Paint; override;
  end;


  TKMMenu = class(TKMPanel)
  private
    fShapeBG: TKMShape;
    fList: TKMColumnBox;
    procedure MenuHide(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure SetItemIndex(aValue: Integer);
    function GetItemIndex: Integer;
    function GetItemTag(aIndex: Integer): Integer;
  public
    constructor Create(aParent: TKMPanel; aWidth: Integer);
    procedure AddItem(aCaption: string; aTag: Integer = 0);
    procedure Clear;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property ItemTags[aIndex: Integer]: Integer read GetItemTag;
    procedure ShowAt(X,Y: Integer);
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

  TKMGraphLine = record
                Title: string;
                Tag: Integer;
                Color: TColor4;
                Visible: Boolean;
                Values: TKMCardinalArray;
                ValuesAlt: TKMCardinalArray;
              end;

  TKMChart = class(TKMControl)
  private
    fCaption: string;
    fFont: TKMFont;
    fCount: Integer;
    fItemHeight: Byte;
    fLegendWidth: Word;
    fLineOver: Integer;
    fLines: array of TKMGraphLine;
    fMaxLength: Cardinal; //Maximum samples (by horizontal axis)
    fMinTime: Cardinal; //Minimum time (in sec), used only for Rendering time ticks
    fMaxTime: Cardinal; //Maximum time (in sec), used only for Rendering time ticks
    fMaxValue: Cardinal; //Maximum value (by vertical axis)
    procedure UpdateMaxValue;
    function GetLine(aIndex:Integer):TKMGraphLine;
  public
    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);

    procedure AddLine(aTitle: string; aColor: TColor4; const aValues: TKMCardinalArray; aTag:Integer=-1);
    procedure AddAltLine(const aAltValues: TKMCardinalArray);
    procedure TrimToFirstVariation;
    property Caption: string read fCaption write fCaption;
    procedure Clear;
    procedure SetLineVisible(aLineID:Integer; aVisible:Boolean);
    property MaxLength: Cardinal read fMaxLength write fMaxLength;
    property MaxTime: Cardinal read fMaxTime write fMaxTime;
    property Lines[aIndex: Integer]: TKMGraphLine read GetLine;
    property LineCount:Integer read fCount;
    property Font: TKMFont read fFont write fFont;

    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;

    procedure Paint; override;
  end;

  //MinimapView relies on fMinimap and fViewport that provide all the data
  //MinimapView itself is just a painter
  TKMMinimapView = class(TKMControl)
  private
    fMinimap: TKMMinimap;
    fView: TViewport;
    fPaintWidth: Integer;
    fPaintHeight: Integer;
    fLeftOffset: Integer;
    fTopOffset: Integer;

    fOnChange, fOnMinimapClick: TPointEvent;
    fShowLocs: Boolean;
    fLocRad: Byte;
    fClickableOnce: Boolean;
  public
    OnLocClick: TIntegerEvent;

    constructor Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);

    function LocalToMapCoords(X,Y: Integer; const Inset: ShortInt = 0): TKMPoint;
    function MapCoordsToLocal(X,Y: Single; const Inset: ShortInt = 0): TKMPoint;
    procedure SetMinimap(aMinimap: TKMMinimap);
    procedure SetViewport(aViewport: TViewport);
    property ShowLocs: Boolean read fShowLocs write fShowLocs;
    property ClickableOnce: Boolean read fClickableOnce write fClickableOnce;
    property OnChange: TPointEvent write fOnChange;
    property OnMinimapClick: TPointEvent read fOnMinimapClick write fOnMinimapClick;

    procedure MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton); override;
    procedure MouseMove(X,Y: Integer; Shift: TShiftState); override;
    procedure Paint; override;
  end;


  function MakeListRow(const aCaption: array of string; aTag: Integer = 0): TKMListRow; overload;
  function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; aTag: Integer = 0): TKMListRow; overload;
  function MakeListRow(const aCaption: array of string; const aColor: array of TColor4; const aPic: array of TKMPic; aTag: Integer = 0): TKMListRow; overload;


implementation
uses KM_Resource, KM_ResourceCursors, KM_Sound, KM_Utils;


function MakeListRow(const aCaption: array of string; aTag: Integer = 0): TKMListRow;
var I: Integer;
begin
  SetLength(Result.Cells, Length(aCaption));

  for I := 0 to High(aCaption) do
  begin
    Result.Cells[I].Caption := aCaption[I];
    Result.Cells[I].Color := $FFFFFFFF;
  end;
  Result.Tag := aTag;
end;


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
  //if Assigned(fOnMouseOver) then fOnMouseOver(Self); { Unused }
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
  Result := Hitable and (fEnabled or aIncludeDisabled) and InRange(X, AbsLeft, AbsLeft + fWidth) and InRange(Y, AbsTop, AbsTop + fHeight);
end;

{One common thing - draw childs for self}
procedure TKMControl.Paint;
var sColor: TColor4; Tmp: TKMPoint;
begin
  Inc(CtrlPaintCount);

  if DrawOutline then
  begin
    TKMRenderUI.WriteShape(AbsLeft-1, AbsTop-1, Width+2, Height+2, $00000000, OutlineColor);
    TKMRenderUI.WriteShape(AbsLeft-2, AbsTop-2, Width+4, Height+4, $00000000, OutlineColor);
  end;

  if SHOW_CONTROLS_FOCUS and (csFocus in State) then
  begin
    TKMRenderUI.WriteShape(AbsLeft-1, AbsTop-1, Width+2, Height+2, $00000000, $FF00D0FF);
    TKMRenderUI.WriteShape(AbsLeft-2, AbsTop-2, Width+4, Height+4, $00000000, $FF00D0FF);
  end;


  if not SHOW_CONTROLS_OVERLAY then exit;

  sColor := $00000000;

  if Self is TKMPanel then sColor := $200000FF;

  if Self is TKMLabel then
  begin //Special case for aligned text
    Tmp := TKMLabel(Self).TextSize;
    TKMRenderUI.WriteShape(TKMLabel(Self).TextLeft, AbsTop, Tmp.X, Tmp.Y, $4000FFFF, $80FFFFFF);
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, fWidth, fHeight, 1, $FFFFFFFF);
    TKMRenderUI.WriteShape(AbsLeft-3, AbsTop-3, 6, 6, sColor or $FF000000, $FFFFFFFF);
    Exit;
  end;

  if Self is TKMLabelScroll then
  begin //Special case for aligned text
    Tmp := TKMLabelScroll(Self).TextSize;
    TKMRenderUI.WriteShape(TKMLabelScroll(Self).TextLeft, AbsTop, Tmp.X, Tmp.Y, $4000FFFF, $80FFFFFF);
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, fWidth, fHeight, 1, $FFFFFFFF);
    TKMRenderUI.WriteShape(AbsLeft-3, AbsTop-3, 6, 6, sColor or $FF000000, $FFFFFFFF);
    Exit;
  end;

  if Self is TKMImage      then sColor := $2000FF00;
  if Self is TKMImageStack then sColor := $2080FF00;
  if Self is TKMCheckBox   then sColor := $20FF00FF;
  if Self is TKMTrackBar   then sColor := $2000FF00;
  if Self is TKMCostsRow   then sColor := $2000FFFF;
  if Self is TKMRadioGroup then sColor := $20FFFF00;

  if csOver in State then sColor := sColor OR $30000000; //Highlight on mouse over

  TKMRenderUI.WriteShape(AbsLeft, AbsTop, fWidth, fHeight, sColor, $FFFFFFFF);
  TKMRenderUI.WriteShape(AbsLeft-3, AbsTop-3, 6, 6, sColor or $FF000000, $FFFFFFFF);
end;


{Shortcuts to Controls properties}
procedure TKMControl.SetAbsLeft(aValue: Integer);
begin
  if Parent = nil then
    Left := aValue
  else
    Left := Round((aValue - Parent.AbsLeft) / Parent.Scale);
end;

procedure TKMControl.SetAbsTop(aValue: Integer);
begin
  if Parent = nil then
    Top := aValue
  else
    Top := Round((aValue - Parent.AbsTop) / Parent.Scale);
end;

function TKMControl.GetAbsLeft: Integer;
begin
  if Parent = nil then
    Result := Round(fLeft)
  else
    Result := Round(fLeft * Parent.Scale) + Parent.GetAbsLeft;
end;

function TKMControl.GetAbsTop: Integer;
begin
  if Parent = nil then
    Result := Round(fTop)
  else
    Result := Round(fTop * Parent.Scale) + Parent.GetAbsTop;
end;

function TKMControl.GetLeft: Integer;
begin
  Result := Round(fLeft)
end;

function TKMControl.GetTop: Integer;
begin
  Result := Round(fTop)
end;

procedure TKMControl.SetLeft(aValue: Integer);
begin
  fLeft := aValue;
end;

procedure TKMControl.SetTop(aValue: Integer);
begin
  fTop := aValue;
end;

function TKMControl.GetHeight: Integer;
begin
  Result := fHeight;
end;

function TKMControl.GetWidth: Integer;
begin
  Result := fWidth;
end;

procedure TKMControl.SetTopF(aValue: Single);
begin
  //Call child classes SetTop methods
  SetTop(Round(aValue));

  //Assign actual FP value
  fTop := aValue;
end;

procedure TKMControl.SetLeftF(aValue: Single);
begin
  //Call child classes SetTop methods
  SetLeft(Round(aValue));

  //Assign actual FP value
  fLeft := aValue;
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
  and (GetTimeSince(fTimeOfLastClick) <= GetDoubleClickTime) then
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
begin
  Result := fVisible and ((Parent = nil) or Parent.Visible);
end;


procedure TKMControl.SetEnabled(aValue: Boolean);
begin
  fEnabled := aValue;
end;


procedure TKMControl.SetVisible(aValue: Boolean);
var OldVisible: Boolean;
begin
  OldVisible := fVisible;
  fVisible := aValue;

  //Only swap focus if visibility is now different
  if (OldVisible <> fVisible) and (Focusable or (Self is TKMPanel)) then
    MasterParent.GetCollection.UpdateFocus(Self);
end;


procedure TKMControl.Enable;  begin SetEnabled(true);  end; //Overrides will be set too
procedure TKMControl.Disable; begin SetEnabled(false); end;


{Will show up entire branch in which control resides}
procedure TKMControl.Show;
begin
  if Parent <> nil then Parent.Show;
  Visible := True;
end;


procedure TKMControl.Hide;    begin Visible := False; end;
procedure TKMControl.Center;  begin Anchors := []; end;
procedure TKMControl.Stretch; begin Anchors := [akLeft, akTop, akRight, akBottom]; end;


function TKMControl.MasterParent: TKMPanel;
var P: TKMPanel;
begin
  if not (Self is TKMPanel) then
    P := Parent
  else
    P := TKMPanel(Self);

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
      Childs[I].SetTopF(Childs[I].fTop + (aValue - fHeight))
    else
      Childs[I].SetTopF(Childs[I].fTop + (aValue - fHeight) / 2);

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
      Childs[I].SetLeftF(Childs[I].fLeft + (aValue - fWidth))
    else
      Childs[I].SetLeftF(Childs[I].fLeft + (aValue - fWidth) / 2);

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
  EdgeAlpha := 0.75; //Default value
end;


procedure TKMBevel.Paint;
begin
  inherited;
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, EdgeAlpha, BackAlpha);
end;


{ TKMShape }
constructor TKMShape.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  LineWidth := 2;
end;


procedure TKMShape.Paint;
begin
  inherited;
  TKMRenderUI.WriteShape(AbsLeft, AbsTop, Width, Height, FillColor);
  TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, LineWidth, LineColor);
end;


{ TKMLabel }
constructor TKMLabel.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fFontColor := $FFFFFFFF;
  fTextAlign := aTextAlign;
  fAutoWrap := False;
  SetCaption(aCaption);
end;


//Same as above but with width/height ommitted, as in most cases we don't know/don't care
constructor TKMLabel.Create(aParent: TKMPanel; aLeft, aTop: Integer; aCaption: string; aFont: TKMFont; aTextAlign: TTextAlign);
begin
  inherited Create(aParent, aLeft, aTop, 0, 0);
  fFont := aFont;
  fFontColor := $FFFFFFFF;
  fTextAlign := aTextAlign;
  fAutoWrap := False;
  SetCaption(aCaption);
end;


function TKMLabel.TextLeft: Integer;
begin
  case fTextAlign of
    taLeft:   Result := AbsLeft;
    taCenter: Result := AbsLeft + Round((Width - fTextSize.X) / 2);
    taRight:  Result := AbsLeft + (Width - fTextSize.X);
    else      Result := AbsLeft;
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
  Result := Hitable and InRange(X, TextLeft, TextLeft + fTextSize.X) and InRange(Y, AbsTop, AbsTop + Height);
end;


//Existing EOLs should be preserved, and new ones added where needed
//Keep original intact incase we need to Reformat text once again
procedure TKMLabel.ReformatText;
begin
  if fAutoWrap then
    fText := fResource.Fonts.WordWrap(fCaption, fFont, Width, True, False)
  else
    fText := fCaption;

  fTextSize := fResource.Fonts.GetTextSize(fText, fFont);
end;


{Send caption to render}
procedure TKMLabel.Paint;
var Col: Cardinal;
begin
  inherited;

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  TKMRenderUI.WriteText(AbsLeft, AbsTop, Width, fText, fFont, fTextAlign, Col);

  if fStrikethrough then
    TKMRenderUI.WriteShape(TextLeft, AbsTop + fTextSize.Y div 2 - 2, fTextSize.X, 3, Col, $FF000000);
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
  TKMRenderUI.SetupClipY(AbsTop, AbsTop + Height);
  NewTop := EnsureRange(AbsTop + Height - GetTimeSince(SmoothScrollToTop) div 50, -MINSHORT, MAXSHORT); //Compute delta and shift by it upwards (Credits page)

  if fEnabled then Col := FontColor
              else Col := $FF888888;

  TKMRenderUI.WriteText(AbsLeft, NewTop, Width, fCaption, fFont, fTextAlign, Col);
  TKMRenderUI.ReleaseClipY;
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
  if Visible and fEnabled then
  begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    Parent.GetCollection.CtrlOver := Self;
    Parent.GetCollection.CtrlUp := Self;
    if Assigned(fOnClick) then fOnClick(Self);
    Result := true; //Click has happened
  end
  else
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
  PaintLightness: Single;
begin
  inherited;
  if fTexID = 0 then Exit; //No picture to draw

  if ClipToBounds then
  begin
    TKMRenderUI.SetupClipX(AbsLeft, AbsLeft + Width);
    TKMRenderUI.SetupClipY(AbsTop,  AbsTop + Height);
  end;

  PaintLightness := Lightness + 0.4 * (Byte(HighlightOnMouseOver and (csOver in State)) + Byte(Highlight));

  TKMRenderUI.WritePicture(AbsLeft, AbsTop, fWidth, fHeight, ImageAnchors, fRX, fTexID, fEnabled, fFlagColor, PaintLightness);
  TKMRenderUI.ReleaseClipX;
  TKMRenderUI.ReleaseClipY;
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
    TKMRenderUI.WritePicture(AbsLeft + CenterX + OffsetX * ((i-1) mod fColumns),
                            AbsTop + CenterY + OffsetY * ((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, [akLeft, akTop, akRight, akBottom], fRX, fTexID1, fEnabled)
  else
    TKMRenderUI.WritePicture(AbsLeft + CenterX + OffsetX * ((i-1) mod fColumns),
                            AbsTop + CenterY + OffsetY * ((i-1) div fColumns),
                            fDrawWidth, fDrawHeight, [akLeft, akTop, akRight, akBottom], fRX, fTexID2, fEnabled);
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
  if Button = mbLeft then
  begin
    NewColor := EnsureRange((Y-AbsTop) div fCellSize, 0, fRowCount-1)*fColumnCount +
                EnsureRange((X-AbsLeft) div fCellSize, 0, fColumnCount-1);
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

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height, 1, fBackAlpha);

  Start := 0;
  if fInclRandom then
  begin
    //Render miniature copy of all available colors with '?' on top
    for i:=0 to Length(Colors)-1 do
      TKMRenderUI.WriteShape(AbsLeft+(i mod fColumnCount)*(fCellSize div fColumnCount)+2, AbsTop+(i div fColumnCount)*(fCellSize div fColumnCount)+2, (fCellSize div fColumnCount), (fCellSize div fColumnCount), Colors[i]);
    TKMRenderUI.WriteText(AbsLeft + fCellSize div 2, AbsTop + fCellSize div 4, 0, '?', fnt_Metal, taCenter);
    Start := 1;
  end;

  for i:=Start to Length(Colors)-1 do
    TKMRenderUI.WriteShape(AbsLeft+(i mod fColumnCount)*fCellSize, AbsTop+(i div fColumnCount)*fCellSize, fCellSize, fCellSize, Colors[i]);

  //Paint selection
  TKMRenderUI.WriteOutline(AbsLeft+(fColorIndex mod fColumnCount)*fCellSize, AbsTop+(fColorIndex div fColumnCount)*fCellSize, fCellSize, fCellSize, 1, $FFFFFFFF);
end;


{ TKMButton }
constructor TKMButton.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aTexID: Word; aRX: TRXType; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fRX         := aRX;
  TexID       := aTexID;
  Caption     := '';
  FlagColor   := $FFFF00FF;
  fStyle      := aStyle;
  MakesSound  := true;
end;


{Different version of button, with caption on it instead of image}
constructor TKMButton.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  TexID       := 0;
  Caption     := aCaption;
  FlagColor   := $FFFF00FF;
  Font        := fnt_Metal;
  fTextAlign  := taCenter; //Thats default everywhere in KaM
  fStyle      := aStyle;
  MakesSound  := True;
end;


//DoClick is called by keyboard shortcuts
//It puts a focus on the button and depresses it if it was DoPress'ed
//It's important that Control must be:
// Visible (can't shortcut invisible/unaccessible button)
// Enabled (can't shortcut disabled function, e.g. Halt during fight)
function TKMButton.Click: Boolean;
begin
  if Visible and fEnabled then
  begin
    //Mark self as CtrlOver and CtrlUp, don't mark CtrlDown since MouseUp manually Nils it
    Parent.GetCollection.CtrlOver := Self;
    Parent.GetCollection.CtrlUp := Self;
    if Assigned(fOnClick) then fOnClick(Self);
    Result := true; //Click has happened
  end
  else
    Result := false; //No, we couldn't click for Control is unreachable
end;


procedure TKMButton.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if fEnabled and MakesSound and (csDown in State) then fSoundLib.Play(sfxn_ButtonClick);
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
  if (csOver in State) and (csDown in State) then
    StateSet := StateSet + [bsDown];
  if not fEnabled then
    StateSet := StateSet + [bsDisabled];

  TKMRenderUI.Write3DButton(AbsLeft, AbsTop, Width, Height, fRX, TexID, FlagColor, StateSet, fStyle);

  if TexID <> 0 then Exit;

  //If disabled then text should be faded
  Col := IfThen(fEnabled, $FFFFFFFF, $FF808080);

  TKMRenderUI.WriteText(AbsLeft + Byte(csDown in State),
                      (AbsTop + Height div 2)-7 + Byte(csDown in State), Width, Caption, Font, fTextAlign, Col);
end;


//Simple version of button, with a caption and image
constructor TKMButtonFlat.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aTexID: Integer; aRX: TRXType = rxGui);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  RX        := aRX;
  TexID     := aTexID;
  FlagColor := $FFFF00FF;
  Font      := fnt_Game;
  Clickable := True;
end;


procedure TKMButtonFlat.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if not Clickable then Exit;
  if fEnabled and (csDown in State) then fSoundLib.Play(sfx_Click);
  inherited;
end;


procedure TKMButtonFlat.Paint;
const
  TextCol: array [Boolean] of TColor4 = ($FF808080, $FFFFFFFF);
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);

  if TexID <> 0 then
    TKMRenderUI.WritePicture(AbsLeft + TexOffsetX,
                             AbsTop + TexOffsetY - 6 * Byte(Caption <> ''),
                             Width, Height, [], RX, TexID, fEnabled, FlagColor);

  if (csOver in State) and fEnabled and not HideHighlight then
    TKMRenderUI.WriteShape(AbsLeft+1, AbsTop+1, Width-2, Height-2, $40FFFFFF);

  TKMRenderUI.WriteText(AbsLeft, AbsTop + (Height div 2) + 4 + CapOffsetY, Width, Caption, Font, taCenter, TextCol[fEnabled]);

  {if not fEnabled then
    TKMRenderUI.WriteShape(Left, Top, Width, Height, $80000000);}

  if Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 1, $FFFFFFFF);
end;


{ TKMFlatButtonShape }
constructor TKMFlatButtonShape.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string; aFont: TKMFont; aShapeColor: TColor4);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fCaption    := aCaption;
  ShapeColor  := aShapeColor;
  fFont       := aFont;
  fFontHeight := fResource.Fonts.FontData[fFont].Unk1 + 2;
  FontColor   := $FFFFFFFF;
end;


procedure TKMFlatButtonShape.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);

  //Shape within bevel
  TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Width - 2, ShapeColor);

  TKMRenderUI.WriteText(AbsLeft, AbsTop + (Height - fFontHeight) div 2,
                      Width, fCaption, fFont, taCenter, FontColor);

  if (csOver in State) and fEnabled then
    TKMRenderUI.WriteShape(AbsLeft + 1, AbsTop + 1, Width - 2, Height - 2, $40FFFFFF);

  if (csDown in State) or Down then
    TKMRenderUI.WriteOutline(AbsLeft, AbsTop, Width, Height, 1, $FFFFFFFF);
end;


{ TKMEdit }
constructor TKMEdit.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fFont := aFont;
  fAllowedChars := acText; //Set to the widest by default
  CursorPos := 0;
  MaxLen := 256; //Default max length is 256

  //Text input fields are focusable by concept
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
    RText := Copy(fText, fLeftIndex+1, Length(fText));
    while fCursorPos-fLeftIndex > fResource.Fonts.CharsThatFit(RText, fFont, Width-8) do
    begin
      Inc(fLeftIndex);
      //Remove characters to the left of fLeftIndex
      RText := Copy(fText, fLeftIndex+1, Length(fText));
    end;
  end;
end;


procedure TKMEdit.SetText(aText: string);
begin
  fText := aText;
  ValidateText; //Validate first since it could change fText
  CursorPos := Math.Min(CursorPos, Length(fText));
  //Setting the text should place cursor to the end
  fLeftIndex := 0;
  SetCursorPos(Length(Text));
end;


//Validates fText basing on predefined sets of allowed or disallowed chars
//It iterates from end to start of a string - deletes chars and moves cursor appropriately
procedure TKMEdit.ValidateText;
var
  I: Integer;
const
  DigitChars: set of Char = ['1' .. '9', '0'];
  NonFileChars: set of Char = [#0 .. #31, '<', '>', '|', '"', '\', '/', ':', '*', '?'];
  NonTextChars: set of Char = [#0 .. #31, '°', '|']; //° has negative width so acts like a backspace in KaM fonts
begin
  //Validate contents
  for I := Length(fText) downto 1 do
  if (fAllowedChars = acDigits) and not(fText[i] in DigitChars) or
     (fAllowedChars = acFileName) and (fText[i] in NonFileChars) or
     (fAllowedChars = acText) and (fText[i] in NonTextChars) then
  begin
    Delete(fText, I, 1);
    if CursorPos >= I then //Keep cursor in place
      CursorPos := CursorPos - 1;
  end;

  //Validate length
  if Length(fText) > MaxLen then
    fText := Copy(fText, 0, MaxLen);
end;


//Key events which have no effect should not be handled (allows scrolling while chat window open with no text entered)
function TKMEdit.KeyEventHandled(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;

  //Don't include backspace/delete because edits should always handle those. Otherwise when you
  //press backspace repeatedly to remove all characters it will apply other shortcuts like
  //resetting the zoom if you press it once too many times.
  case Key of
    VK_UP,
    VK_DOWN,
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END: Result := (fText <> ''); //These keys have no effect when text is blank
  end;

  //We want these keys to be ignored by chat, so game shortcuts still work
  if Key in [VK_F1..VK_F12, VK_ESCAPE] then Result := False;

  //Ctrl can be used as an escape character, e.g. CTRL+B places beacon while chat is open
  if ssCtrl in Shift then Result := (Key in [Ord('C'), Ord('X'), Ord('V')]);
end;


function TKMEdit.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key, Shift);
  if inherited KeyDown(Key, Shift) or ReadOnly then Exit;

  //Clipboard operations
  if (Shift = [ssCtrl]) and (Key <> VK_CONTROL) then
  begin
    case Key of
      Ord('C'): Clipboard.AsText := fText;
      Ord('X'): begin
                  Clipboard.AsText := fText;
                  Text := '';
                end;
      Ord('V'): begin
                  Insert(Clipboard.AsText, fText, CursorPos + 1);
                  ValidateText;
                  CursorPos := CursorPos + Length(Clipboard.AsText);
                end;
    end;
  end;

  case Key of
    VK_BACK:    begin Delete(fText, CursorPos, 1); CursorPos := CursorPos-1; end;
    VK_DELETE:  Delete(fText, CursorPos+1, 1);
    VK_LEFT:    CursorPos := CursorPos-1;
    VK_RIGHT:   CursorPos := CursorPos+1;
    VK_HOME:    CursorPos := 0;
    VK_END:     CursorPos := Length(fText);
  end;

  if Assigned(OnKeyDown) then OnKeyDown(Self, Key);
end;


procedure TKMEdit.KeyPress(Key: Char);
begin
  if ReadOnly then Exit;
  if Length(fText) >= MaxLen then Exit;

  Insert(Key, fText, CursorPos + 1);
  CursorPos := CursorPos + 1; //Before ValidateText so it moves the cursor back if the new char was invalid
  ValidateText;
end;


function TKMEdit.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key, Shift);
  if inherited KeyUp(Key, Shift) or ReadOnly then Exit;

  if Assigned(OnChange) then OnChange(Self);
end;


procedure TKMEdit.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  if ReadOnly then Exit;
  inherited;
  CursorPos := Length(fText);
end;


procedure TKMEdit.Paint;
var
  Col: TColor4;
  RText: string;
  OffX: Integer;
begin
  inherited;
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if Masked then
    RText := StringOfChar('*', Length(fText))
  else
    RText := fText;

  RText := Copy(RText, fLeftIndex+1, Length(RText)); //Remove characters to the left of fLeftIndex

  TKMRenderUI.WriteText(AbsLeft+4, AbsTop+3, Width-8, RText, fFont, taLeft, Col, not ShowColors, True); //Characters that do not fit are trimmed

  //Render text cursor
  if (csFocus in State) and ((TimeGet div 500) mod 2 = 0) then
  begin
    SetLength(RText, CursorPos - fLeftIndex);
    OffX := AbsLeft + 2 + fResource.Fonts.GetTextSize(RText, fFont).X;
    TKMRenderUI.WriteShape(OffX, AbsTop+2, 3, Height-4, Col, $FF000000);
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
var Col: TColor4; CheckSize: Integer;
begin
  inherited;
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  CheckSize := fResource.Fonts.GetTextSize('I', fFont).Y + 1;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, CheckSize-4, CheckSize-4, 1, 0.3);

  if fChecked then
    TKMRenderUI.WriteText(AbsLeft+(CheckSize-4)div 2, AbsTop, 0, 'x', fFont, taCenter, Col);

  TKMRenderUI.WriteText(AbsLeft+CheckSize, AbsTop, Width-Height, fCaption, fFont, taLeft, Col);
end;


{ TKMRadioGroup }
constructor TKMRadioGroup.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  fItemIndex := -1;
end;


procedure TKMRadioGroup.Add(aText: string; aEnabled: Boolean);
begin
  if fCount >= Length(fItems) then
    SetLength(fItems, fCount + 8);

  fItems[fCount].Text := aText;
  fItems[fCount].Enabled := aEnabled;

  Inc(fCount);
end;


procedure TKMRadioGroup.Clear;
begin
  fCount := 0;
end;


procedure TKMRadioGroup.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var
  NewIndex: Integer;
begin
  if (csDown in State) and (Button = mbLeft) then
  begin
    NewIndex := EnsureRange((Y-AbsTop) div Round(Height/Count), 0, Count - 1); //Clicking at wrong place can select invalid ID
    if (NewIndex <> fItemIndex) and (fItems[NewIndex].Enabled) then
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
const FntCol: array [Boolean] of TColor4 = ($FF888888, $FFFFFFFF);
var
  LineHeight, CheckSize: Integer;
  I: Integer;
begin
  inherited;
  if Count = 0 then Exit; //Avoid dividing by zero

  LineHeight := Round(fHeight / Count);
  CheckSize := fResource.Fonts.GetTextSize('I', fFont).Y + 1;

  for I := 0 to Count - 1 do
  begin
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop + I * LineHeight, CheckSize-4, CheckSize-4, 1, 0.3);

    if fItemIndex = I then
      TKMRenderUI.WriteText(AbsLeft+(CheckSize-4)div 2, AbsTop + I * LineHeight, 0, 'x', fFont, taCenter, FntCol[fEnabled and fItems[I].Enabled]);

    TKMRenderUI.WriteText(AbsLeft+CheckSize, AbsTop + I * LineHeight, Width-LineHeight, fItems[I].Text, fFont, taLeft, FntCol[fEnabled and fItems[I].Enabled]);
  end;
end;


{ TKMPercentBar }
constructor TKMPercentBar.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont=fnt_Mini);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := aFont;
  FontColor := $FFFFFFFF;
  fTextAlign := taCenter;
end;


procedure TKMPercentBar.SetPosition(aValue: Single);
begin
  fPosition := EnsureRange(aValue, 0, 1);
end;


procedure TKMPercentBar.SetSeam(aValue: Single);
begin
  fSeam := EnsureRange(aValue, 0, 1);
end;


procedure TKMPercentBar.Paint;
begin
  inherited;

  TKMRenderUI.WritePercentBar(AbsLeft,AbsTop,Width,Height, fSeam, fPosition);

  //Now draw text over the bar, if it is required
  if Caption <> '' then
  begin
    //Shadow
    TKMRenderUI.WriteText(AbsLeft + 2, (AbsTop + Height div 2)+TextYOffset-4, Width-4, Caption, fFont, fTextAlign, $FF000000);
    //Text
    TKMRenderUI.WriteText(AbsLeft + 1, (AbsTop + Height div 2)+TextYOffset-5, Width-4, Caption, fFont, fTextAlign, FontColor);
  end;
end;


{ TKMResourceRow }
procedure TKMResourceRow.Paint;
var I: Integer;
begin
  inherited;
  TKMRenderUI.WriteBevel(AbsLeft,AbsTop,Width,Height);
  TKMRenderUI.WriteText(AbsLeft + 4, AbsTop + 3, Width-8, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for I:=1 to ResourceCount do
    TKMRenderUI.WritePicture((AbsLeft+Width-2-20)-(ResourceCount-i)*14, AbsTop+1, 14, 14, [], RX, TexID);
end;


{ TKMNumericEdit }
constructor TKMNumericEdit.Create(aParent: TKMPanel; aLeft, aTop: Integer; aValueMin, aValueMax: SmallInt);
var
  W: Word;
begin
  // Text width + padding + buttons
  W := fResource.Fonts.GetTextSize(IntToStr(aValueMax), fnt_Grey).X + 16 + 20 + 20;

  inherited Create(aParent, aLeft, aTop, W, 20);

  ValueMin := aValueMin;
  ValueMax := aValueMax;
  Value := 0;
  Focusable := True;
  fFont := fnt_Grey;

  fButtonDec := TKMButton.Create(aParent, aLeft,           aTop, 20, 20, '-', bsGame);
  fButtonInc := TKMButton.Create(aParent, aLeft + W - 20,  aTop, 20, 20, '+', bsGame);
  fButtonDec.OnClickEither := ButtonClick;
  fButtonInc.OnClickEither := ButtonClick;
end;


function TKMNumericEdit.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key, Shift);
  if inherited KeyDown(Key, Shift) then Exit;

  //Clipboard operations
  if (Shift = [ssCtrl]) and (Key <> VK_CONTROL) then
  begin
    case Key of
      Ord('C'): Clipboard.AsText := fText;
      Ord('X'): Clipboard.AsText := fText; //Does the same as Copy because we can't cut whole text and leave field empty
      Ord('V'): begin
                  Insert(Clipboard.AsText, fText, CursorPos + 1);
                  ValidateText;
                  CursorPos := CursorPos + Length(Clipboard.AsText);
                end;
    end;
  end;

  case Key of
    VK_BACK:    begin Delete(fText, CursorPos, 1); CursorPos := CursorPos - 1; end;
    VK_DELETE:  Delete(fText, CursorPos + 1, 1);
    VK_LEFT:    CursorPos := CursorPos - 1;
    VK_RIGHT:   CursorPos := CursorPos + 1;
    VK_UP:      Value := Value + 1;
    VK_DOWN:    Value := Value - 1;
    VK_HOME:    CursorPos := 0;
    VK_END:     CursorPos := Length(fText);
  end;

  if Assigned(OnChange) then
    OnChange(Self);
end;


function TKMNumericEdit.KeyEventHandled(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True;

  //Don't include backspace/delete because edits should always handle those. Otherwise when you
  //press backspace repeatedly to remove all characters it will apply other shortcuts like
  //resetting the zoom if you press it once too many times.
  case Key of
    VK_LEFT,
    VK_RIGHT,
    VK_HOME,
    VK_END: Result := (fText <> ''); //These keys have no effect when text is blank
  end;

  //We want these keys to be ignored by chat, so game shortcuts still work
  if Key in [VK_F1..VK_F12, VK_ESCAPE] then Result := False;

  //Ctrl can be used as an escape character, e.g. CTRL+B places beacon while chat is open
  if ssCtrl in Shift then
    Result := (Key in [Ord('C'), Ord('X'), Ord('V')]);
end;


procedure TKMNumericEdit.KeyPress(Key: Char);
begin
  if Length(fText) >= MaxLength then Exit;

  Insert(Key, fText, CursorPos + 1);
  CursorPos := CursorPos + 1; //Before ValidateText so it moves the cursor back if the new char was invalid
  ValidateText;
end;


function TKMNumericEdit.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := KeyEventHandled(Key, Shift);
  if inherited KeyUp(Key, Shift) then Exit;
end;


function TKMNumericEdit.MaxLength: Byte;
begin
  if Max(Abs(ValueMax), Abs(ValueMin)) <> 0 then
    Result := Trunc(Log10(Max(Abs(ValueMax), Abs(ValueMin)))) + 1
  else
    Result := 1;
end;


procedure TKMNumericEdit.MouseUp(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  CursorPos := Length(fText);
end;


procedure TKMNumericEdit.ButtonClick(Sender: TObject; AButton: TMouseButton);
begin
  if Sender = fButtonDec then
    Value := Value - Byte(AButton = mbLeft) - Byte(AButton = mbRight) * 10
  else
  if Sender = fButtonInc then
    Value := Value + Byte(AButton = mbLeft) + Byte(AButton = mbRight) * 10
  else
    Exit;

  if Assigned(OnChange) then
    OnChange(Self);
end;


procedure TKMNumericEdit.SetSharedHint(aHint: string);
begin
  Hint := aHint;
  fButtonInc.Hint := aHint;
  fButtonDec.Hint := aHint;
end;


procedure TKMNumericEdit.SetTop(aValue: Integer);
begin
  inherited;

  fButtonDec.Top := Top;
  fButtonInc.Top := Top;
end;


procedure TKMNumericEdit.SetLeft(aValue: Integer);
begin
  inherited;

  fButtonDec.Left := Left;
  fButtonInc.Top := Left + Width - 20;
end;


procedure TKMNumericEdit.SetCursorPos(aPos: Integer);
var RText: string;
begin
  fCursorPos := EnsureRange(aPos, 0, Length(fText));
  if fCursorPos < fLeftIndex then
    fLeftIndex := fCursorPos
  else
  begin
    //Remove characters to the left of fLeftIndex
    RText := Copy(fText, fLeftIndex+1, Length(fText));
    while fCursorPos-fLeftIndex > fResource.Fonts.CharsThatFit(RText, fFont, Width-8) do
    begin
      Inc(fLeftIndex);
      //Remove characters to the left of fLeftIndex
      RText := Copy(fText, fLeftIndex+1, Length(fText));
    end;
  end;
end;


procedure TKMNumericEdit.SetEnabled(aValue: Boolean);
begin
  inherited;
  fButtonDec.Enabled := fEnabled;
  fButtonInc.Enabled := fEnabled;
end;


procedure TKMNumericEdit.SetVisible(aValue: Boolean);
begin
  inherited;
  fButtonDec.Visible := fVisible;
  fButtonInc.Visible := fVisible;
end;


procedure TKMNumericEdit.SetValue(aValue: Integer);
begin
  fValue := EnsureRange(aValue, ValueMin, ValueMax);
  fText := IntToStr(fValue);
  SetCursorPos(MaxLength);

  //External Value assignment should not generate OnChange event
end;


procedure TKMNumericEdit.ValidateText;
const
  DigitChars: set of Char = ['1' .. '9', '0'];
var
  I: Integer;
begin
  //Validate contents
  for I := Length(fText) downto 1 do
  if not (fText[I] in DigitChars) then
  begin
    Delete(fText, I, 1);
    if CursorPos >= I then //Keep cursor in place
      CursorPos := CursorPos - 1;
  end;

  if fText = '' then
    Value := 0
  else
    Value := StrToInt(fText);

  if Assigned(OnChange) then
    OnChange(Self);
end;


procedure TKMNumericEdit.Paint;
var
  Col: TColor4;
  RText: string;
  OffX: Integer;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft + 20, AbsTop, Width - 40, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  RText := Copy(fText, fLeftIndex+1, Length(fText)); //Remove characters to the left of fLeftIndex

  TKMRenderUI.WriteText(AbsLeft+24, AbsTop+3, 0, fText, fFont, taLeft, Col); //Characters that do not fit are trimmed

  //Render text cursor
  if (csFocus in State) and ((TimeGet div 500) mod 2 = 0) then
  begin
    SetLength(RText, CursorPos - fLeftIndex);
    OffX := AbsLeft + 22 + fResource.Fonts.GetTextSize(RText, fFont).X;
    TKMRenderUI.WriteShape(OffX, AbsTop+2, 3, Height-4, Col, $FF000000);
  end;
end;


{ TKMResourceOrderRow }
constructor TKMResourceOrderRow.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft+68, aTop, aWidth-68, aHeight);

  fOrderRem := TKMButton.Create(aParent, aLeft, aTop+2, 20, aHeight, '-', bsGame);
  fOrderLab := TKMLabel.Create(aParent, aLeft+33, aTop+4, '', fnt_Grey, taCenter);
  fOrderAdd := TKMButton.Create(aParent, aLeft+46, aTop+2, 20, aHeight, '+', bsGame);
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
var I: Integer;
begin
  inherited;
  fOrderRem.Top := Top; //Use internal fTop instead of GetTop (which will return absolute value)
  fOrderLab.Top := Top + 4;
  fOrderAdd.Top := Top;

  fOrderLab.Caption := IntToStr(OrderCount);

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);
  TKMRenderUI.WriteText(AbsLeft + 4, AbsTop + 3, Width - 8, Caption, fnt_Game, taLeft, $FFE0E0E0);
  for I := 1 to ResourceCount do
    TKMRenderUI.WritePicture((AbsLeft+Width-2-20)-(ResourceCount-I)*14, AbsTop+1, 14, 14, [], RX, TexID);
end;


{ TKMCostsRow }
procedure TKMCostsRow.Paint;
begin
  inherited;
  TKMRenderUI.WriteText(AbsLeft, AbsTop + 4, Width-20, Caption, fnt_Grey, taLeft, $FFFFFFFF);
  if TexID1 <> 0 then TKMRenderUI.WritePicture(AbsLeft+Width-40, AbsTop, 20, fHeight, [], RX, TexID1);
  if TexID2 <> 0 then TKMRenderUI.WritePicture(AbsLeft+Width-20, AbsTop, 20, fHeight, [], RX, TexID2);
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
  ThumbWidth := fResource.Fonts.GetTextSize(IntToStr(MaxValue), fFont).X + 24;

  Font := fnt_Metal;
  Step := 1;
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


procedure TKMTrackBar.SetPosition(aValue: Word);
begin
  fPosition := EnsureRange(aValue, MinValue, MaxValue);
  ThumbText := IntToStr(Position);
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
  if (ssLeft in Shift) and InRange(Y - AbsTop - fTrackTop, 0, fTrackHeight) then
    NewPos := EnsureRange(fMinValue + Round(((X-AbsLeft-ThumbWidth div 2) / (Width - ThumbWidth - 4))*(fMaxValue - fMinValue)/Step)*Step, fMinValue, fMaxValue);

  if NewPos <> Position then
  begin
    Position := NewPos;

    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


procedure TKMTrackBar.Paint;
const //Text color for disabled and enabled control
  TextColor: array [Boolean] of TColor4 = ($FF888888, $FFFFFFFF);
var
  ThumbPos, ThumbHeight: Word;
begin
  inherited;

  if fCaption <> '' then
    TKMRenderUI.WriteText(AbsLeft, AbsTop, Width, fCaption, fFont, taLeft, TextColor[fEnabled]);

  TKMRenderUI.WriteBevel(AbsLeft+2,AbsTop+fTrackTop+2,Width-4,fTrackHeight-4);
  ThumbPos := Round(Mix (0, Width - ThumbWidth - 4, 1-(Position-fMinValue) / (fMaxValue - fMinValue)));

  ThumbHeight := fResource.Sprites[rxGui].RXData.Size[132].Y;

  TKMRenderUI.WritePicture(AbsLeft + ThumbPos + 2, AbsTop+fTrackTop, ThumbWidth, ThumbHeight, [akLeft,akRight], rxGui, 132);
  TKMRenderUI.WriteText(AbsLeft + ThumbPos + ThumbWidth div 2 + 2, AbsTop+fTrackTop+3, 0, ThumbText, fnt_Metal, taCenter, TextColor[fEnabled]);
end;


{ TKMScrollBar }
constructor TKMScrollBar.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aScrollAxis: TScrollAxis; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  BackAlpha := 0.5;
  EdgeAlpha := 0.75;
  fScrollAxis := aScrollAxis;
  fMinValue := 0;
  fMaxValue := 10;
  fPosition := 0;
  fStyle    := aStyle;

  if aScrollAxis = sa_Vertical then
  begin
    fScrollDec := TKMButton.Create(Self, 0, 0, aWidth, aWidth, 591, rxGui, aStyle);
    fScrollInc := TKMButton.Create(Self, 0, aHeight-aWidth, aWidth, aWidth, 590, rxGui, aStyle);
    fScrollDec.Anchors := [akLeft, akTop, akRight];
    fScrollInc.Anchors := [akLeft, akRight, akBottom];
  end;
  if aScrollAxis = sa_Horizontal then
  begin
    fScrollDec := TKMButton.Create(Self, 0, 0, aHeight, aHeight, 2, rxGui, aStyle);
    fScrollInc := TKMButton.Create(Self, aWidth-aHeight, 0, aHeight, aHeight, 3, rxGui, aStyle);
    fScrollDec.Anchors := [akLeft, akTop, akBottom];
    fScrollInc.Anchors := [akTop, akRight, akBottom];
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

  //Update Thumb size
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetWidth(aValue: Integer);
begin
  inherited;

  //Update Thumb size
  UpdateThumbSize;
end;


procedure TKMScrollBar.SetEnabled(aValue: Boolean);
begin
  inherited;
  fScrollDec.Enabled := aValue;
  fScrollInc.Enabled := aValue;
end;


procedure TKMScrollBar.SetMinValue(Value: Integer);
begin
  fMinValue := Max(0, Value);
  Enabled := (fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetMaxValue(Value: Integer);
begin
  fMaxValue := Max(0, Value);
  Enabled := (fMaxValue > fMinValue);
  SetPosition(fPosition);
end;


procedure TKMScrollBar.SetPosition(Value: Integer);
begin
  fPosition := EnsureRange(Value, fMinValue, fMaxValue);
  UpdateThumbPos;
end;


procedure TKMScrollBar.IncPosition(Sender: TObject);
begin
  SetPosition(fPosition + 1);

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMScrollBar.DecPosition(Sender: TObject);
begin
  SetPosition(fPosition - 1);

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMScrollBar.UpdateThumbPos;
begin
  fThumbPos := 0;

  if fMaxValue > fMinValue then
    case fScrollAxis of
      sa_Vertical:   fThumbPos := (fPosition-fMinValue)*(Height-Width*2-fThumbSize) div (fMaxValue-fMinValue);
      sa_Horizontal: fThumbPos := (fPosition-fMinValue)*(Width-Height*2-fThumbSize) div (fMaxValue-fMinValue);
    end
  else
    case fScrollAxis of
      sa_Vertical:   fThumbPos := Math.max((Height-Width*2-fThumbSize),0) div 2;
      sa_Horizontal: fThumbPos := Math.max((Width-Height*2-fThumbSize),0) div 2;
    end;
end;


procedure TKMScrollBar.UpdateThumbSize;
begin
  case fScrollAxis of
    sa_Vertical:   fThumbSize := Math.max(0, (Height-2*Width)) div 4;
    sa_Horizontal: fThumbSize := Math.max(0, (Width-2*Height)) div 4;
  end;

  //If size has changed, then Pos needs to be updated as well (depends on it)
  UpdateThumbPos;
end;


procedure TKMScrollBar.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var
  T: Integer;
begin
  inherited;

  fOffset := 0;
  case fScrollAxis of
    sa_Vertical:    begin
                      T := Y - AbsTop - Width - fThumbPos;
                      if InRange(T, 0, fThumbSize) then
                        fOffset := T - fThumbSize div 2;
                    end;
    sa_Horizontal:  begin
                      T := X - AbsLeft - Height - fThumbPos;
                      if InRange(T, 0, fThumbSize) then
                        fOffset := T - fThumbSize div 2;
                    end;
  end;

  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMScrollBar.MouseMove(X,Y: Integer; Shift: TShiftState);
var
  NewPos: Integer;
  T: Integer;
begin
  inherited;
  if not (ssLeft in Shift) then Exit;

  NewPos := fPosition;

  case fScrollAxis of
    sa_Vertical:
      begin
        T := Y - fOffset - AbsTop - Width;
        if InRange(T, 0, Height - Width * 2) then
          NewPos := Round(fMinValue+((T - fThumbSize / 2) / (Height-Width*2-fThumbSize)) * (fMaxValue - fMinValue) );
      end;

    sa_Horizontal:
      begin
        T := X - fOffset - AbsLeft - Height;
        if InRange(T, 0, Width - Height * 2) then
          NewPos := Round(fMinValue+((T - fThumbSize / 2) / (Width-Height*2-fThumbSize)) * (fMaxValue - fMinValue) );
      end;
  end;

  if NewPos <> fPosition then
  begin
    SetPosition(NewPos);
    if Assigned(fOnChange) then
      fOnChange(Self);
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
  ButtonState: TButtonStateSet;
begin
  inherited;

  case fScrollAxis of
    sa_Vertical:   TKMRenderUI.WriteBevel(AbsLeft, AbsTop+Width, Width, Height - Width*2, EdgeAlpha, BackAlpha);
    sa_Horizontal: TKMRenderUI.WriteBevel(AbsLeft+Height, AbsTop, Width - Height*2, Height, EdgeAlpha, BackAlpha);
  end;

  if fMaxValue > fMinValue then
    ButtonState := []
  else
    ButtonState := [bsDisabled];

  if not (bsDisabled in ButtonState) then //Only show thumb when usable
    case fScrollAxis of
      sa_Vertical:   TKMRenderUI.Write3DButton(AbsLeft,AbsTop+Width+fThumbPos,Width,fThumbSize,rxGui,0,$FFFF00FF,ButtonState,fStyle);
      sa_Horizontal: TKMRenderUI.Write3DButton(AbsLeft+Height+fThumbPos,AbsTop,fThumbSize,Height,rxGui,0,$FFFF00FF,ButtonState,fStyle);
    end;
end;


{ TKMMemo }
constructor TKMMemo.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fItemHeight := 20;
  fItems := TStringList.Create;
  fFont := aFont;

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, aStyle);
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
  fScrollBar.Left := Left + Width - fScrollBar.Width;
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
    NewText := fResource.Fonts.WordWrap(fText, fFont, fWidth - fScrollBar.Width - 8, True, IndentAfterNL)
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

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, PaintWidth, Height, 1, 0.5);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    TKMRenderUI.WriteText(AbsLeft+4, AbsTop+i*fItemHeight+3, Width-8, fItems.Strings[TopIndex+i] , fFont, taLeft);
end;


{ TKMListBox }
constructor TKMListBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fBackAlpha := 0.5;
  fItemHeight := 20;
  fItemIndex := -1;
  fItems := TStringList.Create;
  fFont := aFont;
  fAutoHideScrollBar := False; //Always show the scrollbar by default, then it can be turned off if required
  Focusable := True; //For up/down keys

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-20, aTop, 20, aHeight, sa_Vertical, aStyle);
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


procedure TKMListBox.Add(aItem: string; aTag: Integer=0);
begin
  fItems.Add(aItem);
  SetLength(ItemTags, Length(ItemTags)+1);
  ItemTags[Length(ItemTags)-1] := aTag;
  UpdateScrollBar;
end;


procedure TKMListBox.Clear;
begin
  fItems.Clear;
  SetLength(ItemTags, 0);
  fItemIndex := -1;
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
    VK_UP:      NewIndex := fItemIndex - 1;
    VK_DOWN:    NewIndex := fItemIndex + 1;
    VK_RETURN:  begin
                  //Trigger click to hide drop downs
                  if Assigned(fOnClick) then
                    fOnClick(Self);
                  Exit;
                end;
    else        Exit;
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

  if Assigned(fOnChange) then
    fOnChange(Self);
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
  and InRange(X, AbsLeft, AbsLeft + Width - (fScrollBar.Width*byte(fScrollBar.Visible)))
  and InRange(Y, AbsTop, AbsTop + Height div fItemHeight * fItemHeight)
  then
  begin
    NewIndex := TopIndex + (Y-AbsTop) div fItemHeight;

    if NewIndex > fItems.Count - 1 then
    begin
      //Double clicking not allowed if we are clicking past the end of the list, but keep last item selected
      fTimeOfLastClick := 0;
      NewIndex := fItems.Count - 1;
    end;

    if NewIndex <> fItemIndex then
    begin
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

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, PaintWidth, Height, 1, fBackAlpha);

  if (fItemIndex <> -1) and InRange(fItemIndex - TopIndex, 0, (fHeight div fItemHeight)-1) then
    TKMRenderUI.WriteShape(AbsLeft, AbsTop+fItemHeight*(fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888, $FFFFFFFF);

  for i:=0 to Math.min(fItems.Count-1, (fHeight div fItemHeight)-1) do
    TKMRenderUI.WriteText(AbsLeft+4, AbsTop+i*fItemHeight+3, PaintWidth-8, fItems.Strings[TopIndex+i] , fFont, taLeft);
end;


{ TKMListHeader }
constructor TKMListHeader.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  BackAlpha := 0.5;
  EdgeAlpha := 0.75;
  fSortDirection := sdNone;
  fSortIndex := -1;
end;


destructor TKMListHeader.Destroy;
begin
  ClearColumns;

  inherited;
end;


procedure TKMListHeader.ClearColumns;
var
  I: Integer;
begin
  for I := 0 to fCount - 1 do
    FreeAndNil(fColumns[I]);
end;


function TKMListHeader.GetColumnIndex(X: Integer): Integer;
var I: Integer;
begin
  Result := -1;

  for I := 0 to fCount - 1 do
    if X - AbsLeft > fColumns[I].Offset then
      Result := I;
end;


function TKMListHeader.GetColumn(aIndex: Integer): TKMListHeaderColumn;
begin
  Result := fColumns[aIndex];
end;


//We know we were clicked and now we can decide what to do
procedure TKMListHeader.DoClick(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var ColumnID: Integer;
begin
  ColumnID := GetColumnIndex(X);
  if (ColumnID <> -1) and Assigned(OnColumnClick) then
  begin
    //We could process the clicks here (i.e. do the sorting inplace)
    //but there are various circumstances where plain string sorting will look wrong
    //and the ListBox just misses the knowledge to do it right:
    //MP game status (sort by type), ping (sort 1>9), playercount (sort 9>1), dates (sort by TDateTime)
    //Let the UI communicate to Game and do it right

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


procedure TKMListHeader.SetColumns(aFont: TKMFont; aColumns: array of string; aColumnOffsets: array of Word);
var
  I: Integer;
begin
  Assert(Length(aColumns) = Length(aColumnOffsets));

  fFont := aFont;

  ClearColumns;

  fCount := Length(aColumns);
  SetLength(fColumns, fCount);
  for I := 0 to fCount - 1 do
  begin
    fColumns[I] := TKMListHeaderColumn.Create;
    fColumns[I].Caption := aColumns[I];
    fColumns[I].Offset := aColumnOffsets[I];
  end;
end;


procedure TKMListHeader.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited;
  fColumnHighlight := GetColumnIndex(X);
end;


procedure TKMListHeader.Paint;
var
  I: Integer;
  ColumnLeft: Integer;
  ColumnWidth: Integer;
  TextSize: TKMPoint;
begin
  inherited;

  for I := 0 to fCount - 1 do
  begin
    if I < fCount - 1 then
      ColumnWidth := fColumns[I+1].Offset - fColumns[I].Offset
    else
      ColumnWidth := Width - fColumns[I].Offset;

    if ColumnWidth <= 0 then Break;

    ColumnLeft := AbsLeft + fColumns[I].Offset;

    TKMRenderUI.WriteBevel(ColumnLeft, AbsTop, ColumnWidth, Height, EdgeAlpha, BackAlpha);
    if Assigned(OnColumnClick) and (csOver in State) and (fColumnHighlight = I) then
      TKMRenderUI.WriteShape(ColumnLeft, AbsTop, ColumnWidth, Height, $20FFFFFF);

    if fColumns[I].Glyph.ID <> 0 then
      TKMRenderUI.WritePicture(ColumnLeft + 4, AbsTop, ColumnWidth - 8, Height, [], fColumns[I].Glyph.RX, fColumns[I].Glyph.ID)
    else
    begin
      TextSize := fResource.Fonts.GetTextSize(fColumns[I].Caption, fFont);
      TKMRenderUI.WriteText(ColumnLeft + 4, AbsTop + (Height - TextSize.Y) div 2 + 2, ColumnWidth - 8, fColumns[I].Caption, fFont, fTextAlign);
    end;

    if Assigned(OnColumnClick) and (fSortIndex = I) then
      case fSortDirection of
        sdDown: TKMRenderUI.WritePicture(ColumnLeft + ColumnWidth - 4-10, AbsTop + 6, 10, 11, [], rxGui, 60);
        sdUp:   TKMRenderUI.WritePicture(ColumnLeft + ColumnWidth - 4-10, AbsTop + 6, 10, 11, [], rxGui, 59);
      end;
  end;
end;


{ TKMColumnListBox }
constructor TKMColumnBox.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
const DEF_HEADER_HEIGHT = 24;
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);
  fFont       := aFont;
  fItemHeight := 20;
  fItemIndex  := -1;
  fShowHeader := True;
  SearchColumn := -1; //Disabled by default
  Focusable := True; //For up/down keys

  fHeader := TKMListHeader.Create(aParent, aLeft, aTop, aWidth - fItemHeight, DEF_HEADER_HEIGHT);

  fScrollBar := TKMScrollBar.Create(aParent, aLeft+aWidth-fItemHeight, aTop, fItemHeight, aHeight, sa_Vertical, aStyle);
  UpdateScrollBar; //Initialise the scrollbar

  SetEdgeAlpha(1);
  SetBackAlpha(0.5);
end;


destructor TKMColumnBox.Destroy;
begin
  ClearColumns;

  inherited;
end;


procedure TKMColumnBox.SetSearchColumn(aValue: ShortInt);
begin
  fSearchColumn := aValue;
end;


procedure TKMColumnBox.SetShowHeader(aValue: Boolean);
begin
  fHeader.Visible := aValue;
  fShowHeader := aValue;
end;


procedure TKMColumnBox.SetLeft(aValue: Integer);
begin
  inherited;

  fHeader.Left := Left;
  fScrollBar.Left := Left + Width - fScrollBar.Width;
end;


procedure TKMColumnBox.SetTop(aValue: Integer);
begin
  inherited;

  //Update header and scrollbar so that their Top matched
  fHeader.Top := Top;
  fScrollBar.Top := Top;
end;


procedure TKMColumnBox.SetWidth(aValue: Integer);
begin
  inherited;

  fHeader.Width := Width;
  fScrollBar.Left := Left + Width - fScrollBar.Width;
end;


procedure TKMColumnBox.SetHeight(aValue: Integer);
begin
  inherited;
  fScrollBar.Height := fHeight;
  UpdateScrollBar; //Since height has changed
end;


procedure TKMColumnBox.SetItemIndex(const Value: Smallint);
begin
  if InRange(Value, 0, RowCount - 1) then
    fItemIndex := Value
  else
    fItemIndex := -1;
end;


procedure TKMColumnBox.SetOnColumnClick(const Value: TIntegerEvent);
begin
  fHeader.OnColumnClick := Value;
end;


procedure TKMColumnBox.SetSortDirection(aDirection: TSortDirection);
begin
  fHeader.SortDirection := aDirection;
end;


procedure TKMColumnBox.SetSortIndex(aIndex: Integer);
begin
  fHeader.SortIndex := aIndex;
end;


//Copy property to scrollbar. Otherwise it won't be rendered
procedure TKMColumnBox.SetVisible(aValue: Boolean);
begin
  inherited;
  fHeader.Visible := fVisible and fShowHeader;
  fScrollBar.Visible := fVisible and fScrollBar.Enabled; //Hide scrollbar and its buttons
end;


function TKMColumnBox.GetTopIndex: Integer;
begin
  Result := fScrollBar.Position;
end;


function TKMColumnBox.GetVisibleRows: Integer;
begin
  Result := Floor(GetVisibleRowsExact);
end;


function TKMColumnBox.GetVisibleRowsExact: Single;
begin
  Result := (fHeight - fHeader.Height * Byte(fShowHeader)) / fItemHeight;
end;


procedure TKMColumnBox.SetTopIndex(aIndex: Integer);
begin
  fScrollBar.Position := aIndex;
end;


procedure TKMColumnBox.SetBackAlpha(aValue: Single);
begin
  fBackAlpha := aValue;
  fHeader.BackAlpha := aValue;
  fScrollBar.BackAlpha := aValue;
end;


procedure TKMColumnBox.SetEdgeAlpha(aValue: Single);
begin
  fEdgeAlpha := aValue;
  fHeader.EdgeAlpha := aValue;
  fScrollBar.EdgeAlpha := aValue;
end;


procedure TKMColumnBox.SetEnabled(aValue: Boolean);
begin
  inherited;
  fHeader.Enabled := aValue;
  fScrollBar.Enabled := aValue;
end;


function TKMColumnBox.GetColumn(aIndex: Integer): TKMListColumn;
begin
  Result := fColumns[aIndex];
end;


function TKMColumnBox.GetOnColumnClick: TIntegerEvent;
begin
  Result := fHeader.OnColumnClick;
end;


function TKMColumnBox.GetSortDirection: TSortDirection;
begin
  Result := fHeader.SortDirection;
end;


function TKMColumnBox.GetSortIndex: Integer;
begin
  Result := fHeader.SortIndex;
end;


//fRowCount or Height has changed
procedure TKMColumnBox.UpdateScrollBar;
begin
  fScrollBar.MaxValue := fRowCount - (fHeight - fHeader.Height * Byte(fShowHeader)) div fItemHeight;
  Assert(fScrollBar.MaxValue >= fScrollBar.MinValue);
  fScrollBar.Visible := fVisible and (fScrollBar.MaxValue <> fScrollBar.MinValue);
end;


//If we don't add columns there will be Assert on items add
procedure TKMColumnBox.SetColumns(aHeaderFont: TKMFont; aCaptions: array of string; aOffsets: array of Word);
var
  I: Integer;
begin
  Assert(Length(aCaptions) = Length(aOffsets));

  Clear; //We don't want to conflict with already added rows elements
  ClearColumns;

  fHeader.SetColumns(aHeaderFont, aCaptions, aOffsets);

  SetLength(fColumns, fHeader.ColumnCount);
  for I := 0 to fHeader.ColumnCount - 1 do
  begin
    fColumns[I] := TKMListColumn.Create;
    fColumns[I].Font := fFont; //Reset to default font
    fColumns[I].TextAlign := taLeft; //Default alignment
  end;
end;


procedure TKMColumnBox.AddItem(aItem: TKMListRow);
begin
  Assert(fHeader.ColumnCount > 0);
  Assert(Length(aItem.Cells) = fHeader.ColumnCount);

  if fRowCount >= Length(Rows) then
    SetLength(Rows, fRowCount + 16);

  Rows[fRowCount] := aItem;

  Inc(fRowCount);
  UpdateScrollBar;
end;


procedure TKMColumnBox.Clear;
begin
  fRowCount := 0;
  fItemIndex := -1;
  UpdateScrollBar;
end;


procedure TKMColumnBox.ClearColumns;
var
  I: Integer;
begin
  for I := 0 to fHeader.ColumnCount - 1 do
    FreeAndNil(fColumns[I]);
end;


function TKMColumnBox.KeyDown(Key: Word; Shift: TShiftState): Boolean;
var
  NewIndex: Integer;
begin
  Result := ((Key = VK_UP) or (Key = VK_DOWN)) and not HideSelection;
  if inherited KeyDown(Key, Shift) then Exit;

  if HideSelection then Exit; //Can't change selection if it's hidden
  case Key of
    VK_UP:      NewIndex := fItemIndex - 1;
    VK_DOWN:    NewIndex := fItemIndex + 1;
    VK_RETURN:  begin
                  //Trigger click to hide drop downs
                  if Assigned(fOnClick) then
                    fOnClick(Self);
                  Exit;
                end;
    else        Exit;
  end;

  if InRange(NewIndex, 0, fRowCount - 1) then
  begin
    fItemIndex := NewIndex;
    if TopIndex < fItemIndex - GetVisibleRows + 1 then //Moving down
      TopIndex := fItemIndex - GetVisibleRows + 1
    else
    if TopIndex > fItemIndex then //Moving up
      TopIndex := fItemIndex;
  end;

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMColumnBox.KeyPress(Key: Char);
var
  I: Integer;
begin
  if SearchColumn = -1 then
    Exit;

  //Allow to type several characters in a row to pick some item
  if GetTimeSince(fLastKeyTime) < 1000 then
    fSearch := fSearch + Key
  else
    fSearch := Key;

  fLastKeyTime := TimeGet;

  for I := 0 to fRowCount - 1 do
  if AnsiStartsText(fSearch, Rows[I].Cells[SearchColumn].Caption) then
  begin
    fItemIndex := I;
    TopIndex := fItemIndex - GetVisibleRows div 2;
    Break;
  end;

  if Assigned(fOnChange) then
    fOnChange(Self);
end;


procedure TKMColumnBox.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;
  MouseMove(X,Y,Shift); //Will change Position and call OnChange event
end;


procedure TKMColumnBox.MouseMove(X,Y: Integer; Shift: TShiftState);
var NewIndex: Integer;
begin
  inherited;
  if  InRange(X, AbsLeft, AbsLeft + Width - fScrollBar.Width * Byte(fScrollBar.Visible))
  and InRange(Y, AbsTop + fHeader.Height*Byte(fHeader.Visible), AbsTop + fHeader.Height*Byte(fHeader.Visible) + Floor(GetVisibleRowsExact * fItemHeight) - 1) then
    fMouseOverRow := TopIndex + (Y - AbsTop - fHeader.Height * Byte(fShowHeader)) div fItemHeight
  else
    fMouseOverRow := -1;

  if (ssLeft in Shift) and (fMouseOverRow <> -1) then
  begin
    NewIndex := fMouseOverRow;

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


procedure TKMColumnBox.MouseWheel(Sender: TObject; WheelDelta: Integer);
begin
  inherited;
  SetTopIndex(TopIndex - Sign(WheelDelta));
  fScrollBar.Position := TopIndex; //Make the scrollbar move too when using the wheel
end;


procedure TKMColumnBox.DoPaintLine(aIndex: Integer; X, Y: Integer; PaintWidth: Integer);
var
  I: Integer;
  AvailWidth: Integer;
  TextSize: TKMPoint;
  Color: Cardinal;
begin
  for I := 0 to fHeader.ColumnCount - 1 do
  begin
    //Determine available width
    if I = fHeader.ColumnCount - 1 then
      AvailWidth := PaintWidth - 4 - fHeader.Columns[I].Offset - 4
    else
      AvailWidth := fHeader.Columns[I+1].Offset - fHeader.Columns[I].Offset - 4;
    //Trim the width based on our allowed PaintWidth
    AvailWidth := Min(AvailWidth, PaintWidth - fHeader.Columns[I].Offset);

    if AvailWidth <= 0 then Continue; //If the item overflows our allowed PaintWidth do not paint it

    if Rows[aIndex].Cells[I].Pic.ID <> 0 then
      TKMRenderUI.WritePicture(X + 4 + fHeader.Columns[I].Offset, Y + 1,
                             AvailWidth, fItemHeight, [],
                             Rows[aIndex].Cells[I].Pic.RX,
                             Rows[aIndex].Cells[I].Pic.ID,
                             True,
                             Rows[aIndex].Cells[I].Color);

    if Rows[aIndex].Cells[I].Caption <> '' then
      if Rows[aIndex].Cells[I].Hint <> '' then
      begin
        TextSize := fResource.Fonts.GetTextSize(Rows[aIndex].Cells[I].Caption, fFont);
        TKMRenderUI.WriteText(X + 4 + fHeader.Columns[I].Offset,
                            Y + 4,
                            AvailWidth,
                            Rows[aIndex].Cells[I].Caption,
                            fColumns[I].Font, fColumns[I].TextAlign, Rows[aIndex].Cells[I].Color);
        TKMRenderUI.WriteText(X + 4 + fHeader.Columns[I].Offset,
                            Y + fItemHeight div 2 + 1,
                            AvailWidth,
                            Rows[aIndex].Cells[I].Hint,
                            fColumns[I].HintFont, fColumns[I].TextAlign, $FFB0B0B0);
      end else
      begin
        TextSize := fResource.Fonts.GetTextSize(Rows[aIndex].Cells[I].Caption, fFont);
        if HighlightOnMouseOver and (csOver in State) and (fMouseOverRow = aIndex) then
          Color := Rows[aIndex].Cells[I].HighlightColor //Brighten(Rows[aIndex].Cells[I].Color)
        else
          Color := Rows[aIndex].Cells[I].Color;
        TKMRenderUI.WriteText(X + 4 + fHeader.Columns[I].Offset,
                            Y + (fItemHeight - TextSize.Y) div 2 + 2,
                            AvailWidth,
                            Rows[aIndex].Cells[I].Caption,
                            fColumns[I].Font, fColumns[I].TextAlign, Color);
      end;
  end;
end;


procedure TKMColumnBox.Paint;
var
  I, PaintWidth, MaxItem, Y: Integer;
begin
  inherited;

  if fScrollBar.Visible then
    PaintWidth := Width - fScrollBar.Width //Leave space for scrollbar
  else
    PaintWidth := Width; //List takes up the entire width

  fHeader.Width := PaintWidth;

  Y := AbsTop + fHeader.Height * Byte(fShowHeader);
  MaxItem := GetVisibleRows;

  TKMRenderUI.WriteBevel(AbsLeft, Y, PaintWidth, Height - fHeader.Height * Byte(fShowHeader), fEdgeAlpha, fBackAlpha);

  //Grid lines should be below selection focus
  if fShowLines then
  for I := 0 to Math.min(fRowCount - 1, MaxItem) do
    TKMRenderUI.WriteShape(AbsLeft+1, Y + I * fItemHeight - 1, PaintWidth - 2, 1, $FFBBBBBB);

  TKMRenderUI.SetupClipY(AbsTop, AbsTop + Height);

  //Selection highlight
  if not HideSelection
  and (fItemIndex <> -1)
  and InRange(ItemIndex - TopIndex, 0, MaxItem) then
  begin
    TKMRenderUI.WriteShape(AbsLeft, Y + fItemHeight * (fItemIndex - TopIndex), PaintWidth, fItemHeight, $88888888);
    TKMRenderUI.WriteOutline(AbsLeft, Y + fItemHeight * (fItemIndex - TopIndex), PaintWidth, fItemHeight, 1 + Byte(fShowLines), $FFFFFFFF);
  end;

  //Paint rows text and icons above selection for clear visibility
  for I := 0 to Math.min(fRowCount - TopIndex - 1, MaxItem) do
    DoPaintLine(TopIndex + I, AbsLeft, Y + I * fItemHeight, PaintWidth);

  TKMRenderUI.ReleaseClipY;
end;


{ TKMMenu }
constructor TKMMenu.Create(aParent: TKMPanel; aWidth: Integer);
begin
  inherited Create(aParent.MasterParent, 0, 0, aWidth, 0);

  fShapeBG := TKMShape.Create(Self, 0, 0, aParent.Width, aParent.Height);
  fShapeBG.Stretch;
  fShapeBG.OnClick := MenuHide;
  fShapeBG.Hide;

  fList := TKMColumnBox.Create(Self, 0, 0, aWidth, 0, fnt_Grey, bsMenu);
  fList.Stretch;
  fList.BackAlpha := 0.8;
  fList.Focusable := False;
  fList.SetColumns(fnt_Grey, [''], [0]);
  fList.ShowHeader := False;
  fList.OnClick := MenuClick;
  fList.Hide;

  Hide;
end;


procedure TKMMenu.Clear;
begin
  fList.Clear;
end;


function TKMMenu.GetItemIndex: Integer;
begin
  Result := fList.ItemIndex;
end;


function TKMMenu.GetItemTag(aIndex: Integer): Integer;
begin
  Result := fList.Rows[aIndex].Tag;
end;


procedure TKMMenu.SetItemIndex(aValue: Integer);
begin
  fList.ItemIndex := aValue;
end;


procedure TKMMenu.AddItem(aCaption: string; aTag: Integer = 0);
begin
  fList.AddItem(MakeListRow(aCaption, aTag));
  Height := fList.ItemHeight * fList.RowCount;
end;


procedure TKMMenu.MenuClick(Sender: TObject);
begin
  if Assigned(fOnClick) then
    fOnClick(Self);

  MenuHide(Self);
end;


procedure TKMMenu.MenuHide(Sender: TObject);
begin
  Hide;
  fList.Hide;
  fShapeBG.Hide;
end;


procedure TKMMenu.ShowAt(X, Y: Integer);
begin
  fList.AbsLeft := X;
  fList.AbsTop := Y;

  //Reset previously selected item
  fList.ItemIndex := -1;

  Show;
  fShapeBG.Show;
  fList.Show;
end;


{ TKMDropCommon }
constructor TKMDropCommon.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aStyle: TButtonStyle);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fDropCount := 10;
  fDropUp := False;
  fFont := aFont;

  fButton := TKMButton.Create(aParent, aLeft+aWidth-aHeight, aTop, aHeight, aHeight, 590, rxGui, aStyle);
  fButton.OnClick := ButtonClick;
  fButton.MakesSound := False;

  P := MasterParent;
  fShape := TKMShape.Create(P, 0, 0, P.Width, P.Height);
  fShape.Stretch; //todo: Should be stretched to whole screen, not just Panel_Main which is constrained by MENU_DESIGN_SIZE
  fShape.fOnClick := ListHide;
end;


procedure TKMDropCommon.ButtonClick(Sender: TObject);
begin
  //Call the DoDlick event to show the list AND generate DropBox.OnClick event
  DoClick(fButton.AbsLeft + fButton.Width div 2, fButton.AbsTop + fButton.Height div 2, [], mbLeft);
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


procedure TKMDropCommon.DoClick(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  //It's common behavior when click on dropbox will show the list
  ListShow(Self);

  inherited;
end;


procedure TKMDropCommon.ListClick(Sender: TObject);
begin
  if (ItemIndex <> -1) then
    if Assigned(fOnChange) then fOnChange(Self);
  ListHide(nil);
end;


procedure TKMDropCommon.ListChange(Sender: TObject);
begin
  if (ItemIndex <> -1) then
    if Assigned(fOnChange) then fOnChange(Self);
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


procedure TKMDropCommon.SetTop(aValue: Integer);
begin
  inherited;
  //Stick the button to us
  fButton.Top := Top;
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
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);
end;


{ TKMDropList }
constructor TKMDropList.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string; aStyle: TButtonStyle);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aFont, aStyle);

  fDefaultCaption := aDefaultCaption;

  P := MasterParent;

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fList := TKMListBox.Create(P, AbsLeft-P.AbsLeft, AbsTop+aHeight-P.AbsTop, aWidth, 0, fFont, aStyle);
  fList.Height := fList.ItemHeight * fDropCount;
  fList.AutoHideScrollBar := True; //A drop box should only have a scrollbar if required
  fList.BackAlpha := 0.85;
  fList.fOnClick := ListClick;
  fList.fOnChange := ListChange;

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


procedure TKMDropList.ListChange(Sender: TObject);
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
      fList.AbsTop := AbsTop - fList.Height
    else
      fList.AbsTop := AbsTop + Height;
  end;
end;


procedure TKMDropList.Add(aItem: string; aTag: Integer=0);
begin
  fList.Add(aItem, aTag);
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


procedure TKMDropList.SelectByTag(aTag: Integer);
var i: Integer;
begin
  fList.ItemIndex := -1;
  for i:=0 to fList.Count-1 do
    if fList.ItemTags[i] = aTag then
      SetItemIndex(i);
end;


function TKMDropList.GetTag(aIndex: Integer): Integer;
begin
  Result := fList.ItemTags[aIndex];
end;


function TKMDropList.GetSelectedTag: Integer;
begin
  Result := GetTag(fList.fItemIndex);
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
  TKMRenderUI.WriteText(AbsLeft+4, AbsTop+4, Width-8, fCaption, fFont, taLeft, Col);
end;


{ TKMDropColumns }
constructor TKMDropColumns.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer; aFont: TKMFont; aDefaultCaption: string; aStyle: TButtonStyle);
var P: TKMPanel;
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight, aFont, aStyle);

  fDefaultCaption := aDefaultCaption;

  P := MasterParent;

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fList := TKMColumnBox.Create(P, AbsLeft-P.AbsLeft, AbsTop+aHeight-P.AbsTop, aWidth, 0, fFont, aStyle);
  fList.BackAlpha := 0.85;
  fList.OnClick := ListClick;
  fList.OnChange := ListChange;

  DropWidth := aWidth;

  ListHide(nil);
end;


procedure TKMDropColumns.ListShow(Sender: TObject);
begin
  inherited;
  if ListVisible or (Count < 1) then Exit;

  UpdateDropPosition;

  //Make sure the selected item is on top of the list when it's opened
  if ItemIndex <> -1 then
    fList.TopIndex := ItemIndex;

  fList.Show;
end;


procedure TKMDropColumns.ListClick(Sender: TObject);
begin
  inherited;
end;


procedure TKMDropColumns.ListChange(Sender: TObject);
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


procedure TKMDropColumns.SetDropWidth(aDropWidth: Integer);
begin
  fList.AbsLeft := AbsLeft + Width - aDropWidth;
  fList.Width := aDropWidth;
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
    fList.Height := Math.min(fDropCount, fList.RowCount) * fList.ItemHeight + fList.Header.Height * Byte(fList.ShowHeader);
    if fDropUp then
      fList.AbsTop := AbsTop - fList.Height
    else
      fList.AbsTop := AbsTop + Height;
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
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);
  if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;

  if ItemIndex <> -1 then
    fList.DoPaintLine(ItemIndex, AbsLeft, AbsTop, Width - fButton.Width)

    {TKMRenderUI.WritePicture(Left + 4, Top + 1,
                            fList.Rows[ItemIndex].Cells[0].Pic.RX,
                            fList.Rows[ItemIndex].Cells[0].Pic.ID,
                            fList.Rows[ItemIndex].Cells[0].Color,
                            fEnabled or not FadeImageWhenDisabled)}
  else
    TKMRenderUI.WriteText(AbsLeft + 4, AbsTop + 4, Width - 8 - fButton.Width, fDefaultCaption, fFont, taLeft, Col);
end;


{ TKMDropColorBox }
constructor TKMDropColors.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight,aCount: Integer);
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
  fShape := TKMShape.Create(P, 0, 0, P.Width, P.Height);
  fShape.fOnClick := ListHide;

  Size := Round(Sqrt(aCount)+0.5); //Round up

  //In FullScreen mode P initialized already with offset (P.Top <> 0)
  fSwatch := TKMColorSwatch.Create(P, AbsLeft-P.AbsLeft, AbsTop+aHeight-P.AbsTop, Size, Size, aWidth div Size);
  fSwatch.BackAlpha := 0.75;
  fSwatch.fOnClick := ListClick;

  ListHide(nil);
end;


procedure TKMDropColors.ListShow(Sender: TObject);
begin
  if fSwatch.Visible then
  begin
    ListHide(nil);
    exit;
  end;

  fSwatch.Show;
  fShape.Show;
end;


procedure TKMDropColors.ListClick(Sender: TObject);
begin
  fColorIndex := fSwatch.ColorIndex;
  if Assigned(fOnChange) then fOnChange(Self);
  ListHide(nil);
end;


procedure TKMDropColors.ListHide(Sender: TObject);
begin
  fSwatch.Hide;
  fShape.Hide;
end;


procedure TKMDropColors.SetEnabled(aValue: Boolean);
begin
  inherited;
  fButton.Enabled := aValue;
  fSwatch.Enabled := aValue;
end;


//Set ColorIndex to fSwatch as well since it holds the actual color that we use on Paint
procedure TKMDropColors.SetColorIndex(aIndex: Integer);
begin
  fColorIndex := aIndex;
  fSwatch.ColorIndex := aIndex;
end;


procedure TKMDropColors.SetColors(const aColors: array of TColor4; aRandomCaption: string = '');
begin
  //Store local copy of flag to substitute 0 color with "Random" text
  fRandomCaption := aRandomCaption;
  fSwatch.SetColors(aColors, (fRandomCaption <> ''));
end;


procedure TKMDropColors.Paint;
var Col: TColor4;
begin
  inherited;
  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width-fButton.Width, Height);
  TKMRenderUI.WriteShape(AbsLeft+2, AbsTop+1, Width-fButton.Width-3, Height-2, fSwatch.GetColor);
  if (fRandomCaption <> '') and (fSwatch.ColorIndex = 0) then
  begin
    if fEnabled then Col:=$FFFFFFFF else Col:=$FF888888;
    TKMRenderUI.WriteText(AbsLeft+4, AbsTop+3, 0, fRandomCaption, fnt_Metal, taLeft, Col);
  end;
end;


{ TKMMinimap }
constructor TKMMinimapView.Create(aParent: TKMPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  //Radius of circle around player location
  fLocRad := 8;
end;


procedure TKMMinimapView.SetMinimap(aMinimap: TKMMinimap);
begin
  fMinimap := aMinimap;

  if fMinimap.MapX > fMinimap.MapY then
  begin
    fPaintWidth := Width;
    fPaintHeight := Round(Height * fMinimap.MapY / Max(fMinimap.MapX, 1)); // X could = 0
    fLeftOffset := 0;
    fTopOffset := (Height - fPaintHeight) div 2;
  end
  else
  begin
    fPaintWidth := Round(Width * fMinimap.MapX / Max(fMinimap.MapY, 1)); // Y could = 0
    fPaintHeight := Height;
    fLeftOffset := (Width - fPaintWidth) div 2;
    fTopOffset := 0;
  end;
end;


procedure TKMMinimapView.SetViewport(aViewport: TViewport);
begin
  fView := aViewport;
end;


function TKMMinimapView.LocalToMapCoords(X,Y: Integer; const Inset: ShortInt = 0): TKMPoint;
begin
  Assert(Inset >= -1, 'Min allowed inset is -1, to be within TKMPoint range of 0..n');
  Result.X := EnsureRange(Round((X - AbsLeft - fLeftOffset) * fMinimap.MapX / fPaintWidth),  1+Inset, fMinimap.MapX-Inset);
  Result.Y := EnsureRange(Round((Y - AbsTop  - fTopOffset ) * fMinimap.MapY / fPaintHeight), 1+Inset, fMinimap.MapY-Inset);
end;


function TKMMinimapView.MapCoordsToLocal(X,Y: Single; const Inset: ShortInt = 0): TKMPoint;
begin
  Assert(Inset >= -1, 'Min allowed inset is -1, to be within TKMPoint range of 0..n');
  Result.X := AbsLeft + fLeftOffset + EnsureRange(Round(X * fPaintWidth /  fMinimap.MapX), Inset, fPaintWidth  - Inset);
  Result.Y := AbsTop  + fTopOffset  + EnsureRange(Round(Y * fPaintHeight / fMinimap.MapY), Inset, fPaintHeight - Inset);
end;


procedure TKMMinimapView.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  inherited;

  MouseMove(X,Y,Shift);
end;


procedure TKMMinimapView.MouseMove(X,Y: Integer; Shift: TShiftState);
var ViewPos: TKMPoint;
begin
  inherited;

  if (ssLeft in Shift) and not fClickableOnce then
  begin
    ViewPos := LocalToMapCoords(X,Y);
    if Assigned(fOnChange) then
      fOnChange(Self, ViewPos.X, ViewPos.Y);
  end;
end;


procedure TKMMinimapView.MouseUp(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
var
  ViewPos: TKMPoint;
  I: Integer;
  T: TKMPoint;
begin
  inherited;

  if fClickableOnce then
  begin
    fClickableOnce := False; //Not clickable anymore
    ViewPos := LocalToMapCoords(X,Y);
    if Assigned(fOnMinimapClick) then
      fOnMinimapClick(Self, ViewPos.X, ViewPos.Y);
  end;

  if fShowLocs then
  for I := 0 to MAX_PLAYERS - 1 do
  if fMinimap.PlayerShow[I] and not KMSamePoint(fMinimap.PlayerLoc[I], KMPoint(0,0)) then
  begin
    T := MapCoordsToLocal(fMinimap.PlayerLoc[I].X, fMinimap.PlayerLoc[I].Y, fLocRad);
    if Sqr(T.X - X) + Sqr(T.Y - Y) < Sqr(fLocRad) then
    begin
      if Assigned(OnLocClick) then
        OnLocClick(I);

      //Do not repeat events for stacked locations
      Break;
    end;
  end;
end;


procedure TKMMinimapView.Paint;
const
  ALERT_RAD = 4;
var
  I,K: Integer;
  R: TKMRect;
  T, T1, T2: TKMPoint;
begin
  inherited;

  if (fMinimap = nil) or (fMinimap.MapX * fMinimap.MapY = 0) then
    Exit;

  if (fMinimap.MapTex.Tex <> 0) then
    TKMRenderUI.WriteTexture(AbsLeft + fLeftOffset, AbsTop + fTopOffset, fPaintWidth, fPaintHeight, fMinimap.MapTex, $FFFFFFFF)
  else
    TKMRenderUI.WriteBevel(AbsLeft, AbsTop, fWidth, fHeight);

  //Alerts (under viewport rectangle)
  if (fMinimap.Alerts <> nil) then
  for I := 0 to fMinimap.Alerts.Count - 1 do
  if fMinimap.Alerts[I].VisibleMinimap then
  begin
    T := MapCoordsToLocal(fMinimap.Alerts[I].Loc.X, fMinimap.Alerts[I].Loc.Y, ALERT_RAD);
    TKMRenderUI.WritePicture(T.X, T.Y, 0, 0, [],
                             fMinimap.Alerts[I].TexMinimap.RX, fMinimap.Alerts[I].TexMinimap.ID,
                             True, fMinimap.Alerts[I].TeamColor, Abs((TimeGet mod 1000) / 500 - 1));
  end;

  //Viewport rectangle
  if fView <> nil then
  begin
    R := fView.GetMinimapClip;
    if (R.Right - R.Left) * (R.Bottom - R.Top) > 0 then
      TKMRenderUI.WriteOutline(AbsLeft + fLeftOffset + Round(R.Left*fPaintWidth / fMinimap.MapX),
                               AbsTop  + fTopOffset  + Round(R.Top*fPaintHeight / fMinimap.MapY),
                               Round((R.Right - R.Left)*fPaintWidth / fMinimap.MapX),
                               Round((R.Bottom - R.Top)*fPaintHeight / fMinimap.MapY), 1, $FFFFFFFF);
  end;

  if not fShowLocs then Exit;

  //Connect allied players
  for I := 0 to MAX_PLAYERS - 1 do
  if fMinimap.PlayerShow[I] and not KMSamePoint(fMinimap.PlayerLoc[I], KMPoint(0,0)) then
    for K := I + 1 to MAX_PLAYERS - 1 do
    if fMinimap.PlayerShow[K] and not KMSamePoint(fMinimap.PlayerLoc[K], KMPoint(0,0)) then
      if (fMinimap.PlayerTeam[I] <> 0) and (fMinimap.PlayerTeam[I] = fMinimap.PlayerTeam[K]) then
      begin
        T1 := MapCoordsToLocal(fMinimap.PlayerLoc[I].X, fMinimap.PlayerLoc[I].Y, fLocRad);
        T2 := MapCoordsToLocal(fMinimap.PlayerLoc[K].X, fMinimap.PlayerLoc[K].Y, fLocRad);
        TKMRenderUI.WriteLine(T1.X, T1.Y, T2.X, T2.Y, $FFFFFFFF);
      end;

  //Draw all the circles, THEN all the numbers so the numbers are not covered by circles when they are close
  for I := 0 to MAX_PLAYERS - 1 do
  if fMinimap.PlayerShow[I] and not KMSamePoint(fMinimap.PlayerLoc[I], KMPoint(0,0)) then
  begin
    T := MapCoordsToLocal(fMinimap.PlayerLoc[I].X, fMinimap.PlayerLoc[I].Y, fLocRad);
    TKMRenderUI.WriteCircle(T.X, T.Y, fLocRad, fMinimap.PlayerColors[I]);
  end;

  for I := 0 to MAX_PLAYERS - 1 do
  if fMinimap.PlayerShow[I] and not KMSamePoint(fMinimap.PlayerLoc[I], KMPoint(0,0)) then
  begin
    T := MapCoordsToLocal(fMinimap.PlayerLoc[I].X, fMinimap.PlayerLoc[I].Y, fLocRad);
    TKMRenderUI.WriteText(T.X, T.Y - 6, 0, IntToStr(I+1), fnt_Outline, taCenter);
  end;
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

  TKMRenderUI.Write3DButton(AbsLeft, AbsTop, Width, Height, rxGui, 0, $FFFF00FF, StateSet, bsGame);
end;


{ TKMChart }
constructor TKMChart.Create(aParent: TKMPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited Create(aParent, aLeft, aTop, aWidth, aHeight);

  fFont := fnt_Outline;
  fItemHeight := 13;
  fLineOver := -1;
  fLegendWidth := 150;
end;


procedure TKMChart.AddLine(aTitle: string; aColor: TColor4; const aValues: TKMCardinalArray; aTag:Integer=-1);
begin
  if fMaxLength = 0 then Exit;

  //Make sure there is enough Values to copy to local storage with Move procedure
  Assert(Length(aValues) >= fMaxLength);

  SetLength(fLines, fCount + 1);

  fLines[fCount].Color := aColor;
  fLines[fCount].Title := aTitle;
  fLines[fCount].Tag := aTag;
  fLines[fCount].Visible := True;
  SetLength(fLines[fCount].Values, fMaxLength);
  if SizeOf(aValues) <> 0 then
    Move(aValues[0], fLines[fCount].Values[0], SizeOf(aValues[0]) * fMaxLength);
  Inc(fCount);

  UpdateMaxValue;
end;


//Add alternative values line (e.g. wares count vs. wares produced)
procedure TKMChart.AddAltLine(const aAltValues: TKMCardinalArray);
begin
  Assert(Length(aAltValues) >= fMaxLength);

  SetLength(fLines[fCount-1].ValuesAlt, fMaxLength);
  if SizeOf(aAltValues) <> 0 then
    Move(aAltValues[0], fLines[fCount-1].ValuesAlt[0], SizeOf(aAltValues[0]) * fMaxLength);

  UpdateMaxValue;
end;


//Trims the graph until 5% before the first variation
procedure TKMChart.TrimToFirstVariation;
var
  I, K, FirstVarSample: Integer;
  StartVal: Cardinal;
begin
  FirstVarSample := -1;
  for I:=0 to fCount-1 do
    if Length(fLines[I].Values) > 0 then
    begin
      StartVal := fLines[I].Values[0];
      for K:=1 to Length(fLines[I].Values)-1 do
        if fLines[I].Values[K] <> StartVal then
        begin
          if (K < FirstVarSample) or (FirstVarSample = -1) then
            FirstVarSample := K;
          Break;
        end;
    end;
  if FirstVarSample = -1 then Exit; //No variation at all, so don't trim it
  //Take 5% before the first varied sample
  FirstVarSample := Max(0, FirstVarSample - Round(0.05*fMaxLength));
  //Trim all fLines[I].Values to start at FirstVarSample
  for I:=0 to fCount-1 do
  begin
    Move(fLines[I].Values[FirstVarSample], fLines[I].Values[0], (Length(fLines[I].Values)-FirstVarSample)*SizeOf(fLines[I].Values[0]));
    SetLength(fLines[I].Values, Length(fLines[I].Values)-FirstVarSample);
  end;
  //Set start time so the horizontal time ticks are rendered correctly
  fMinTime := Round((FirstVarSample/fMaxLength) * fMaxTime);
  //All lines have now been trimmed, so update fMaxLength
  fMaxLength := fMaxLength - FirstVarSample;
end;


procedure TKMChart.Clear;
begin
  fCount := 0;
  SetLength(fLines, 0);
  fMaxValue := 0;
end;


procedure TKMChart.SetLineVisible(aLineID: Integer; aVisible: Boolean);
begin
  fLines[aLineID].Visible := aVisible;
  UpdateMaxValue;
end;


procedure TKMChart.UpdateMaxValue;
var I, K: Integer;
begin
  fMaxValue := 0;
  for I := 0 to fCount - 1 do
    if fLines[I].Visible then
      for K := 0 to fMaxLength - 1 do
        if fLines[I].Values[K] > fMaxValue then
          fMaxValue := fLines[I].Values[K];
end;


function TKMChart.GetLine(aIndex: Integer): TKMGraphLine;
begin
  Result := fLines[aIndex];
end;


procedure TKMChart.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited;

  fLineOver := -1;
  if X < AbsLeft + Width - fLegendWidth+5 then Exit;
  fLineOver := (Y - AbsTop + 2) div fItemHeight;
end;


procedure TKMChart.MouseUp(X, Y: Integer; Shift: TShiftState; Button: TMouseButton);
var I: Integer;
begin
  inherited;

  if X < AbsLeft + Width - fLegendWidth+5 then Exit;

  I := (Y - AbsTop + 2) div fItemHeight;
  if not InRange(I, 0, fCount - 1) then Exit;

  fLines[I].Visible := not fLines[I].Visible;

  UpdateMaxValue;
end;


procedure TKMChart.Paint;
  function EnsureColorBlend(aColor: TColor4): TColor4;
  var
    R, G, B: Byte;
    Hue, Sat, Bri: Single;
  begin
    ConvertRGB2HSB(aColor and $FF, aColor shr 8 and $FF, aColor shr 16 and $FF, Hue, Sat, Bri);
    //Lighten colors to ensure they are visible on black background
    Bri := Max(Bri, 0.2);
    ConvertHSB2RGB(Hue, Sat, Bri, R, G, B);
    Result := (R + G shl 8 + B shl 16) or $FF000000;
  end;
const
  IntervalCount: array [0..9] of Word = (1, 5, 10, 50, 100, 500, 1000, 5000, 10000, 50000);
  IntervalTime: array [0..10] of Word = (30, 1*60, 5*60, 10*60, 15*60, 30*60, 1*60*60, 2*60*60, 3*60*60, 4*60*60, 5*60*60);
var
  I: Integer;
  G: TKMRect;
  Best, TopValue, Tmp: Integer;
  NewColor, TextColor: TColor4;
begin
  inherited;

  G := KMRect(AbsLeft + 40, AbsTop, AbsLeft + Width - fLegendWidth, AbsTop + Height - 20);

  //Add margin to MaxValue so that it does not blends with upper border
  TopValue := Max(Round(fMaxValue * 1.1), fMaxValue + 1);

  //Find first interval that will have less than 10 ticks
  Best := 0;
  for I := Low(IntervalCount) to High(IntervalCount) do
    if TopValue div IntervalCount[I] < 10 then
    begin
      Best := IntervalCount[I];
      Break;
    end;

  //Dashed lines in the background
  if Best <> 0 then
  for I := 1 to (TopValue div Best) do
  begin
    Tmp := G.Top + Round((1 - I * Best / TopValue) * (G.Bottom - G.Top));
    TKMRenderUI.WriteText(G.Left - 5, Tmp - 6, 0, IntToStr(I * Best), fnt_Game, taRight);
    TKMRenderUI.WriteLine(G.Left, Tmp, G.Right, Tmp, $FF606060, $CCCC);
  end;

  //Charts and legend
  for I := 0 to fCount - 1 do
  begin
    //Adjust the color if it blends with black background
    NewColor := EnsureColorBlend(fLines[I].Color);
    TextColor := $FFFFFFFF;

    if (csOver in State) and (I = fLineOver) then
    begin
      NewColor := $FFFF00FF;
      TextColor := $FFFF00FF;
    end;

    //Charts
    if fLines[I].Visible then
    begin
      TKMRenderUI.WritePlot(G.Left, G.Top, G.Right-G.Left, G.Bottom-G.Top, fLines[I].Values, TopValue, NewColor, 2);
      if Length(fLines[I].ValuesAlt) > 0 then
        TKMRenderUI.WritePlot(G.Left, G.Top, G.Right-G.Left, G.Bottom-G.Top, fLines[I].ValuesAlt, TopValue, NewColor, 1);
    end;

    //Checkboxes
    TKMRenderUI.WriteShape(G.Right + 5, G.Top - 2 + I*fItemHeight+2, 11, 11, NewColor);
    if fLines[I].Visible then
      TKMRenderUI.WriteText(G.Right + 5, G.Top - 2 + I*fItemHeight - 1, 0, 'v', fnt_Game, taLeft, $FF000000);

    //Legend
    TKMRenderUI.WriteText(G.Right + 18, G.Top - 2 + I*fItemHeight, 0, fLines[I].Title, fnt_Game, taLeft, TextColor);
  end;

  //Render the highlighted line above all the others and thicker so you can see where it goes under others
  if (csOver in State) and InRange(fLineOver, 0, fCount-1) and fLines[fLineOver].Visible then
    TKMRenderUI.WritePlot(G.Left, G.Top, G.Right-G.Left, G.Bottom-G.Top, fLines[fLineOver].Values, TopValue, $FFFF00FF, 3);

  //Outline
  TKMRenderUI.WriteOutline(G.Left, G.Top, G.Right-G.Left, G.Bottom-G.Top, 1, $FFFFFFFF);

  //Title
  TKMRenderUI.WriteText(G.Left + 5, G.Top + 5, 0, fCaption, fFont, taLeft);

  //Render vertical axis captions
  TKMRenderUI.WriteText(G.Left - 5, G.Bottom - 6, 0, IntToStr(0), fnt_Game, taRight);
  //TKMRenderUI.WriteText(Left+20, Top + 20, 0, 0, IntToStr(fMaxValue), fnt_Game, taRight);

  //Render horizontal axis ticks
  //Find first interval that will have less than 10 ticks
  Best := 0;
  for I := Low(IntervalTime) to High(IntervalTime) do
    if (fMaxTime-fMinTime) div IntervalTime[I] < 6 then
    begin
      Best := IntervalTime[I];
      Break;
    end;

  //Paint time axis labels
  if (Best <> 0) and (fMaxTime <> fMinTime) then
  for I := Ceil(fMinTime / Best) to (fMaxTime div Best) do
  begin
    TKMRenderUI.WriteShape(G.Left + Round((I * Best - fMinTime) / (fMaxTime-fMinTime) * (G.Right - G.Left)), G.Bottom - 2, 2, 5, $FFFFFFFF);
    TKMRenderUI.WriteText (G.Left + Round((I * Best - fMinTime) / (fMaxTime-fMinTime) * (G.Right - G.Left)), G.Bottom + 4, 0, TimeToString((I * Best) / 24 / 60 / 60), fnt_Game, taLeft);
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
end;


destructor TKMMasterControl.Destroy;
begin
  fCtrl.Free; //Will destroy all its childs as well

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


//Update focused control
procedure TKMMasterControl.UpdateFocus(aSender: TKMControl);
  function FindFocusable(C: TKMPanel): Boolean;
  var I: Integer;
  begin
    Result := False;

    //Check for focusable controls
    for I := 1 to C.ChildCount do
    if C.Childs[I].fVisible
    and C.Childs[I].Enabled
    and C.Childs[I].Focusable then
    begin
      CtrlFocus := C.Childs[I];
      Result := True;
      Exit;
    end;

    for I := 1 to C.ChildCount do
    if C.Childs[I].fVisible
    and C.Childs[I].Enabled
    and (C.Childs[I] is TKMPanel) then
    begin
      Result := FindFocusable(TKMPanel(C.Childs[I]));
      if Result then Exit;
    end;
  end;
begin
  //Something showed up
  if aSender.Visible then
  begin
    //If something showed up - focus on it
    if not (aSender is TKMPanel) and aSender.Focusable then
      CtrlFocus := aSender;
    //If panel showed up - try to focus on its contents
    if aSender is TKMPanel then
      FindFocusable(TKMPanel(aSender));
  end
  else
  //Something went hidden
  begin
    if (CtrlFocus = nil) or not CtrlFocus.Visible or not CtrlFocus.Enabled then
    begin
      //If something went hidden
      CtrlFocus := nil;
      FindFocusable(fCtrl);
    end;
  end;
end;


{ Recursing function to find topmost control (excl. Panels)}
function TKMMasterControl.HitControl(X,Y: Integer; aIncludeDisabled: Boolean=false): TKMControl;
  function ScanChild(P: TKMPanel; aX,aY: Integer): TKMControl;
  var I: Integer;
  begin
    Result := nil;
    //Process controls in reverse order since last created are on top
    for I := P.ChildCount downto 1 do
    if P.Childs[I].fVisible then //If we can't see it, we can't touch it
    begin
      //Scan Panels childs first, if none is hit - hittest the panel
      if (P.Childs[I] is TKMPanel) then
      begin
        Result := ScanChild(TKMPanel(P.Childs[I]),aX,aY);
        if Result <> nil then exit;
      end;
      if P.Childs[I].HitTest(aX, aY, aIncludeDisabled) then
      begin
        Result := P.Childs[I];
        Exit;
      end;
    end;
  end;
begin
  Result := ScanChild(fCtrl, X, Y);
end;


function TKMMasterControl.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  //CtrlFocus could be on another menu page and no longer visible
  if (CtrlFocus <> nil) and CtrlFocus.Visible then
    Result := CtrlFocus.KeyDown(Key, Shift)
  else
    Result := false;
end;


procedure TKMMasterControl.KeyPress(Key: Char);
begin
  //CtrlFocus could be on another menu page and no longer visible
  if (CtrlFocus <> nil) and CtrlFocus.Visible then
    CtrlFocus.KeyPress(Key);
end;


function TKMMasterControl.KeyUp(Key: Word; Shift: TShiftState): Boolean;
begin
  //CtrlFocus could be on another menu page and no longer visible
  if (CtrlFocus <> nil) and CtrlFocus.Visible then
    Result := CtrlFocus.KeyUp(Key, Shift)
  else
    Result := false;
end;


procedure TKMMasterControl.MouseDown(X,Y: Integer; Shift: TShiftState; Button: TMouseButton);
begin
  CtrlDown := HitControl(X,Y);
  if CtrlDown <> nil then
    CtrlDown.MouseDown(X, Y, Shift, Button);
end;


procedure TKMMasterControl.MouseMove(X,Y: Integer; Shift: TShiftState);
var HintControl: TKMControl;
begin
  CtrlOver := HitControl(X,Y);

  //User is dragging some Ctrl (e.g. scrollbar) and went away from Ctrl bounds
  if CtrlDown <> nil then
    CtrlDown.MouseMove(X, Y, Shift);

  //The Game hides cursor when using DirectionSelector, don't spoil it
  if fResource.Cursors.Cursor <> kmc_Invisible then
    if CtrlOver is TKMEdit then
      fResource.Cursors.Cursor := kmc_Edit
    else
    if CtrlOver is TKMDragger then
      fResource.Cursors.Cursor := kmc_DragUp
    else
      if fResource.Cursors.Cursor in [kmc_Edit, kmc_DragUp] then
        fResource.Cursors.Cursor := kmc_Default; //Reset the cursor from these two special cursors

  HintControl := HitControl(X, Y, True); //Include disabled controls
  if (CtrlDown = nil) and (HintControl <> nil) and Assigned(fOnHint) then
    fOnHint(HintControl);
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

  if CtrlUp <> nil then
    CtrlUp.MouseUp(X, Y, Shift, Button);

  //Do not place any code here, we could have Exited in OnClick event
end;


procedure TKMMasterControl.MouseWheel(X,Y: Integer; WheelDelta: Integer);
var C: TKMControl;
begin
  C := HitControl(X, Y);
  if C <> nil then C.MouseWheel(C, WheelDelta);
end;


{Paint controls}
{Leave painting of childs to their parent control}
procedure TKMMasterControl.Paint;
begin
  CtrlPaintCount := 0;
  fCtrl.Paint;

  if MODE_DESIGN_CONTORLS and (CtrlFocus <> nil) then
    TKMRenderUI.WriteText(CtrlFocus.AbsLeft, CtrlFocus.AbsTop-14, 0, inttostr(CtrlFocus.AbsLeft)+':'+inttostr(CtrlFocus.AbsTop), fnt_Grey, taLeft);
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
