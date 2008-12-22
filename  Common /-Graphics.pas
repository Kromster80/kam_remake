
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1995-2001 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit Graphics;

{$P+,S-,W-,R-,T-,X+,H+,B-}
{$C PRELOAD}

interface

uses Windows, Types, SysUtils, Classes;

{ Graphics Objects }

type
  PColor = ^TColor;
  TColor = -$7FFFFFFF-1..$7FFFFFFF;
  {$NODEFINE TColor}

  (*$HPPEMIT 'namespace Graphics'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '  enum TColor {clMin=-0x7fffffff-1, clMax=0x7fffffff};'*)
  (*$HPPEMIT '}'*)


const
  clScrollBar = TColor(COLOR_SCROLLBAR or $80000000);
  clBackground = TColor(COLOR_BACKGROUND or $80000000);
  clActiveCaption = TColor(COLOR_ACTIVECAPTION or $80000000);
  clInactiveCaption = TColor(COLOR_INACTIVECAPTION or $80000000);
  clMenu = TColor(COLOR_MENU or $80000000);
  clWindow = TColor(COLOR_WINDOW or $80000000);
  clWindowFrame = TColor(COLOR_WINDOWFRAME or $80000000);
  clMenuText = TColor(COLOR_MENUTEXT or $80000000);
  clWindowText = TColor(COLOR_WINDOWTEXT or $80000000);
  clCaptionText = TColor(COLOR_CAPTIONTEXT or $80000000);
  clActiveBorder = TColor(COLOR_ACTIVEBORDER or $80000000);
  clInactiveBorder = TColor(COLOR_INACTIVEBORDER or $80000000);
  clAppWorkSpace = TColor(COLOR_APPWORKSPACE or $80000000);
  clHighlight = TColor(COLOR_HIGHLIGHT or $80000000);
  clHighlightText = TColor(COLOR_HIGHLIGHTTEXT or $80000000);
  clBtnFace = TColor(COLOR_BTNFACE or $80000000);
  clBtnShadow = TColor(COLOR_BTNSHADOW or $80000000);
  clGrayText = TColor(COLOR_GRAYTEXT or $80000000);
  clBtnText = TColor(COLOR_BTNTEXT or $80000000);
  clInactiveCaptionText = TColor(COLOR_INACTIVECAPTIONTEXT or $80000000);
  clBtnHighlight = TColor(COLOR_BTNHIGHLIGHT or $80000000);
  cl3DDkShadow = TColor(COLOR_3DDKSHADOW or $80000000);
  cl3DLight = TColor(COLOR_3DLIGHT or $80000000);
  clInfoText = TColor(COLOR_INFOTEXT or $80000000);
  clInfoBk = TColor(COLOR_INFOBK or $80000000);
  clGradientActiveCaption = TColor(COLOR_GRADIENTACTIVECAPTION or $80000000);
  clGradientInactiveCaption = TColor(COLOR_GRADIENTINACTIVECAPTION or $80000000);

  clBlack = TColor($000000);
  clMaroon = TColor($000080);
  clGreen = TColor($008000);
  clOlive = TColor($008080);
  clNavy = TColor($800000);
  clPurple = TColor($800080);
  clTeal = TColor($808000);
  clGray = TColor($808080);
  clSilver = TColor($C0C0C0);
  clRed = TColor($0000FF);
  clLime = TColor($00FF00);
  clYellow = TColor($00FFFF);
  clBlue = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua = TColor($FFFF00);
  clLtGray = TColor($C0C0C0);
  clDkGray = TColor($808080);
  clWhite = TColor($FFFFFF);
  StandardColorsCount = 16;

  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);
  ExtendedColorsCount = 4;

  clNone = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

  //clUnused = TColor($D4D4D4);

const
  cmBlackness = BLACKNESS;
  cmDstInvert = DSTINVERT;
  cmMergeCopy = MERGECOPY;
  cmMergePaint = MERGEPAINT;
  cmNotSrcCopy = NOTSRCCOPY;
  cmNotSrcErase = NOTSRCERASE;
  cmPatCopy = PATCOPY;
  cmPatInvert = PATINVERT;
  cmPatPaint = PATPAINT;
  cmSrcAnd = SRCAND;
  cmSrcCopy = SRCCOPY;
  cmSrcErase = SRCERASE;
  cmSrcInvert = SRCINVERT;
  cmSrcPaint = SRCPAINT;
  cmWhiteness = WHITENESS;

type
  {$EXTERNALSYM HMETAFILE}
  HMETAFILE = THandle;
  {$EXTERNALSYM HENHMETAFILE}
  HENHMETAFILE = THandle;

  EInvalidGraphic = class(Exception);
  EInvalidGraphicOperation = class(Exception);

  TGraphic = class;
  TBitmap = class;
  TIcon = class;
  TMetafile = class;

  TResData = record
    Handle: THandle;
  end;

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = type string;
  TFontCharset = 0..255;

  { Changes to the following types should be reflected in the $HPPEMIT directives. }

  TFontDataName = string[LF_FACESIZE - 1];
  {$NODEFINE TFontDataName}
  TFontStyle = (fsBold, fsItalic, fsUnderline, fsStrikeOut);
  {$NODEFINE TFontStyle}
  TFontStyles = set of TFontStyle;
  TFontStylesBase = set of TFontStyle;
  {$NODEFINE TFontStylesBase}

  (*$HPPEMIT 'namespace Graphics'*)
  (*$HPPEMIT '{'*)
  (*$HPPEMIT '  enum TFontStyle { fsBold, fsItalic, fsUnderline, fsStrikeOut };'*)
  (*$HPPEMIT '  typedef SmallStringBase<31> TFontDataName;'*)
  (*$HPPEMIT '  typedef SetBase<TFontStyle, fsBold, fsStrikeOut> TFontStylesBase;'*)
  (*$HPPEMIT '}'*)

  TFontData = record
    Handle: HFont;
    Height: Integer;
    Pitch: TFontPitch;
    Style: TFontStylesBase;
    Charset: TFontCharset;
    Name: TFontDataName;
  end;

  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame);
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
    pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
    pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);

  TPenData = record
    Handle: HPen;
    Color: TColor;
    Width: Integer;
    Style: TPenStyle;
  end;

  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

  TBrushData = record
    Handle: HBrush;
    Color: TColor;
    Bitmap: TBitmap;
    Style: TBrushStyle;
  end;

  PResource = ^TResource;
  TResource = record
    Next: PResource;
    RefCount: Integer;
    Handle: THandle;
    HashCode: Word;
    case Integer of
      0: (Data: TResData);
      1: (Font: TFontData);
      2: (Pen: TPenData);
      3: (Brush: TBrushData);
  end;

  TGraphicsObject = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FResource: PResource;
    FOwnerLock: PRTLCriticalSection;
  protected
    procedure Changed; dynamic;
    procedure Lock;
    procedure Unlock;
  public
    function HandleAllocated: Boolean;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OwnerCriticalSection: PRTLCriticalSection read FOwnerLock write FOwnerLock;
  end;

  IChangeNotifier = interface
    ['{1FB62321-44A7-11D0-9E93-0020AF3D82DA}']
    procedure Changed;
  end;

  TFont = class(TGraphicsObject)
  private
    FColor: TColor;
    FPixelsPerInch: Integer;
    FNotify: IChangeNotifier;
    procedure GetData(var FontData: TFontData);
    procedure SetData(const FontData: TFontData);
  protected
    procedure Changed; override;
    function GetHandle: HFont;
    function GetHeight: Integer;
    function GetName: TFontName;
    function GetPitch: TFontPitch;
    function GetSize: Integer;
    function GetStyle: TFontStyles;
    function GetCharset: TFontCharset;
    procedure SetColor(Value: TColor);
    procedure SetHandle(Value: HFont);
    procedure SetHeight(Value: Integer);
    procedure SetName(const Value: TFontName);
    procedure SetPitch(Value: TFontPitch);
    procedure SetSize(Value: Integer);
    procedure SetStyle(Value: TFontStyles);
    procedure SetCharset(Value: TFontCharset);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FontAdapter: IChangeNotifier read FNotify write FNotify;
    property Handle: HFont read GetHandle write SetHandle;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
  published
    property Charset: TFontCharset read GetCharset write SetCharset;
    property Color: TColor read FColor write SetColor;
    property Height: Integer read GetHeight write SetHeight;
    property Name: TFontName read GetName write SetName;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Size: Integer read GetSize write SetSize stored False;
    property Style: TFontStyles read GetStyle write SetStyle;
  end;

  TPen = class(TGraphicsObject)
  private
    FMode: TPenMode;
    procedure GetData(var PenData: TPenData);
    procedure SetData(const PenData: TPenData);
  protected
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetHandle: HPen;
    procedure SetHandle(Value: HPen);
    procedure SetMode(Value: TPenMode);
    function GetStyle: TPenStyle;
    procedure SetStyle(Value: TPenStyle);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HPen read GetHandle write SetHandle;
  published
    property Color: TColor read GetColor write SetColor default clBlack;
    property Mode: TPenMode read FMode write SetMode default pmCopy;
    property Style: TPenStyle read GetStyle write SetStyle default psSolid;
    property Width: Integer read GetWidth write SetWidth default 1;
  end;

  TBrush = class(TGraphicsObject)
  private
    procedure GetData(var BrushData: TBrushData);
    procedure SetData(const BrushData: TBrushData);
  protected
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetHandle: HBrush;
    procedure SetHandle(Value: HBrush);
    function GetStyle: TBrushStyle;
    procedure SetStyle(Value: TBrushStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Handle: HBrush read GetHandle write SetHandle;
  published
    property Color: TColor read GetColor write SetColor default clWhite;
    property Style: TBrushStyle read GetStyle write SetStyle default bsSolid;
  end;

  TFontRecall = class(TRecall)
  public
    constructor Create(AFont: TFont);
  end;

  TPenRecall = class(TRecall)
  public
    constructor Create(APen: TPen);
  end;

  TBrushRecall = class(TRecall)
  public
    constructor Create(ABrush: TBrush);
  end;

  TFillStyle = (fsSurface, fsBorder);
  TFillMode = (fmAlternate, fmWinding);

  TCopyMode = Longint;

  TCanvasStates = (csHandleValid, csFontValid, csPenValid, csBrushValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (coLeftToRight, coRightToLeft);

  TCanvas = class(TPersistent)
  private
    FHandle: HDC;
    State: TCanvasState;
    FFont: TFont;
    FPen: TPen;
    FBrush: TBrush;
    FPenPos: TPoint;
    FCopyMode: TCopyMode;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
    FTextFlags: Longint;
    procedure CreateBrush;
    procedure CreateFont;
    procedure CreatePen;
    procedure BrushChanged(ABrush: TObject);
    procedure DeselectHandles;
    function GetCanvasOrientation: TCanvasOrientation;
    function GetClipRect: TRect;
    function GetHandle: HDC;
    function GetPenPos: TPoint;
    function GetPixel(X, Y: Integer): TColor;
    procedure FontChanged(AFont: TObject);
    procedure PenChanged(APen: TObject);
    procedure SetBrush(Value: TBrush);
    procedure SetFont(Value: TFont);
    procedure SetHandle(Value: HDC);
    procedure SetPen(Value: TPen);
    procedure SetPenPos(Value: TPoint);
    procedure SetPixel(X, Y: Integer; Value: TColor);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure CreateHandle; virtual;
    procedure RequiredState(ReqState: TCanvasState);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
      const Source: TRect; Color: TColor);
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure CopyRect(const Dest: TRect; Canvas: TCanvas;
      const Source: TRect);
    procedure Draw(X, Y: Integer; Graphic: TGraphic);
    procedure DrawFocusRect(const Rect: TRect);
    procedure Ellipse(X1, Y1, X2, Y2: Integer); overload;
    procedure Ellipse(const Rect: TRect); overload;
    procedure FillRect(const Rect: TRect);
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);
    procedure FrameRect(const Rect: TRect);
    function HandleAllocated: Boolean;
    procedure LineTo(X, Y: Integer);
    procedure Lock;
    procedure MoveTo(X, Y: Integer);
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
    procedure Polygon(const Points: array of TPoint);
    procedure Polyline(const Points: array of TPoint);
    procedure PolyBezier(const Points: array of TPoint);
    procedure PolyBezierTo(const Points: array of TPoint);
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure Rectangle(const Rect: TRect); overload;
    procedure Refresh;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic);
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    procedure TextOut(X, Y: Integer; const Text: string);
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string);
    function TextWidth(const Text: string): Integer;
    function TryLock: Boolean;
    procedure Unlock;
    property ClipRect: TRect read GetClipRect;
    property Handle: HDC read GetHandle write SetHandle;
    property LockCount: Integer read FLockCount;
    property CanvasOrientation: TCanvasOrientation read GetCanvasOrientation;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property TextFlags: Longint read FTextFlags write FTextFlags;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
  end;

  { TProgressEvent is a generic progress notification event which may be
        used by TGraphic classes with computationally intensive (slow)
        operations, such as loading, storing, or transforming image data.
    Event params:
      Stage - Indicates whether this call to the OnProgress event is to
        prepare for, process, or clean up after a graphic operation.  If
        OnProgress is called at all, the first call for a graphic operation
        will be with Stage = psStarting, to allow the OnProgress event handler
        to allocate whatever resources it needs to process subsequent progress
        notifications.  After Stage = psStarting, you are guaranteed that
        OnProgress will be called again with Stage = psEnding to allow you
        to free those resources, even if the graphic operation is aborted by
        an exception.  Zero or more calls to OnProgress with Stage = psRunning
        may occur between the psStarting and psEnding calls.
      PercentDone - The ratio of work done to work remaining, on a scale of
        0 to 100.  Values may repeat or even regress (get smaller) in
        successive calls.  PercentDone is usually only a guess, and the
        guess may be dramatically altered as new information is discovered
        in decoding the image.
      RedrawNow - Indicates whether the graphic can be/should be redrawn
        immediately.  Useful for showing successive approximations of
        an image as data is available instead of waiting for all the data
        to arrive before drawing anything.  Since there is no message loop
        activity during graphic operations, you should call Update to force
        a control to be redrawn immediately in the OnProgress event handler.
        Redrawing a graphic when RedrawNow = False could corrupt the image
        and/or cause exceptions.
      Rect - Area of image that has changed and needs to be redrawn.
      Msg - Optional text describing in one or two words what the graphic
        class is currently working on.  Ex:  "Loading" "Storing"
        "Reducing colors".  The Msg string can also be empty.
        Msg strings should be resourced for translation,  should not
        contain trailing periods, and should be used only for
        display purposes.  (do not: if Msg = 'Loading' then...)
  }

  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  { The TGraphic class is a abstract base class for dealing with graphic images
    such as metafile, bitmaps, icons, and other image formats.
      LoadFromFile - Read the graphic from the file system.  The old contents of
        the graphic are lost.  If the file is not of the right format, an
        exception will be generated.
      SaveToFile - Writes the graphic to disk in the file provided.
      LoadFromStream - Like LoadFromFile except source is a stream (e.g.
        TBlobStream).
      SaveToStream - stream analogue of SaveToFile.
      LoadFromClipboardFormat - Replaces the current image with the data
        provided.  If the TGraphic does not support that format it will generate
        an exception.
      SaveToClipboardFormats - Converts the image to a clipboard format.  If the
        image does not support being translated into a clipboard format it
        will generate an exception.
      Height - The native, unstretched, height of the graphic.
      Palette - Color palette of image.  Zero if graphic doesn't need/use palettes.
      Transparent - Image does not completely cover its rectangular area
      Width - The native, unstretched, width of the graphic.
      OnChange - Called whenever the graphic changes
      PaletteModified - Indicates in OnChange whether color palette has changed.
        Stays true until whoever's responsible for realizing this new palette
        (ex: TImage) sets it to False.
      OnProgress - Generic progress indicator event. Propagates out to TPicture
        and TImage OnProgress events.}

  TGraphic = class(TInterfacedPersistent, IStreamPersist)
  private
    FOnChange: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FModified: Boolean;
    FTransparent: Boolean;
    FPaletteModified: Boolean;
    procedure SetModified(Value: Boolean);
  protected
    procedure Changed(Sender: TObject); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function Equals(Graphic: TGraphic): Boolean; virtual;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetPalette: HPALETTE; virtual;
    function GetTransparent: Boolean; virtual;
    function GetWidth: Integer; virtual; abstract;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure ReadData(Stream: TStream); virtual;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetPalette(Value: HPALETTE); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual;
  public
    constructor Create; virtual;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure SaveToFile(const Filename: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); virtual; abstract;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); virtual; abstract;
    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Modified: Boolean read FModified write SetModified;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Width: Integer read GetWidth write SetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TGraphicClass = class of TGraphic;

  { TPicture }
  { TPicture is a TGraphic container.  It is used in place of a TGraphic if the
    graphic can be of any TGraphic class.  LoadFromFile and SaveToFile are
    polymorphic. For example, if the TPicture is holding an Icon, you can
    LoadFromFile a bitmap file, where if the class was TIcon you could only read
    .ICO files.
      LoadFromFile - Reads a picture from disk.  The TGraphic class created
        determined by the file extension of the file.  If the file extension is
        not recognized an exception is generated.
      SaveToFile - Writes the picture to disk.
      LoadFromClipboardFormat - Reads the picture from the handle provided in
        the given clipboard format.  If the format is not supported, an
        exception is generated.
      SaveToClipboardFormats - Allocates a global handle and writes the picture
        in its native clipboard format (CF_BITMAP for bitmaps, CF_METAFILE
        for metafiles, etc.).  Formats will contain the formats written.
        Returns the number of clipboard items written to the array pointed to
        by Formats and Datas or would be written if either Formats or Datas are
        nil.
      SupportsClipboardFormat - Returns true if the given clipboard format
        is supported by LoadFromClipboardFormat.
      Assign - Copys the contents of the given TPicture.  Used most often in
        the implementation of TPicture properties.
      RegisterFileFormat - Register a new TGraphic class for use in
        LoadFromFile.
      RegisterClipboardFormat - Registers a new TGraphic class for use in
        LoadFromClipboardFormat.
      UnRegisterGraphicClass - Removes all references to the specified TGraphic
        class and all its descendents from the file format and clipboard format
        internal lists.
      Height - The native, unstretched, height of the picture.
      Width - The native, unstretched, width of the picture.
      Graphic - The TGraphic object contained by the TPicture
      Bitmap - Returns a bitmap.  If the contents is not already a bitmap, the
        contents are thrown away and a blank bitmap is returned.
      Icon - Returns an icon.  If the contents is not already an icon, the
        contents are thrown away and a blank icon is returned.
      Metafile - Returns a metafile.  If the contents is not already a metafile,
        the contents are thrown away and a blank metafile is returned. }

  TPicture = class(TInterfacedPersistent, IStreamPersist)
  private
    FGraphic: TGraphic;
    FOnChange: TNotifyEvent;
    FNotify: IChangeNotifier;
    FOnProgress: TProgressEvent;
    procedure ForceType(GraphicType: TGraphicClass);
    function GetBitmap: TBitmap;
    function GetHeight: Integer;
    function GetIcon: TIcon;
    function GetMetafile: TMetafile;
    function GetWidth: Integer;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetGraphic(Value: TGraphic);
    procedure SetIcon(Value: TIcon);
    procedure SetMetafile(Value: TMetafile);
    procedure WriteData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(Sender: TObject); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream); 
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE);
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE);
    class function SupportsClipboardFormat(AFormat: Word): Boolean;
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterFileFormat(const AExtension, ADescription: string;
      AGraphicClass: TGraphicClass);
    class procedure RegisterFileFormatRes(const AExtension: String;
      ADescriptionResID: Integer; AGraphicClass: TGraphicClass);
    class procedure RegisterClipboardFormat(AFormat: Word;
      AGraphicClass: TGraphicClass);
    class procedure UnregisterGraphicClass(AClass: TGraphicClass);
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Icon: TIcon read GetIcon write SetIcon;
    property Metafile: TMetafile read GetMetafile write SetMetafile;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  { TMetafile }
  { TMetafile is an encapsulation of the Win32 Enhanced metafile.
      Handle - The metafile handle.
      Enhanced - determines how the metafile will be stored on disk.
        Enhanced = True (default) stores as EMF (Win32 Enhanced Metafile),
        Enhanced = False stores as WMF (Windows 3.1 Metafile, with Aldus header).
        The in-memory format is always EMF.  WMF has very limited capabilities;
        storing as WMF will lose information that would be retained by EMF.
        This property is set to match the metafile type when loaded from a
        stream or file.  This maintains form file compatibility with 16 bit
        Delphi (If loaded as WMF, then save as WMF).
      Inch - The units per inch assumed by a WMF metafile.  Used to alter
        scale when writing as WMF, but otherwise this property is obsolete.
        Enhanced metafiles maintain complete scale information internally.
      MMWidth,
      MMHeight: Width and Height in 0.01 millimeter units, the native
        scale used by enhanced metafiles.  The Width and Height properties
        are always in screen device pixel units; you can avoid loss of
        precision in converting between device pixels and mm by setting
        or reading the dimentions in mm with these two properties.
      CreatedBy - Optional name of the author or application used to create
        the metafile.
      Description - Optional text description of the metafile.
      You can set the CreatedBy and Description of a new metafile by calling
      TMetafileCanvas.CreateWithComment.

    TMetafileCanvas
      To create a metafile image from scratch, you must draw the image in
      a metafile canvas.  When the canvas is destroyed, it transfers the
      image into the metafile object provided to the canvas constructor.
      After the image is drawn on the canvas and the canvas is destroyed,
      the image is 'playable' in the metafile object.  Like this:

      MyMetafile := TMetafile.Create;
      MyMetafile.Width := 200;
      MyMetafile.Height := 200;
      with TMetafileCanvas.Create(MyMetafile, 0) do
      try
        Brush.Color := clRed;
        Ellipse(0,0,100,100);
        ...
      finally
        Free;
      end;
      Form1.Canvas.Draw(0,0,MyMetafile);  (* 1 red circle  *)

      To add to an existing metafile image, create a metafile canvas
      and play the source metafile into the metafile canvas.  Like this:

      (* continued from previous example, so MyMetafile contains an image *)
      with TMetafileCanvas.Create(MyMetafile, 0) do
      try
        Draw(0,0,MyMetafile);
        Brush.Color := clBlue;
        Ellipse(100,100,200,200);
        ...
      finally
        Free;
      end;
      Form1.Canvas.Draw(0,0,MyMetafile);  (* 1 red circle and 1 blue circle *)
  }

  TMetafileCanvas = class(TCanvas)
  private
    FMetafile: TMetafile;
  public
    constructor Create(AMetafile: TMetafile; ReferenceDevice: HDC);
    constructor CreateWithComment(AMetafile: TMetafile; ReferenceDevice: HDC;
      const CreatedBy, Description: String);
    destructor Destroy; override;
  end;

  TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference;
    procedure Release;
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  end;

  TMetafileImage = class(TSharedImage)
  private
    FHandle: HENHMETAFILE;
    FWidth: Integer;      // FWidth and FHeight are in 0.01 mm logical pixels
    FHeight: Integer;     // These are converted to device pixels in TMetafile
    FPalette: HPALETTE;
    FInch: Word;          // Used only when writing WMF files.
    FTempWidth: Integer;  // FTempWidth and FTempHeight are in device pixels
    FTempHeight: Integer; // Used only when width/height are set when FHandle = 0
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TMetafile = class(TGraphic)
  private
    FImage: TMetafileImage;
    FEnhanced: Boolean;
    function GetAuthor: String;
    function GetDesc: String;
    function GetHandle: HENHMETAFILE;
    function GetInch: Word;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
    procedure NewImage;
    procedure SetHandle(Value: HENHMETAFILE);
    procedure SetInch(Value: Word);
    procedure SetMMHeight(Value: Integer);
    procedure SetMMWidth(Value: Integer);
    procedure UniqueImage;
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure ReadData(Stream: TStream); override;
    procedure ReadEMFStream(Stream: TStream);
    procedure ReadWMFStream(Stream: TStream; Length: Longint);
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    function  TestEMF(Stream: TStream): Boolean;
    procedure WriteData(Stream: TStream); override;
    procedure WriteEMFStream(Stream: TStream);
    procedure WriteWMFStream(Stream: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function HandleAllocated: Boolean;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const Filename: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure Assign(Source: TPersistent); override;
    function ReleaseHandle: HENHMETAFILE;
    property CreatedBy: String read GetAuthor;
    property Description: String read GetDesc;
    property Enhanced: Boolean read FEnhanced write FEnhanced default True;
    property Handle: HENHMETAFILE read GetHandle write SetHandle;
    property MMWidth: Integer read GetMMWidth write SetMMWidth;
    property MMHeight: Integer read GetMMHeight write SetMMHeight;
    property Inch: Word read GetInch write SetInch;
  end;

  { TBitmap }
  { TBitmap is an encapsulation of a Windows HBITMAP and HPALETTE.  It manages
    the palette realizing automatically as well as having a Canvas to allow
    modifications to the image.  Creating copies of a TBitmap is very fast
    since the handle is copied not the image.  If the image is modified, and
    the handle is shared by more than one TBitmap object, the image is copied
    before the modification is performed (i.e. copy on write).
      Canvas - Allows drawing on the bitmap.
      Handle - The HBITMAP encapsulated by the TBitmap.  Grabbing the handle
        directly should be avoided since it causes the HBITMAP to be copied if
        more than one TBitmap share the handle.
      Palette - The HPALETTE realized by the TBitmap.  Grabbing this handle
        directly should be avoided since it causes the HPALETTE to be copied if
        more than one TBitmap share the handle.
      Monochrome - True if the bitmap is a monochrome bitmap }

  TBitmapImage = class(TSharedImage)
  private
    FHandle: HBITMAP;     // DDB or DIB handle, used for drawing
    FMaskHandle: HBITMAP; // DDB handle
    FPalette: HPALETTE;
    FDIBHandle: HBITMAP;  // DIB handle corresponding to TDIBSection
    FDIB: TDIBSection;
    FSaveStream: TMemoryStream; // Save original RLE stream until image is modified
    FOS2Format: Boolean;  // Write BMP file header, color table in OS/2 format
    FHalftone: Boolean;   // FPalette is halftone; don't write to file
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TBitmapHandleType = (bmDIB, bmDDB);
  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
  TTransparentMode = (tmAuto, tmFixed);

  TBitmap = class(TGraphic)
  private
    FImage: TBitmapImage;
    FCanvas: TCanvas;
    FIgnorePalette: Boolean;
    FMaskBitsValid: Boolean;
    FMaskValid: Boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    procedure Changing(Sender: TObject);
    procedure CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
    procedure DIBNeeded;
    procedure FreeContext;
    function GetCanvas: TCanvas;
    function GetHandle: HBITMAP; virtual;
    function GetHandleType: TBitmapHandleType;
    function GetMaskHandle: HBITMAP; virtual;
    function GetMonochrome: Boolean;
    function GetPixelFormat: TPixelFormat;
    function GetScanline(Row: Integer): Pointer;
    function GetTransparentColor: TColor;
    procedure NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
      const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
    procedure ReadStream(Stream: TStream; Size: Longint);
    procedure ReadDIB(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader = nil);
    procedure SetHandle(Value: HBITMAP);
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMaskHandle(Value: HBITMAP);
    procedure SetMonochrome(Value: Boolean);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    function TransparentColorStored: Boolean;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean);
  protected
    procedure Changed(Sender: TObject); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadData(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Dormant;
    procedure FreeImage;
    function HandleAllocated: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
    procedure Mask(TransparentColor: TColor);
    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    property Canvas: TCanvas read GetCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property IgnorePalette: Boolean read FIgnorePalette write FIgnorePalette;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
    property TransparentColor: TColor read GetTransparentColor
      write SetTransparentColor stored TransparentColorStored;
    property TransparentMode: TTransparentMode read FTransparentMode
      write SetTransparentMode default tmAuto;
  end;

  { TIcon }
  { TIcon encapsulates window HICON handle. Drawing of an icon does not stretch
    so calling stretch draw is not meaningful.
      Handle - The HICON used by the TIcon. }

  TIconImage = class(TSharedImage)
  private
    FHandle: HICON;
    FMemoryImage: TCustomMemoryStream;
    FSize: TPoint;
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TIcon = class(TGraphic)
  private
    FImage: TIconImage;
    FRequestedSize: TPoint;
    function GetHandle: HICON;
    procedure HandleNeeded;
    procedure ImageNeeded;
    procedure NewImage(NewHandle: HICON; NewImage: TMemoryStream);
    procedure SetHandle(Value: HICON);
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HandleAllocated: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReleaseHandle: HICON;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    property Handle: HICON read GetHandle write SetHandle;
  end;

var    // New TFont instances are intialized with the values in this structure:
  DefFontData: TFontData = (
    Handle: 0;
    Height: 0;
    Pitch: fpDefault;
    Style: [];
    Charset : DEFAULT_CHARSET;
    Name: 'MS Sans Serif');

var
  SystemPalette16: HPalette; // 16 color palette that maps to the system palette

var
  DDBsOnly: Boolean = False; // True = Load all BMPs as device bitmaps.
                             // Not recommended.

function GraphicFilter(GraphicClass: TGraphicClass): string;
function GraphicExtension(GraphicClass: TGraphicClass): string;
function GraphicFileMask(GraphicClass: TGraphicClass): string;

function ColorToRGB(Color: TColor): Longint;
function ColorToString(Color: TColor): string;
function StringToColor(const S: string): TColor;
procedure GetColorValues(Proc: TGetStrProc);
function ColorToIdent(Color: Longint; var Ident: string): Boolean;
function IdentToColor(const Ident: string; var Color: Longint): Boolean;
procedure GetCharsetValues(Proc: TGetStrProc);
function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;

procedure GetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD);
function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;

function CopyPalette(Palette: HPALETTE): HPALETTE;

procedure PaletteChanged;
procedure FreeMemoryContexts;

function GetDefFontCharSet: TFontCharSet;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;

function CreateMappedBmp(Handle: HBITMAP; const OldColors, NewColors: array of TColor): HBITMAP;
function CreateMappedRes(Instance: THandle; ResName: PChar; const OldColors, NewColors: array of TColor): HBITMAP;
function CreateGrayMappedBmp(Handle: HBITMAP): HBITMAP;
function CreateGrayMappedRes(Instance: THandle; ResName: PChar): HBITMAP;

function AllocPatternBitmap(BkColor, FgColor: TColor): TBitmap;

// Alignment must be a power of 2.  Color BMPs require DWORD alignment (32).
function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;

implementation

{ Things left out
  ---------------
  Regions
  PatBlt
  Tabbed text
  Clipping regions
  Coordinate transformations
  Paths
  Beziers }

uses Consts;

const
  csAllValid = [csHandleValid..csBrushValid];

var
  ScreenLogPixels: Integer;
  StockPen: HPEN;
  StockBrush: HBRUSH;
  StockFont: HFONT;
  StockIcon: HICON;
  BitmapImageLock: TRTLCriticalSection;
  CounterLock: TRTLCriticalSection;

procedure InternalDeletePalette(Pal: HPalette);
begin
  if (Pal <> 0) and (Pal <> SystemPalette16) then
    DeleteObject(Pal);
end;

{ Resource managers }

const
  ResInfoSize = SizeOf(TResource) - SizeOf(TFontData);

type
  TResourceManager = class(TObject)
    ResList: PResource;
    FLock: TRTLCriticalSection;
    ResDataSize: Word;
    constructor Create(AResDataSize: Word);
    destructor Destroy; override;
    function AllocResource(const ResData): PResource;
    procedure FreeResource(Resource: PResource);
    procedure ChangeResource(GraphicsObject: TGraphicsObject; const ResData);
    procedure AssignResource(GraphicsObject: TGraphicsObject;
      AResource: PResource);
    procedure Lock;
    procedure Unlock;
  end;

var
  FontManager: TResourceManager;
  PenManager: TResourceManager;
  BrushManager: TResourceManager;

function GetHashCode(const Buffer; Count: Integer): Word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

constructor TResourceManager.Create(AResDataSize: Word);
begin
  ResDataSize := AResDataSize;
  InitializeCriticalSection(FLock);
end;

destructor TResourceManager.Destroy;
begin
  DeleteCriticalSection(FLock);
end;

procedure TResourceManager.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TResourceManager.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TResourceManager.AllocResource(const ResData): PResource;
var
  ResHash: Word;
begin
  ResHash := GetHashCode(ResData, ResDataSize);
  Lock;
  try
    Result := ResList;
    while (Result <> nil) and ((Result^.HashCode <> ResHash) or
      not CompareMem(@Result^.Data, @ResData, ResDataSize)) do
      Result := Result^.Next;
    if Result = nil then
    begin
      GetMem(Result, ResDataSize + ResInfoSize);
      with Result^ do
      begin
        Next := ResList;
        RefCount := 0;
        Handle := TResData(ResData).Handle;
        HashCode := ResHash;
        Move(ResData, Data, ResDataSize);
      end;
      ResList := Result;
    end;
    Inc(Result^.RefCount);
  finally
    Unlock;
  end;
end;

procedure TResourceManager.FreeResource(Resource: PResource);
var
  P: PResource;
  DeleteIt: Boolean;
begin
  if Resource <> nil then
    with Resource^ do
    begin
      Lock;
      try
        Dec(RefCount);
        DeleteIt := RefCount = 0;
        if DeleteIt then
        begin
          if Resource = ResList then
            ResList := Resource^.Next
          else
          begin
            P := ResList;
            while P^.Next <> Resource do P := P^.Next;
            P^.Next := Resource^.Next;
          end;
        end;
      finally
        Unlock;
      end;
      if DeleteIt then
      begin  // this is outside the critsect to minimize lock time
        if Handle <> 0 then DeleteObject(Handle);
        FreeMem(Resource);
      end;
    end;
end;

procedure TResourceManager.ChangeResource(GraphicsObject: TGraphicsObject;
  const ResData);
var
  P: PResource;
begin
  Lock;
  try  // prevent changes to GraphicsObject.FResource pointer between steps
    P := GraphicsObject.FResource;
    GraphicsObject.FResource := AllocResource(ResData);
    if GraphicsObject.FResource <> P then GraphicsObject.Changed;
    FreeResource(P);
  finally
    Unlock;
  end;
end;

procedure TResourceManager.AssignResource(GraphicsObject: TGraphicsObject;
  AResource: PResource);
var
  P: PResource;
begin
  Lock;
  try
    P := GraphicsObject.FResource;
    if P <> AResource then
    begin
      Inc(AResource^.RefCount);
      GraphicsObject.FResource := AResource;
      GraphicsObject.Changed;
      FreeResource(P);
    end;
  finally
    Unlock;
  end;
end;

var
  CanvasList: TThreadList;

procedure PaletteChanged;

  procedure ClearColor(ResMan: TResourceManager);
  var
    Resource: PResource;
  begin
    ResMan.Lock;
    try
      Resource := ResMan.ResList;
      while Resource <> nil do
      begin
        with Resource^ do
        { Assumes Pen.Color and Brush.Color share the same location }
          if (Handle <> 0) and (Pen.Color < 0) then
          begin
            DeleteObject(Handle);
            Handle := 0;
          end;
        Resource := Resource^.Next;
      end;
    finally
      ResMan.Unlock;
    end;
  end;

var
  I,J: Integer;
begin
  { Called when the system palette has changed (WM_SYSCOLORCHANGE) }
  I := 0;
  with CanvasList.LockList do
  try
    while I < Count do
    begin
      with TCanvas(Items[I]) do
      begin
        Lock;
        Inc(I);
        DeselectHandles;
      end;
    end;
    ClearColor(PenManager);
    ClearColor(BrushManager);
  finally
    for J := 0 to I-1 do  // Only unlock the canvases we actually locked
      TCanvas(Items[J]).Unlock;
    CanvasList.UnlockList;
  end;
end;

{ Color mapping routines }

const
  Colors: array[0..48{9}] of TIdentMapEntry = (
    (Value: clBlack; Name: 'clBlack'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clGray; Name: 'clGray'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clRed; Name: 'clRed'),
    (Value: clLime; Name: 'clLime'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clAqua; Name: 'clAqua'),
    (Value: clWhite; Name: 'clWhite'),

    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue'),
    (Value: clCream; Name: 'clCream'),
    (Value: clMedGray; Name: 'clMedGray'),

    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clAppWorkSpace; Name: 'clAppWorkSpace'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clDefault; Name: 'clDefault'),
    (Value: clGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clInfoBk; Name: 'clInfoBk'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clNone; Name: 'clNone'),
    (Value: clScrollBar; Name: 'clScrollBar'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    //(Value: clUnused; Name: 'clUnused'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clWindowText; Name: 'clWindowText'));


function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;

function ColorToString(Color: TColor): string;
begin
  if not ColorToIdent(Color, Result) then
    FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;

function StringToColor(const S: string): TColor;
begin
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToInt(S));
end;

procedure GetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do Proc(Colors[I].Name);
end;

function ColorToIdent(Color: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Color, Ident, Colors);
end;

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Color, Colors);
end;

{ TGraphicsObject }

procedure TGraphicsObject.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphicsObject.Lock;
begin
  if Assigned(FOwnerLock) then EnterCriticalSection(FOwnerLock^);
end;

procedure TGraphicsObject.Unlock;
begin
  if Assigned(FOwnerLock) then LeaveCriticalSection(FOwnerLock^);
end;

function TGraphicsObject.HandleAllocated: Boolean;
begin
  Result := (FResource <> nil) and (FResource^.Handle <> 0);
end;

{ TFont }

const
  FontCharsets: array[0..17] of TIdentMapEntry = (
    (Value: 0; Name: 'ANSI_CHARSET'),
    (Value: 1; Name: 'DEFAULT_CHARSET'),
    (Value: 2; Name: 'SYMBOL_CHARSET'),
    (Value: 77; Name: 'MAC_CHARSET'),
    (Value: 128; Name: 'SHIFTJIS_CHARSET'),
    (Value: 129; Name: 'HANGEUL_CHARSET'),
    (Value: 130; Name: 'JOHAB_CHARSET'),
    (Value: 134; Name: 'GB2312_CHARSET'),
    (Value: 136; Name: 'CHINESEBIG5_CHARSET'),
    (Value: 161; Name: 'GREEK_CHARSET'),
    (Value: 162; Name: 'TURKISH_CHARSET'),
    (Value: 177; Name: 'HEBREW_CHARSET'),
    (Value: 178; Name: 'ARABIC_CHARSET'),
    (Value: 186; Name: 'BALTIC_CHARSET'),
    (Value: 204; Name: 'RUSSIAN_CHARSET'),
    (Value: 222; Name: 'THAI_CHARSET'),
    (Value: 238; Name: 'EASTEUROPE_CHARSET'),
    (Value: 255; Name: 'OEM_CHARSET'));

procedure GetCharsetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(FontCharsets) to High(FontCharsets) do Proc(FontCharsets[I].Name);
end;

function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Charset, Ident, FontCharsets);
end;

function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;
begin
  Result := IdentToInt(Ident, CharSet, FontCharsets);
end;

function GetFontData(Font: HFont): TFontData;
var
  LogFont: TLogFont;
begin
  Result := DefFontData;
  if Font <> 0 then
  begin
    if GetObject(Font, SizeOf(LogFont), @LogFont) <> 0 then
    with Result, LogFont do
    begin
      Height := lfHeight;
      if lfWeight >= FW_BOLD then
        Include(Style, fsBold);
      if lfItalic = 1 then
        Include(Style, fsItalic);
      if lfUnderline = 1 then
        Include(Style, fsUnderline);
      if lfStrikeOut = 1 then
        Include(Style, fsStrikeOut);
      Charset := TFontCharset(lfCharSet);
      Name := lfFaceName;
      case lfPitchAndFamily and $F of
        VARIABLE_PITCH: Pitch := fpVariable;
        FIXED_PITCH: Pitch := fpFixed;
      else
        Pitch := fpDefault;
      end;
      Handle := Font;
    end;
  end;
end;

constructor TFont.Create;
begin
  DefFontData.Handle := 0;
  FResource := FontManager.AllocResource(DefFontData);
  FColor := clWindowText;
  FPixelsPerInch := ScreenLogPixels;
end;

destructor TFont.Destroy;
begin
  FontManager.FreeResource(FResource);
end;

procedure TFont.Changed;
begin
  inherited Changed;
  if FNotify <> nil then FNotify.Changed;
end;

procedure TFont.Assign(Source: TPersistent);
begin
  if Source is TFont then
  begin
    Lock;
    try
      TFont(Source).Lock;
      try
        FontManager.AssignResource(Self, TFont(Source).FResource);
        Color := TFont(Source).Color;
        if PixelsPerInch <> TFont(Source).PixelsPerInch then
          Size := TFont(Source).Size;
      finally
        TFont(Source).Unlock;
      end;
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TFont.GetData(var FontData: TFontData);
begin
  FontData := FResource^.Font;
  FontData.Handle := 0;
end;

procedure TFont.SetData(const FontData: TFontData);
begin
  Lock;
  try
    FontManager.ChangeResource(Self, FontData);
  finally
    Unlock;
  end;
end;

procedure TFont.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

function TFont.GetHandle: HFont;
var
  LogFont: TLogFont;
begin
  with FResource^ do
  begin
    if Handle = 0 then
    begin
      FontManager.Lock;
      with LogFont do
      try
        if Handle = 0 then
        begin
          lfHeight := Font.Height;
          lfWidth := 0; { have font mapper choose }
          lfEscapement := 0; { only straight fonts }
          lfOrientation := 0; { no rotation }
          if fsBold in Font.Style then
            lfWeight := FW_BOLD
          else
            lfWeight := FW_NORMAL;
          lfItalic := Byte(fsItalic in Font.Style);
          lfUnderline := Byte(fsUnderline in Font.Style);
          lfStrikeOut := Byte(fsStrikeOut in Font.Style);
          lfCharSet := Byte(Font.Charset);
          if AnsiCompareText(Font.Name, 'Default') = 0 then  // do not localize
            StrPCopy(lfFaceName, DefFontData.Name)
          else
            StrPCopy(lfFaceName, Font.Name);
          lfQuality := DEFAULT_QUALITY;
          { Everything else as default }
          lfOutPrecision := OUT_DEFAULT_PRECIS;
          lfClipPrecision := CLIP_DEFAULT_PRECIS;
          case Pitch of
            fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
            fpFixed: lfPitchAndFamily := FIXED_PITCH;
          else
            lfPitchAndFamily := DEFAULT_PITCH;
          end;
          Handle := CreateFontIndirect(LogFont);
        end;
      finally
        FontManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
end;

procedure TFont.SetHandle(Value: HFont);
begin
  SetData(GetFontData(Value));
end;

function TFont.GetHeight: Integer;
begin
  Result := FResource^.Font.Height;
end;

procedure TFont.SetHeight(Value: Integer);
var
  FontData: TFontData;
begin
  GetData(FontData);
  FontData.Height := Value;
  SetData(FontData);
end;

function TFont.GetName: TFontName;
begin
  Result := FResource^.Font.Name;
end;

procedure TFont.SetName(const Value: TFontName);
var
  FontData: TFontData;
begin
  if Value <> '' then
  begin
    GetData(FontData);
    FillChar(FontData.Name, SizeOf(FontData.Name), 0);
    FontData.Name := Value;
    SetData(FontData);
  end;
end;

function TFont.GetSize: Integer;
begin
  Result := -MulDiv(Height, 72, FPixelsPerInch);
end;

procedure TFont.SetSize(Value: Integer);
begin
  Height := -MulDiv(Value, FPixelsPerInch, 72);
end;

function TFont.GetStyle: TFontStyles;
begin
  Result := FResource^.Font.Style;
end;

procedure TFont.SetStyle(Value: TFontStyles);
var
  FontData: TFontData;
begin
  GetData(FontData);
  FontData.Style := Value;
  SetData(FontData);
end;

function TFont.GetPitch: TFontPitch;
begin
  Result := FResource^.Font.Pitch;
end;

procedure TFont.SetPitch(Value: TFontPitch);
var
  FontData: TFontData;
begin
  GetData(FontData);
  FontData.Pitch := Value;
  SetData(FontData);
end;

function TFont.GetCharset: TFontCharset;
begin
  Result := FResource^.Font.Charset;
end;

procedure TFont.SetCharset(Value: TFontCharset);
var
  FontData: TFontData;
begin
  GetData(FontData);
  FontData.Charset := Value;
  SetData(FontData);
end;

{ TPen }

const
  DefPenData: TPenData = (
    Handle: 0;
    Color: clBlack;
    Width: 1;
    Style: psSolid);

constructor TPen.Create;
begin
  FResource := PenManager.AllocResource(DefPenData);
  FMode := pmCopy;
end;

destructor TPen.Destroy;
begin
  PenManager.FreeResource(FResource);
end;

procedure TPen.Assign(Source: TPersistent);
begin
  if Source is TPen then
  begin
    Lock;
    try
      TPen(Source).Lock;
      try
        PenManager.AssignResource(Self, TPen(Source).FResource);
        SetMode(TPen(Source).FMode);
      finally
        TPen(Source).Unlock;
      end;
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TPen.GetData(var PenData: TPenData);
begin
  PenData := FResource^.Pen;
  PenData.Handle := 0;
end;

procedure TPen.SetData(const PenData: TPenData);
begin
  Lock;
  try
    PenManager.ChangeResource(Self, PenData);
  finally
    Unlock;
  end;
end;

function TPen.GetColor: TColor;
begin
  Result := FResource^.Pen.Color;
end;

procedure TPen.SetColor(Value: TColor);
var
  PenData: TPenData;
begin
  GetData(PenData);
  PenData.Color := Value;
  SetData(PenData);
end;

function TPen.GetHandle: HPen;
const
  PenStyles: array[TPenStyle] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME);
var
  LogPen: TLogPen;
begin
  with FResource^ do
  begin
    if Handle = 0 then
    begin
      PenManager.Lock;
      with LogPen do
      try
        if Handle = 0 then
        begin
          lopnStyle := PenStyles[Pen.Style];
          lopnWidth.X := Pen.Width;
          lopnColor := ColorToRGB(Pen.Color);
          Handle := CreatePenIndirect(LogPen);
        end;
      finally
        PenManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
end;

procedure TPen.SetHandle(Value: HPen);
var
  PenData: TPenData;
begin
  PenData := DefPenData;
  PenData.Handle := Value;
  SetData(PenData);
end;

procedure TPen.SetMode(Value: TPenMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

function TPen.GetStyle: TPenStyle;
begin
  Result := FResource^.Pen.Style;
end;

procedure TPen.SetStyle(Value: TPenStyle);
var
  PenData: TPenData;
begin
  GetData(PenData);
  PenData.Style := Value;
  SetData(PenData);
end;

function TPen.GetWidth: Integer;
begin
  Result := FResource^.Pen.Width;
end;

procedure TPen.SetWidth(Value: Integer);
var
  PenData: TPenData;
begin
  if Value >= 0 then
  begin
    GetData(PenData);
    PenData.Width := Value;
    SetData(PenData);
  end;
end;

{ TBrush }

const
  DefBrushData: TBrushData = (
    Handle: 0;
    Color: clWhite;
    Bitmap: nil;
    Style: bsSolid);

constructor TBrush.Create;
begin
  FResource := BrushManager.AllocResource(DefBrushData);
end;

destructor TBrush.Destroy;
begin
  BrushManager.FreeResource(FResource);
end;

procedure TBrush.Assign(Source: TPersistent);
begin
  if Source is TBrush then
  begin
    Lock;
    try
      TBrush(Source).Lock;
      try
        BrushManager.AssignResource(Self, TBrush(Source).FResource);
      finally
        TBrush(Source).Unlock;
      end;
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TBrush.GetData(var BrushData: TBrushData);
begin
  BrushData := FResource^.Brush;
  BrushData.Handle := 0;
  BrushData.Bitmap := nil;
end;

procedure TBrush.SetData(const BrushData: TBrushData);
begin
  Lock;
  try
    BrushManager.ChangeResource(Self, BrushData);
  finally
    Unlock;
  end;
end;

function TBrush.GetBitmap: TBitmap;
begin
  Result := FResource^.Brush.Bitmap;
end;

procedure TBrush.SetBitmap(Value: TBitmap);
var
  BrushData: TBrushData;
begin
  BrushData := DefBrushData;
  BrushData.Bitmap := Value;
  SetData(BrushData);
end;

function TBrush.GetColor: TColor;
begin
  Result := FResource^.Brush.Color;
end;

procedure TBrush.SetColor(Value: TColor);
var
  BrushData: TBrushData;
begin
  GetData(BrushData);
  BrushData.Color := Value;
  if BrushData.Style = bsClear then BrushData.Style := bsSolid;
  SetData(BrushData);
end;

function TBrush.GetHandle: HBrush;
var
  LogBrush: TLogBrush;
begin
  with FResource^ do
  begin
    if Handle = 0 then
    begin
      BrushManager.Lock;
      try
        if Handle = 0 then
        begin
          with LogBrush do
          begin
            if Brush.Bitmap <> nil then
            begin
              lbStyle := BS_PATTERN;
              Brush.Bitmap.HandleType := bmDDB;
              lbHatch := Brush.Bitmap.Handle;
            end else
            begin
              lbHatch := 0;
              case Brush.Style of
                bsSolid: lbStyle := BS_SOLID;
                bsClear: lbStyle := BS_HOLLOW;
              else
                lbStyle := BS_HATCHED;
                lbHatch := Ord(Brush.Style) - Ord(bsHorizontal);
              end;
            end;
            lbColor := ColorToRGB(Brush.Color);
          end;
          Handle := CreateBrushIndirect(LogBrush);
        end;
      finally
        BrushManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
end;

procedure TBrush.SetHandle(Value: HBrush);
var
  BrushData: TBrushData;
begin
  BrushData := DefBrushData;
  BrushData.Handle := Value;
  SetData(BrushData);
end;

function TBrush.GetStyle: TBrushStyle;
begin
  Result := FResource^.Brush.Style;
end;

procedure TBrush.SetStyle(Value: TBrushStyle);
var
  BrushData: TBrushData;
begin
  GetData(BrushData);
  BrushData.Style := Value;
  if BrushData.Style = bsClear then BrushData.Color := clWhite;
  SetData(BrushData);
end;

{ TFontRecall }

constructor TFontRecall.Create(AFont: TFont);
begin
  inherited Create(TFont.Create, AFont);
end;

{ TPenRecall }

constructor TPenRecall.Create(APen: TPen);
begin
  inherited Create(TPen.Create, APen);
end;

{ TBrushRecall }

constructor TBrushRecall.Create(ABrush: TBrush);
begin
  inherited Create(TBrush.Create, ABrush);
end;

{ TCanvas }

constructor TCanvas.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.OwnerCriticalSection := @FLock;
  FPen := TPen.Create;
  FPen.OnChange := PenChanged;
  FPen.OwnerCriticalSection := @FLock;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FBrush.OwnerCriticalSection := @FLock;
  FCopyMode := cmSrcCopy;
  State := [];
  CanvasList.Add(Self);
end;

destructor TCanvas.Destroy;
begin
  CanvasList.Remove(Self);
  SetHandle(0);
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Arc(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

procedure TCanvas.BrushCopy(const Dest: TRect; Bitmap: TBitmap;
  const Source: TRect; Color: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  SrcW, SrcH, DstW, DstH: Integer;
  crBack, crText: TColorRef;
  MaskDC: HDC;
  Mask: TBitmap;
  MaskHandle: HBITMAP;
begin
  if Bitmap = nil then Exit;
  Lock;
  try
    Changing;
    RequiredState([csHandleValid, csBrushValid]);
    Bitmap.Canvas.Lock;
    try
      DstW := Dest.Right - Dest.Left;
      DstH := Dest.Bottom - Dest.Top;
      SrcW := Source.Right - Source.Left;
      SrcH := Source.Bottom - Source.Top;

      if Bitmap.TransparentColor = Color then
      begin
        Mask := nil;
        MaskHandle := Bitmap.MaskHandle;
        MaskDC := CreateCompatibleDC(0);
        MaskHandle := SelectObject(MaskDC, MaskHandle);
      end
      else
      begin
        Mask := TBitmap.Create;
        Mask.Assign(Bitmap);
        { Replace Color with black and all other colors with white }
        Mask.Mask(Color);
        Mask.Canvas.RequiredState([csHandleValid]);
        MaskDC := Mask.Canvas.FHandle;
        MaskHandle := 0;
      end;

      try
        Bitmap.Canvas.RequiredState([csHandleValid]);
        { Draw transparently or use brush color to fill background }
        if Brush.Style = bsClear then
        begin
          TransparentStretchBlt(FHandle, Dest.Left, Dest.Top, DstW, DstH,
            Bitmap.Canvas.FHandle, Source.Left, Source.Top, SrcW, SrcH,
            MaskDC, Source.Left, Source.Top);
        end
        else
        begin
          StretchBlt(FHandle, Dest.Left, Dest.Top, DstW, DstH,
            Bitmap.Canvas.FHandle, Source.Left, Source.Top, SrcW, SrcH, SrcCopy);
          crText := SetTextColor(Self.FHandle, 0);
          crBack := SetBkColor(Self.FHandle, $FFFFFF);
          StretchBlt(Self.FHandle, Dest.Left, Dest.Top, DstW, DstH,
            MaskDC, Source.Left, Source.Top, SrcW, SrcH, ROP_DSPDxax);
          SetTextColor(Self.FHandle, crText);
          SetBkColor(Self.FHandle, crBack);
        end;
      finally
        if Assigned(Mask) then Mask.Free
        else
        begin
          if MaskHandle <> 0 then SelectObject(MaskDC, MaskHandle);
          DeleteDC(MaskDC);
        end;
      end;
    finally
      Bitmap.Canvas.Unlock;
    end;
    Changed;
  finally
    Unlock;
  end;
end;

procedure TCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Chord(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

procedure TCanvas.CopyRect(const Dest: TRect; Canvas: TCanvas;
  const Source: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Canvas.RequiredState([csHandleValid, csBrushValid]);
  StretchBlt(FHandle, Dest.Left, Dest.Top, Dest.Right - Dest.Left,
    Dest.Bottom - Dest.Top, Canvas.FHandle, Source.Left, Source.Top,
    Source.Right - Source.Left, Source.Bottom - Source.Top, CopyMode);
  Changed;
end;

procedure TCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  if (Graphic <> nil) and not Graphic.Empty then
  begin
    Changing;
    RequiredState([csHandleValid]);
    SetBkColor(FHandle, ColorToRGB(FBrush.Color));
    SetTextColor(FHandle, ColorToRGB(FFont.Color));
    Graphic.Draw(Self, Rect(X, Y, X + Graphic.Width, Y + Graphic.Height));
    Changed;
  end;
end;

procedure TCanvas.DrawFocusRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Windows.DrawFocusRect(FHandle, Rect);
  Changed;
end;

procedure TCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Ellipse(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

procedure TCanvas.Ellipse(const Rect: TRect);
begin
  Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TCanvas.FillRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Windows.FillRect(FHandle, Rect, Brush.GetHandle);
  Changed;
end;

procedure TCanvas.FloodFill(X, Y: Integer; Color: TColor;
  FillStyle: TFillStyle);
const
  FillStyles: array[TFillStyle] of Word =
    (FLOODFILLSURFACE, FLOODFILLBORDER);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Windows.ExtFloodFill(FHandle, X, Y, Color, FillStyles[FillStyle]);
  Changed;
end;

procedure TCanvas.FrameRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Windows.FrameRect(FHandle, Rect, Brush.GetHandle);
  Changed;
end;

function TCanvas.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TCanvas.LineTo(X, Y: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.LineTo(FHandle, X, Y);
  Changed;
end;

procedure TCanvas.Lock;
begin
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
end;

procedure TCanvas.MoveTo(X, Y: Integer);
begin
  RequiredState([csHandleValid]);
  Windows.MoveToEx(FHandle, X, Y, nil);
end;

procedure TCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Pie(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

type
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;

procedure TCanvas.Polygon(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Polygon(FHandle, PPoints(@Points)^, High(Points) + 1);
  Changed;
end;

procedure TCanvas.Polyline(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.Polyline(FHandle, PPoints(@Points)^, High(Points) + 1);
  Changed;
end;

procedure TCanvas.PolyBezier(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.PolyBezier(FHandle, PPoints(@Points)^, High(Points) + 1);
  Changed;
end;

procedure TCanvas.PolyBezierTo(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Windows.PolyBezierTo(FHandle, PPoints(@Points)^, High(Points) + 1);
  Changed;
end;

procedure TCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  Windows.Rectangle(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

procedure TCanvas.Rectangle(const Rect: TRect);
begin
  Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TCanvas.Refresh;
begin
  DeselectHandles;
end;

procedure TCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  Windows.RoundRect(FHandle, X1, Y1, X2, Y2, X3, Y3);
  Changed;
end;

procedure TCanvas.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  if Graphic <> nil then
  begin
    Changing;
    RequiredState(csAllValid);
    Graphic.Draw(Self, Rect);
    Changed;
  end;
end;

function TCanvas.GetCanvasOrientation: TCanvasOrientation;
var
  Point: TPoint;
begin
  Result := coLeftToRight;
  if (FTextFlags and ETO_RTLREADING) <> 0 then
  begin
    GetWindowOrgEx(Handle, Point);
    if Point.X <> 0 then Result := coRightToLeft
  end;
end;

procedure TCanvas.TextOut(X, Y: Integer; const Text: String);
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  if CanvasOrientation = coRightToLeft then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOut(FHandle, X, Y, FTextFlags, nil, PChar(Text),
   Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
  Changed;
end;

procedure TCanvas.TextRect(Rect: TRect; X, Y: Integer; const Text: string);
var
  Options: Longint;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or FTextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((FTextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOut(FHandle, X, Y, Options, @Rect, PChar(Text),
    Length(Text), nil);
  Changed;
end;

function TCanvas.TextExtent(const Text: string): TSize;
begin
  RequiredState([csHandleValid, csFontValid]);
  Result.cX := 0;
  Result.cY := 0;
  Windows.GetTextExtentPoint32(FHandle, PChar(Text), Length(Text), Result);
end;

function TCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

function TCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

function TCanvas.TryLock: Boolean;
begin
  EnterCriticalSection(CounterLock);
  try
    Result := FLockCount = 0;
    if Result then Lock;
  finally
    LeaveCriticalSection(CounterLock);
  end;
end;

procedure TCanvas.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
end;

procedure TCanvas.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCanvas.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TCanvas.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

function TCanvas.GetPenPos: TPoint;
begin
  RequiredState([csHandleValid]);
  Windows.GetCurrentPositionEx(FHandle, @Result);
end;

procedure TCanvas.SetPenPos(Value: TPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  RequiredState([csHandleValid]);
  GetPixel := Windows.GetPixel(FHandle, X, Y);
end;

procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  Windows.SetPixel(FHandle, X, Y, ColorToRGB(Value));
  Changed;
end;

function TCanvas.GetClipRect: TRect;
begin
  RequiredState([csHandleValid]);
  GetClipBox(FHandle, Result);
end;

function TCanvas.GetHandle: HDC;
begin
  Changing;
  RequiredState(csAllValid);
  Result := FHandle;
end;

procedure TCanvas.DeselectHandles;
begin
  if (FHandle <> 0) and (State - [csPenValid, csBrushValid, csFontValid] <> State) then
  begin
    SelectObject(FHandle, StockPen);
    SelectObject(FHandle, StockBrush);
    SelectObject(FHandle, StockFont);
    State := State - [csPenValid, csBrushValid, csFontValid];
  end;
end;

procedure TCanvas.CreateHandle;
begin
end;

procedure TCanvas.SetHandle(Value: HDC);
begin
  if FHandle <> Value then
  begin
    if FHandle <> 0 then
    begin
      DeselectHandles;
      FPenPos := GetPenPos;
      FHandle := 0;
      Exclude(State, csHandleValid);
    end;
    if Value <> 0 then
    begin
      Include(State, csHandleValid);
      FHandle := Value;
      SetPenPos(FPenPos);
    end;
  end;
end;

procedure TCanvas.RequiredState(ReqState: TCanvasState);
var
  NeededState: TCanvasState;
begin
  NeededState := ReqState - State;
  if NeededState <> [] then
  begin
    if csHandleValid in NeededState then
    begin
      CreateHandle;
      if FHandle = 0 then
        raise EInvalidOperation.CreateRes(@SNoCanvasHandle);
    end;
    if csFontValid in NeededState then CreateFont;
    if csPenValid in NeededState then CreatePen;
    if csBrushValid in NeededState then CreateBrush;
    State := State + NeededState;
  end;
end;

procedure TCanvas.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TCanvas.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCanvas.CreateFont;
begin
  SelectObject(FHandle, Font.GetHandle);
  SetTextColor(FHandle, ColorToRGB(Font.Color));
end;

procedure TCanvas.CreatePen;
const
  PenModes: array[TPenMode] of Word =
    (R2_BLACK, R2_WHITE, R2_NOP, R2_NOT, R2_COPYPEN, R2_NOTCOPYPEN, R2_MERGEPENNOT,
     R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN, R2_MERGEPEN, R2_NOTMERGEPEN,
     R2_MASKPEN, R2_NOTMASKPEN, R2_XORPEN, R2_NOTXORPEN);
begin
  SelectObject(FHandle, Pen.GetHandle);
  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

procedure TCanvas.CreateBrush;
begin
  UnrealizeObject(Brush.Handle);
  SelectObject(FHandle, Brush.Handle);
  if Brush.Style = bsSolid then
  begin
    SetBkColor(FHandle, ColorToRGB(Brush.Color));
    SetBkMode(FHandle, OPAQUE);
  end
  else
  begin
    { Win95 doesn't draw brush hatches if bkcolor = brush color }
    { Since bkmode is transparent, nothing should use bkcolor anyway }
    SetBkColor(FHandle, not ColorToRGB(Brush.Color));
    SetBkMode(FHandle, TRANSPARENT);
  end;
end;

procedure TCanvas.FontChanged(AFont: TObject);
begin
  if csFontValid in State then
  begin
    Exclude(State, csFontValid);
    SelectObject(FHandle, StockFont);
  end;
end;

procedure TCanvas.PenChanged(APen: TObject);
begin
  if csPenValid in State then
  begin
    Exclude(State, csPenValid);
    SelectObject(FHandle, StockPen);
  end;
end;

procedure TCanvas.BrushChanged(ABrush: TObject);
begin
  if csBrushValid in State then
  begin
    Exclude(State, csBrushValid);
    SelectObject(FHandle, StockBrush);
  end;
end;

{ Picture support }

{ Icon and cursor types }

const
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;

type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;


{ Metafile types }

const
  WMFKey = Integer($9AC6CDD7);
  WMFWord = $CDD7;

type
  PMetafileHeader = ^TMetafileHeader;
  TMetafileHeader = packed record
    Key: Longint;
    Handle: SmallInt;
    Box: TSmallRect;
    Inch: Word;
    Reserved: Longint;
    CheckSum: Word;
  end;

{ Exception routines }

procedure InvalidOperation(Str: PResStringRec); 
begin
  raise EInvalidGraphicOperation.CreateRes(Str);
end;

procedure InvalidGraphic(Str: PResStringRec);
begin
  raise EInvalidGraphic.CreateRes(Str);
end;

procedure InvalidBitmap; 
begin
  InvalidGraphic(@SInvalidBitmap);
end;

procedure InvalidIcon;
begin
  InvalidGraphic(@SInvalidIcon);
end;

procedure InvalidMetafile;
begin
  InvalidGraphic(@SInvalidMetafile);
end;

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
var
  ErrorCode: Integer;
  Buf: array [Byte] of Char;
begin
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, sizeof(Buf), nil) <> 0) then
    raise EOutOfResources.Create(Buf)
  else
    OutOfResources;
end;

function GDICheck(Value: Integer): Integer;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

function DupBits(Src: HBITMAP; Size: TPoint; Mono: Boolean): HBITMAP;
var
  DC, Mem1, Mem2: HDC;
  Old1, Old2: HBITMAP;
  Bitmap: Windows.TBitmap;
begin
  Mem1 := CreateCompatibleDC(0);
  Mem2 := CreateCompatibleDC(0);

  try
    GetObject(Src, SizeOf(Bitmap), @Bitmap);
    if Mono then
      Result := CreateBitmap(Size.X, Size.Y, 1, 1, nil)
    else
    begin
      DC := GetDC(0);
      if DC = 0 then GDIError;
      try
        Result := CreateCompatibleBitmap(DC, Size.X, Size.Y);
        if Result = 0 then GDIError;
      finally
        ReleaseDC(0, DC);
      end;
    end;

    if Result <> 0 then
    begin
      Old1 := SelectObject(Mem1, Src);
      Old2 := SelectObject(Mem2, Result);

      StretchBlt(Mem2, 0, 0, Size.X, Size.Y, Mem1, 0, 0, Bitmap.bmWidth,
        Bitmap.bmHeight, SrcCopy);
      if Old1 <> 0 then SelectObject(Mem1, Old1);
      if Old2 <> 0 then SelectObject(Mem2, Old2);
    end;
  finally
    DeleteDC(Mem1);
    DeleteDC(Mem2);
  end;
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;
  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(0));
  try
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, SrcW, SrcH));
    Save := SelectObject(MemDC, MemBmp);
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
    SelectPalette(SrcDC, SavePal, False);
    if SavePal <> 0 then
      SavePal := SelectPalette(MemDC, SavePal, True)
    else
      SavePal := SelectPalette(MemDC, SystemPalette16, True);
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if Save <> 0 then SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if SavePal <> 0 then SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;

{ RGBTripleToQuad performs in-place conversion of an OS2 color
  table into a DIB color table.   }
procedure RGBTripleToQuad(var ColorTable);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 255 downto 1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin                     // order is significant for last item moved
      rgbRed := rgbtRed;
      rgbGreen := rgbtGreen;
      rgbBlue := rgbtBlue;
      rgbReserved := 0;
    end;
  P4^[0].rgbReserved := 0;
end;

{ RGBQuadToTriple performs the inverse of RGBTripleToQuad. }
procedure RGBQuadToTriple(var ColorTable; var ColorCount: Integer);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 1 to ColorCount-1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin
      rgbtRed := rgbRed;
      rgbtGreen := rgbGreen;
      rgbtBlue := rgbBlue;
    end;
  if ColorCount < 256 then
  begin
    FillChar(P3^[ColorCount], (256 - ColorCount) * sizeof(TRGBTriple), 0);
    ColorCount := 256;   // OS2 color tables always have 256 entries
  end;
end;

procedure ByteSwapColors(var Colors; Count: Integer);
var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  asm
        MOV   EDX, Colors
        MOV   ECX, Count
        DEC   ECX
        JS    @@END
        LEA   EAX, SysInfo
        CMP   [EAX].TSystemInfo.wProcessorLevel, 3
        JE    @@386
  @@1:  MOV   EAX, [EDX+ECX*4]
        BSWAP EAX
        SHR   EAX,8
        MOV   [EDX+ECX*4],EAX
        DEC   ECX
        JNS   @@1
        JMP   @@END
  @@386:
        PUSH  EBX
  @@2:  XOR   EBX,EBX
        MOV   EAX, [EDX+ECX*4]
        MOV   BH, AL
        MOV   BL, AH
        SHR   EAX,16
        SHL   EBX,8
        MOV   BL, AL
        MOV   [EDX+ECX*4],EBX
        DEC   ECX
        JNS   @@2
        POP   EBX
    @@END:
  end;
end;

function CreateSystemPalette(const Entries: array of TColor): HPALETTE;
var
  DC: HDC;
  SysPalSize: Integer;
  Pal: TMaxLogPalette;
begin
  Pal.palVersion := $300;
  Pal.palNumEntries := 16;
  Move(Entries, Pal.palPalEntry, 16 * SizeOf(TColor));
  DC := GetDC(0);
  try
    SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
    { Ignore the disk image of the palette for 16 color bitmaps.
      Replace with the first and last 8 colors of the system palette }
    if SysPalSize >= 16 then
    begin
      GetSystemPaletteEntries(DC, 0, 8, Pal.palPalEntry);
      { Is light and dark gray swapped? }
      if TColor(Pal.palPalEntry[7]) = clSilver then
      begin
        GetSystemPaletteEntries(DC, SysPalSize - 8, 1, Pal.palPalEntry[7]);
        GetSystemPaletteEntries(DC, SysPalSize - 7, 7, Pal.palPalEntry[Pal.palNumEntries - 7]);
        GetSystemPaletteEntries(DC, 7, 1, Pal.palPalEntry[8]);
      end
      else
        GetSystemPaletteEntries(DC, SysPalSize - 8, 8, Pal.palPalEntry[Pal.palNumEntries - 8]);
    end
    else
    begin
    end;
  finally
    ReleaseDC(0,DC);
  end;
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

function SystemPaletteOverride(var Pal: TMaxLogPalette): Boolean;
var
  DC: HDC;
  SysPalSize: Integer;
begin
  Result := False;
  if SystemPalette16 <> 0 then
  begin
    DC := GetDC(0);
    try
      SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
      if SysPalSize >= 16 then
      begin
        { Ignore the disk image of the palette for 16 color bitmaps.
          Replace with the first and last 8 colors of the system palette }
        GetPaletteEntries(SystemPalette16, 0, 8, Pal.palPalEntry);
        GetPaletteEntries(SystemPalette16, 8, 8, Pal.palPalEntry[Pal.palNumEntries - 8]);
        Result := True;
      end
    finally
      ReleaseDC(0,DC);
    end;
  end;
end;

function PaletteFromDIBColorTable(DIBHandle: THandle; ColorTable: Pointer;
  ColorCount: Integer): HPalette;
var
  DC: HDC;
  Save: THandle;
  Pal: TMaxLogPalette;
begin
  Result := 0;
  Pal.palVersion := $300;
  if DIBHandle <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    Save := SelectObject(DC, DIBHandle);
    Pal.palNumEntries := GetDIBColorTable(DC, 0, 256, Pal.palPalEntry);
    SelectObject(DC, Save);
    DeleteDC(DC);
  end
  else
  begin
    Pal.palNumEntries := ColorCount;
    Move(ColorTable^, Pal.palPalEntry, ColorCount * 4);
  end;
  if Pal.palNumEntries = 0 then Exit;
  if (Pal.palNumEntries <> 16) or not SystemPaletteOverride(Pal) then
    ByteSwapColors(Pal.palPalEntry, Pal.palNumEntries);
  Result := CreatePalette(PLogPalette(@Pal)^);
end;

function PaletteToDIBColorTable(Pal: HPalette;
  var ColorTable: array of TRGBQuad): Integer;
begin
  Result := 0;
  if (Pal = 0) or
     (GetObject(Pal, sizeof(Result), @Result) = 0) or
     (Result = 0) then Exit;
  if Result > High(ColorTable)+1 then Result := High(ColorTable)+1;
  GetPaletteEntries(Pal, 0, Result, ColorTable);
  ByteSwapColors(ColorTable, Result);
end;

procedure TwoBitsFromDIB(var BI: TBitmapInfoHeader; var XorBits, AndBits: HBITMAP;
  const IconSize: TPoint);
type
  PLongArray = ^TLongArray;
  TLongArray = array[0..1] of Longint;
var
  Temp: HBITMAP;
  NumColors: Integer;
  DC: HDC;
  Bits: Pointer;
  Colors: PLongArray;
begin
  with BI do
  begin
    biHeight := biHeight shr 1; { Size in record is doubled }
    biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
    NumColors := GetDInColors(biBitCount);
  end;
  DC := GetDC(0);
  if DC = 0 then OutOfResources;
  try
    Bits := Pointer(Longint(@BI) + SizeOf(BI) + NumColors * SizeOf(TRGBQuad));
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      XorBits := DupBits(Temp, IconSize, False);
    finally
      DeleteObject(Temp);
    end;
    with BI do
    begin
      Inc(Longint(Bits), biSizeImage);
      biBitCount := 1;
      biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
      biClrUsed := 2;
      biClrImportant := 2;
    end;
    Colors := Pointer(Longint(@BI) + SizeOf(BI));
    Colors^[0] := 0;
    Colors^[1] := $FFFFFF;
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      AndBits := DupBits(Temp, IconSize, True);
    finally
      DeleteObject(Temp);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure ReadIcon(Stream: TStream; var Icon: HICON; ImageCount: Integer;
  StartOffset: Integer; const RequestedSize: TPoint; var IconSize: TPoint);
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array[0..300] of TIconRec;
var
  List: PIconRecArray;
  HeaderLen, Length: Integer;
  BitsPerPixel: Word;
  Colors, BestColor, C1, N, Index: Integer;
  DC: HDC;
  BI: PBitmapInfoHeader;
  ResData: Pointer;
  XorBits, AndBits: HBITMAP;
  XorInfo, AndInfo: Windows.TBitmap;
  XorMem, AndMem: Pointer;
  XorLen, AndLen: Integer;
(*
var
  P: PChar;
begin
  P := Pointer(Integer((Stream as TCustomMemoryStream).Memory) + Stream.Position);
//  N := LookupIconIdFromDirectoryEx(Pointer(P), True, 0, 0, LR_DEFAULTCOLOR);
  Icon := GDICheck(CreateIconFromResourceEx(
    Pointer(P + PIconRec(P)^.DIBOffset - StartOffset),
    PIconRec(P)^.DIBSize, True, $00030000, 0, 0, LR_DEFAULTCOLOR));
end;
*)

  function AdjustColor(I: Integer): Integer;
  begin
    if I = 0 then
      Result := MaxInt
    else
      Result := I;
  end;

  function BetterSize(const Old, New: TIconRec): Boolean;
  var
    NewX, NewY, OldX, OldY: Integer;
  begin
    NewX := New.Width - IconSize.X;
    NewY := New.Height - IconSize.Y;
    OldX := Old.Width - IconSize.X;
    OldY := Old.Height - IconSize.Y;
    Result := (Abs(NewX) <= Abs(OldX)) and ((NewX <= 0) or (NewX <= OldX)) and
       (Abs(NewY) <= Abs(OldY)) and ((NewY <= 0) or (NewY <= OldY));
  end;

begin
  HeaderLen := SizeOf(TIconRec) * ImageCount;
  List := AllocMem(HeaderLen);
  try
    Stream.Read(List^, HeaderLen);
    if (RequestedSize.X or RequestedSize.Y) = 0 then
    begin
      IconSize.X := GetSystemMetrics(SM_CXICON);
      IconSize.Y := GetSystemMetrics(SM_CYICON);
    end
    else
      IconSize := RequestedSize;
    DC := GetDC(0);
    if DC = 0 then OutOfResources;
    try
      BitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if BitsPerPixel > 8 then
        Colors := MaxInt
      else
        Colors := 1 shl BitsPerPixel;
    finally
      ReleaseDC(0, DC);
    end;

    { Find the image that most closely matches (<=) the current screen color
      depth and the requested image size.  }
    Index := 0;
    BestColor := AdjustColor(List^[0].Colors);
    for N := 1 to ImageCount-1 do
    begin
      C1 := AdjustColor(List^[N].Colors);
      if (C1 <= Colors) and (C1 >= BestColor) and
        BetterSize(List^[Index], List^[N]) then
      begin
        Index := N;
        BestColor := C1;
      end;
    end;

    { the following code determines which image most closely matches the
      current device. It is not meant to absolutely match Windows
      (known broken) algorithm }
(*    C2 := 0;
    for N := 0 to ImageCount - 1 do
    begin
      C1 := List^[N].Colors;
      if C1 = Colors then
      begin
        Index := N;
        if (IconSize.X = List^[N].Width) and (IconSize.Y = List^[N].Height) then
          Break;  // exact match on size and color
      end
      else if Index = -1 then
      begin            // take the first icon with fewer colors than screen
        if C1 <= Colors then
        begin
          Index := N;
          C2 := C1;
        end;
      end
      else if C1 > C2 then  // take icon with more colors than first match
        Index := N;
    end;
    if Index = -1 then Index := 0;
*)
    with List^[Index] do
    begin
      IconSize.X := Width;
      IconSize.Y := Height;
      BI := AllocMem(DIBSize);
      try
        Stream.Seek(DIBOffset  - (HeaderLen + StartOffset), 1);
        Stream.Read(BI^, DIBSize);
        TwoBitsFromDIB(BI^, XorBits, AndBits, IconSize);
        GetObject(AndBits, SizeOf(Windows.TBitmap), @AndInfo);
        GetObject(XorBits, SizeOf(Windows.TBitmap), @XorInfo);
        with AndInfo do
          AndLen := bmWidthBytes * bmHeight * bmPlanes;
        with XorInfo do
          XorLen :=  bmWidthBytes * bmHeight * bmPlanes;
        Length := AndLen + XorLen;
        ResData := AllocMem(Length);
        try
          AndMem := ResData;
          with AndInfo do
            XorMem := Pointer(Longint(ResData) + AndLen);
          GetBitmapBits(AndBits, AndLen, AndMem);
          GetBitmapBits(XorBits, XorLen, XorMem);
          DeleteObject(XorBits);
          DeleteObject(AndBits);
          Icon := CreateIcon(HInstance, IconSize.X, IconSize.Y,
            XorInfo.bmPlanes, XorInfo.bmBitsPixel, AndMem, XorMem);
          if Icon = 0 then GDIError;
        finally
          FreeMem(ResData, Length);
        end;
      finally
        FreeMem(BI, DIBSize);
      end;
    end;
  finally
    FreeMem(List, HeaderLen);
  end;
end;

function ComputeAldusChecksum(var WMF: TMetafileHeader): Word;
type
  PWord = ^Word;
var
  pW: PWord;
  pEnd: PWord;
begin
  Result := 0;
  pW := @WMF;
  pEnd := @WMF.CheckSum;
  while Longint(pW) < Longint(pEnd) do
  begin
    Result := Result xor pW^;
    Inc(Longint(pW), SizeOf(Word));
  end;
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
  if Bytes = 0 then InvalidBitmap
  else if (Bytes >= (sizeof(DS.dsbm) + sizeof(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(sizeof(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
    FillChar(BI, sizeof(BI), 0);
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;

procedure GetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD);
begin
  InternalGetDIBSizes(Bitmap, InfoHeaderSize, ImageSize, 0);
end;

function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; Colors: Integer): Boolean;
var
  OldPal: HPALETTE;
  DC: HDC;
begin
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
  finally
    if OldPal <> 0 then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;
begin
  Result := InternalGetDIB(Bitmap, Palette, BitmapInfo, Bits, 0);
end;

procedure WinError;
begin
end;

procedure CheckBool(Result: Bool);
begin
  if not Result then WinError;
end;

procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean);
var
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
  Length: Longint;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  CheckBool(GetIconInfo(Icon, IconInfo));
  try
    InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, 2);
    InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, 16);
    MonoInfo := nil;
    MonoBits := nil;
    ColorInfo := nil;
    ColorBits := nil;
    try
      MonoInfo := AllocMem(MonoInfoSize);
      MonoBits := AllocMem(MonoBitsSize);
      ColorInfo := AllocMem(ColorInfoSize);
      ColorBits := AllocMem(ColorBitsSize);
      InternalGetDIB(IconInfo.hbmMask, 0, MonoInfo^, MonoBits^, 2);
      InternalGetDIB(IconInfo.hbmColor, 0, ColorInfo^, ColorBits^, 16);
      if WriteLength then
      begin
        Length := SizeOf(CI) + SizeOf(List) + ColorInfoSize +
          ColorBitsSize + MonoBitsSize;
        Stream.Write(Length, SizeOf(Length));
      end;
      with CI do
      begin
        CI.wType := RC3_ICON;
        CI.Count := 1;
      end;
      Stream.Write(CI, SizeOf(CI));
      with List, PBitmapInfoHeader(ColorInfo)^ do
      begin
        Width := biWidth;
        Height := biHeight;
        Colors := biPlanes * biBitCount;
        DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
        DIBOffset := SizeOf(CI) + SizeOf(List);
      end;
      Stream.Write(List, SizeOf(List));
      with PBitmapInfoHeader(ColorInfo)^ do
        Inc(biHeight, biHeight); { color height includes mono bits }
      Stream.Write(ColorInfo^, ColorInfoSize);
      Stream.Write(ColorBits^, ColorBitsSize);
      Stream.Write(MonoBits^, MonoBitsSize);
    finally
      FreeMem(ColorInfo, ColorInfoSize);
      FreeMem(ColorBits, ColorBitsSize);
      FreeMem(MonoInfo, MonoInfoSize);
      FreeMem(MonoBits, MonoBitsSize);
    end;
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end;
end;

{ TGraphic }

constructor TGraphic.Create;
begin                 // This stub is required for C++ compatibility.
  inherited Create;   // C++ doesn't support abstract virtual constructors.
end;

procedure TGraphic.Changed(Sender: TObject);
begin
  FModified := True;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphic.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGraphic) or
        not Equals(TGraphic(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

function TGraphic.Equals(Graphic: TGraphic): Boolean;
var
  MyImage, GraphicsImage: TMemoryStream;
begin
  Result := (Graphic <> nil) and (ClassType = Graphic.ClassType);
  if Empty or Graphic.Empty then
  begin
    Result := Empty and Graphic.Empty;
    Exit;
  end;
  if Result then
  begin
    MyImage := TMemoryStream.Create;
    try
      WriteData(MyImage);
      GraphicsImage := TMemoryStream.Create;
      try
        Graphic.WriteData(GraphicsImage);
        Result := (MyImage.Size = GraphicsImage.Size) and
          CompareMem(MyImage.Memory, GraphicsImage.Memory, MyImage.Size);
      finally
        GraphicsImage.Free;
      end;
    finally
      MyImage.Free;
    end;
  end;
end;

function TGraphic.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TGraphic.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TGraphic.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.SetPalette(Value: HPalette);
begin
end;

procedure TGraphic.SetModified(Value: Boolean);
begin
  if Value then
    Changed(Self) else
    FModified := False;
end;

procedure TGraphic.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Changed(Self);
  end;
end;

procedure TGraphic.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

{ TPicture }

type
  PFileFormat = ^TFileFormat;
  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  TFileFormatsList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TGraphicClass);
    function FindExt(Ext: string): TGraphicClass;
    function FindClassName(const Classname: string): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    procedure BuildFilterStrings(GraphicClass: TGraphicClass;
      var Descriptions, Filters: string);
  end;

constructor TFileFormatsList.Create;
begin
  inherited Create;
  Add('wmf', SVMetafiles, 0, TMetafile);
  Add('emf', SVEnhMetafiles, 0, TMetafile);
  Add('ico', SVIcons, 0, TIcon);
  Add('bmp', SVBitmaps, 0, TBitmap);
end;

destructor TFileFormatsList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PFileFormat(Items[I]));
  inherited Destroy;
end;

procedure TFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
  AClass: TGraphicClass);
var
  NewRec: PFileFormat;
begin
  New(NewRec);
  with NewRec^ do
  begin
    Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(NewRec);
end;

function TFileFormatsList.FindExt(Ext: string): TGraphicClass;
var
  I: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count-1 downto 0 do
    with PFileFormat(Items[I])^ do
      if Extension = Ext then
      begin
        Result := GraphicClass;
        Exit;
      end;
  Result := nil;
end;

function TFileFormatsList.FindClassName(const ClassName: string): TGraphicClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := PFileFormat(Items[I])^.GraphicClass;
    if Result.ClassName = Classname then Exit;
  end;
  Result := nil;
end;

procedure TFileFormatsList.Remove(AClass: TGraphicClass);
var
  I: Integer;
  P: PFileFormat;
begin
  for I := Count-1 downto 0 do
  begin
    P := PFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

procedure TFileFormatsList.BuildFilterStrings(GraphicClass: TGraphicClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  P: PFileFormat;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := Count-1 downto 0 do
  begin
    P := PFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(GraphicClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [sAllFilter, Filters, Descriptions]);
end;

type
  TClipboardFormats = class
  private
    FClasses: TList;
    FFormats: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Fmt: Word; AClass: TGraphicClass);
    function FindFormat(Fmt: Word): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
  end;

constructor TClipboardFormats.Create;
begin
  FClasses := TList.Create;
  FFormats := TList.Create;
  Add(CF_METAFILEPICT, TMetafile);
  Add(CF_ENHMETAFILE, TMetafile);
  Add(CF_BITMAP, TBitmap);
end;

destructor TClipboardFormats.Destroy;
begin
  FClasses.Free;
  FFormats.Free;
end;

procedure TClipboardFormats.Add(Fmt: Word; AClass: TGraphicClass);
var
  I: Integer;
begin
  I := FClasses.Add(AClass);
  try
    FFormats.Add(Pointer(Integer(Fmt)));
  except
    FClasses.Delete(I);
    raise;
  end;
end;

function TClipboardFormats.FindFormat(Fmt: Word): TGraphicClass;
var
  I: Integer;
begin
  for I := FFormats.Count-1 downto 0 do
    if Word(FFormats[I]) = Fmt then
    begin
      Result := FClasses[I];
      Exit;
    end;
  Result := nil;
end;

procedure TClipboardFormats.Remove(AClass: TGraphicClass);
var
  I: Integer;
begin
  for I := FClasses.Count-1 downto 0 do
    if TGraphicClass(FClasses[I]).InheritsFrom(AClass) then
    begin
      FClasses.Delete(I);
      FFormats.Delete(I);
    end;
end;

var
  ClipboardFormats: TClipboardFormats = nil;
  FileFormats: TFileFormatsList = nil;

function GetFileFormats: TFileFormatsList;
begin
  if FileFormats = nil then FileFormats := TFileFormatsList.Create;
  Result := FileFormats;
end;

function GetClipboardFormats: TClipboardFormats;
begin
  if ClipboardFormats = nil then ClipboardFormats := TClipboardFormats.Create;
  Result := ClipboardFormats;
end;

constructor TPicture.Create;
begin
  inherited Create;
  GetFileFormats;
  GetClipboardFormats;
end;

destructor TPicture.Destroy;
begin
  FGraphic.Free;
  inherited Destroy;
end;

procedure TPicture.AssignTo(Dest: TPersistent);
begin
  if Graphic is Dest.ClassType then
    Dest.Assign(Graphic)
  else
    inherited AssignTo(Dest);
end;

procedure TPicture.ForceType(GraphicType: TGraphicClass);
begin
  if not (Graphic is GraphicType) then
  begin
    FGraphic.Free;
    FGraphic := nil;
    FGraphic := GraphicType.Create;
    FGraphic.OnChange := Changed;
    FGraphic.OnProgress := Progress;
    Changed(Self);
  end;
end;

function TPicture.GetBitmap: TBitmap;
begin
  ForceType(TBitmap);
  Result := TBitmap(Graphic);
end;

function TPicture.GetIcon: TIcon;
begin
  ForceType(TIcon);
  Result := TIcon(Graphic);
end;

function TPicture.GetMetafile: TMetafile;
begin
  ForceType(TMetafile);
  Result := TMetafile(Graphic);
end;

procedure TPicture.SetBitmap(Value: TBitmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetIcon(Value: TIcon);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetMetafile(Value: TMetafile);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetGraphic(Value: TGraphic);
var
  NewGraphic: TGraphic;
begin
  NewGraphic := nil;
  if Value <> nil then
  begin
    NewGraphic := TGraphicClass(Value.ClassType).Create;
    NewGraphic.Assign(Value);
    NewGraphic.OnChange := Changed;
    NewGraphic.OnProgress := Progress;
  end;
  try
    FGraphic.Free;
    FGraphic := NewGraphic;
    Changed(Self);
  except
    NewGraphic.Free;
    raise;
  end;
end;

{ Based on the extension of Filename, create the cooresponding TGraphic class
  and call its LoadFromFile method. }

procedure TPicture.LoadFromFile(const Filename: string);
var
  Ext: string;
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  Ext := ExtractFileExt(Filename);
  Delete(Ext, 1, 1);
  GraphicClass := FileFormats.FindExt(Ext);
  if GraphicClass = nil then
    raise EInvalidGraphic.CreateFmt(SUnknownExtension, [Ext]);

  NewGraphic := GraphicClass.Create;
  try
    NewGraphic.OnProgress := Progress;
    NewGraphic.LoadFromFile(Filename);
  except
    NewGraphic.Free;
    raise;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := Changed;
  Changed(Self);
end;

procedure TPicture.SaveToFile(const Filename: string);
begin
  if FGraphic <> nil then FGraphic.SaveToFile(Filename);
end;

procedure TPicture.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  GraphicClass := ClipboardFormats.FindFormat(AFormat);
  if GraphicClass = nil then
    InvalidGraphic(@SUnknownClipboardFormat);

  NewGraphic := GraphicClass.Create;
  try
    NewGraphic.OnProgress := Progress;
    NewGraphic.LoadFromClipboardFormat(AFormat, AData, APalette);
  except
    NewGraphic.Free;
    raise;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  FGraphic.OnChange := Changed;
  Changed(Self);
end;

procedure TPicture.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  if FGraphic <> nil then
    FGraphic.SaveToClipboardFormat(AFormat, AData, APalette);
end;

class function TPicture.SupportsClipboardFormat(AFormat: Word): Boolean;
begin
  Result := GetClipboardFormats.FindFormat(AFormat) <> nil;
end;

procedure TPicture.LoadFromStream(Stream: TStream);
begin
  Bitmap.LoadFromStream(Stream);
end;

procedure TPicture.SaveToStream(Stream: TStream);
begin
  Bitmap.SaveToStream(Stream);
end;

procedure TPicture.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetGraphic(nil)
  else if Source is TPicture then
    SetGraphic(TPicture(Source).Graphic)
  else if Source is TGraphic then
    SetGraphic(TGraphic(Source))
  else
    inherited Assign(Source);
end;

class procedure TPicture.RegisterFileFormat(const AExtension,
  ADescription: string; AGraphicClass: TGraphicClass);
begin
  GetFileFormats.Add(AExtension, ADescription, 0, AGraphicClass);
end;

class procedure TPicture.RegisterFileFormatRes(const AExtension: String;
  ADescriptionResID: Integer; AGraphicClass: TGraphicClass);
begin
  GetFileFormats.Add(AExtension, '', ADescriptionResID, AGraphicClass);
end;

class procedure TPicture.RegisterClipboardFormat(AFormat: Word;
  AGraphicClass: TGraphicClass);
begin
  GetClipboardFormats.Add(AFormat, AGraphicClass);
end;

class procedure TPicture.UnRegisterGraphicClass(AClass: TGraphicClass);
begin
  if FileFormats <> nil then FileFormats.Remove(AClass);
  if ClipboardFormats <> nil then ClipboardFormats.Remove(AClass);
end;

procedure TPicture.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if FNotify <> nil then FNotify.Changed;
end;

procedure TPicture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TPicture.ReadData(Stream: TStream);
var
  CName: string[63];
  NewGraphic: TGraphic;
  GraphicClass: TGraphicClass;
begin
  Stream.Read(CName[0], 1);
  Stream.Read(CName[1], Integer(CName[0]));
  GraphicClass := FileFormats.FindClassName(CName);
  NewGraphic := nil;
  if GraphicClass <> nil then
  begin
    NewGraphic := GraphicClass.Create;
    try
      NewGraphic.ReadData(Stream);
    except
      NewGraphic.Free;
      raise;
    end;
  end;
  FGraphic.Free;
  FGraphic := NewGraphic;
  if NewGraphic <> nil then
  begin
    NewGraphic.OnChange := Changed;
    NewGraphic.OnProgress := Progress;
  end;
  Changed(Self);
end;

procedure TPicture.WriteData(Stream: TStream);
var
  CName: string[63];
begin
  with Stream do
  begin
    if Graphic <> nil then
      CName := Graphic.ClassName else
      CName := '';
    Write(CName, Length(CName) + 1);
    if Graphic <> nil then
      Graphic.WriteData(Stream);
  end;
end;

procedure TPicture.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    Ancestor: TPicture;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TPicture then
      begin
        Ancestor := TPicture(Filer.Ancestor);
        Result := not ((Graphic = Ancestor.Graphic) or
          ((Graphic <> nil) and (Ancestor.Graphic <> nil) and
          Graphic.Equals(Ancestor.Graphic)));
      end;
    end
    else Result := Graphic <> nil;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

function TPicture.GetWidth: Integer;
begin
  Result := 0;
  if FGraphic <> nil then Result := FGraphic.Width;
end;

function TPicture.GetHeight: Integer;
begin
  Result := 0;
  if FGraphic <> nil then Result := FGraphic.Height;
end;

{ TMetafileImage }

destructor TMetafileImage.Destroy;
begin
  if FHandle <> 0 then DeleteEnhMetafile(FHandle);
  InternalDeletePalette(FPalette);
  inherited Destroy;
end;

procedure TMetafileImage.FreeHandle;
begin
end;


{ TMetafileCanvas }

constructor TMetafileCanvas.Create(AMetafile: TMetafile; ReferenceDevice: HDC);
begin
  CreateWithComment(AMetafile, ReferenceDevice, AMetafile.CreatedBy,
    AMetafile.Description);
end;

constructor TMetafileCanvas.CreateWithComment(AMetafile : TMetafile;
  ReferenceDevice: HDC; const CreatedBy, Description: String);
var
  RefDC: HDC;
  R: TRect;
  Temp: HDC;
  P: PChar;
begin
  inherited Create;
  FMetafile := AMetafile;
  RefDC := ReferenceDevice;
  if ReferenceDevice = 0 then RefDC := GetDC(0);
  try
    if FMetafile.MMWidth = 0 then
      if FMetafile.Width = 0 then
        FMetafile.MMWidth := GetDeviceCaps(RefDC, HORZSIZE)*100
      else
        FMetafile.MMWidth := MulDiv(FMetafile.Width,
          GetDeviceCaps(RefDC, HORZSIZE)*100, GetDeviceCaps(RefDC, HORZRES));
    if FMetafile.MMHeight = 0 then
      if FMetafile.Height = 0 then
        FMetafile.MMHeight := GetDeviceCaps(RefDC, VERTSIZE)*100
      else
        FMetafile.MMHeight := MulDiv(FMetafile.Height,
          GetDeviceCaps(RefDC, VERTSIZE)*100, GetDeviceCaps(RefDC, VERTRES));
    R := Rect(0,0,FMetafile.MMWidth,FMetafile.MMHeight);
    if (Length(CreatedBy) > 0) or (Length(Description) > 0) then
      P := PChar(CreatedBy+#0+Description+#0#0)
    else
      P := nil;
    Temp := CreateEnhMetafile(RefDC, nil, @R, P);
    if Temp = 0 then GDIError;
    Handle := Temp;
  finally
    if ReferenceDevice = 0 then ReleaseDC(0, RefDC);
  end;
end;

destructor TMetafileCanvas.Destroy;
var
  Temp: HDC;
begin
  Temp := Handle;
  Handle := 0;
  FMetafile.Handle := CloseEnhMetafile(Temp);
  inherited Destroy;
end;

{ TMetafile }

constructor TMetafile.Create;
begin
  inherited Create;
  FEnhanced := True;
  FTransparent := True;
  Assign(nil);
end;

destructor TMetafile.Destroy;
begin
  FImage.Release;
  inherited Destroy;
end;

procedure TMetafile.Assign(Source: TPersistent);
var
  Pal: HPalette;
begin
  if (Source = nil) or (Source is TMetafile) then
  begin
    Pal := 0;
    if FImage <> nil then
    begin
      Pal := FImage.FPalette;
      FImage.Release;
    end;
    if Assigned(Source) then
    begin
      FImage := TMetafile(Source).FImage;
      FEnhanced := TMetafile(Source).Enhanced;
    end
    else
    begin
      FImage := TMetafileImage.Create;
      FEnhanced := True;
    end;
    FImage.Reference;
    PaletteModified := (Pal <> Palette) and (Palette <> 0);
    Changed(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TMetafile.Clear;
begin
  NewImage;
end;

procedure TMetafile.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  MetaPal, OldPal: HPALETTE;
  R: TRect;
begin
  if FImage = nil then Exit;
  MetaPal := Palette;
  OldPal := 0;
  if MetaPal <> 0 then
  begin
    OldPal := SelectPalette(ACanvas.Handle, MetaPal, True);
    RealizePalette(ACanvas.Handle);
  end;
  R := Rect;
  Dec(R.Right);  // Metafile rect includes right and bottom coords
  Dec(R.Bottom);
  PlayEnhMetaFile(ACanvas.Handle, FImage.FHandle, R);
  if MetaPal <> 0 then
    SelectPalette(ACanvas.Handle, OldPal, True);
end;

function TMetafile.GetAuthor: String;
var
  Temp: Integer;
begin
  Result := '';
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  Temp := GetEnhMetafileDescription(FImage.FHandle, 0, nil);
  if Temp <= 0 then Exit;
  SetLength(Result, Temp);
  GetEnhMetafileDescription(FImage.FHandle, Temp, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function TMetafile.GetDesc: String;
var
  Temp: Integer;
begin
  Result := '';
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  Temp := GetEnhMetafileDescription(FImage.FHandle, 0, nil);
  if Temp <= 0 then Exit;
  SetLength(Result, Temp);
  GetEnhMetafileDescription(FImage.FHandle, Temp, PChar(Result));
  Delete(Result, 1, StrLen(PChar(Result))+1);
  SetLength(Result, StrLen(PChar(Result)));
end;

function TMetafile.GetEmpty;
begin
  Result := FImage = nil;
end;

function TMetafile.GetHandle: HENHMETAFILE;
begin
  if Assigned(FImage) then
    Result := FImage.FHandle
  else
    Result := 0;
end;

function TMetaFile.HandleAllocated: Boolean;
begin
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
end;

const
  HundredthMMPerInch = 2540;

function TMetafile.GetHeight: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
   if FInch = 0 then
     if FHandle = 0 then
       Result := FTempHeight
     else
     begin               { convert 0.01mm units to referenceDC device pixels }
       GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
       Result := MulDiv(FHeight,                     { metafile height in 0.01mm }
         EMFHeader.szlDevice.cy,                      { device height in pixels }
         EMFHeader.szlMillimeters.cy*100);            { device height in mm }
     end
   else          { for WMF files, convert to font dpi based device pixels }
     Result := MulDiv(FHeight, ScreenLogPixels, HundredthMMPerInch);
end;

function TMetafile.GetInch: Word;
begin
  Result := 0;
  if FImage <> nil then Result := FImage.FInch;
end;

function TMetafile.GetMMHeight: Integer;
begin
  if FImage = nil then NewImage;
  Result := FImage.FHeight;
end;

function TMetafile.GetMMWidth: Integer;
begin
  if FImage = nil then NewImage;
  Result := FImage.FWidth;
end;

function TMetafile.GetPalette: HPALETTE;
var
  LogPal: TMaxLogPalette;
  Count: Integer;
begin
  Result := 0;
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  if FImage.FPalette = 0 then
  begin
    Count := GetEnhMetaFilePaletteEntries(FImage.FHandle, 0, nil);
    if Count = 0 then
      Exit
    else if Count > 256 then
      Count := Count and $FF;
    InternalDeletePalette(FImage.FPalette);
    LogPal.palVersion := $300;
    LogPal.palNumEntries := Count;
    GetEnhMetaFilePaletteEntries(FImage.FHandle, Count, @LogPal.palPalEntry);
    FImage.FPalette := CreatePalette(PLogPalette(@LogPal)^);
  end;
  Result := FImage.FPalette;
end;

function TMetafile.GetWidth: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        Result := FTempWidth
      else
      begin     { convert 0.01mm units to referenceDC device pixels }
        GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
        Result := MulDiv(FWidth,                      { metafile width in 0.01mm }
          EMFHeader.szlDevice.cx,                      { device width in pixels }
          EMFHeader.szlMillimeters.cx*100);            { device width in 0.01mm }
      end
    else      { for WMF files, convert to font dpi based device pixels }
      Result := MulDiv(FWidth, ScreenLogPixels, HundredthMMPerInch);
end;

procedure TMetafile.LoadFromStream(Stream: TStream);
begin
  if TestEMF(Stream) then
    ReadEMFStream(Stream)
  else
    ReadWMFStream(Stream, Stream.Size - Stream.Position);
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.NewImage;
begin
  FImage.Release;
  FImage := TMetafileImage.Create;
  FImage.Reference;
end;

procedure TMetafile.ReadData(Stream: TStream);
var
  Length: Longint;
begin
  Stream.Read(Length, SizeOf(Longint));
  if Length <= 4 then
    Assign(nil)
  else
    if TestEMF(Stream) then
      ReadEMFStream(Stream)
    else
      ReadWMFStream(Stream, Length - Sizeof(Length));
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.ReadEMFStream(Stream: TStream);
var
  EnhHeader: TEnhMetaheader;
  Buf: PChar;
begin
  NewImage;
  Stream.ReadBuffer(EnhHeader, Sizeof(EnhHeader));
  if EnhHeader.dSignature <> ENHMETA_SIGNATURE then InvalidMetafile;
  GetMem(Buf, EnhHeader.nBytes);
  with FImage do
  try
    Move(EnhHeader, Buf^, Sizeof(EnhHeader));
    Stream.ReadBuffer(PChar(Buf + Sizeof(EnhHeader))^,
      EnhHeader.nBytes - Sizeof(EnhHeader));
    FHandle := SetEnhMetafileBits(EnhHeader.nBytes, Buf);
    if FHandle = 0 then InvalidMetafile;
    FInch := 0;
    with EnhHeader.rclFrame do
    begin
      FWidth := Right - Left;    { in 0.01 mm units }
      FHeight := Bottom - Top;
    end;
    Enhanced := True;
  finally
    FreeMem(Buf, EnhHeader.nBytes);
  end;
end;

procedure TMetafile.ReadWMFStream(Stream: TStream; Length: Longint);
var
  WMF: TMetafileHeader;
  BitMem: Pointer;
  MFP: TMetaFilePict;
  EMFHeader: TEnhMetaheader;
begin
  NewImage;
  Stream.Read(WMF, SizeOf(WMF));
  if (WMF.Key <> WMFKEY) or (ComputeAldusChecksum(WMF) <> WMF.CheckSum) then
    InvalidMetafile;
  Dec(Length, SizeOf(WMF));
  GetMem(Bitmem, Length);
  with FImage do
  try
    Stream.Read(BitMem^, Length);
    FImage.FInch := WMF.Inch;
    if WMF.Inch = 0 then WMF.Inch := 96;
    FWidth := MulDiv(WMF.Box.Right - WMF.Box.Left,HundredthMMPerInch,WMF.Inch);
    FHeight := MulDiv(WMF.Box.Bottom - WMF.Box.Top,HundredthMMPerInch,WMF.Inch);
    with MFP do
    begin
      MM := MM_ANISOTROPIC;
      xExt := 0;
      yExt := 0;
      hmf := 0;
    end;
    FHandle := SetWinMetaFileBits(Length, BitMem, 0, MFP);
    if FHandle = 0 then InvalidMetafile;
    // Get the maximum extent actually used by the metafile output
    // and re-convert the wmf data using the new extents.
    // This helps preserve whitespace margins in WMFs
    GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
    with MFP, EMFHeader.rclFrame do
    begin
      MM := MM_ANISOTROPIC;
      xExt := Right;
      yExt := Bottom;
      hmf := 0;
    end;
    DeleteEnhMetafile(FHandle);
    FHandle := SetWinMetaFileBits(Length, BitMem, 0, MFP);
    if FHandle = 0 then InvalidMetafile;
    Enhanced := False;
  finally
    Freemem(BitMem, Length);
  end;
end;

procedure TMetafile.SaveToFile(const Filename: String);
var
  SaveEnh: Boolean;
begin
  SaveEnh := Enhanced;
  if AnsiLowerCaseFileName(ExtractFileExt(Filename)) = '.wmf' then
    Enhanced := False;              { For 16 bit compatibility }
  inherited SaveToFile(Filename);
  Enhanced := SaveEnh;
end;

procedure TMetafile.SaveToStream(Stream: TStream);
begin
  if FImage <> nil then
    if Enhanced then
      WriteEMFStream(Stream)
    else
      WriteWMFStream(Stream);
end;

procedure TMetafile.SetHandle(Value: HENHMETAFILE);
var
  EnhHeader: TEnhMetaHeader;
begin
  if (Value <> 0) and
    (GetEnhMetafileHeader(Value, sizeof(EnhHeader), @EnhHeader) = 0) then
    InvalidMetafile;
  UniqueImage;
  if FImage.FHandle <> 0 then DeleteEnhMetafile(FImage.FHandle);
  InternalDeletePalette(FImage.FPalette);
  FImage.FPalette := 0;
  FImage.FHandle := Value;
  FImage.FTempWidth := 0;
  FImage.FTempHeight := 0;
  if Value <> 0 then
    with EnhHeader.rclFrame do
    begin
      FImage.FWidth := Right - Left;
      FImage.FHeight := Bottom - Top;
    end;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.SetHeight(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        FTempHeight := Value
      else
      begin                 { convert device pixels to 0.01mm units }
        GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
        MMHeight := MulDiv(Value,                      { metafile height in pixels }
          EMFHeader.szlMillimeters.cy*100,             { device height in 0.01mm }
          EMFHeader.szlDevice.cy);                     { device height in pixels }
      end
    else
      MMHeight := MulDiv(Value, HundredthMMPerInch, ScreenLogPixels);
end;

procedure TMetafile.SetInch(Value: Word);
begin
  if FImage = nil then NewImage;
  if FImage.FInch <> Value then
  begin
    UniqueImage;
    FImage.FInch := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetMMHeight(Value: Integer);
begin
  if FImage = nil then NewImage;
  FImage.FTempHeight := 0;
  if FImage.FHeight <> Value then
  begin
    UniqueImage;
    FImage.FHeight := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetMMWidth(Value: Integer);
begin
  if FImage = nil then NewImage;
  FImage.FTempWidth := 0;
  if FImage.FWidth <> Value then
  begin
    UniqueImage;
    FImage.FWidth := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetTransparent(Value: Boolean);
begin
  // Ignore assignments to this property.
  // Metafiles must always be considered transparent.
end;

procedure TMetafile.SetWidth(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        FTempWidth := Value
      else
      begin                 { convert device pixels to 0.01mm units }
        GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
        MMWidth := MulDiv(Value,                      { metafile width in pixels }
          EMFHeader.szlMillimeters.cx*100,            { device width in mm }
          EMFHeader.szlDevice.cx);                    { device width in pixels }
      end
    else
      MMWidth := MulDiv(Value, HundredthMMPerInch, ScreenLogPixels);
end;

function TMetafile.TestEMF(Stream: TStream): Boolean;
var
  Size: Longint;
  Header: TEnhMetaHeader;
begin
  Size := Stream.Size - Stream.Position;
  if Size > Sizeof(Header) then
  begin
    Stream.Read(Header, Sizeof(Header));
    Stream.Seek(-Sizeof(Header), soFromCurrent);
  end;
  Result := (Size > Sizeof(Header)) and
    (Header.iType = EMR_HEADER) and (Header.dSignature = ENHMETA_SIGNATURE);
end;

procedure TMetafile.UniqueImage;
var
  NewImage: TMetafileImage;
begin
  if FImage = nil then
    Self.NewImage
  else
    if FImage.FRefCount > 1 then
    begin
      NewImage:= TMetafileImage.Create;
      if FImage.FHandle <> 0 then
        NewImage.FHandle := CopyEnhMetafile(FImage.FHandle, nil);
      NewImage.FHeight := FImage.FHeight;
      NewImage.FWidth := FImage.FWidth;
      NewImage.FInch := FImage.FInch;
      NewImage.FTempWidth := FImage.FTempWidth;
      NewImage.FTempHeight := FImage.FTempHeight;
      FImage.Release;
      FImage := NewImage;
      FImage.Reference;
    end;
end;

procedure TMetafile.WriteData(Stream: TStream);
var
  SavePos: Longint;
begin
  if FImage <> nil then
  begin
    SavePos := 0;
    Stream.Write(SavePos, Sizeof(SavePos));
    SavePos := Stream.Position - Sizeof(SavePos);
    if Enhanced then
      WriteEMFStream(Stream)
    else
      WriteWMFStream(Stream);
    Stream.Seek(SavePos, soFromBeginning);
    SavePos := Stream.Size - SavePos;
    Stream.Write(SavePos, Sizeof(SavePos));
    Stream.Seek(0, soFromEnd);
  end;
end;

procedure TMetafile.WriteEMFStream(Stream: TStream);
var
  Buf: Pointer;
  Length: Longint;
begin
  if FImage = nil then Exit;
  Length := GetEnhMetaFileBits(FImage.FHandle, 0, nil);
  if Length = 0 then Exit;
  GetMem(Buf, Length);
  try
    GetEnhMetaFileBits(FImage.FHandle, Length, Buf);
    Stream.WriteBuffer(Buf^, Length);
  finally
    FreeMem(Buf, Length);
  end;
end;

procedure TMetafile.WriteWMFStream(Stream: TStream);
var
  WMF: TMetafileHeader;
  Bits: Pointer;
  Length: UINT;
  RefDC: HDC;
begin
  if FImage = nil then Exit;
  FillChar(WMF, SizeOf(WMF), 0);
  with FImage do
  begin
    with WMF do
    begin
      Key := WMFKEY;
      if FInch = 0 then
        Inch := 96          { WMF defaults to 96 units per inch }
      else
        Inch := FInch;
      with Box do
      begin
        Right := MulDiv(FWidth, WMF.Inch, HundredthMMPerInch);
        Bottom := MulDiv(FHeight, WMF.Inch, HundredthMMPerInch);
      end;
      CheckSum := ComputeAldusChecksum(WMF);
    end;
    RefDC := GetDC(0);
    try
      Length := GetWinMetaFileBits(FHandle, 0, nil, MM_ANISOTROPIC, RefDC);
      GetMem(Bits, Length);
      try
        if GetWinMetaFileBits(FHandle, Length, Bits, MM_ANISOTROPIC,
          RefDC) < Length then GDIError;
        Stream.WriteBuffer(WMF, SizeOf(WMF));
        Stream.WriteBuffer(Bits^, Length);
      finally
        FreeMem(Bits, Length);
      end;
    finally
      ReleaseDC(0, RefDC);
    end;
  end;
end;

procedure TMetafile.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  EnhHeader: TEnhMetaHeader;
begin
  AData := GetClipboardData(CF_ENHMETAFILE); // OS will convert WMF to EMF
  if AData = 0 then  InvalidGraphic(@SUnknownClipboardFormat);
  NewImage;
  with FImage do
  begin
    FHandle := CopyEnhMetafile(AData, nil);
    GetEnhMetaFileHeader(FHandle, sizeof(EnhHeader), @EnhHeader);
    with EnhHeader.rclFrame do
    begin
      FWidth := Right - Left;
      FHeight := Bottom - Top;
    end;
    FInch := 0;
  end;
  Enhanced := True;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  if FImage = nil then Exit;
  AFormat := CF_ENHMETAFILE;
  APalette := 0;
  AData := CopyEnhMetaFile(FImage.FHandle, nil);
end;

function TMetafile.ReleaseHandle: HENHMETAFILE;
begin
  UniqueImage;
  Result := FImage.FHandle;
  FImage.FHandle := 0;
end;

var
  BitmapCanvasList: TThreadList = nil;

{ TBitmapCanvas }
{ Create a canvas that gets its DC from the memory DC cache }
type
  TBitmapCanvas = class(TCanvas)
  private
    FBitmap: TBitmap;
    FOldBitmap: HBITMAP;
    FOldPalette: HPALETTE;
    procedure FreeContext;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TBitmap);
    destructor Destroy; override;
  end;

{ FreeMemoryContexts is called by the VCL main winproc to release
  memory DCs after every message is processed (garbage collection).
  Only memory DCs not locked by other threads will be freed.
}
procedure FreeMemoryContexts;
var
  I: Integer;
begin
  with BitmapCanvasList.LockList do
  try
    for I := Count-1 downto 0 do
    with TBitmapCanvas(Items[I]) do
      if TryLock then
      try
        FreeContext;
      finally
        Unlock;
      end;
  finally
    BitmapCanvasList.UnlockList;
  end;
end;

{ DeselectBitmap is called to ensure that a bitmap handle is not
  selected into any memory DC anywhere in the system.  If the bitmap
  handle is in use by a locked canvas, DeselectBitmap must wait for
  the canvas to unlock. }

procedure DeselectBitmap(AHandle: HBITMAP);
var
  I: Integer;
begin
  if AHandle = 0 then Exit;
  with BitmapCanvasList.LockList do
  try
    for I := Count - 1 downto 0 do
      with TBitmapCanvas(Items[I]) do
        if (FBitmap <> nil) and (FBitmap.FImage.FHandle = AHandle) then
          FreeContext;
  finally
    BitmapCanvasList.UnlockList;
  end;
end;

constructor TBitmapCanvas.Create(ABitmap: TBitmap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

destructor TBitmapCanvas.Destroy;
begin
  FreeContext;
  inherited Destroy;
end;

procedure TBitmapCanvas.FreeContext;
var
  H: HBITMAP;
begin
  if FHandle <> 0 then
  begin
    Lock;
    try
      if FOldBitmap <> 0 then SelectObject(FHandle, FOldBitmap);
      if FOldPalette <> 0 then SelectPalette(FHandle, FOldPalette, True);
      H := FHandle;
      Handle := 0;
      DeleteDC(H);
      BitmapCanvasList.Remove(Self);
    finally
      Unlock;
    end;
  end;
end;

procedure TBitmapCanvas.CreateHandle;
var
  H: HBITMAP;
begin
  if FBitmap <> nil then
  begin
    Lock;
    try
      FBitmap.HandleNeeded;
      DeselectBitmap(FBitmap.FImage.FHandle);
//!!      DeselectBitmap(FBitmap.FImage.FMaskHandle);
      FBitmap.PaletteNeeded;
      H := CreateCompatibleDC(0);
      if FBitmap.FImage.FHandle <> 0 then
        FOldBitmap := SelectObject(H, FBitmap.FImage.FHandle) else
        FOldBitmap := 0;
      if FBitmap.FImage.FPalette <> 0 then
      begin
        FOldPalette := SelectPalette(H, FBitmap.FImage.FPalette, True);
        RealizePalette(H);
      end
      else
        FOldPalette := 0;
      Handle := H;
      BitmapCanvasList.Add(Self);
    finally
      Unlock;
    end;
  end;
end;

{ TSharedImage }

procedure TSharedImage.Reference;
begin
  Inc(FRefCount);
end;

procedure TSharedImage.Release;
begin
  if Pointer(Self) <> nil then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
    begin
      FreeHandle;
      Free;
    end;
  end;
end;

{ TBitmapImage }

destructor TBitmapImage.Destroy;
begin
  if FDIBHandle <> 0 then
  begin
    DeselectBitmap(FDIBHandle);
    DeleteObject(FDIBHandle);
    FDIBHandle := 0;
  end;
  FreeHandle;
  if FDIB.dshSection <> 0 then CloseHandle(FDIB.dshSection);
  FreeAndNil(FSaveStream);
  inherited Destroy;
end;

procedure TBitmapImage.FreeHandle;
begin
  if (FHandle <> 0) and (FHandle <> FDIBHandle) then
  begin
    DeselectBitmap(FHandle);
    DeleteObject(FHandle);
  end;
  if FMaskHandle <> 0 then
  begin
    DeselectBitmap(FMaskHandle);
    DeleteObject(FMaskHandle);
    FMaskHandle := 0;
  end;
  InternalDeletePalette(FPalette);
  FHandle := 0;
  FPalette := 0;
end;

{ TBitmap }

const
  { Mapping from color in DIB to system color }
  Grays: array[0..3] of TColor = (clWhite, clSilver, clGray, clBlack);
  SysGrays: array[0..3] of TColor = (clBtnHighlight, clBtnFace, clBtnShadow,
    clBtnText);

{ This function will replace OldColors in Handle's colortable with NewColors and
  return a new DDB which uses that color table.  For bitmap's with more than
  256 colors (8bpp) this function returns the original bitmap. }
function CreateMappedBmp(Handle: HBITMAP; const OldColors, NewColors: array of TColor): HBITMAP;
var
  Bitmap: PBitmapInfoHeader;
  ColorCount: Integer;
  BitmapInfoSize: DWORD;
  BitmapBitsSize: DWORD;
  Bits: Pointer;
  Colors: PRGBQuadArray;
  I, J: Integer;
  OldColor, NewColor: Integer;
  ScreenDC, DC: HDC;
  Save: HBITMAP;
begin
  Result := Handle;
  if Handle = 0 then Exit;
  InternalGetDIBSizes(Handle, BitmapInfoSize, BitmapBitsSize, 0);
  Bitmap := AllocMem(DWORD(BitmapInfoSize) + BitmapBitsSize);
  try
    Bits := Pointer(DWORD(Bitmap) + BitmapInfoSize);
    InternalGetDIB(Handle, 0, Bitmap^, Bits^, 0);
    if Bitmap^.biBitCount <= 8 then
    begin
      ColorCount := 1 shl (Bitmap^.biBitCount);
      Colors := Pointer(DWORD(Bitmap) + Bitmap^.biSize);
      ByteSwapColors(Colors^, ColorCount);
      for I := 0 to ColorCount - 1 do
        for J := Low(OldColors) to High(OldColors) do
        begin
          OldColor := ColorToRGB(OldColors[J]);
          if Integer(Colors[I]) = OldColor then
          begin
            NewColor := ColorToRGB(NewColors[J]);
            Integer(Colors[I]) := NewColor;
          end;
        end;
      ByteSwapColors(Colors^, ColorCount);
      ScreenDC := GetDC(0);
      try
        DC := CreateCompatibleDC(ScreenDC);
        if DC <> 0 then
          with Bitmap^ do
          begin
            Result := CreateCompatibleBitmap(ScreenDC, biWidth, biHeight);
            if Result <> 0 then
            begin
              Save := SelectObject(DC, Result);
              StretchDIBits(DC, 0, 0, biWidth, biHeight, 0, 0, biWidth, biHeight,
                Bits, PBitmapInfo(Bitmap)^, DIB_RGB_COLORS, SrcCopy);
              SelectObject(DC, Save);
            end;
          end;
          DeleteDC(DC);
      finally
        ReleaseDC(0, ScreenDC);
      end;
    end;
  finally
    FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
  end;
end;

{ This function will create a new DDB from the bitmap resource, replacing
  OldColors in the colortable with NewColors.  If the bitmap resource has more
  than 256 colors (8bpp) this function returns the new DDB without color
  modifications. }
function CreateMappedRes(Instance: THandle; ResName: PChar;
  const OldColors, NewColors: array of TColor): HBITMAP;
var
  Rsrc: HRSRC;
  Res: THandle;
  ColorCount: DWORD;
  BitmapInfoSize: Integer;
  Bitmap: PBitmapInfoHeader;
  BitmapInfo: PBitmapInfoHeader;
  Colors: PRGBQuadArray;
  I, J: Integer;
  OldColor, NewColor: Integer;
  Bits: Pointer;
  ScreenDC, DC: HDC;
  Save: HBITMAP;
  Temp: TBitmap;
begin
  Result := 0;
  Rsrc := FindResource(Instance, ResName, RT_BITMAP);
  if Rsrc = 0 then Exit;
  Res := LoadResource(Instance, Rsrc);
  try
    { Lock the bitmap and get a pointer to the color table. }
    Bitmap := LockResource(Res);
    if Bitmap <> nil then
    try
      if (Bitmap^.biBitCount * Bitmap^.biPlanes) <= 8 then
      begin
        ColorCount := 1 shl (Bitmap^.biBitCount);
        BitmapInfoSize := Bitmap^.biSize + ColorCount * SizeOf(TRGBQuad);
        GetMem(BitmapInfo, BitmapInfoSize);
        try
          Move(Bitmap^, BitmapInfo^, BitmapInfoSize);
          if Bitmap^.biBitCount <= 8 then
          begin
            Colors := Pointer(DWORD(BitmapInfo) + BitmapInfo^.biSize);
            ByteSwapColors(Colors^, ColorCount);
            for I := 0 to ColorCount - 1 do
              for J := Low(OldColors) to High(OldColors) do
              begin
                OldColor := ColorToRGB(OldColors[J]);
                if Integer(Colors[I]) = OldColor then
                begin
                  NewColor := ColorToRGB(NewColors[J]);
                  Integer(Colors[I]) := NewColor;
                end;
              end;
            ByteSwapColors(Colors^, ColorCount);
          end;
          { First skip over the header structure and color table entries, if any. }
          Bits := Pointer(Longint(Bitmap) + BitmapInfoSize);
          { Create a color bitmap compatible with the display device. }
          ScreenDC := GetDC(0);
          try
            DC := CreateCompatibleDC(ScreenDC);
            if DC <> 0 then
              with BitmapInfo^ do
              begin
                Result := CreateCompatibleBitmap(ScreenDC, biWidth, biHeight);
                if Result <> 0 then
                begin
                  Save := SelectObject(DC, Result);
                  StretchDIBits(DC, 0, 0, biWidth, biHeight, 0, 0, biWidth, biHeight,
                    Bits, PBitmapInfo(BitmapInfo)^, DIB_RGB_COLORS, SrcCopy);
                  SelectObject(DC, Save);
                end;
              end;
              DeleteDC(DC);
          finally
            ReleaseDC(0, ScreenDC);
          end;
        finally
          FreeMem(BitmapInfo, BitmapInfoSize);
        end;
      end
      else
      begin
        Temp := TBitmap.Create;
        try
          Temp.LoadFromResourceID(Instance, Integer(ResName));
          Result := Temp.ReleaseHandle;
        finally
          Temp.Free;
        end;
      end;
    finally
      UnlockResource(Res);
    end;
  finally
    FreeResource(Res);
  end;
end;

{ This function replaces the standard gray colors in a bitmap with the system
  grays (Grays, SysGrays). }
function CreateGrayMappedBmp(Handle: HBITMAP): HBITMAP;
begin
  Result := CreateMappedBmp(Handle, Grays, SysGrays);
end;

{ This function replaces the standard gray colors in a bitmap resource with the
  system grays (Grays, SysGrays). }
function CreateGrayMappedRes(Instance: THandle; ResName: PChar): HBITMAP;
begin
  Result := CreateMappedRes(Instance, ResName, Grays, SysGrays);
end;

procedure UpdateDIBColorTable(DIBHandle: HBITMAP; Pal: HPalette;
  const DIB: TDIBSection);
var
  ScreenDC, DC: HDC;
  OldBM: HBitmap;
  ColorCount: Integer;
  Colors: array [Byte] of TRGBQuad;
begin
  if (DIBHandle <> 0) and (DIB.dsbmih.biBitCount <= 8) then
  begin
    ColorCount := PaletteToDIBColorTable(Pal, Colors);
    if ColorCount = 0 then Exit;
    ScreenDC := GetDC(0);
    DC := CreateCompatibleDC(ScreenDC);
    OldBM := SelectObject(DC, DIBHandle);
    try
      SetDIBColorTable(DC, 0, ColorCount, Colors);
    finally
      SelectObject(DC, OldBM);
      DeleteDC(DC);
      ReleaseDC(0, ScreenDC);
    end;
  end;
end;

procedure FixupBitFields(var DIB: TDIBSection);
begin
  if (DIB.dsbmih.biCompression and BI_BITFIELDS <> 0) and
    (DIB.dsBitFields[0] = 0) then
    if DIB.dsbmih.biBitCount = 16 then
    begin
      // fix buggy 16 bit color drivers
      DIB.dsBitFields[0] := $F800;
      DIB.dsBitFields[1] := $07E0;
      DIB.dsBitFields[2] := $001F;
    end else if DIB.dsbmih.biBitCount = 32 then
    begin
      // fix buggy 32 bit color drivers
      DIB.dsBitFields[0] := $00FF0000;
      DIB.dsBitFields[1] := $0000FF00;
      DIB.dsBitFields[2] := $000000FF;
    end;
end;

function CopyBitmap(Handle: HBITMAP; OldPalette, NewPalette: HPALETTE;
  var DIB: TDIBSection; Canvas: TCanvas): HBITMAP;
var
  OldScr, NewScr: HBITMAP;
  ScreenDC, NewImageDC, OldImageDC: HDC;
  BI: PBitmapInfo;
  BitsMem: Pointer;
  SrcDIB: TDIBSection;
  MonoColors: array [0..1] of Integer;
  Pal1, Pal2: HPalette;
begin
  Result := 0;
  with DIB, dsbm, dsbmih do
  begin
    if (biSize <> 0) and ((biWidth = 0) or (biHeight = 0)) then Exit;
    if (biSize = 0) and ((bmWidth = 0) or (bmHeight = 0)) then Exit;
  end;

  DeselectBitmap(Handle);

  SrcDIB.dsbmih.biSize := 0;
  if Handle <> 0 then
    if GetObject(Handle, sizeof(SrcDIB), @SrcDIB) < sizeof(SrcDIB.dsbm) then
      InvalidBitmap;

  ScreenDC := GDICheck(GetDC(0));
  NewImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
  with DIB.dsbm do
  try
    if DIB.dsbmih.biSize < DWORD(sizeof(DIB.dsbmih)) then
      if (bmPlanes or bmBitsPixel) = 1 then // monochrome
        Result := GDICheck(CreateBitmap(bmWidth, bmHeight, 1, 1, nil))
      else  // Create DDB
        Result := GDICheck(CreateCompatibleBitmap(ScreenDC, bmWidth, bmHeight))
    else  // Create DIB
    begin
      GetMem(BI, sizeof(TBitmapInfo) + 256 * sizeof(TRGBQuad));
      with DIB.dsbmih do
      try
        biSize := sizeof(BI.bmiHeader);
        biPlanes := 1;
        if biBitCount = 0 then
          biBitCount := GetDeviceCaps(ScreenDC, BITSPIXEL) * GetDeviceCaps(ScreenDC, PLANES);
        BI.bmiHeader := DIB.dsbmih;
        bmWidth := biWidth;
        bmHeight := biHeight;

        if (biBitCount <= 8) then
        begin
          if (biBitCount = 1) and ((Handle = 0) or (SrcDIB.dsbm.bmBits = nil)) then
          begin  // set mono DIB to white/black when converting from DDB.
            Integer(BI^.bmiColors[0]) := 0;
            PInteger(Integer(@BI^.bmiColors) + sizeof(Integer))^ := $FFFFFF;
          end
          else if (NewPalette <> 0) then
            PaletteToDIBColorTable(NewPalette, PRGBQuadArray(@BI.bmiColors)^)
          else if Handle <> 0 then
          begin
            NewScr := SelectObject(NewImageDC, Handle);
            if (SrcDIB.dsbmih.biSize > 0) and (SrcDIB.dsbm.bmBits <> nil) then
              biClrUsed := GetDIBColorTable(NewImageDC, 0, 256, BI^.bmiColors)
            else
              GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), nil, BI^, DIB_RGB_COLORS);
            SelectObject(NewImageDC, NewScr);
          end;
        end
        else if ((biBitCount = 16) or (biBitCount = 32)) and
          ((biCompression and BI_BITFIELDS) <> 0) then
        begin
          FixupBitFields(DIB);
          Move(DIB.dsBitFields, BI.bmiColors, sizeof(DIB.dsBitFields));
        end;

        Result := GDICheck(CreateDIBSection(ScreenDC, BI^, DIB_RGB_COLORS, BitsMem, 0, 0));
        if (BitsMem = nil) then GDIError;

        if (Handle <> 0) and (SrcDIB.dsbm.bmWidth = biWidth) and
          (SrcDIB.dsbm.bmHeight = biHeight) and (biBitCount > 8) then
        begin    // shortcut bitblt steps
          GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), BitsMem, BI^, DIB_RGB_COLORS);
          Exit;
        end;
      finally
        FreeMem(BI);
      end;
    end;

    GDICheck(Result);
    NewScr := GDICheck(SelectObject(NewImageDC, Result));
    try
      try
        Pal1 := 0;
        Pal2 := 0;
        if NewPalette <> 0 then
        begin
          Pal1 := SelectPalette(NewImageDC, NewPalette, False);
          RealizePalette(NewImageDC);
        end;
        try
          if Canvas <> nil then
          begin
            FillRect(NewImageDC, Rect(0, 0, bmWidth, bmHeight),
              Canvas.Brush.Handle);
            SetTextColor(NewImageDC, ColorToRGB(Canvas.Font.Color));
            SetBkColor(NewImageDC, ColorToRGB(Canvas.Brush.Color));
            if (DIB.dsbmih.biBitCount = 1) and (DIB.dsbm.bmBits <> nil) then
            begin
              MonoColors[0] := ColorToRGB(Canvas.Font.Color);
              MonoColors[1] := ColorToRGB(Canvas.Brush.Color);
              SetDIBColorTable(NewImageDC, 0, 2, MonoColors);
            end;
          end
          else
            PatBlt(NewImageDC, 0, 0, bmWidth, bmHeight, WHITENESS);
          if Handle <> 0 then
          begin
            OldImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              OldScr := GDICheck(SelectObject(OldImageDC, Handle));
              if OldPalette <> 0 then
              begin
                Pal2 := SelectPalette(OldImageDC, OldPalette, False);
                RealizePalette(OldImageDC);
              end;
              if Canvas <> nil then
              begin
                SetTextColor(OldImageDC, ColorToRGB(Canvas.Font.Color));
                SetBkColor(OldImageDC, ColorToRGB(Canvas.Brush.Color));
              end;
              BitBlt(NewImageDC, 0, 0, bmWidth, bmHeight, OldImageDC, 0, 0, SRCCOPY);
              if OldPalette <> 0 then
                SelectPalette(OldImageDC, Pal2, True);
              GDICheck(SelectObject(OldImageDC, OldScr));
            finally
              DeleteDC(OldImageDC);
            end;
          end;
        finally
          if NewPalette <> 0 then
            SelectPalette(NewImageDC, Pal1, True);
        end;
      finally
        SelectObject(NewImageDC, NewScr);
      end;
    except
      DeleteObject(Result);
      raise;
    end;
  finally
    DeleteDC(NewImageDC);
    ReleaseDC(0, ScreenDC);
    if (Result <> 0) then GetObject(Result, sizeof(DIB), @DIB);
  end;
end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), @PaletteSize) = 0 then Exit;
  if PaletteSize = 0 then Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
  Result := CreatePalette(PLogPalette(@LogPal)^);
end;

function CopyBitmapAsMask(Handle: HBITMAP; Palette: HPALETTE;
  TransparentColor: TColorRef): HBITMAP;
var
  DIB: TDIBSection;
  ScreenDC, BitmapDC, MonoDC: HDC;
  BkColor: TColorRef;
  Remove: Boolean;
  SaveBitmap, SaveMono: HBITMAP;
begin
  Result := 0;
  if (Handle <> 0) and (GetObject(Handle, SizeOf(DIB), @DIB) <> 0) then
  begin
    DeselectBitmap(Handle);
    ScreenDC := 0;
    MonoDC := 0;
    try
      ScreenDC := GDICheck(GetDC(0));
      MonoDC := GDICheck(CreateCompatibleDC(ScreenDC));
      with DIB, dsBm do
      begin
        Result := CreateBitmap(bmWidth, bmHeight, 1, 1, nil);
        if Result <> 0 then
        begin
          SaveMono := SelectObject(MonoDC, Result);
          if TransparentColor = TColorRef(clNone) then
            PatBlt(MonoDC, 0, 0, bmWidth, bmHeight, Blackness)
          else
          begin
            BitmapDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              { Convert DIB to DDB }
              if bmBits <> nil then
              begin
                Remove := True;
                DIB.dsbmih.biSize := 0;
                Handle := CopyBitmap(Handle, Palette, Palette, DIB, nil);
              end
              else Remove := False;
              SaveBitmap := SelectObject(BitmapDC, Handle);
              if Palette <> 0 then
              begin
                SelectPalette(BitmapDC, Palette, False);
                RealizePalette(BitmapDC);
                SelectPalette(MonoDC, Palette, False);
                RealizePalette(MonoDC);
              end;
              BkColor := SetBkColor(BitmapDC, TransparentColor);
              BitBlt(MonoDC, 0, 0, bmWidth, bmHeight, BitmapDC, 0, 0, SrcCopy);
              SetBkColor(BitmapDC, BkColor);
              if SaveBitmap <> 0 then SelectObject(BitmapDC, SaveBitmap);
              if Remove then DeleteObject(Handle);
            finally
              DeleteDC(BitmapDC);
            end;
          end;
          if SaveMono <> 0 then SelectObject(MonoDC, SaveMono);
        end;
      end;
    finally
      if MonoDC <> 0 then DeleteDC(MonoDC);
      if ScreenDC <> 0 then ReleaseDC(0, ScreenDC);
    end;
  end;
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FTransparentColor := clDefault;
  FImage := TBitmapImage.Create;
  FImage.Reference;
  if DDBsOnly then HandleType := bmDDB;
end;

destructor TBitmap.Destroy;
begin
  FreeContext;
  FImage.Release;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TBitmap.Assign(Source: TPersistent);
var
  DIB: TDIBSection;
begin
  if (Source = nil) or (Source is TBitmap) then
  begin
    EnterCriticalSection(BitmapImageLock);
    try
      if Source <> nil then
      begin
        TBitmap(Source).FImage.Reference;
        FImage.Release;
        FImage := TBitmap(Source).FImage;
        FTransparent := TBitmap(Source).FTransparent;
        FTransparentColor := TBitmap(Source).FTransparentColor;
        FTransparentMode := TBitmap(Source).FTransparentMode;
      end
      else
      begin
        FillChar(DIB, Sizeof(DIB), 0);
        NewImage(0, 0, DIB, False);
      end;
    finally
      LeaveCriticalSection(BitmapImageLock);
    end;
    PaletteModified := Palette <> 0;
    Changed(Self);
  end
  else inherited Assign(Source);
end;

procedure TBitmap.CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
var
  NewHandle, NewPalette: THandle;
begin
  FreeContext;
  NewHandle := 0;
  NewPalette := 0;
  try
    if APalette = SystemPalette16 then
      NewPalette := APalette
    else
      NewPalette := CopyPalette(APalette);
    NewHandle := CopyBitmap(AHandle, APalette, NewPalette, DIB, FCanvas);
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
end;

{ Called by the FCanvas whenever an operation is going to be performed on the
  bitmap that would modify it.  Since modifications should only affect this
  TBitmap, the handle needs to be 'cloned' if it is being refered to by more
  than one TBitmap }
procedure TBitmap.Changing(Sender: TObject);
begin
  FreeImage;
  FImage.FDIB.dsbmih.biClrUsed := 0;
  FImage.FDIB.dsbmih.biClrImportant := 0;
  FreeAndNil(FImage.FSaveStream);
end;

procedure TBitmap.Changed(Sender: TObject);
begin
  FMaskBitsValid := False;
  inherited Changed(Sender);
end;

procedure TBitmap.Dormant;
var
  s: TMemoryStream;
  DIB: TDIBSection;
begin
  s := TMemoryStream.Create;
  SaveToStream(s);
  S.Size := S.Size;  // compact to minimum buffer
  DIB := FImage.FDIB;
  DIB.dsbm.bmBits := nil;
  FreeContext; // InternalDeletePalette requires this
  FreeAndNil(FCanvas);
  NewImage(0, 0, DIB, FImage.FOS2Format, s);
end;

procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  MaskDC: HDC;
  Save: THandle;
begin
  with Rect, FImage do
  begin
    ACanvas.RequiredState(csAllValid);
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if FPalette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.FHandle, FPalette, True);
      RealizePalette(ACanvas.FHandle);
      RestorePalette := True;
    end;
    BPP := GetDeviceCaps(ACanvas.FHandle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.FHandle, PLANES);
    DoHalftone := (BPP <= 8) and (BPP < (FDIB.dsbm.bmBitsPixel * FDIB.dsbm.bmPlanes));
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.FHandle, pt);
      SetStretchBltMode(ACanvas.FHandle, HALFTONE);
      SetBrushOrgEx(ACanvas.FHandle, pt.x, pt.y, @pt);
    end else if not Monochrome then
      SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      { Call MaskHandleNeeded prior to creating the canvas handle since
        it causes FreeContext to be called. }
      if Transparent then MaskHandleNeeded;
      Canvas.RequiredState(csAllValid);
      if Transparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := GDICheck(CreateCompatibleDC(0));
          Save := SelectObject(MaskDC, FMaskHandle);
          TransparentStretchBlt(ACanvas.FHandle, Left, Top, Right - Left,
            Bottom - Top, Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth,
            FDIB.dsbm.bmHeight, MaskDC, 0, 0);
        finally
          if Save <> 0 then SelectObject(MaskDC, Save);
          if MaskDC <> 0 then DeleteDC(MaskDC);
        end;
      end
      else
        StretchBlt(ACanvas.FHandle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth,
          FDIB.dsbm.bmHeight, ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.FHandle, OldPalette, True);
    end;
  end;
end;

{ FreeImage:
  If there are multiple references to the image, create a unique copy of the image.
  If FHandle = FDIBHandle, the DIB memory will be updated when the drawing
  handle is drawn upon, so no changes are needed to maintain image integrity.
  If FHandle <> FDIBHandle, the DIB will not track with changes made to
  the DDB, so destroy the DIB handle (but keep the DIB pixel format info).  }
procedure TBitmap.FreeImage;
var
  P: HPalette;
begin
  with FImage do
    if FRefCount > 1 then
    begin
      HandleNeeded;
      if FHalftone then
        P := 0
      else
        P := FPalette;
      CopyImage(FHandle, P, FDIB)
    end
    else if (FHandle <> 0) and (FHandle <> FDIBHandle) then
    begin
      if FDIBHandle <> 0 then
        if not DeleteObject(FDIBHandle) then GDIError;
      FDIBHandle := 0;
      FDIB.dsbm.bmBits := nil;
    end;
end;

function TBitmap.GetEmpty;
begin
  with FImage do
    Result := (FHandle = 0) and (FDIBHandle = 0) and (FSaveStream = nil);
end;

function TBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    HandleNeeded;
    if FCanvas = nil then    // possible recursion
    begin
      FCanvas := TBitmapCanvas.Create(Self);
      FCanvas.OnChange := Changed;
      FCanvas.OnChanging := Changing;
    end;
  end;
  Result := FCanvas;
end;

{ Since the user might modify the contents of the HBITMAP it must not be
  shared by another TBitmap when given to the user nor should it be selected
  into a DC. }
function TBitmap.GetHandle: HBITMAP;
begin
  FreeContext;
  HandleNeeded;
  Changing(Self);
  Result := FImage.FHandle;
end;

function TBitmap.HandleAllocated: Boolean;
begin
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
end;

function TBitmap.GetHandleType: TBitmapHandleType;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FHandle = FDIBHandle) then
      if FDIBHandle = 0 then
        if FDIB.dsbmih.biSize = 0 then
          Result := bmDDB
        else
          Result := bmDIB
      else
        Result := bmDIB
    else
      Result := bmDDB;
  end;
end;

function TBitmap.GetHeight: Integer;
begin
  Result := Abs(FImage.FDIB.dsbm.bmHeight);
end;

function TBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FImage.FMaskHandle;
end;

function TBitmap.GetMonochrome: Boolean;
begin
  with FImage.FDIB.dsbm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

function TBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := FImage.FPalette;
end;

function TBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := pfCustom;
  if HandleType = bmDDB then
    Result := pfDevice
  else
    with FImage.FDIB, dsbmih do
      case biBitCount of
        1: Result := pf1Bit;
        4: Result := pf4Bit;
        8: Result := pf8Bit;
       16: case biCompression of
             BI_RGB : Result := pf15Bit;
             BI_BITFIELDS: if dsBitFields[1] = $7E0 then Result := pf16Bit;
           end;
       24: Result := pf24Bit;
       32: if biCompression = BI_RGB then Result := pf32Bit;
      end;
end;

function TBitmap.GetScanLine(Row: Integer): Pointer;
begin
  Changing(Self);
  with FImage.FDIB, dsbm, dsbmih do
  begin
    if (Row < 0) or (Row >= bmHeight) then
      InvalidOperation(@SScanLine);
    DIBNeeded;
    GDIFlush;
    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Integer(Result) := Integer(bmBits) +
      Row * BytesPerScanline(biWidth, biBitCount, 32);
  end;
end;

function TBitmap.GetTransparentColor: TColor;
begin
  if FTransparentColor = clDefault then
  begin
    if Monochrome then
      Result := clWhite
    else
      Result := Canvas.Pixels[0, Height - 1];
  end
  else Result := ColorToRGB(FTransparentColor);
  Result := Result or $02000000;
end;

function TBitmap.GetWidth: Integer;
begin
  Result := FImage.FDIB.dsbm.bmWidth;
end;

procedure TBitmap.DIBNeeded;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FDIBHandle <> 0) then Exit;
    PaletteNeeded;
    if FDIB.dsbmih.biSize = 0 then
    begin
      GetObject(FHandle, sizeof(FDIB), @FDIB);
      with FDIB, dsbm, dsbmih do
      begin
        biSize := sizeof(dsbmih);
        biWidth := bmWidth;
        biHeight := bmHeight;
        biPlanes := 1;
        biBitCount := bmPlanes * bmBitsPixel;
      end;
    end;
    FDIBHandle := CopyBitmap(FHandle, FPalette, FPalette, FDIB, nil);
  end;
end;

procedure TBitmap.FreeContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeContext;
end;

procedure TBitmap.HandleNeeded;
var
  vChange: TNotifyEvent;
begin
  if (FImage.FHandle = 0) and (FImage.FDIBHandle = 0) and (FImage.FSaveStream <> nil) then
  begin
    FImage.FSaveStream.Position := 0;
    vChange := OnChange;
    try
      OnChange := nil;
      LoadFromStream(FImage.FSaveStream);  // Current FImage may be destroyed here
    finally
      OnChange := vChange;
    end;
  end;

  with FImage do
    if FHandle = 0 then
      FHandle := FDIBHandle;
end;

procedure TBitmap.Mask(TransparentColor: TColor);
var
  NewHandle, NewPalette: THandle;
  DIB: TDIBSection;
begin
  NewHandle := 0;
  NewPalette := 0;
  try
    FreeContext;
    HandleNeeded;
    NewHandle := CopyBitmapAsMask(FImage.FHandle, FImage.FPalette,
      ColorToRGB(TransparentColor));
    FillChar(DIB, SizeOf(DIB), 0);
    GetObject(NewHandle, SizeOf(DIB), @DIB);
    if FImage.FPalette = SystemPalette16 then
      NewPalette := FImage.FPalette
    else
      NewPalette := CopyPalette(FImage.FPalette);
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
  Changed(Self);
end;

procedure TBitmap.MaskHandleNeeded;
begin
  if FMaskValid and FMaskBitsValid then Exit;
  with FImage do
  begin
    { Delete existing mask if any }
    if FMaskHandle <> 0 then
    begin
      DeselectBitmap(FMaskHandle);
      DeleteObject(FMaskHandle);
      FMaskHandle := 0;
    end;
    FreeContext;
    HandleNeeded;
    FMaskHandle := CopyBitmapAsMask(FHandle, FPalette, GetTransparentColor);
    FMaskValid := True;
    FMaskBitsValid := True;
  end;
end;

procedure TBitmap.PaletteNeeded;
var
  DC: HDC;
begin
  with FImage do
  begin
    if FIgnorePalette or (FPalette <> 0) or (FDIBHandle = 0) then Exit;
    if FHandle = FDIBHandle then DeselectBitmap(FDIBHandle);
    FPalette := PaletteFromDIBColorTable(FDIBHandle, nil, 1 shl FDIB.dsbmih.biBitCount);
    if FPalette <> 0 then Exit;
    DC := GDICheck(GetDC(0));
    FHalftone := FHalftone or
      ((GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES)) <
      (FDIB.dsbm.bmBitsPixel * FDIB.dsbm.bmPlanes));
    if FHalftone then FPalette := CreateHalftonePalette(DC);
    ReleaseDC(0, DC);
    if FPalette = 0 then IgnorePalette := True;
  end;
end;

procedure TBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  if (AFormat <> CF_BITMAP) or (AData = 0) then
    InvalidGraphic(@SUnknownClipboardFormat);
  FreeContext;
  FillChar(DIB, sizeof(DIB), 0);
  GetObject(AData, sizeof(DIB), @DIB);
  if DIB.dsbm.bmBits = nil then DIB.dsbmih.biSize := 0;
  CopyImage(AData, APalette, DIB);
  FImage.FOS2Format := False;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, Stream.Size - Stream.Position);
end;

procedure TBitmap.LoadFromResourceName(Instance: THandle; const ResName: string);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TBitmap.LoadFromResourceID(Instance: THandle; ResID: Integer);
var
  Stream: TCustomMemoryStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TBitmap.NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
  const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
var
  Image: TBitmapImage;
begin
  Image := TBitmapImage.Create;
  with Image do
  try
    FHandle := NewHandle;
    FPalette := NewPalette;
    FDIB := NewDIB;
    FOS2Format := OS2Format;
    if FDIB.dsbm.bmBits <> nil then FDIBHandle := FHandle;
    FSaveStream := RLEStream as TMemoryStream;
  except
    Image.Free;
    raise;
  end;
  //!! replace with InterlockedExchange()
  EnterCriticalSection(BitmapImageLock);
  try
    FImage.Release;
    FImage := Image;
    FImage.Reference;
  finally
    LeaveCriticalSection(BitmapImageLock);
  end;
  FMaskValid := False;
end;

procedure TBitmap.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  ReadStream(Stream, Size);
end;

procedure TBitmap.ReadDIB(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader);
const
  DIBPalSizes: array [Boolean] of Byte = (sizeof(TRGBQuad), sizeof(TRGBTriple));
var
  DC, MemDC: HDC;
  BitsMem: Pointer;
  OS2Header: TBitmapCoreHeader;
  BitmapInfo: PBitmapInfo;
  ColorTable: Pointer;
  HeaderSize: Integer;
  OS2Format: Boolean;
  BMHandle, OldBMP: HBITMAP;
  DIB: TDIBSection;
  Pal, OldPal: HPalette;
  RLEStream: TStream;
  vbmf: TBitmapFileHeader;
begin
  Pal := 0;
  BMHandle := 0;
  RLEStream := nil;
  Stream.Read(HeaderSize, sizeof(HeaderSize));
  OS2Format := HeaderSize = sizeof(OS2Header);
  if OS2Format then HeaderSize := sizeof(TBitmapInfoHeader);
  GetMem(BitmapInfo, HeaderSize + 12 + 256 * sizeof(TRGBQuad));
  with BitmapInfo^ do
  try
    try
      if OS2Format then  // convert OS2 DIB to Win DIB
      begin
        Stream.Read(Pointer(Longint(@OS2Header) + sizeof(HeaderSize))^,
          sizeof(OS2Header) - sizeof(HeaderSize));
        FillChar(bmiHeader, sizeof(bmiHeader), 0);
        with bmiHeader, OS2Header do
        begin
          biWidth := bcWidth;
          biHeight := bcHeight;
          biPlanes := bcPlanes;
          biBitCount := bcBitCount;
        end;
        Dec(ImageSize, sizeof(OS2Header));
      end
      else
      begin // support bitmap headers larger than TBitmapInfoHeader
        Stream.Read(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
          HeaderSize - sizeof(HeaderSize));
        Dec(ImageSize, HeaderSize);

        if (bmiHeader.biCompression <> BI_BITFIELDS) and
          (bmiHeader.biCompression <> BI_RGB) then
        begin // Preserve funky non-DIB data (like RLE) until modified
          RLEStream := TMemoryStream.Create;
          // source stream could be unidirectional.  don't reverse seek
          if bmf = nil then
          begin
            FillChar(vbmf, sizeof(vbmf), 0);
            vbmf.bfType := $4D42;
            vbmf.bfSize := ImageSize + Cardinal(HeaderSize);
            bmf := @vbmf;
          end;
          RLEStream.Write(bmf^, sizeof(bmf^));
          RLEStream.Write(HeaderSize, sizeof(HeaderSize));
          RLEStream.Write(Pointer(Longint(BitmapInfo) + sizeof(HeaderSize))^,
            HeaderSize - sizeof(HeaderSize));
          RLEStream.CopyFrom(Stream, ImageSize);
          RLEStream.Seek(-ImageSize, soFromEnd);
          Stream := RLEStream;  // the rest of the proc reads from RLEStream
        end;
      end;

      with bmiHeader do
      begin
        biSize := HeaderSize;
        ColorTable := Pointer(Longint(BitmapInfo) + HeaderSize);

        { check number of planes. DIBs must be 1 color plane (packed pixels) }
        if biPlanes <> 1 then InvalidBitmap;

        // 3 DWORD color element bit masks (ie 888 or 565) can precede colors
        // TBitmapInfoHeader sucessors include these masks in the headersize
        if (HeaderSize = sizeof(TBitmapInfoHeader)) and
          ((biBitCount = 16) or (biBitCount = 32)) and
          (biCompression = BI_BITFIELDS) then
        begin
          Stream.ReadBuffer(ColorTable^, 3 * sizeof(DWORD));
          Inc(Longint(ColorTable), 3 * sizeof(DWORD));
          Dec(ImageSize, 3 * sizeof(DWORD));
        end;

        // Read the color palette
        if biClrUsed = 0 then
          biClrUsed := GetDInColors(biBitCount);
        Stream.ReadBuffer(ColorTable^, biClrUsed * DIBPalSizes[OS2Format]);
        Dec(ImageSize, biClrUsed * DIBPalSizes[OS2Format]);

        // biSizeImage can be zero. If zero, compute the size.
        if (bmiHeader.biCompression = BI_RGB) then            // top-down DIBs have negative height
          biSizeImage := BytesPerScanLine(biWidth, biBitCount, 32) * Abs(biHeight);

        if biSizeImage < ImageSize then ImageSize := biSizeImage;
      end;

      { convert OS2 color table to DIB color table }
      if OS2Format then RGBTripleToQuad(ColorTable^);

      DC := GDICheck(GetDC(0));
      try
        if ((bmiHeader.biCompression <> BI_RGB) and
          (bmiHeader.biCompression <> BI_BITFIELDS)) or DDBsOnly then
        begin
          MemDC := 0;
          GetMem(BitsMem, ImageSize);
          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
            MemDC := GDICheck(CreateCompatibleDC(DC));
            OldBMP := SelectObject(MemDC, CreateCompatibleBitmap(DC, 1, 1));
            OldPal := 0;
            if bmiHeader.biClrUsed > 0 then
            begin
              Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);
              OldPal := SelectPalette(MemDC, Pal, False);
              RealizePalette(MemDC);
            end;

            try
              BMHandle := CreateDIBitmap(MemDC, BitmapInfo^.bmiHeader, CBM_INIT, BitsMem,
                BitmapInfo^, DIB_RGB_COLORS);
              if (BMHandle = 0) then
                if GetLastError = 0 then InvalidBitmap else RaiseLastOSError;
            finally
              if OldPal <> 0 then
                SelectPalette(MemDC, OldPal, True);
              DeleteObject(SelectObject(MemDC, OldBMP));
            end;
          finally
            if MemDC <> 0 then DeleteDC(MemDC);
            FreeMem(BitsMem);
          end;
        end
        else
        begin
          BMHandle := CreateDIBSection(DC, BitmapInfo^, DIB_RGB_COLORS, BitsMem, 0, 0);
          if (BMHandle = 0) or (BitsMem = nil) then
            if GetLastError = 0 then InvalidBitmap else RaiseLastOSError;

          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
          except
            DeleteObject(BMHandle);
            raise;
          end;
        end;
      finally
        ReleaseDC(0, DC);
      end;
      // Hi-color DIBs don't preserve color table, so create palette now
      if (bmiHeader.biBitCount > 8) and (bmiHeader.biClrUsed > 0) and (Pal = 0)then
        Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);

      FillChar(DIB, sizeof(DIB), 0);
      GetObject(BMHandle, Sizeof(DIB), @DIB);
      // GetObject / CreateDIBSection don't preserve these info values
      DIB.dsBmih.biXPelsPerMeter := bmiHeader.biXPelsPerMeter;
      DIB.dsBmih.biYPelsPerMeter := bmiHeader.biYPelsPerMeter;
      DIB.dsBmih.biClrUsed := bmiHeader.biClrUsed;
      DIB.dsBmih.biClrImportant := bmiHeader.biClrImportant;
    except
      RLEStream.Free;
      raise;
    end;
  finally
    FreeMem(BitmapInfo);
  end;
  NewImage(BMHandle, Pal, DIB, OS2Format, RLEStream);
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TBitmap.ReadStream(Stream: TStream; Size: Longint);
var
  Bmf: TBitmapFileHeader;
  DIB: TDIBSection;
begin
  FreeContext;
  if Size = 0 then
  begin
    FillChar(DIB, sizeof(DIB), 0);
    NewImage(0, 0, DIB, False);
  end
  else
  begin
    Stream.ReadBuffer(Bmf, sizeof(Bmf));
    if Bmf.bfType <> $4D42 then InvalidBitmap;
    ReadDIB(Stream, Size - sizeof(Bmf), @Bmf);
  end;
end;

procedure TBitmap.SetHandle(Value: HBITMAP);
var
  DIB: TDIBSection;
  APalette: HPALETTE;
begin
  with FImage do
    if FHandle <> Value then
    begin
      FreeContext;
      FillChar(DIB, sizeof(DIB), 0);
      if Value <> 0 then
        GetObject(Value, SizeOf(DIB), @DIB);
      if FRefCount = 1 then
      begin
        APalette := FPalette;
        FPalette := 0;
      end
      else
        if FPalette = SystemPalette16 then
          APalette := SystemPalette16
        else
          APalette := CopyPalette(FPalette);
      try
        NewImage(Value, APalette, DIB, False);
      except
        InternalDeletePalette(APalette);
        raise;
      end;
      Changed(Self);
    end;
end;

procedure TBitmap.SetHandleType(Value: TBitmapHandleType);
var
  DIB: TDIBSection;
  AHandle: HBITMAP;
  NewPalette: HPALETTE;
  DoCopy: Boolean;
begin
  if Value = GetHandleType then Exit;
  with FImage do
  begin
    if (FHandle = 0) and (FDIBHandle = 0) then
      if Value = bmDDB then
        FDIB.dsbmih.biSize := 0
      else
        FDIB.dsbmih.biSize := sizeof(FDIB.dsbmih)
    else
    begin
      if Value = bmDIB then
      begin
        if (FDIBHandle <> 0) and (FDIBHandle = FHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIBNeeded;
        if FRefCount = 1 then
        begin
          AHandle := FDIBHandle;
          FDIBHandle := 0;
          NewPalette := FPalette;
          FPalette := 0;
          NewImage(AHandle, NewPalette, FDIB, FOS2Format);
        end
        else
          CopyImage(FDIBHandle, FPalette, FDIB);
      end
      else
      begin
        if (FHandle <> 0) and (FHandle <> FDIBHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIB := FDIB;
        DIB.dsbmih.biSize := 0;   // flag to tell CopyBitmap to create a DDB
        DoCopy := FRefCount = 1;
        if DoCopy then
          NewPalette := FPalette
        else
          NewPalette := CopyPalette(FPalette);
        AHandle := CopyBitmap(FDIBHandle, FPalette, NewPalette, DIB, nil);
        if DoCopy then
          FHandle := AHandle
        else
          NewImage(AHandle, NewPalette, DIB, FOS2Format);
      end;
      Changed(Self);
    end;
  end;
end;

procedure TBitmap.SetHeight(Value: Integer);
var
  DIB: TDIBSection;
begin
  with FImage do
    if FDIB.dsbm.bmHeight <> Value then
    begin
      HandleNeeded;
      DIB := FDIB;
      DIB.dsbm.bmHeight := Value;
      DIB.dsbmih.biHeight := Value;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

procedure TBitmap.SetMaskHandle(Value: HBITMAP);
begin
  with FImage do
    if FMaskHandle <> Value then
    begin
      FMaskHandle := Value;
      FMaskValid := True;
      FMaskBitsValid := True;
    end;
end;

procedure TBitmap.SetMonochrome(Value: Boolean);
var
  DIB: TDIBSection;
begin
  with FImage, FDIB.dsbmih do
    if Value <> ((biPlanes = 1) and (biBitCount = 1)) then
    begin
      HandleNeeded;
      DIB := FDIB;
      with DIB.dsbmih, DIB.dsbm do
      begin
        biSize := 0;   // request DDB handle
        biPlanes := Byte(Value);  // 0 = request screen BMP format
        biBitCount := Byte(Value);
        bmPlanes := Byte(Value);
        bmBitsPixel := Byte(Value);
      end;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

procedure TBitmap.SetPalette(Value: HPALETTE);
var
  AHandle: HBITMAP;
  DIB: TDIBSection;
begin
  if FImage.FPalette <> Value then
  begin
    with FImage do
      if (Value = 0) and (FRefCount = 1) then
      begin
        InternalDeletePalette(FPalette);
        FPalette := 0;
      end
      else
      begin
        FreeContext;
        HandleNeeded;
        DIB := FDIB;
        AHandle := CopyBitmap(FHandle, FPalette, Value, DIB, nil);
        try
          NewImage(AHandle, Value, DIB, FOS2Format);
        except
          DeleteObject(AHandle);
          raise;
        end;
      end;
    UpdateDIBColorTable(FImage.FDIBHandle, Value, FImage.FDIB);
    PaletteModified := True;
    Changed(Self);
  end;
end;

procedure TBitmap.SetPixelFormat(Value: TPixelFormat);
const
  BitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);
var
  DIB: TDIBSection;
  Pal: HPalette;
  DC: HDC;
  KillPal: Boolean;
begin
  if Value = GetPixelFormat then Exit;
  case Value of
    pfDevice:
      begin
        HandleType := bmDDB;
        Exit;
      end;
    pfCustom: InvalidGraphic(@SInvalidPixelFormat);
  else
    FillChar(DIB, sizeof(DIB), 0);
    DIB.dsbm := FImage.FDIB.dsbm;
    KillPal := False;
    with DIB, dsbm, dsbmih do
    begin
      bmBits := nil;
      biSize := sizeof(DIB.dsbmih);
      biWidth := bmWidth;
      biHeight := bmHeight;
      biPlanes := 1;
      biBitCount := BitCounts[Value];
      Pal := FImage.FPalette;
      case Value of
        pf4Bit: Pal := SystemPalette16;
        pf8Bit:
          begin
            DC := GDICheck(GetDC(0));
            Pal := CreateHalftonePalette(DC);
            KillPal := True;
            ReleaseDC(0, DC);
          end;
        pf16Bit:
          begin
            biCompression := BI_BITFIELDS;
            dsBitFields[0] := $F800;
            dsBitFields[1] := $07E0;
            dsBitFields[2] := $001F;
          end;
      end;
      try
        CopyImage(Handle, Pal, DIB);
        PaletteModified := Pal <> 0;
      finally
        if KillPal then DeleteObject(Pal);
      end;
      Changed(Self);
    end;
  end;
end;

procedure TBitmap.SetTransparentColor(Value: TColor);
begin
  if Value <> FTransparentColor then
  begin
    if Value = clDefault then
      FTransparentMode := tmAuto else
      FTransparentMode := tmFixed;
    FTransparentColor := Value;
    if FImage.FRefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;
    Changed(Self);
  end;
end;

procedure TBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  if Value <> FTransparentMode then
  begin
    if Value = tmAuto then
      SetTransparentColor(clDefault) else
      SetTransparentColor(GetTransparentColor);
  end;
end;

procedure TBitmap.SetWidth(Value: Integer);
var
  DIB: TDIBSection;
begin
  with FImage do
    if FDIB.dsbm.bmWidth <> Value then
    begin
      HandleNeeded;
      DIB := FDIB;
      DIB.dsbm.bmWidth := Value;
      DIB.dsbmih.biWidth := Value;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

procedure TBitmap.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
const
  PalSize: array [Boolean] of Byte = (sizeof(TRGBQuad), sizeof(TRGBTriple));
var
  Size, ColorCount: DWORD;
  HeaderSize: DWORD;
  BMF: TBitmapFileHeader;
  Save: THandle;
  BC: TBitmapCoreHeader;
  Colors: array [Byte] of TRGBQuad;
begin
  FillChar(BMF, sizeof(BMF), 0);
  BMF.bfType := $4D42;
  if FImage.FSaveStream <> nil then
  begin
    Size := FImage.FSaveStream.Size;
    if WriteSize then
      Stream.WriteBuffer(Size, sizeof(Size));
    Stream.Write(FImage.FSaveStream.Memory^, FImage.FSaveStream.Size);
    Exit;
  end;
  DIBNeeded;
  with FImage do
  begin
    Size := 0;
    if FDIBHandle <> 0 then
    begin
      InternalGetDIBSizes(FDIBHandle, HeaderSize, Size, FDIB.dsbmih.biClrUsed);
      if FOS2Format then
      begin // OS2 format cannot have partial palette
        HeaderSize := sizeof(BC);
        if FDIB.dsbmih.biBitCount <= 8 then
          Inc(HeaderSize, sizeof(TRGBTriple) * (1 shl FDIB.dsbmih.biBitCount));
      end;
      Inc(Size, HeaderSize + sizeof(BMF));

      FillChar(BMF, sizeof(BMF), 0);
      BMF.bfType := $4D42;

      Canvas.RequiredState([csHandleValid]);
      Save := GDICheck(SelectObject(FCanvas.FHandle, FDIBHandle));
      ColorCount := GetDIBColorTable(FCanvas.FHandle, 0, 256, Colors);
      SelectObject(FCanvas.FHandle, Save);
      // GetDIBColorTable always reports the full palette; trim it back for partial palettes
      if (0 < FDIB.dsbmih.biClrUsed) and (FDIB.dsbmih.biClrUsed < ColorCount) then
        ColorCount := FDIB.dsbmih.biClrUsed;
      if (not FOS2Format) and (ColorCount = 0) and (FPalette <> 0) and not FHalftone then
      begin
        ColorCount := PaletteToDIBColorTable(FPalette, Colors);
        if FDIB.dsbmih.biBitCount > 8 then
        begin  // optional color palette for hicolor images (non OS2)
          Inc(Size, ColorCount * sizeof(TRGBQuad));
          Inc(HeaderSize, ColorCount * sizeof(TRGBQuad));
        end;
      end;

      BMF.bfSize := Size;
      BMF.bfOffBits := sizeof(BMF) + HeaderSize;
    end;

    if WriteSize then Stream.WriteBuffer(Size, SizeOf(Size));

    if Size <> 0 then
    begin
      FixupBitFields(FDIB);
      if (ColorCount <> 0) then
      begin
        if (FDIB.dsbmih.biClrUsed = 0) or (FDIB.dsbmih.biClrUsed <> ColorCount) then
          FDIB.dsbmih.biClrUsed := ColorCount;
        if FOS2Format then RGBQuadToTriple(Colors, Integer(ColorCount));
      end;
      if FOS2Format then
      begin
        with BC, FDIB.dsbmih do
        begin
          bcSize := sizeof(BC);
          bcWidth := biWidth;
          bcHeight := biHeight;
          bcPlanes := 1;
          bcBitCount := biBitCount;
        end;
        Stream.WriteBuffer(BMF, sizeof(BMF));
        Stream.WriteBuffer(BC, sizeof(BC));
      end
      else
      begin
        Stream.WriteBuffer(BMF, Sizeof(BMF));
        Stream.WriteBuffer(FDIB.dsbmih, Sizeof(FDIB.dsbmih));
        if (FDIB.dsbmih.biBitCount > 8) and
          ((FDIB.dsbmih.biCompression and BI_BITFIELDS) <> 0) then
          Stream.WriteBuffer(FDIB.dsBitfields, 12);
      end;
      Stream.WriteBuffer(Colors, ColorCount * PalSize[FOS2Format]);
      Stream.WriteBuffer(FDIB.dsbm.bmBits^, FDIB.dsbmih.biSizeImage);
    end;
  end;
end;

{ ReleaseHandle gives up ownership of the bitmap handle the TBitmap contains. }
function TBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  Changing(Self);
  with FImage do
  begin
    Result := FHandle;
    if FHandle = FDIBHandle then
    begin
      FDIBHandle := 0;
      FDIB.dsbm.bmBits := nil;
    end;
    FHandle := 0;
  end;
end;

function TBitmap.ReleaseMaskHandle: HBITMAP;
begin
  Result := GetMaskHandle;
  FImage.FMaskHandle := 0;
end;

function TBitmap.ReleasePalette: HPALETTE;
begin
  HandleNeeded;
  Changing(Self);
  Result := FImage.FPalette;
  FImage.FPalette := 0;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

procedure TBitmap.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  Format := CF_BITMAP;
  HandleNeeded;
  with FImage do
  begin
    DIB := FDIB;
    DIB.dsbmih.biSize := 0;   // copy to device bitmap
    DIB.dsbm.bmBits := nil;
    Data := CopyBitmap(FHandle, FPalette, FPalette, DIB, FCanvas);
  end;
  try
    APalette := CopyPalette(FImage.FPalette);
  except
    DeleteObject(Data);
    raise;
  end;
end;

function TBitmap.TransparentColorStored: Boolean;
begin
  Result := FTransparentMode = tmFixed;
end;

{ TIconImage }

destructor TIconImage.Destroy;
begin
  FMemoryImage.Free;
  inherited Destroy;
end;

procedure TIconImage.FreeHandle;
begin
  if FHandle <> 0 then DestroyIcon(FHandle);
  FHandle := 0;
end;

{ TIcon }

constructor TIcon.Create;
begin
  inherited Create;
  FTransparent := True;
  FImage := TIconImage.Create;
  FImage.Reference;
end;

destructor TIcon.Destroy;
begin
  FImage.Release;
  inherited Destroy;
end;

procedure TIcon.Assign(Source: TPersistent);
begin
  if (Source = nil) or (Source is TIcon) then
  begin
    if Source <> nil then
    begin
      TIcon(Source).FImage.Reference;
      FImage.Release;
      FImage := TIcon(Source).FImage;
    end else
      NewImage(0, nil);
    Changed(Self);
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TIcon.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  with Rect.TopLeft do
  begin
    ACanvas.RequiredState([csHandleValid]);
    DrawIconEx(ACanvas.FHandle, X, Y, Handle, 0, 0, 0, 0, DI_NORMAL);
  end;
end;

function TIcon.GetEmpty: Boolean;
begin
  with FImage do
    Result := (FHandle = 0) and (FMemoryImage = nil);
end;

function TIcon.GetHandle: HICON;
begin
  HandleNeeded;
  Result := FImage.FHandle;
end;

function TIcon.HandleAllocated: Boolean;
begin
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
end;

function TIcon.GetHeight: Integer;
begin
  Result := FImage.FSize.y;
  if Result = 0 then
    Result := GetSystemMetrics(SM_CYICON)
end;

function TIcon.GetWidth: Integer;
begin
  Result := FImage.FSize.X;
  if Result = 0 then
    Result := GetSystemMetrics(SM_CXICON);
end;

procedure TIcon.HandleNeeded;
var
  CI: TCursorOrIcon;
  NewHandle: HICON;
begin
  with FImage do
  begin
    if FHandle <> 0 then Exit;
    if FMemoryImage = nil then Exit;
    FMemoryImage.Position := 0;
    FMemoryImage.ReadBuffer(CI, SizeOf(CI));
    case CI.wType of
      RC3_STOCKICON: NewHandle := StockIcon;
      RC3_ICON: ReadIcon(FMemoryImage, NewHandle, CI.Count, SizeOf(CI),
        FRequestedSize, FSize);
    else
      InvalidIcon;
    end;
    FHandle := NewHandle;
  end;
end;

procedure TIcon.ImageNeeded;
var
  Image: TMemoryStream;
  CI: TCursorOrIcon;
begin
  with FImage do
  begin
    if FMemoryImage <> nil then Exit;
    if FHandle = 0 then InvalidIcon;
    Image := TMemoryStream.Create;
    try
      if GetHandle = StockIcon then
      begin
        FillChar(CI, SizeOf(CI), 0);
        Image.WriteBuffer(CI, SizeOf(CI));
      end
      else
        WriteIcon(Image, Handle, False);
    except
      Image.Free;
      raise;
    end;
    FMemoryImage := Image;
  end;
end;

procedure TIcon.LoadFromStream(Stream: TStream);
var
  Image: TMemoryStream;
  CI: TCursorOrIcon;
begin
  Image := TMemoryStream.Create;
  try
    Image.SetSize(Stream.Size - Stream.Position);
    Stream.ReadBuffer(Image.Memory^, Image.Size);
    Image.ReadBuffer(CI, SizeOf(CI));
    if not (CI.wType in [RC3_STOCKICON, RC3_ICON]) then InvalidIcon;
    NewImage(0, Image);
  except
    Image.Free;
    raise;
  end;
  Changed(Self);
end;

procedure TIcon.NewImage(NewHandle: HICON; NewImage: TMemoryStream);
var
  Image: TIconImage;
begin
  Image := TIconImage.Create;
  try
    Image.FHandle := NewHandle;
    Image.FMemoryImage := NewImage;
  except
    Image.Free;
    raise;
  end;
  Image.Reference;
  FImage.Release;
  FImage := Image;
end;

function TIcon.ReleaseHandle: HICON;
begin
  with FImage do
  begin
    if FRefCount > 1 then NewImage(CopyIcon(FHandle), nil);
    Result := FHandle;
    FHandle := 0;
  end;
  Changed(Self);
end;

procedure TIcon.SetHandle(Value: HICON);
begin
  NewImage(Value, nil);
  Changed(Self);
end;

procedure TIcon.SetHeight(Value: Integer);
begin
  if FImage.FHandle = 0 then
    FRequestedSize.Y := Value
  else
    InvalidOperation(@SChangeIconSize);
end;

procedure TIcon.SetTransparent(Value: Boolean);
begin
  // Ignore assignments to this property.
  // Icons are always transparent.
end;

procedure TIcon.SetWidth(Value: Integer);
begin
  if FImage.FHandle = 0 then
    FRequestedSize.X := Value
  else
    InvalidOperation(@SChangeIconSize);
end;

procedure TIcon.SaveToStream(Stream: TStream);
begin
  ImageNeeded;
  with FImage.FMemoryImage do Stream.WriteBuffer(Memory^, Size);
end;

procedure TIcon.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  InvalidOperation(@SIconToClipboard);
end;

procedure TIcon.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
begin
  InvalidOperation(@SIconToClipboard);
end;


function GraphicFilter(GraphicClass: TGraphicClass): string;
var
  Filters: string;
begin
  GetFileFormats.BuildFilterStrings(GraphicClass, Result, Filters);
end;

function GraphicExtension(GraphicClass: TGraphicClass): string;
var
  I: Integer;
begin
  for I := GetFileFormats.Count-1 downto 0 do
    if PFileFormat(FileFormats[I])^.GraphicClass.ClassName = GraphicClass.ClassName then
    begin
      Result := PFileFormat(FileFormats[I])^.Extension;
      Exit;
    end;
  Result := '';
end;

function GraphicFileMask(GraphicClass: TGraphicClass): string;
var
  Descriptions: string;
begin
  GetFileFormats.BuildFilterStrings(GraphicClass, Descriptions, Result);
end;

procedure InitScreenLogPixels;
const
  Pal16: array [0..15] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clDkGray,
     clLtGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
var
  DC: HDC;
begin
  DC := GetDC(0);
  ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0,DC);
//!!  SystemPalette16 := GetStockObject(DEFAULT_PALETTE);
  SystemPalette16 := CreateSystemPalette(Pal16);
end;

function GetDefFontCharSet: TFontCharSet;
var
  DisplayDC: HDC;
  TxtMetric: TTEXTMETRIC;
begin
  Result := DEFAULT_CHARSET;
  DisplayDC := GetDC(0);
  if (DisplayDC <> 0) then
  begin
    if (SelectObject(DisplayDC, StockFont) <> 0) then
      if (GetTextMetrics(DisplayDC, TxtMetric)) then
        Result := TxtMetric.tmCharSet;
    ReleaseDC(0, DisplayDC);
  end;
end;

procedure InitDefFontData;
var
  Charset: TFontCharset;
begin
  DefFontData.Height := -MulDiv(8, ScreenLogPixels, 72);
  if not SysLocale.FarEast then Exit;
  Charset := GetDefFontCharset;
  case Charset of
    SHIFTJIS_CHARSET:
      begin
        DefFontData.Name := '‚l‚r ‚oƒSƒVƒbƒN';
        DefFontData.Height := -MulDiv(9, ScreenLogPixels, 72);
        DefFontData.CharSet := CharSet;
      end;
  end;
end;

type
  PPattern = ^TPattern;
  TPattern = record
    Next: PPattern;
    Bitmap: TBitmap;
    BkColorRef: TColorRef;
    FgColorRef: TColorRef;
  end;

  TPatternManager = class(TObject)
  private
    List: PPattern;
    FLock: TRTLCriticalSection;
    function CreateBitmap(BkColor, FgColor: TColor): TBitmap;
  public
    constructor Create;
    destructor Destroy; override;
    function AllocPattern(BkColor, FgColor: TColorRef): PPattern;
    procedure FreePatterns;
    procedure Lock;
    procedure Unlock;
  end;

constructor TPatternManager.Create;
begin
  InitializeCriticalSection(FLock);
end;

destructor TPatternManager.Destroy;
begin
  FreePatterns;
  DeleteCriticalSection(FLock);
end;

procedure TPatternManager.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TPatternManager.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TPatternManager.AllocPattern(BkColor, FgColor: TColorRef): PPattern;
begin
  Lock;
  try
    Result := List;
    while (Result <> nil) and ((Result^.BkColorRef <> BkColor) or
      (Result^.FgColorRef <> FgColor)) do
      Result := Result^.Next;
    if Result = nil then
    begin
      GetMem(Result, SizeOf(TPattern));
      with Result^ do
      begin
        Next := List;
        Bitmap := CreateBitmap(BkColor, FgColor);
        BkColorRef := BkColor;
        FgColorRef := FgColor;
      end;
      List := Result;
    end;
  finally
    Unlock;
  end;
end;

function TPatternManager.CreateBitmap(BkColor, FgColor: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Width := 8;
      Height := 8;
      with Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := BkColor;
        FillRect(Rect(0, 0, Width, Height));
        for Y := 0 to 8 do
          for X := 0 to 8 do
            if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
              Pixels[X, Y] := FgColor;     { on even/odd rows }
      end;
      Dormant;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TPatternManager.FreePatterns;
var
  P: PPattern;
begin
  while List <> nil do
  begin
    P := List;
    with P^ do
    begin
      Lock;
      try
        List := Next
      finally
        Unlock;
      end;
      if Bitmap <> nil then Bitmap.Free;
    end;
    FreeMem(P);
  end;
end;

var
  PatternManager: TPatternManager;


function AllocPatternBitmap(BkColor, FgColor: TColor): TBitmap;
begin
  if PatternManager <> nil then
    Result := PatternManager.AllocPattern(ColorToRGB(BkColor),
      ColorToRGB(FgColor)).Bitmap
    else
      Result := nil;
end;

initialization
  InitScreenLogPixels;
  InitializeCriticalSection(BitmapImageLock);
  InitializeCriticalSection(CounterLock);
  StockPen := GetStockObject(BLACK_PEN);
  StockBrush := GetStockObject(HOLLOW_BRUSH);
  StockFont := GetStockObject(SYSTEM_FONT);
  StockIcon := LoadIcon(0, IDI_APPLICATION);
  InitDefFontData;
  FontManager := TResourceManager.Create(SizeOf(TFontData));
  PenManager := TResourceManager.Create(SizeOf(TPenData));
  BrushManager := TResourceManager.Create(SizeOf(TBrushData));
  PatternManager := TPatternManager.Create;
  BitmapCanvasList := TThreadList.Create;
  CanvasList := TThreadList.Create;
  RegisterIntegerConsts(TypeInfo(TColor), IdentToColor, ColorToIdent);
  RegisterIntegerConsts(TypeInfo(TFontCharset), IdentToCharset, CharsetToIdent);
finalization
  PatternManager.Free;
  FileFormats.Free;
  ClipboardFormats.Free;
  FreeMemoryContexts;
  BitmapCanvasList.Free;
  CanvasList.Free;
  FontManager.Free;
  PenManager.Free;
  BrushManager.Free;
  DeleteObject(SystemPalette16);
  DeleteCriticalSection(BitmapImageLock);
  DeleteCriticalSection(CounterLock);
end.
