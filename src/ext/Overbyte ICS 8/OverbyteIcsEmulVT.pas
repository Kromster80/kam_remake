{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      EMULVT.PAS
Description:  Delphi component which does Ansi terminal emulation
              Not every escape sequence is implemented, but a large subset.
Author:       François PIETTE
Creation:     May, 1996
Version:      8.03
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2016 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Jul 22, 1997  Some optimization
              Adapted to Delphi 3
Sep 05, 1997  Version 2.01
Dec 16, 1997  V2.02 Corrected a bug int the paint routine which caused GDI
                    resource leak when color was used.
Feb 24, 1998  V2.03 Added AddFKey function
Jul 15, 1998  V2.04 Adapted to Delphi 4 (moved DoKeyBuffer to protected section)
Dec 04, 1998  V2.05 Added 'single char paint' and 'char zoom' features.
Dec 09, 1998  V2.10 Added graphic char drawing using graphic primitives
                    Added (with permission) scroll back code developed by Steve
                    Endicott <s_endicott@compuserve.com>
Dec 21, 1998  V2.11 Corrected some screen update problems related to scrollback.
                    Added fixes from Steve Endicott.
                    Beautified code.
Mar 14, 1999  V2.12 Added OnKeyDown event.
                    Corrected a missing band at right of screen when painting.
Aug 15, 1999  V2.13 Moved KeyPress procedure to public section for BCB4 compat.
Aug 20, 1999  V2.14 Added compile time options. Revised for BCB4.
Nov 12, 1999  V2.15 Corrected display attribute error in delete line.
                    Checked for range in SetLines/GetLine
Aug 09, 2000  V2.16 Wilfried Mestdagh" <wilfried_sonal@compuserve.com> and
                    Steve Endicott <s_endicott@compuserve.com> corrected a
                    bug related to scroll back buffer. See WM + SE 09/08/00
                    tags in code.
Jul 28, 2001  V2.17 Made FCharPos and FLinePos member variables instead of
                    global to avoid conflict when sevaral components are used
                    simultaneously. Suggested by Jeroen Cranendonk
                    <j.p.cranendonk@student.utwente.nl>
Jan 03, 2002  V2.19 Don't adjust scroll bar if not visible
                    Make properties with TopMargin, LeftMargin, RightMargin,
                    BottomMargin.
Jan 10, 2002  V2.19 Fixed SetLineHeight to fill FLinePos up to last item.
May 31, 2004  V2.20 Used ICSDEFS.INC
Jan 10, 2005  V2.21 Fixed TCustomEmulVT.SetCaret so that caret is updated
                    only when component has the focus. Thanks to Xie, Qi-Ming
                    <xieqm@axp1.csie.ncu.edu.tw> for finding an easy test case.
Jan 13, 2005 V2.22 Replaced symbol "Debug" by "DEBUG_OUTPUT"
Mar 26, 2006 V2.23 Added key #$1C (Enter key) in all 3 FKeys tables so that
                   using the enter key is the same as sending CRLF.
Mar 26, 2006 V6.00 New version 6 started from V5
Feb 12, 2008 V6.01 Added LogFileName property so name and path can be changed
Mar 24, 2008 V6.02 Francois Piette made some changes to prepare code
                   for Unicode.
Jul 20, 2008 V6.03 F. Piette made some more changes for UniCode
                   Currently the component works as AnsiChar emulator. More
                   changes are needed to make it works as a full unicode
                   emulator.
Aug 15, 2008 V7.00 Delphi 2009 (Unicode) support. The terminal is not
             unicode, but the component support unicode strings.
Oct 03, 2008 V7.01 A. Garrels moved IsCharInSysCharSet, xdigit and xdigit2
                   to OverbyteIcsUtils.pas.
Mar 03, 2011 V7.02 F.Piette fixed TScreen.Eol
May 06, 2011 V7.03 Small change to prepare for 64-bit.
Jul 17, 2011 V7.04 Arno fixed some bugs with non-Windows-1252 code pages.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Dec 28, 2013 V8.01 Arno fixed a bug with file IO in TCustomEmulVT.SetLog
May 28, 2014 v8.02 DrJohn fixed problem with (border) colours
                   AutoResize property added with improved font resizing
                   SoundOn property added
                   GetScreenText function added
Feb 20, 2016 V8.03 Angus fixed typos COMPILE12_UP

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsEmulVT;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

{$DEFINE SINGLE_CHAR_PAINT}
{$DEFINE CHAR_ZOOM}

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$IFDEF USEWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
{$IFDEF COMPILER20_UP}
    {$IFDEF RTL_NAMESPACES}System.UITypes{$ELSE}UITypes{$ENDIF},
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Graphics{$ELSE}Graphics{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Controls{$ELSE}Controls{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.StdCtrls{$ELSE}StdCtrls{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.ClipBrd{$ELSE}ClipBrd{$ENDIF},
    OverbyteIcsUtils;

const
  EmulVTVersion      = 803;
  CopyRight : String = ' TEmulVT (c) 1996-2016 F. Piette V8.03 ';
  MAX_ROW            = 50;
  MAX_COL            = 160;
  NumPaletteEntries  = 16;

type
  TBackColors     = (vtsBlack, vtsRed,     vtsGreen, vtsYellow,
                     vtsBlue,  vtsMagenta, vtsCyan,  vtsWhite);

  TScreenOption   = (vtoBackColor, vtoCopyBackOnClear);
  TScreenOptions  = set of TScreenOption;
  TXlatTable      = array [0..255] of AnsiChar;
  PXlatTable      = ^TXlatTable;
  TFuncKeyValue   = String;
  PFuncKeyValue   = ^TFuncKeyValue;
  TFuncKey        = record
                        ScanCode : Char;
                        Shift    : TShiftState;
                        Ext      : Boolean;
                        Value    : TFuncKeyValue;
                    end;
  TFuncKeysTable  = array [0..63] of TFuncKey;
  PFuncKeysTable  = ^TFuncKeysTable;
  TKeyBufferEvent = procedure (Sender : TObject; Buffer : PChar; Len : Integer) of object;
  TKeyDownEvent   = procedure (Sender        : TObject;
                               var VirtKey   : Integer;
                               var Shift     : TShiftState;
                               var ShiftLock : Boolean;
                               var ScanCode  : Char;
                               var Ext       : Boolean) of object;
  TNFont = class(TFont)                                           {drjohn}
  private
     FPenColor  : TBackColors;
     FBackColor : TBackColors;
  published
     property Color  : TBackColors  read FPenColor  write FPenColor;
     property BackColor : TBackColors  read FBackColor write FBackColor;
     property Charset;
     property Height;
     property Name;
     property Pitch;
     property Size;
     property Style;
  end;


type
  { TLine is an object used to hold one line of text on screen }
  TLine = class(TObject)
  public
    // The code is written with conditional compilation so that Txt can be
    // either AnsiChar or Char. Eventually, it will be Char when the
    // component fully support unicode
{$IFDEF UNICODE}                                                     {drjohn}
    Txt : array [0..MAX_COL] of Char;                                {drjohn}
{$ELSE}                                                              {drjohn}
    Txt : array [0..MAX_COL] of AnsiChar;
{$ENDIF}                                                             {drjohn}
    Att : array [0..MAX_COL] of Byte;
    constructor Create;
    procedure   Clear(Attr : Byte);
  end;
  TLineArray      = array [0..16382] of TLine;
  PLineArray      = ^TLineArray;

  { TScreen is an object to hold an entire screen of line and handle }
  { Ansi escape sequences to update this virtual screen              }
  TScreen = class(TObject)
  public
    FLines           : PLineArray;
    FRow             : Integer;
    FCol             : Integer;
    FRowSaved        : Integer;
    FColSaved        : Integer;
    FScrollRowTop    : Integer;
    FScrollRowBottom : Integer;
    FAttribute       : Byte;
    FDefAttribute    : Byte;                        {JMP }
    FForceHighBit    : Boolean;
    FReverseVideo    : Boolean;
    FUnderLine       : Boolean;
    FRowCount        : Integer;
    FColCount        : Integer;
    FBackRowCount    : Integer;
    FBackEndRow      : Integer;
    FBackColor       : TBackColors;
    FOptions         : TScreenOptions;
    FEscBuffer       : String{$IFNDEF COMPILER12_UP}[80]{$ENDIF};
    FEscFlag         : Boolean;
    Focused          : Boolean;
    FAutoLF          : Boolean;
    FAutoCR          : Boolean;
    FAutoWrap        : Boolean;
    FCursorOff       : Boolean;
    FCKeyMode        : Boolean;
    FNoXlat          : Boolean;
    FNoXlatInitial   : Boolean;
    FCntLiteral      : Integer;
    FCarbonMode      : Boolean;
    FSound           : Boolean;                     {drjohn}
    FXlatInputTable  : PXlatTable;
    FXlatOutputTable : PXlatTable;
    FCharSetG0       : Char;
    FCharSetG1       : Char;
    FCharSetG2       : Char;
    FCharSetG3       : Char;
    FAllInvalid      : Boolean;
    FInvRect         : TRect;
    FFont            : TNFont;                        {drjohn}
    FOnCursorVisible : TNotifyEvent;
    constructor Create;
    destructor  Destroy; override;
    procedure   AdjustFLines(NewCount : Integer);
    procedure   CopyScreenToBack;
    procedure   SetRowCount(NewCount : Integer);
    procedure   SetBackRowCount(NewCount : Integer);
    procedure   InvRect(nRow, nCol : Integer);
    procedure   InvClear;
    procedure   SetLines(I : Integer; Value : TLine);
    function    GetLines(I : Integer) : TLine;
    procedure   WriteChar(Ch : Char); {$IFDEF COMPILER12_UP} overload;
    procedure   WriteChar(Ch : AnsiChar); overload;
{$ENDIF}
    procedure   WriteStr(Str : String);
    function    ReadStr : String;
    procedure   GotoXY(X, Y : Integer);
    procedure   WriteLiteralChar(Ch : Char);
    procedure   ProcessEscape(EscCmd : Char);
    procedure   SetAttr(Att : Char);
    procedure   SetFontColors;                                  {drjohn}
    procedure   CursorRight;
    procedure   CursorLeft;
    procedure   CursorDown;
    procedure   CursorUp;
    procedure   CarriageReturn;
    procedure   ScrollUp;
    procedure   ScrollDown;
    procedure   ClearScreen;
    procedure   BackSpace;
    procedure   Eol;
    procedure   Eop;
    procedure   ProcessESC_D;                { Index                   }
    procedure   ProcessESC_M;                { Reverse index           }
    procedure   ProcessESC_E;                { Next line               }
    procedure   ProcessCSI_u;                { Restore Cursor          }
    procedure   ProcessCSI_I;                { Select IBM char set     }
    procedure   ProcessCSI_J;                { Clear the screen        }
    procedure   ProcessCSI_K;                { Erase to End of Line    }
    procedure   ProcessCSI_L;                { Insert Line             }
    procedure   ProcessCSI_M;                { Delete Line             }
    procedure   ProcessCSI_m_lc;             { Select Attributes       }
    procedure   ProcessCSI_n_lc;             { Cursor position report  }
    procedure   ProcessCSI_at;               { Insert character        }
    procedure   ProcessCSI_r_lc;             { Scrolling margins       }
    procedure   ProcessCSI_s_lc;             { Save cursor location    }
    procedure   ProcessCSI_u_lc;             { Restore cursor location }
    procedure   ProcessCSI_7;                { Save cursor location    }
    procedure   ProcessCSI_8;                { Restore cursor location }
    procedure   ProcessCSI_H;                { Set Cursor Position     }
    procedure   ProcessCSI_h_lc;             { Terminal mode set       }
    procedure   ProcessCSI_l_lc;             { Terminal mode reset     }
    procedure   ProcessCSI_A;                { Cursor Up               }
    procedure   ProcessCSI_B;                { Cursor Down             }
    procedure   ProcessCSI_C;                { Cursor Right            }
    procedure   ProcessCSI_D;                { Cursor Left             }
    procedure   ProcessCSI_P;                { Delete Character        }
    procedure   ProcessCSI_S;                { Scroll up               }
    procedure   ProcessCSI_T;                { Scroll down             }
    procedure   process_charset_G0(EscCmd : Char);{ G0 character set   }
    procedure   process_charset_G1(EscCmd : Char);{ G1 character set   }
    procedure   process_charset_G2(EscCmd : Char);{ G2 character set   }
    procedure   process_charset_G3(EscCmd : Char);{ G3 character set   }
    procedure   UnimplementedEscape(EscCmd : Char);
    procedure   InvalidEscape(EscCmd : Char);
    function    GetEscapeParam(From : Integer; var Value : Integer) : Integer;
    property    OnCursorVisible : TNotifyEvent read  FonCursorVisible
                                               write FOnCursorVisible;
    property    Lines[I : Integer] : TLine read GetLines write SetLines;
  end;

  { TCustomEmulVT is an visual component which does the actual display }
  { of a TScreen object which is the virtual screen                    }
  { No property is published. See TEmulVT class                        }
  TCustomEmulVT = class(TCustomControl)
  private
    FCharPos         : array [0..MAX_COL + 1] of integer;
    FLinePos         : array [0..MAX_ROW + 1] of integer;
    FFileHandle      : TextFile;
    FCursorVisible   : Boolean;
    FCaretShown      : Boolean;
    FCaretCreated    : Boolean;
    FLineHeight      : Single;            {drjohn}
    FLineZoom        : Single;
    FCharWidth       : Single;             {drjohn}
    FCharZoom        : Single;
    FColCount          : integer;          {drjohn}
    FGraphicDraw     : Boolean;
    FInternalLeading : Integer;
    FBorderStyle     : TBorderStyle;
    FBorderWidth     : Integer;
    FAutoRepaint     : Boolean;
//    FFont            : TFont;             {drjohn}
    FVScrollBar      : TScrollBar;
    FTopLine         : Integer;
    FLocalEcho       : Boolean;
    FOnKeyBuffer     : TKeyBufferEvent;
    FOnKeyDown       : TKeyDownEvent;
    FFKeys           : Integer;
    FMonoChrome      : Boolean;
    FLog             : Boolean;
    FLogFileName     : String;   // angus V6.01
    FAppOnMessage    : TMessageEvent;
    FFlagCirconflexe : Boolean;
    FFlagTrema       : Boolean;
    FSelectRect      : TRect;
    FTopMargin       : Integer;
    FLeftMargin      : Integer;
    FRightMargin     : Integer;
    FBottomMargin    : Integer;
    FPal             : HPalette;
    FPaletteEntries  : array[0..NumPaletteEntries - 1] of TPaletteEntry;
    FMarginColor     : Integer;
    FResize          : Boolean;                 {drjohn}
    procedure   WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure   WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure   WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure   WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure   WMPaletteChanged(var Message : TMessage); message WM_PALETTECHANGED;
    procedure   VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure   VResize(Sender: TObject);               {drjohn}
    procedure   SetCaret;
    procedure   AdjustScrollBar;
    function    ProcessFKeys(ScanCode: Char; Shift: TShiftState; Ext: Boolean) : Boolean;
    function    FindFKeys(ScanCode: Char; Shift: TShiftState;
                          Ext: Boolean) : PFuncKeyValue;
    procedure   CursorVisibleEvent(Sender : TObject);
    procedure   SetFont(Value : TNFont);                    {drjohn}
    procedure   SetAutoLF(Value : Boolean);
    procedure   SetAutoCR(Value : Boolean);
    procedure   SetXlat(Value : Boolean);
    procedure   SetLog(Value : Boolean);
    procedure   SetRows(Value : Integer);
    procedure   SetCols(Value : Integer);
    procedure   SetBackRows(Value : Integer);
    procedure   SetTopLine(Value : Integer);
    procedure   SetBackColor(Value : TBackColors);
    procedure   SetOptions(Value : TScreenOptions);
    procedure   SetLineHeight(Value : Single);                {drjohn}
    function    GetFont : TNFont;                           {drjohn}
    function    GetAutoLF : Boolean;
    function    GetAutoCR : Boolean;
    function    GetXlat : Boolean;
    function    GetRows : Integer;
    function    GetCols : Integer;
    function    GetBackRows : Integer;
    function    GetBackColor : TBackColors;
    function    GetOptions : TScreenOptions;
    function    GetSound : boolean;                         {JMP }
    function    GetResize : boolean;                        {drjohn}
    procedure   SetMarginColor(const Value: Integer);
    procedure   SetLeftMargin(const Value: Integer);
    procedure   SetBottomMargin(const Value: Integer);
    procedure   SetRightMargin(const Value: Integer);
    procedure   SetTopMargin(const Value: Integer);
    procedure   SetSound(const Value: boolean );              {drjohn}
    procedure   SetResize(const Value: boolean );             {drjohn}

  protected
    FScreen          : TScreen;
    procedure   AppMessageHandler(var Msg: TMsg; var Handled: Boolean);
    procedure   DoKeyBuffer(Buffer : PChar; Len : Integer); virtual;
    procedure   PaintGraphicChar(DC   : HDC;
                                 X, Y : Integer;
                                 rc   : PRect;
                                 ch   : Char);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ShowCursor;
    procedure   SetCursor(Row, Col : Integer);
    procedure   WriteChar(Ch : Char); overload;
{$IFDEF COMPILER12_UP}
    procedure   WriteChar(Ch : AnsiChar); overload;
{$ENDIF}
    procedure   WriteStr(Str : String);
    procedure   WriteBuffer(Buffer : PChar; Len : Integer); overload;
{$IFDEF COMPILER12_UP}    { V8.03 }
    procedure   WriteBuffer(Buffer : PAnsiChar; Len : Integer); overload;
{$ENDIF}
    function    ReadStr : String;
    function    GetScreenText(Buffer: PChar; BufSize: Integer): Integer;      {drjohn}
    procedure   CopyHostScreen;
    procedure   Clear;
    procedure   UpdateScreen;
    function    SnapPixelToRow(Y : Integer) : Integer;
    function    SnapPixelToCol(X : Integer) : Integer;
    function    PixelToRow(Y : Integer) : Integer;
    function    PixelToCol(X : Integer) : Integer;
    procedure   MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    procedure   SetLineZoom(newValue : Single);
    procedure   SetCharWidth(newValue : Single);                {drjohn}
    procedure   SetCharZoom(newValue : Single);
    procedure   KeyPress(var Key: Char); override;
    property    LineZoom  : Single        read FLineZoom     write SetLineZoom;
    property    CharWidth : Single        read FCharWidth    write SetCharWidth;      {drjohn}
    property    CharZoom  : Single        read FCharZoom     write SetCharZoom;
    property    GraphicDraw : Boolean     read FGraphicDraw  write FGraphicDraw;
    property    TopLine     : Integer     read FTopLine      write SetTopLine;
    property    VScrollBar  : TScrollBar  read FVScrollBar;
    property    TopMargin       : Integer read FTopMargin    write SetTopMargin;
    property    LeftMargin      : Integer read FLeftMargin   write SetLeftMargin;
    property    RightMargin     : Integer read FRightMargin  write SetRightMargin;
    property    BottomMargin    : Integer read FBottomMargin write SetBottomMargin;
    property    MarginColor     : Integer read FMarginColor  write SetMarginColor;
  private
    procedure   PaintOneLine(DC: HDC; Y, Y1 : Integer; const Line : TLine;
                             nColFrom : Integer; nColTo : Integer; Blank : Boolean);
    procedure   SetupFont;
    property Text : String read ReadStr write WriteStr;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
    property OnResize;              {drjohn}
    property OnKeyPress;
    property OnKeyBuffer : TKeyBufferEvent read FOnKeyBuffer write FOnKeyBuffer;
    property OnKeyDown   : TKeyDownEvent   read FOnKeyDown   write FOnKeyDown;
    property Ctl3D;
    property Align;
    property TabStop;
    property TabOrder;
    Property AutoResize  : Boolean     read GetResize    write SetResize;               {drjohn}
    Property SoundOn  : Boolean        read GetSound     write SetSound;                {drjohn}
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property AutoRepaint : Boolean     read FAutoRepaint write FAutoRepaint;
    property Font : TNFont             read GetFont      write SetFont;             {drjohn}
    property LocalEcho : Boolean       read FLocalEcho   write FLocalEcho;
    property AutoLF : Boolean          read GetAutoLF    write SetAutoLF;
    property AutoCR : Boolean          read GetAutoCR    write SetAutoCR;
    property Xlat : Boolean            read GetXlat      write SetXlat;
    property MonoChrome : Boolean      read FMonoChrome  write FMonoChrome;
    property Log : Boolean             read FLog         write SetLog;
    property LogFileName : String      read FLogFileName write FLogFileName;   // angus V6.01
    property Rows : Integer            read GetRows      write SetRows;
    property Cols : Integer            read GetCols      write SetCols;
    property LineHeight : Single       read FLineHeight  write SetLineHeight;               {drjohn}
    property FKeys : Integer           read FFKeys       write FFKeys;
    property SelectRect : TRect        read FSelectRect  write FSelectRect;
    property BackRows : Integer        read GetBackRows  write SetBackRows;
    property BackColor : TBackColors   read GetBackColor write SetBackColor;
    property Options : TScreenOptions  read GetOptions   write SetOptions;
  end;

  { Same as TCustomEmulVT, but with published properties }
  TEmulVT = class(TCustomEmulVT)
  public
    property Screen : TScreen read FScreen;
    property SelectRect;
    property Text;
  published
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnClick;
    property OnResize;              {drjohn}
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyBuffer;
    property Ctl3D;
    property Align;
    property BorderStyle;
    property AutoRepaint;
    property Font;
    property LocalEcho;
    property AutoLF;
    property AutoCR;
    property Xlat;
    property MonoChrome;
    property Log;
    property LogFileName;        // angus V6.01
    property Rows;
    property Cols;
    property BackRows;
    property BackColor;
    property Options;
    property LineHeight;
    property CharWidth;
    property SoundOn;               {drjohn}
    property AutoReSize;            {drjohn}
    property TabStop;
    property TabOrder;
    property FKeys;
    property TopMargin;
    property LeftMargin;
    property RightMargin;
    property BottomMargin;
    property MarginColor;
  end;

const
 { F_BLACK   = $00;              // These are never used
  F_BLUE    = $01;
  F_GREEN   = $02;
  F_CYAN    = $03;
  F_RED     = $04;
  F_MAGENTA = $05;
  F_BROWN   = $06; }       {drjohn}
  F_WHITE   = $07;

 { B_BLACK   = $00;
  B_BLUE    = $01;
  B_GREEN   = $02;
  B_CYAN    = $03;
  B_RED     = $04;
  B_MAGENTA = $05;
  B_BROWN   = $06;
  B_WHITE   = $07;   }     {drjohn}

  F_INTENSE = $08;
  B_BLINK   = $80;

{$IFDEF UNICODE}
  { Function keys (SCO Console) }
  FKeys1 : TFuncKeysTable = (
      (ScanCode: #$0048; Shift: []; Ext: TRUE ; Value: #$001B + '[A'),   { UP    }
      (ScanCode: #$0050; Shift: []; Ext: TRUE ; Value: #$001B + '[B'),   { DOWN  }
      (ScanCode: #$004D; Shift: []; Ext: TRUE ; Value: #$001B + '[C'),   { RIGHT }
      (ScanCode: #$004B; Shift: []; Ext: TRUE ; Value: #$001B + '[D'),   { LEFT  }
      (ScanCode: #$0049; Shift: []; Ext: TRUE ; Value: #$001B + '[I'),   { PREV  }
      (ScanCode: #$0051; Shift: []; Ext: TRUE ; Value: #$001B + '[G'),   { NEXT  }
      (ScanCode: #$0047; Shift: []; Ext: TRUE ; Value: #$001B + '[H'),   { HOME  }
      (ScanCode: #$004F; Shift: []; Ext: TRUE ; Value: #$001B + '[F'),   { END   }
      (ScanCode: #$0052; Shift: []; Ext: TRUE ; Value: #$001B + '[L'),   { INS   }
      (ScanCode: #$000F; Shift: []; Ext: FALSE; Value: #$001B + '[Z'),   { RTAB  }
      (ScanCode: #$0053; Shift: []; Ext: TRUE ; Value: #$007F       ),   { DEL   }
      (ScanCode: #$003B; Shift: []; Ext: FALSE; Value: #$001B + '[M'),   { F1    }
      (ScanCode: #$003C; Shift: []; Ext: FALSE; Value: #$001B + '[N'),
      (ScanCode: #$003D; Shift: []; Ext: FALSE; Value: #$001B + '[O'),
      (ScanCode: #$003E; Shift: []; Ext: FALSE; Value: #$001B + '[P'),
      (ScanCode: #$003F; Shift: []; Ext: FALSE; Value: #$001B + '[Q'),
      (ScanCode: #$0040; Shift: []; Ext: FALSE; Value: #$001B + '[R'),
      (ScanCode: #$0041; Shift: []; Ext: FALSE; Value: #$001B + '[S'),
      (ScanCode: #$0042; Shift: []; Ext: FALSE; Value: #$001B + '[T'),
      (ScanCode: #$0043; Shift: []; Ext: FALSE; Value: #$001B + '[U'),
      (ScanCode: #$0044; Shift: []; Ext: FALSE; Value: #$001B + '[V'),   { F10   }
      (ScanCode: #$0085; Shift: []; Ext: FALSE; Value: #$001B + '[W'),   { F11   }
      (ScanCode: #$0086; Shift: []; Ext: FALSE; Value: #$001B + '[X'),   { F12   }
      (ScanCode: #$003B; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[V'),{ SF1 should be 'Y' }
      (ScanCode: #$003C; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[Z'),
      (ScanCode: #$003D; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[a'),
      (ScanCode: #$003E; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[b'),
      (ScanCode: #$003F; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[c'),
      (ScanCode: #$0040; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[d'),
      (ScanCode: #$0041; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[e'),
      (ScanCode: #$0042; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[f'),
      (ScanCode: #$0043; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[g'),
      (ScanCode: #$0044; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[h'),
      (ScanCode: #$0085; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[i'),
      (ScanCode: #$0086; Shift: [ssShift]; Ext: FALSE; Value: #$001B + '[j'),{ SF10 }
      (ScanCode: #$003B; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[k'), { CF1  }
      (ScanCode: #$003C; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[l'),
      (ScanCode: #$003D; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[m'),
      (ScanCode: #$003E; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[n'),
      (ScanCode: #$003F; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[o'),
      (ScanCode: #$0040; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[p'),
      (ScanCode: #$0041; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[q'),
      (ScanCode: #$0042; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[r'),
      (ScanCode: #$0043; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[s'),
      (ScanCode: #$0044; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[t'),
      (ScanCode: #$0085; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[u'),
      (ScanCode: #$0086; Shift: [ssCtrl]; Ext: FALSE; Value: #$001B + '[v'),   { CF12 }
      (ScanCode: #$001C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         )
      );

{ Alternate function keys (ordinary VT keys) }
  FKeys2 : TFuncKeysTable = (
      (ScanCode: #$0048; Shift: []; Ext: TRUE ; Value: #$001B + '[A'),   { UP      }
      (ScanCode: #$0050; Shift: []; Ext: TRUE ; Value: #$001B + '[B'),   { DOWN    }
      (ScanCode: #$004D; Shift: []; Ext: TRUE ; Value: #$001B + '[C'),   { RIGHT   }
      (ScanCode: #$004B; Shift: []; Ext: TRUE ; Value: #$001B + '[D'),   { LEFT    }
      (ScanCode: #$0049; Shift: []; Ext: TRUE ; Value: #$001B + '[5~'),  { PREV    }
      (ScanCode: #$0051; Shift: []; Ext: TRUE ; Value: #$001B + '[6~'),  { NEXT    }
      (ScanCode: #$0052; Shift: []; Ext: TRUE ; Value: #$001B + '[2~'),  { INSERT  }
      (ScanCode: #$0053; Shift: []; Ext: TRUE ; Value: #$007F       ),   { DELETE  }
      (ScanCode: #$003B; Shift: []; Ext: FALSE; Value: #$001B + 'OP'),   { F1->PF1 }
      (ScanCode: #$003C; Shift: []; Ext: FALSE; Value: #$001B + 'OQ'),   { F2->PF2 }
      (ScanCode: #$003D; Shift: []; Ext: FALSE; Value: #$001B + 'OR'),   { F3->PF3 }
      (ScanCode: #$003E; Shift: []; Ext: FALSE; Value: #$001B + 'OS'),   { F4->PF4 }
      (ScanCode: #$0057; Shift: []; Ext: FALSE; Value: #$001B + '[28~'), { F11->Aide }
      (ScanCode: #$0058; Shift: []; Ext: FALSE; Value: #$001B + '[29~'), { F12->Executer }
      (ScanCode: #$001C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         )
      );

{ A-Series Telnet function keys (ordinary VT100 keys + specials) }
  FKeys3 : TFuncKeysTable = (
      (ScanCode: #$0048; Shift: []; Ext: TRUE ; Value: #$001B + '[A'),   { UP      }
      (ScanCode: #$0050; Shift: []; Ext: TRUE ; Value: #$001B + '[B'),   { DOWN    }
      (ScanCode: #$004D; Shift: []; Ext: TRUE ; Value: #$001B + '[C'),   { RIGHT   }
      (ScanCode: #$004B; Shift: []; Ext: TRUE ; Value: #$001B + '[D'),   { LEFT    }
      (ScanCode: #$0049; Shift: []; Ext: TRUE ; Value: #$001B + '-'),    { PREV    }
      (ScanCode: #$0051; Shift: []; Ext: TRUE ; Value: #$001B + '+'),    { NEXT    }
      (ScanCode: #$0047; Shift: []; Ext: TRUE ; Value: #$001B + 'H'),    { HOME    }
      (ScanCode: #$0047; Shift: [ssCtrl]; Ext: TRUE ; Value: #$001B + 'C'),{ HOME  }
      (ScanCode: #$004F; Shift: []; Ext: TRUE ; Value: #$001B + 'R'),    { END     }
      (ScanCode: #$0052; Shift: []; Ext: TRUE ; Value: #$001B + 'I'),    { INSERT  }
      (ScanCode: #$0053; Shift: []; Ext: TRUE ; Value: #$007F       ),   { DELETE  }
      (ScanCode: #$003B; Shift: []; Ext: FALSE; Value: #$001B + 'OP'),   { F1->PF1 }
      (ScanCode: #$003C; Shift: []; Ext: FALSE; Value: #$001B + 'OQ'),   { F2->PF2 }
      (ScanCode: #$003D; Shift: []; Ext: FALSE; Value: #$001B + 'OR'),   { F3->PF3 }
      (ScanCode: #$003E; Shift: []; Ext: FALSE; Value: #$001B + 'OS'),   { F4->PF4 }
      (ScanCode: #$0043; Shift: []; Ext: FALSE; Value: #$001B + 'OP'),   { F9      }
      (ScanCode: #$0044; Shift: []; Ext: FALSE; Value: ''),            { F10     }
      (ScanCode: #$0057; Shift: []; Ext: FALSE; Value: #$001B + 'OQ'),   { F11     }
      (ScanCode: #$0058; Shift: []; Ext: FALSE; Value: #$001B + 'OS'),   { F12     }
      (ScanCode: #$000F; Shift: []; Ext: FALSE; Value: #$001B + 'Z'),    { RTAB    }
      (ScanCode: #$0040; Shift: []; Ext: FALSE; Value: #$001B + 'K'),    { F6      }
      (ScanCode: #$0053; Shift: [ssCtrl]; Ext: TRUE ; Value: #$001B + 'D'), { CDEL }
      (ScanCode: #$0052; Shift: [ssCtrl]; Ext: TRUE ; Value: #$001B + 'L'), { CINS }
      (ScanCode: #$001C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$0000; Shift: []; Ext: FALSE; Value: ''         )
      );
{$ELSE UNICODE}
 { Function keys (SCO Console) }
  FKeys1 : TFuncKeysTable = (
      (ScanCode: #$48; Shift: []; Ext: TRUE ; Value: #$1B + '[A'),   { UP    }
      (ScanCode: #$50; Shift: []; Ext: TRUE ; Value: #$1B + '[B'),   { DOWN  }
      (ScanCode: #$4D; Shift: []; Ext: TRUE ; Value: #$1B + '[C'),   { RIGHT }
      (ScanCode: #$4B; Shift: []; Ext: TRUE ; Value: #$1B + '[D'),   { LEFT  }
      (ScanCode: #$49; Shift: []; Ext: TRUE ; Value: #$1B + '[I'),   { PREV  }
      (ScanCode: #$51; Shift: []; Ext: TRUE ; Value: #$1B + '[G'),   { NEXT  }
      (ScanCode: #$47; Shift: []; Ext: TRUE ; Value: #$1B + '[H'),   { HOME  }
      (ScanCode: #$4F; Shift: []; Ext: TRUE ; Value: #$1B + '[F'),   { END   }
      (ScanCode: #$52; Shift: []; Ext: TRUE ; Value: #$1B + '[L'),   { INS   }
      (ScanCode: #$0F; Shift: []; Ext: FALSE; Value: #$1B + '[Z'),   { RTAB  }
      (ScanCode: #$53; Shift: []; Ext: TRUE ; Value: #$7F       ),   { DEL   }
      (ScanCode: #$3B; Shift: []; Ext: FALSE; Value: #$1B + '[M'),   { F1    }
      (ScanCode: #$3C; Shift: []; Ext: FALSE; Value: #$1B + '[N'),
      (ScanCode: #$3D; Shift: []; Ext: FALSE; Value: #$1B + '[O'),
      (ScanCode: #$3E; Shift: []; Ext: FALSE; Value: #$1B + '[P'),
      (ScanCode: #$3F; Shift: []; Ext: FALSE; Value: #$1B + '[Q'),
      (ScanCode: #$40; Shift: []; Ext: FALSE; Value: #$1B + '[R'),
      (ScanCode: #$41; Shift: []; Ext: FALSE; Value: #$1B + '[S'),
      (ScanCode: #$42; Shift: []; Ext: FALSE; Value: #$1B + '[T'),
      (ScanCode: #$43; Shift: []; Ext: FALSE; Value: #$1B + '[U'),
      (ScanCode: #$44; Shift: []; Ext: FALSE; Value: #$1B + '[V'),   { F10   }
      (ScanCode: #$85; Shift: []; Ext: FALSE; Value: #$1B + '[W'),   { F11   }
      (ScanCode: #$86; Shift: []; Ext: FALSE; Value: #$1B + '[X'),   { F12   }
      (ScanCode: #$3B; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[V'),{ SF1 should be 'Y' }
      (ScanCode: #$3C; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[Z'),
      (ScanCode: #$3D; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[a'),
      (ScanCode: #$3E; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[b'),
      (ScanCode: #$3F; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[c'),
      (ScanCode: #$40; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[d'),
      (ScanCode: #$41; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[e'),
      (ScanCode: #$42; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[f'),
      (ScanCode: #$43; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[g'),
      (ScanCode: #$44; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[h'),
      (ScanCode: #$85; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[i'),
      (ScanCode: #$86; Shift: [ssShift]; Ext: FALSE; Value: #$1B + '[j'),{ SF10 }
      (ScanCode: #$3B; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[k'), { CF1  }
      (ScanCode: #$3C; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[l'),
      (ScanCode: #$3D; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[m'),
      (ScanCode: #$3E; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[n'),
      (ScanCode: #$3F; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[o'),
      (ScanCode: #$40; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[p'),
      (ScanCode: #$41; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[q'),
      (ScanCode: #$42; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[r'),
      (ScanCode: #$43; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[s'),
      (ScanCode: #$44; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[t'),
      (ScanCode: #$85; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[u'),
      (ScanCode: #$86; Shift: [ssCtrl]; Ext: FALSE; Value: #$1B + '[v'),   { CF12 }
      (ScanCode: #$1C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         )
      );

{ Alternate function keys (ordinary VT keys) }
  FKeys2 : TFuncKeysTable = (
      (ScanCode: #$48; Shift: []; Ext: TRUE ; Value: #$1B + '[A'),   { UP      }
      (ScanCode: #$50; Shift: []; Ext: TRUE ; Value: #$1B + '[B'),   { DOWN    }
      (ScanCode: #$4D; Shift: []; Ext: TRUE ; Value: #$1B + '[C'),   { RIGHT   }
      (ScanCode: #$4B; Shift: []; Ext: TRUE ; Value: #$1B + '[D'),   { LEFT    }
      (ScanCode: #$49; Shift: []; Ext: TRUE ; Value: #$1B + '[5~'),  { PREV    }
      (ScanCode: #$51; Shift: []; Ext: TRUE ; Value: #$1B + '[6~'),  { NEXT    }
      (ScanCode: #$52; Shift: []; Ext: TRUE ; Value: #$1B + '[2~'),  { INSERT  }
      (ScanCode: #$53; Shift: []; Ext: TRUE ; Value: #$7F       ),   { DELETE  }
      (ScanCode: #$3B; Shift: []; Ext: FALSE; Value: #$1B + 'OP'),   { F1->PF1 }
      (ScanCode: #$3C; Shift: []; Ext: FALSE; Value: #$1B + 'OQ'),   { F2->PF2 }
      (ScanCode: #$3D; Shift: []; Ext: FALSE; Value: #$1B + 'OR'),   { F3->PF3 }
      (ScanCode: #$3E; Shift: []; Ext: FALSE; Value: #$1B + 'OS'),   { F4->PF4 }
      (ScanCode: #$57; Shift: []; Ext: FALSE; Value: #$1B + '[28~'), { F11->Aide }
      (ScanCode: #$58; Shift: []; Ext: FALSE; Value: #$1B + '[29~'), { F12->Exécuter }
      (ScanCode: #$1C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         )
      );

{ A-Series Telnet function keys (ordinary VT100 keys + specials) }
  FKeys3 : TFuncKeysTable = (
      (ScanCode: #$48; Shift: []; Ext: TRUE ; Value: #$1B + '[A'),   { UP      }
      (ScanCode: #$50; Shift: []; Ext: TRUE ; Value: #$1B + '[B'),   { DOWN    }
      (ScanCode: #$4D; Shift: []; Ext: TRUE ; Value: #$1B + '[C'),   { RIGHT   }
      (ScanCode: #$4B; Shift: []; Ext: TRUE ; Value: #$1B + '[D'),   { LEFT    }
      (ScanCode: #$49; Shift: []; Ext: TRUE ; Value: #$1B + '-'),    { PREV    }
      (ScanCode: #$51; Shift: []; Ext: TRUE ; Value: #$1B + '+'),    { NEXT    }
      (ScanCode: #$47; Shift: []; Ext: TRUE ; Value: #$1B + 'H'),    { HOME    }
      (ScanCode: #$47; Shift: [ssCtrl]; Ext: TRUE ; Value: #$1B + 'C'),{ HOME  }
      (ScanCode: #$4F; Shift: []; Ext: TRUE ; Value: #$1B + 'R'),    { END     }
      (ScanCode: #$52; Shift: []; Ext: TRUE ; Value: #$1B + 'I'),    { INSERT  }
      (ScanCode: #$53; Shift: []; Ext: TRUE ; Value: #$7F       ),   { DELETE  }
      (ScanCode: #$3B; Shift: []; Ext: FALSE; Value: #$1B + 'OP'),   { F1->PF1 }
      (ScanCode: #$3C; Shift: []; Ext: FALSE; Value: #$1B + 'OQ'),   { F2->PF2 }
      (ScanCode: #$3D; Shift: []; Ext: FALSE; Value: #$1B + 'OR'),   { F3->PF3 }
      (ScanCode: #$3E; Shift: []; Ext: FALSE; Value: #$1B + 'OS'),   { F4->PF4 }
      (ScanCode: #$43; Shift: []; Ext: FALSE; Value: #$1B + 'OP'),   { F9      }
      (ScanCode: #$44; Shift: []; Ext: FALSE; Value: ''),            { F10     }
      (ScanCode: #$57; Shift: []; Ext: FALSE; Value: #$1B + 'OQ'),   { F11     }
      (ScanCode: #$58; Shift: []; Ext: FALSE; Value: #$1B + 'OS'),   { F12     }
      (ScanCode: #$0F; Shift: []; Ext: FALSE; Value: #$1B + 'Z'),    { RTAB    }
      (ScanCode: #$40; Shift: []; Ext: FALSE; Value: #$1B + 'K'),    { F6      }
      (ScanCode: #$53; Shift: [ssCtrl]; Ext: TRUE ; Value: #$1B + 'D'), { CDEL }
      (ScanCode: #$52; Shift: [ssCtrl]; Ext: TRUE ; Value: #$1B + 'L'), { CINS }
      (ScanCode: #$1C; Shift: []; Ext: FALSE; Value: #13#10           ),   { Enter key }
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         ),
      (ScanCode: #$00; Shift: []; Ext: FALSE; Value: ''         )
      );
{$ENDIF}
  { Ethernet to screen }
  ibm_iso8859_1_G0 : TXlatTable = (
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,   { 00 - 07 }
      #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
      #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
      #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
      #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
      #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
      #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
      #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
      #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
      #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
      #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
      #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
      #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,   { 68 - 6F }
      #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,   { 70 - 77 }
      #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 80 - 87 }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 88 - 8F }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 90 - 97 }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 98 - 9F }
      #$B1, #$AD, #$9B, #$9C, #$0F, #$9D, #$B3, #$15,   { A0 - A7 }
      #$20, #$43, #$A6, #$AE, #$AA, #$C4, #$52, #$C4,   { A8 - AF }
      #$F8, #$F1, #$FD, #$20, #$27, #$E6, #$14, #$FA,   { B0 - B7 }
      #$2C, #$20, #$A7, #$AF, #$AC, #$AB, #$20, #$A8,   { B8 - BF }
      #$41, #$41, #$41, #$41, #$8E, #$8F, #$92, #$80,   { C0 - C7 }
      #$45, #$45, #$45, #$45, #$45, #$49, #$49, #$49,   { C8 - CF }
      #$44, #$A5, #$4F, #$4F, #$4F, #$4F, #$4F, #$78,   { D0 - D7 }
      #$ED, #$55, #$55, #$55, #$55, #$59, #$70, #$E1,   { D8 - DF }
      #$85, #$A0, #$83, #$61, #$84, #$86, #$91, #$87,   { E0 - E7 }
      #$8A, #$82, #$88, #$89, #$8D, #$A1, #$8C, #$49,   { E8 - EF }
      #$64, #$A4, #$95, #$A2, #$93, #$6F, #$94, #$F6,   { F0 - F7 }
      #$ED, #$97, #$A3, #$96, #$9A, #$79, #$70, #$98);  { F8 - FF }

{ Ethernet to screen }
  ibm_iso8859_1_G1 : TXlatTable = (
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,   { 00 - 07 }
      #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
      #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
      #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
      #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
      #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
      #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
      #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
      #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
      #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
      #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
      #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
      #$68, #$69, #$D9, #$BF, #$DA, #$C0, #$C5, #$6F,   { 68 - 6F }
      #$70, #$C4, #$72, #$73, #$C3, #$B4, #$C1, #$C2,   { 70 - 77 }
      #$B3, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 80 - 87 }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 88 - 8F }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 90 - 97 }
      #$20, #$20, #$20, #$20, #$20, #$20, #$20, #$20,   { 98 - 9F }
      #$B1, #$AD, #$9B, #$9C, #$0F, #$9D, #$B3, #$15,   { A0 - A7 }
      #$20, #$43, #$A6, #$AE, #$AA, #$C4, #$52, #$C4,   { A8 - AF }
      #$F8, #$F1, #$FD, #$20, #$27, #$E6, #$14, #$FA,   { B0 - B7 }
      #$2C, #$20, #$A7, #$AF, #$AC, #$AB, #$20, #$A8,   { B8 - BF }
      #$41, #$41, #$41, #$41, #$8E, #$8F, #$92, #$80,   { C0 - C7 }
      #$45, #$45, #$45, #$45, #$45, #$49, #$49, #$49,   { C8 - CF }
      #$44, #$A5, #$4F, #$4F, #$4F, #$4F, #$4F, #$78,   { D0 - D7 }
      #$ED, #$55, #$55, #$55, #$55, #$59, #$70, #$E1,   { D8 - DF }
      #$85, #$A0, #$83, #$61, #$84, #$86, #$91, #$87,   { E0 - E7 }
      #$8A, #$82, #$88, #$89, #$8D, #$A1, #$8C, #$49,   { E8 - EF }
      #$64, #$A4, #$95, #$A2, #$93, #$6F, #$94, #$F6,   { F0 - F7 }
      #$ED, #$97, #$A3, #$96, #$9A, #$79, #$70, #$98);  { F8 - FF }

{ Keyboard to Ethernet }
  Output : TXlatTable = (
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,   { 00 - 07 }
      #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,   { 08 - 0F }
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,   { 10 - 17 }
      #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,   { 18 - 1F }
      #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27,   { 20 - 27 }
      #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,   { 28 - 2F }
      #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37,   { 30 - 37 }
      #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,   { 38 - 3F }
      #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47,   { 40 - 47 }
      #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,   { 48 - 4F }
      #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57,   { 50 - 57 }
      #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,   { 58 - 5F }
      #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67,   { 60 - 67 }
      #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,   { 68 - 6F }
      #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77,   { 70 - 77 }
      #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,   { 78 - 7F }
      #$C7, #$FC, #$E9, #$E2, #$E4, #$E0, #$E5, #$E7,   { 80 - 87 }
      #$EA, #$EB, #$E8, #$EF, #$EE, #$EC, #$C4, #$C5,   { 88 - 8F }
      #$C9, #$E6, #$C6, #$F4, #$F6, #$F2, #$FB, #$F9,   { 90 - 97 }
      #$FF, #$F6, #$FC, #$A2, #$A3, #$A5, #$DE, #$20,   { 98 - 9F }
      #$E1, #$ED, #$F3, #$FA, #$F1, #$D1, #$AA, #$BA,   { A0 - A7 }
      #$BF, #$20, #$AC, #$BD, #$BC, #$A1, #$AB, #$BB,   { A8 - AF }
      #$A0, #$A0, #$A0, #$A6, #$A6, #$A6, #$A6, #$AD,   { B0 - B7 }
      #$2B, #$A6, #$A6, #$2B, #$2B, #$2B, #$2B, #$2B,   { B8 - BF }
      #$2B, #$AD, #$AD, #$AD, #$A6, #$AD, #$2B, #$A6,   { C0 - C7 }
      #$2B, #$2B, #$AD, #$AD, #$A6, #$AD, #$2B, #$AD,   { C8 - CF }
      #$AD, #$AD, #$AD, #$2B, #$2B, #$2B, #$2B, #$2B,   { D0 - D7 }
      #$2B, #$2B, #$2B, #$A0, #$A0, #$A0, #$A0, #$A0,   { D8 - DF }
      #$20, #$20, #$20, #$AD, #$20, #$20, #$B5, #$20,   { E0 - E7 }
      #$20, #$20, #$20, #$20, #$20, #$F8, #$20, #$20,   { E8 - EF }
      #$A0, #$B1, #$20, #$20, #$20, #$20, #$F7, #$20,   { F0 - F7 }
      #$B0, #$B0, #$B0, #$20, #$20, #$B2, #$A0, #$20);  { F8 - FF }

procedure FKeysToFile(var FKeys : TFuncKeysTable; FName : String);
procedure FileToFKeys(var FKeys : TFuncKeysTable; FName : String);
function  AddFKey(var FKeys : TFuncKeysTable;
                  ScanCode  : Char;
                  Shift     : TShiftState;
                  Ext       : Boolean;
                  Value     : TFuncKeyValue) : Boolean;



implementation
{-$DEFINE DEBUG_OUTPUT}   { Add or remove minus sign before dollar sign to }
                          { generate code for debug message output         }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF UNICODE}
procedure FillWChar(
    const Txt : PWideChar;
    ByteLen   : Integer;       // Number of BYTES !!
    Value     : WideChar);
var
    I : Integer;
begin
    I := ByteLen div 2;
    while I >= 0 do begin
        Txt[I] := Value;
        Dec(I);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ShiftStateToString(var State : TShiftState) : String;
begin
    Result := '';
    if ssShift in State then
        Result := Result + 'ssShift ';
    if ssAlt in State then
        Result := Result + 'ssAlt ';
    if ssCtrl in State then
        Result := Result + 'ssCtrl ';
    if ssLeft in State then
        Result := Result + 'ssLeft ';
    if ssRight in State then
        Result := Result + 'ssRight ';
    if ssMiddle in State then
        Result := Result + 'ssMiddle ';
    if ssDouble in State then
        Result := Result + 'ssDouble ';
    if Result = '' then
        Result := 'ssNormal';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToShiftState(var S : String) : TShiftState;
begin
    Result := [];
    if Pos('ssShift', S) <> 0 then
        Result := Result + [ssShift];
    if Pos('ssAlt', S) <> 0 then
        Result := Result + [ssAlt];
    if Pos('ssCtrl', S) <> 0 then
        Result := Result + [ssCtrl];
    if Pos('ssLeft', S) <> 0 then
        Result := Result + [ssLeft];
    if Pos('ssRight', S) <> 0 then
        Result := Result + [ssRight];
    if Pos('ssMiddle', S) <> 0 then
        Result := Result + [ssMiddle];
    if Pos('ssDouble', S) <> 0 then
        Result := Result + [ssDouble];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FuncKeyValueToString(var S : TFuncKeyValue) : String;
var
    I : Integer;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        if (Ord(S[I]) < 32) or (Ord(S[I]) >= 127) or
           (S[I] = '''') or (S[I] = '\') then
            Result := Result + '\x' + IntToHex(Ord(S[I]), 2)
        else
            Result := Result + S[I];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToFuncKeyValue(var S : String) : TFuncKeyValue;
var
    I : Integer;
begin
    Result := '';
    I := 1;
    while I <= Length(S) do begin
        if (S[I] = '\') and
           ((I + 3) <= Length(S)) and
           (S[I + 1] = 'x') then begin
            Result := Result + chr(XDigit2(PChar(@S[I + 2])));
            I := I + 3;
        end
        else
            Result := Result + S[I];
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AddFKey(var FKeys : TFuncKeysTable;
                  ScanCode  : Char;
                  Shift     : TShiftState;
                  Ext       : Boolean;
                  Value     : TFuncKeyValue) : Boolean;
var
    I : Integer;
begin
    { Search for existing key definition to replace it }
    for I := Low(FKeys) to High(FKeys) do begin
        if (FKeys[I].ScanCode = ScanCode) and
           (FKeys[I].Shift    = Shift) and
           (FKeys[I].Ext      = Ext) then begin
            FKeys[I].Value := Value;
            Result         := TRUE;     { Success}
            Exit;
        end;
    end;

    { Key not existing, add in an empty space }
    for I := Low(FKeys) to High(FKeys) do begin
        if FKeys[I].ScanCode = #0 then begin
            FKeys[I].ScanCode := ScanCode;
            FKeys[I].Shift    := Shift;
            FKeys[I].Ext      := Ext;
            FKeys[I].Value    := Value;
            Result            := TRUE;     { Success}
            Exit;
        end;
    end;

    { Failure, no more space available }
    Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FKeysToFile(var FKeys : TFuncKeysTable; FName : String);
var
    I : Integer;
    F : TextFile;
begin
    AssignFile(F, FName);
    Rewrite(F);
    for I := Low(FKeys) to High(FKeys) do begin
        with FKeys[I] do begin
            if ScanCode <> chr(0) then
                WriteLn(F, IntToHex(Ord(ScanCode), 2), ', ',
                           ShiftStateToString(Shift), ', ',
                           Ext, ', ''',
                           FuncKeyValueToString(Value), '''');
        end;
    end;
    CloseFile(F);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(var S : String; var I : Integer; Delim : Char) : String;
begin
    Result := '';
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    while (I <= Length(S)) and (S[I] <> Delim) do begin
        Result := Result + S[I];
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure FileToFKeys(var FKeys : TFuncKeysTable; FName : String);
var
    I, J : Integer;
    F : TextFile;
    S, T : String;
    sc   : Integer;
begin
    AssignFile(F, FName);
 {$I-}
    Reset(F);
    if IOResult <> 0 then begin
        { File do not exist, create default one }
        FKeysToFile(FKeys, FName);
        Exit;
    end;

    for I := Low(FKeys) to High(FKeys) do begin
        with FKeys[I] do begin
            ScanCode := chr(0);
            Shift    := [];
            Ext      := FALSE;
            Value    := '';
            if not Eof(F) then begin
                { 71, ssNormal, TRUE, '\x1B[H' }
                ReadLn(F, S);
                J  := 1;
                T  := GetToken(S, J, ',');
                if (Length(T) > 0) and (T[1] <> ';') then begin
                    sc := XDigit2(PChar(@T[1]));
                    if sc <> 0 then begin
                        ScanCode := chr(sc);
                        Inc(J);
                        T := GetToken(S, J, ',');
                        Shift := StringToShiftState(T);
                        Inc(J);
                        T := GetToken(S, J, ',');
                        Ext := UpperCase(T) = 'TRUE';
                        Inc(J);
                        T := GetToken(S, J, '''');
                        Inc(J);
                        T := GetToken(S, J, '''');
                        Value := StringToFuncKeyValue(T);
                    end;
                end;
            end;
        end;
    end;
    CloseFile(F);
{$I+}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugString(Msg : String);
const
    Cnt : Integer = 0;
{$IFDEF DEBUG_OUTPUT}
var
    Buf : String[20];
{$ENDIF}
begin
{$IFDEF DEBUG_OUTPUT}
    Cnt := Cnt + 1;
    Buf := IntToHex(Cnt, 4) + ' ' + #0;
    OutputDebugString(@Buf[1]);

{$IFDEF CLR}
    if Length(Msg) < High(Msg) then
        Msg[Length(Msg) + 1] := #0;
{$ENDIF}

    OutputDebugString(@Msg[1]);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
procedure SetLength(var S: String; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TLine.Create;
begin
    inherited Create;
{$IF SizeOf(Txt[0]) <> 1}
//    FillWChar(Txt, SizeOf(Txt), ' ');                       {drjohn}
    FillWChar(Txt, length(Txt), ' ');
{$ELSE}
    FillChar(Txt, SizeOf(Txt), ' ');
{$IFEND}
    FillChar(Att, SizeOf(Att), Chr(F_WHITE));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TLine.Clear(Attr : Byte);
begin
{$IF SizeOf(Txt[0]) <> 1}
//    FillWChar(Txt, SizeOf(Txt), ' ');                       {drjohn}
    FillWChar(Txt, length(Txt), ' ');
{$ELSE}
    FillChar(Txt, SizeOF(Txt), ' ');
{$IFEND}
    FillChar(Att, SizeOf(Att), Attr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TScreen.Create;
begin
    inherited Create;
    FRowCount        := 0;
    FBackRowCount    := 0;
    FBackEndRow      := 0;
    FBackColor       := vtsWhite;
    FOptions         := [vtoBackColor];
    SetRowCount(25);
    FColCount        := 80;
    FRowSaved        := -1;
    FColSaved        := -1;
    FScrollRowTop    := 0;
    {FScrollRowBottom := FRowCount - 1; // WM + SE 09/08/00 }
    FAttribute       := F_WHITE;
    FDefAttribute      := F_WHITE;              {drjohn}
    InvClear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TScreen.Destroy;
var
    nRow : Integer;
begin
    for nRow := 0 to FRowCount + FBackRowCount - 1 do
        FLines^[nRow].Free;
    FreeMem (FLines, (FRowCount + FBackRowCount) * SizeOf(TObject));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.AdjustFLines(NewCount : Integer);
var
    NewLines        : PLineArray;
    CurrCount       : Integer;
    nRow            : Integer;
begin
    CurrCount := FRowCount + FBackRowCount;
    if  (NewCount <> CurrCount) and (NewCount > 0) then begin
        GetMem(NewLines, NewCount * SizeOf(TObject));
        if NewCount > CurrCount then begin
            if CurrCount <> 0 then
                Move(FLines^, NewLines^, CurrCount * SizeOf(TObject));
            for nRow := CurrCount to NewCount - 1 do
                NewLines^[nRow] := TLine.Create;
            if CurrCount <> 0 then
                FreeMem(FLines, CurrCount * SizeOf(TObject));
        end
        else begin
            Move (FLines^, NewLines^, NewCount * SizeOf(TObject));
            for nRow := NewCount to CurrCount - 1 do
                FLines^[nRow].Free;
            FreeMem(FLines, CurrCount * SizeOf(TObject));
        end;
        FLines := NewLines;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetRowCount(NewCount : Integer);
begin
    if NewCount <> FRowCount then begin
        AdjustFLines(NewCount + FBackRowCount);
        FRowCount := NewCount;
        FScrollRowBottom := FRowCount - 1; { WM + SE 09/08/00 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetBackRowCount(NewCount : Integer);
begin
    if NewCount <> FBackRowCount then begin
        AdjustFLines(FRowCount + NewCount);
        FBackRowCount := NewCount;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CopyScreenToBack;
{ Copies the current host screen into the scrollback buffer. }
var
    Temp : TLine;
    Row  : Integer;
    Pass : Integer;
    nCol : Integer;
begin
    if FBackRowCount >= FRowCount then begin
        Dec (FBackEndRow, FRowCount);
        if (0 - FBackEndRow) >= FBackRowCount then
            FBackEndRow := 1 - FBackRowCount;
        { We have to make  FRowCount  lines available at the head of the
          scrollback buffer.  These will come from the end of the scrollback
          buffer.  We'll make  FRowCount  passes through the scrollback buffer
          moving the available lines up to the top and the existing lines
          down a page at a time.
          Net result is that we only move each line once. }
        For Pass := 0 To FRowCount - 1 Do begin
            Row := FBackEndRow + Pass;
            Temp := Lines[Row];
            Inc (Row, FRowCount);
            While Row < 0 Do begin
                Lines[Row - FRowCount] := Lines[Row];
                Inc (Row, FRowCount);
            end;
            Lines[Row - FRowCount] := Temp;
        end;

        { Now, copy the host screen lines to the ones we made available. }
        For Row := 0 To FRowCount - 1 Do begin
            Move (Lines[Row].Txt, Lines[Row - FRowCount].Txt, FColCount);
            Move (Lines[Row].Att, Lines[Row - FRowCount].Att, FColCount);
            if vtoBackColor in FOptions then begin
                with Lines[Row - FRowCount] do begin
                    for nCol := 0 to FColCount - 1 do begin
                        Att[nCol] := Att[nCol] And $8F Or (Ord (FBackColor) shl 4);
                    end;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ScrollUp;
var
    Temp : TLine;
    Row  : Integer;
    nCol : Integer;
begin
    if FBackRowCount > 0 then begin
        if (0 - FBackEndRow) < (FBackRowCount - 1) then
            Dec (FBackEndRow);
        Temp := Lines[FBackEndRow];
        For Row := FBackEndRow + 1 To -1 Do begin
            Lines[Row - 1] := Lines[Row];
        end;
        Lines[-1] := Lines[FScrollRowTop];
            if vtoBackColor in FOptions then begin
                with Lines[-1] do begin
                    for nCol := 0 to FColCount - 1 do begin
                        Att[nCol] := Att[nCol] And $8F Or (Ord (FBackColor) shl 4);
                    end;
                end;
            end;

    end
    else
        Temp := Lines[FScrollRowTop];

    for Row := FScrollRowTop + 1 to FScrollRowBottom do
        Lines[Row - 1] := Lines[Row];
    Lines[FScrollRowBottom] := Temp;
    FAttribute := FDefAttribute;                {drjohn}
    Temp.Clear( FAttribute );                 {drjohn}
    FAllInvalid := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ScrollDown;
var
    Temp : TLine;
    Row  : Integer;
begin
    Temp := Lines[FScrollRowBottom];
    for Row := FScrollRowBottom DownTo FScrollRowTop + 1 do
        Lines[Row] := Lines[Row - 1];
    Lines[FScrollRowTop] := Temp;
    FAttribute := FDefAttribute;                {drjohn}
    Temp.Clear( FAttribute );                 {drjohn}
    FAllInvalid := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorDown;
begin
    Inc(FRow);
    if FRow > FScrollRowBottom then begin
        FRow := FScrollRowBottom;
        ScrollUp;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorUp;
begin
    Dec(FRow);
    if FRow < 0 then begin
        Inc(FRow);
        ScrollDown;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorRight;
begin
    Inc(FCol);
    if FCol >= FColCount then begin
        FCol := 0;
        CursorDown;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CursorLeft;
begin
    Dec(FCol);
    if FCol < 0 then begin
        FCol := FColCount - 1;
        CursorUp;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.CarriageReturn;
begin
    FCol := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScreen.GetEscapeParam(From : Integer; var Value : Integer) : Integer;
begin
    while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
        From := From + 1;

    Value := 0;
    while (From <= Length(FEscBuffer)) and
          ((FEscBuffer[From] >= '0') and (FEscBuffer[From] <= '9')) do begin
        Value := Value * 10 + Ord(FEscBuffer[From]) - Ord('0');
        From := From + 1;
    end;

    Result := From;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.UnimplementedEscape(EscCmd : Char);
{var
    Buf : String;}
begin
    DebugString('Unimplemented Escape Sequence: ' + FEscBuffer + EscCmd + #13 + #10);
{   Buf := FEscBuffer + EscCmd + #0;
    MessageBox(0, @Buf[1], 'Unimplemented Escape Sequence', MB_OK); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvalidEscape(EscCmd : Char);
{var
    Buf : String;}
begin
    DebugString('Invalid Escape Sequence: ' + FEscBuffer + EscCmd + #13 + #10);
{   Buf := FEscBuffer + EscCmd + #0;
    MessageBox(0, @Buf[1], 'Invalid Escape Sequence', MB_OK); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessESC_D;                   { Index                   }
begin
    UnimplementedEscape('D');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Move cursor Up, scroll down if necessary                                  }
procedure TScreen.ProcessESC_M;                   { Reverse index           }
begin
    Dec(FRow);
    if FRow < FScrollRowTop then begin
        FRow := FScrollRowTop;
        ScrollDown;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessESC_E;                   { Next line               }
begin
    UnimplementedEscape('E');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_u;                  { Restore Cursor          }
begin
    UnimplementedEscape('u');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ IBM character set operation (not part of the ANSI standard)               }
{ <ESC>[0I              => Set IBM character set                            }
{ <ESC>[1;nnnI          => Literal mode for nnn next characters             }
{ <ESC>[2;onoffI        => Switch carbon mode on (1) or off (0)             }
{ <ESC>[3;ch;cl;sh;slI  => Receive carbon mode keyboard code                }
{ <ESC>[4I              => Select ANSI character set                        }
procedure TScreen.ProcessCSI_I;
var
    From, mode, nnn : Integer;
    ch, cl, sh, sl  : Integer;
begin
    From := GetEscapeParam(2, Mode);

    case Mode of
    0:  begin                { Select IBM character set                     }
            FNoXlat := TRUE;
        end;
    1:  begin                { Set literal mode for next N characters       }
            if FEscBuffer[From] = ';' then
                GetEscapeParam(From + 1, FCntLiteral)
            else
                FCntLiteral := 1;
        end;
    2:  begin                { Switch carbon mode on or off                 }
            if FEscBuffer[From] = ';' then
                GetEscapeParam(From + 1, nnn)
            else
                nnn := 0;
            FCarbonMode := (nnn <> 0);
        end;
    3:  begin                { Receive carbon mode key code                 }
            ch := 0; cl := 0; sh := 0; sl := 0;
            if FEscBuffer[From] = ';' then begin
                From := GetEscapeParam(From + 1, cl);
                if FEscBuffer[From] = ';' then begin
                    From := GetEscapeParam(From + 1, ch);
                    if FEscBuffer[From] = ';' then begin
                        From := GetEscapeParam(From + 1, sl);
                        if FEscBuffer[From] = ';' then begin
                            GetEscapeParam(From + 1, sh);
                        end;
                    end;
                end;
            end;
            DebugString('Special key ' +
                        IntToHex(ch, 2) + IntToHex(cl, 2) + ' ' +
                        IntToHex(sh, 2) + IntToHex(sl, 2));
        end;
    4:  begin                { Select ANSI character set                    }
            FNoXlat := FALSE;
        end;
    else
        UnimplementedEscape('I');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.BackSpace;
begin
    if FCol > 0 then
        Dec(FCol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ClearScreen;
var
    Row : Integer;
begin
   SetFontColors;               {drjohn}
    for Row := 0 to FRowCount - 1 do
        Lines[Row].Clear(FAttribute);
    FRow := 0;
    FCol := 0;
    FAllInvalid := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvClear;
begin
    with FInvRect do begin
        Top    := 9999;
        Left   := 9999;
        Right  := -1;
        Bottom := -1;
    end;
    FAllInvalid := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.InvRect(nRow, nCol : Integer);
begin
    if not FAllInvalid then begin
        if FInvRect.Top > nRow then
            FInvRect.Top := nRow;
        if FInvRect.Bottom < nRow then
            FInvRect.Bottom := nRow;
        if FInvRect.Left > nCol then
            FInvRect.Left := nCol;
        if FInvRect.Right < nCol then
            FInvRect.Right := nCol;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The FLines array is inverted with the last host line at position 0 and
  the first host line as position FRowCount - 1. }
procedure Tscreen.SetLines(I : Integer; Value : TLine);
begin
    if I >= FRowCount then
        FLines^[0] := Value
    else
        FLines^[FRowCount - 1 - I] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TScreen.GetLines(I : Integer) : TLine;
begin
    if I >= FRowCount then
        Result := FLines^[0]
    else
        Result := FLines^[FRowCount - 1 - I];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.Eol;
begin
    with Lines[FRow] do begin
{$IF SizeOf(Txt[0]) <> 1}  // V 7.02
//        FillWChar(@Txt[FCol], FColCount - FCol, ' ');
        FillWChar(@Txt[FCol], (FColCount - FCol)*SizeOf(Txt[0]) , ' ');            {drjohn}
{$ELSE}
        FillChar(Txt[FCol], FColCount - FCol, ' ');
{$IFEND}
        FillChar(Att[FCol], (FColCount - FCol) * SizeOf(Att[FCol]), FAttribute);
    end;
    InvRect(Frow, FCol);
    InvRect(Frow, FColCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.Eop;
var
    NRow : Integer;
begin
    Eol;
    for NRow := FRow + 1 to FRowCount - 1 do
        Lines[NRow].Clear(FAttribute);
    if FRow = 0 then
        FAllInvalid := TRUE
    else begin
       InvRect(FRow, 0);
       InvRect(FRowCount, FColCount);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_J;                  { Clear the screen         }
var
    Mode : Integer;
    Row  : Integer;
begin
    GetEscapeParam(2, Mode);
    case Mode of
    0: begin                                   { Cursor to end of screen    }
           FAttribute := FDefAttribute;                {drjohn}
           Eop;
       end;
    1: begin                                   { Start of screen to cursor  }
           for Row := 0 to FRow do
               Lines[Row].Clear(FAttribute);
           InvRect(0, 0);
           InvRect(FRow, FColCount);
       end;
    2: begin                                   { Entire screen              }
           if vtoCopyBackOnClear in FOptions then CopyScreenToBack;
           ClearScreen;
       end;
    else
        InvalidEscape('J');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_K;                  { Erase to End of Line    }
begin
    Eol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_L;                   { Insert Line             }
var
    nLine : Integer;
    nRow  : Integer;
    Temp  : TLine;
begin
    FCol := 0;
    GetEscapeParam(2, nLine);
    if nLine = 0 then
        nLine := 1;

    if (FRow + nLine) > FScrollRowBottom then begin
        for nRow := FRow to FScrollRowBottom do
            Lines[nRow].Clear(FAttribute);
        Exit;
    end;

    for nRow := FScrollRowBottom downto FRow + nLine do begin
        Temp                := Lines[nRow];
        Lines[nRow]         := Lines[nRow - nLine];
        Lines[nRow - nLine] := Temp;
    end;

    for nRow := FRow to FRow + nLine - 1 do
        Lines[nRow].Clear(FAttribute);

    FAllInvalid := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_M;                   { Delete Line             }
var
    nLine : Integer;
    nRow  : Integer;
    Temp  : TLine;
begin
    FAllInvalid := TRUE;
    FCol := 0;
    GetEscapeParam(2, nLine);
    if nLine = 0 then
        nLine := 1;

    if (FRow + nLine) > FScrollRowBottom then begin
        for nRow := FRow to FScrollRowBottom do
            Lines[nRow].Clear(FAttribute);
        Exit;
    end;
    FAttribute := FDefAttribute;                {drjohn}

    for nRow := FRow to FRow + nLine - 1 do
        Lines[nRow].Clear( FAttribute );                   {drjohn}
    for nRow := FRow to FScrollRowBottom - nLine do begin
        Temp                := Lines[nRow];
        Lines[nRow]         := Lines[nRow + nLine];
        Lines[nRow + nLine] := Temp;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_m_lc;                { Select Attributes       }
var
    From, n : Integer;
begin
    if FEscBuffer[1] <> '[' then
        Exit;

    if Length(FEscBuffer) < 2 then begin
        FAttribute := FDefAttribute;                {drjohn}
        FReverseVideo := FALSE;
        Exit;
    end;

    From := 2;
    while From <= Length(FEscBuffer) do begin
        if (FEscBuffer[From] = ' ') or
           (FEscBuffer[From] = '[') or
           (FEscBuffer[From] = ';') then
            Inc(From)
        else begin
            From := GetEscapeParam(From, n);
            case n of
            0:  begin                   { All attributes off       }
                    FAttribute    := FDefAttribute;             {drjohn}
                    FReverseVideo := FALSE;
                    FUnderLine    := FALSE;
                end;
            1:  begin                   { High intensity           }
                    FAttribute := FAttribute or F_INTENSE;
                end;
            4:  begin                   { Underline                }
                    FUnderLine := TRUE;
                end;
            5:  begin                   { Blinking                 }
                    FAttribute := FAttribute or B_BLINK;
                end;
            7:  begin                   { Reverse video            }
                    FReverseVideo := TRUE;
                end;
            8:  begin                   { Secret                   }
                    FAttribute := 0;
                end;
            10: begin                   { Don't force high bit     }
                    FForceHighBit := FALSE;
                end;
            12: begin                   { Force high bit on        }
                    FForceHighBit := TRUE;
                end;
            22: begin                   { Normal intensity         }
                    FAttribute := FAttribute and (not F_INTENSE);
                end;
            27: begin                   { Normal characters        }
                    FAttribute := FDefAttribute;                {drjohn}
                    FReverseVideo := FALSE;
                end;
            30, 31, 32, 33, 34, 35, 36, 37:
               { vtsBlack, vtsRed, vtsGreen, vtsYellow, vtsBlue,  vtsMagenta, vtsCyan,  vtsWhite}  {drjohn}
                begin                   { Foreground color         }
                    FAttribute := (n mod 10) or (FAttribute and $F8);
                end;
            40, 41, 42, 43, 44, 45, 46, 47:
                begin                   { Background color         }
                    FAttribute := ((n mod 10) shl 4) or (FAttribute and $8F);
                end;
            else
                InvalidEscape('m');
            end;
        end;
    end;
    if FReverseVideo then begin
        FAttribute := ((FAttribute and 7) shl 4) or
                      ((FAttribute shr 4) and 7) or
                      (FAttribute and $88);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_n_lc;                { Cursor position report  }
begin
    UnimplementedEscape('n');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_at;                 { Insert character        }
var
    nChar : Integer;
    nCnt  : Integer;
    nCol  : Integer;
    Line  : TLine;
begin
    GetEscapeParam(2, nChar);
    if nChar = 0 then
        nChar := 1;

    nCnt := FColCount - FCol - nChar;
    if nCnt <= 0 then begin
        Eol;
        Exit;
    end;

    Line := Lines[FRow];
    for nCol := FColCount - 1 downto FCol + nChar do begin
        Line.Txt[nCol] := Line.Txt[nCol - nChar];
        Line.Att[nCol] := Line.Att[nCol - nChar];
        InvRect(Frow, nCol);
    end;

    for nCol := FCol to FCol + nChar - 1 do begin
        Line.Txt[nCol] := ' ';
        Line.Att[nCol] := FAttribute;
        InvRect(Frow, nCol);
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_r_lc;                { Scrolling margins       }
var
    From, Top, Bottom : Integer;
begin
    From := GetEscapeParam(2, Top);
    if Top = 0 then begin                         { Default = full screen   }
        FScrollRowTop    := 0;
        FScrollRowBottom := FRowCount - 1;
    end
    else begin
        while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
            From := From + 1;
        if FEscBuffer[From] = ';' then
            GetEscapeParam(From + 1, Bottom)
        else
            Bottom := 1;

        FScrollRowTop    := Top    - 1;
        FScrollRowBottom := Bottom - 1;

        if (FScrollRowBottom <= FScrollRowTop) or
           (FScrollRowTop < 0) or
           (FScrollRowBottom >= FRowCount) then begin
            FScrollRowTop    := 0;
            FScrollRowBottom := FRowCount - 1;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_s_lc;                { Save cursor location    }
begin
    ProcessCSI_7;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_u_lc;                { Restore cursor location }
begin
    ProcessCSI_8;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_7;                   { Save cursor location    }
begin
    FRowSaved := FRow;
    FColSaved := FCol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_8;                   { Restore cursor location }
begin
    if FRowSaved = -1 then
        GotoXY(0, 0)
    else
        GotoXY(FColSaved, FRowSaved);
    FRowSaved := -1;
    FColSaved := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_H;                   { Set Cursor Position     }
var
    From, Row, Col : Integer;
begin
    From := GetEscapeParam(2, Row);
    while (From <= Length(FEscBuffer)) and (FEscBuffer[From] = ' ') do
        From := From + 1;
    if FEscBuffer[From] = ';' then
        GetEscapeParam(From + 1, Col)
    else
        Col := 1;

    GotoXY(Col - 1, Row - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_h_lc;                { Terminal mode set       }
var
    Priv : Boolean;
    Mode : Integer;
begin
    if FEscBuffer[1] <> '[' then begin
        UnimplementedEscape('h');
        Exit;
    end;

    Priv := (FEscBuffer[2] = '?');
    if not Priv then begin
        UnimplementedEscape('h');
        Exit;
    end;

    GetEscapeParam(3, Mode);
    case Mode of
    1 :  { ANSI cursor keys }
         FCKeyMode := TRUE;
    3 :  { change column width }                {drjohn}
             FColCount := 132;
    4 :  { Smooth scroll OFF }
         { Ignore };
    7:   { Auto-wrap OFF }
         FAutoWrap := TRUE;
    25:  { Cursor visible }
         begin
             FCursorOff := FALSE;
             if Assigned(FOnCursorVisible) then
                 FOnCursorVisible(Self);
         end;
    else
        UnimplementedEscape('h');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_l_lc;                { Terminal mode reset     }
var
    Priv : Boolean;
    Mode : Integer;
begin
    if FEscBuffer[1] <> '[' then begin
        UnimplementedEscape('l');
        Exit;
    end;

    Priv := (FEscBuffer[2] = '?');
    if not Priv then begin
        UnimplementedEscape('l');
        Exit;
    end;

    GetEscapeParam(3, Mode);
    case Mode of
    1 :  { ANSI cursor keys }
         FCKeyMode := FALSE;
    3 :  { change column width }                {drjohn}
             FColCount := 80;
    4 :  { Smooth scroll OFF }
         { Ignore };
    7:   { Auto-wrap OFF }
         FAutoWrap := FALSE;
    25:  { Cursor invisible }
         begin
             FCursorOff := TRUE;
             if Assigned(FOnCursorVisible) then
                 FOnCursorVisible(Self);
         end;
    else
        UnimplementedEscape('l');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_A;                   { Cursor Up               }
var
    Row : Integer;
begin
    GetEscapeParam(2, Row);
    if Row <= 0 then
        Row := 1;
    FRow := FRow - Row;
    if FRow < 0 then
        FRow := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_B;                   { Cursor Down             }
var
    Row : Integer;
begin
    GetEscapeParam(2, Row);
    if Row <= 0 then
        Row := 1;
    FRow := FRow + Row;
    if FRow >= FRowCount then
        FRow := FRowCount - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_C;                   { Cursor Right            }
var
    Col : Integer;
begin
    GetEscapeParam(2, Col);
    if Col <= 0 then
        Col := 1;
    FCol := FCol + Col;
    if FCol >= FColCount then begin
        if FAutoWrap then begin
            FCol := FCol - FColCount;
            Inc(FRow);
            if FRow >= FRowCount then
                FRow := FRowCount - 1;
        end
        else
            FCol := FColCount - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_D;                   { Cursor Left             }
var
    Col : Integer;
begin
    GetEscapeParam(2, Col);
    if Col <= 0 then
        Col := 1;
    FCol := FCol - Col;
    if FCol < 0 then
        FCol := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_P;                   { Delete Character        }
var
    Count : Integer;
    nCol  : Integer;
begin
    GetEscapeParam(2, Count);
    if Count <= 0 then
        Count := 1;
    with Lines[FRow] do begin
        for nCol := Fcol to FColCount - Count - 1 do begin
            Txt[nCol] := Txt[nCol + Count];
            Att[nCol] := Att[nCol + Count];
        end;
        for nCol := FcolCount - Count - 1 to FColCount - 1 do begin
            Txt[nCol] := ' ';
            Att[nCol] := FAttribute;                {drjohn}
        end;
    end;
    InvRect(Frow, FCol);
    InvRect(Frow, FColCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_S;                   { Scroll up               }
begin
    ScrollUp;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessCSI_T;                  { Scroll down             }
begin
    UnimplementedEscape('T');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G0(EscCmd : Char); { G0 character set     }
begin
    case EscCmd of
    '0': begin
             FCharSetG0       := EscCmd;
             FXlatInputTable  := @ibm_iso8859_1_G1;
             FXlatOutputTable := @ibm_iso8859_1_G1;
             FNoXlat          := FNoXlatInitial;
{             FNoXlat          := FALSE;}
         end;
    'B': begin
             FCharSetG0       := EscCmd;
             FXlatInputTable  := @ibm_iso8859_1_G0;
             FXlatOutputTable := @ibm_iso8859_1_G0;
             FNoXlat          := FNoXlatInitial;
         end;
    else
        InvalidEscape(EscCmd);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G1(EscCmd : Char); { G1 character set     }
begin
    FCharSetG1 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G2(EscCmd : Char); { G2 character set     }
begin
    FCharSetG2 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.process_charset_G3(EscCmd : Char); { G2 character set     }
begin
    FCharSetG3 := EscCmd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.ProcessEscape(EscCmd : Char);
begin
    if Length(FEscBuffer) = 0 then begin
        case EscCmd of
        'D': ProcessESC_D;         { Index                   }
        'M': ProcessESC_M;         { Reverse index           }
        'E': ProcessESC_E;         { Next line               }
        'H': ;                     { Tabulation set          }
        '7': ProcessCSI_7;         { Save cursor             }
        '8': ProcessCSI_8;      { Restore Cursor          }
        '=': ;     { VT52 }        { Enter Alternate keypad  }
        '>': ;     { VT52 }        { Exit Alternate keypad   }
        '<': ;     { VT52 }        { Enter ANSI mode         }
        else
            InvalidEscape(EscCmd);
            WriteLiteralChar(EscCmd);
        end;

        Exit;
    end;

    case FEscBuffer[1] of
    ' ': begin
             case EscCmd of
             'F': ;
             else
                 InvalidEscape(EscCmd);
             end;
         end;
    '[': begin
             case EscCmd of
             'I': ProcessCSI_I;                { Select IBM char set     }
                                                { Extension F. Piette !!  }
             'J': ProcessCSI_J;                { Clear the screen        }
             'K': ProcessCSI_K;                { Erase to End of Line    }
             'L': ProcessCSI_L;                { Insert Line             }
             'M': ProcessCSI_M;                { Delete Line             }
             'm': ProcessCSI_m_lc;             { Select Attributes       }
             'n': ProcessCSI_n_lc;             { Cursor position report  }
             '@': ProcessCSI_at;               { Insert character        }
             'r': ProcessCSI_r_lc;             { Set Top and Bottom marg }
             's': ProcessCSI_s_lc;             { Save cursor location    }
             'u': ProcessCSI_u_lc;             { Restore cursor location }
             'H': ProcessCSI_H;                { Set Cursor Position     }
             'f': ProcessCSI_H;                { Set Cursor Position     }
             'g': ;                            { Tabulation Clear        }
             'h': ProcessCSI_h_lc;             { Terminal mode set       }
             'l': ProcessCSI_l_lc;             { Terminal mode reset     }
             'A': ProcessCSI_A;                { Cursor Up               }
             'B': ProcessCSI_B;                { Cursor Down             }
             'C': ProcessCSI_C;                { Cursor Right            }
             'D': ProcessCSI_D;                { Cursor Left             }
             'P': ProcessCSI_P;                { Delete Character        }
             'S': ProcessCSI_S;                { Scroll up               }
             'T': ProcessCSI_T;                { Scroll down             }
             '>': ;                            {                         }
             else
                 InvalidEscape(EscCmd);
             end;
         end;
    '(': process_charset_G0(EscCmd);           { G0 character set        }
    ')': process_charset_G1(EscCmd);           { G1 character set        }
    '*': process_charset_G2(EscCmd);           { G2 character set        }
    '+': process_charset_G3(EscCmd);           { G3 character set        }
    else
        InvalidEscape(EscCmd);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.WriteLiteralChar(Ch : Char);
var
    Line : TLine;
begin
    if FCol >= FColCount then begin
        if FAutoWrap then begin
            FCol := 0;
            Inc(FRow);
            if FRow >= FRowCount then begin
                Dec(FRow);
                ScrollUp;
            end;
        end;
    end;

    if FForceHighBit then
        Ch := Char(Ord(Ch) or $80 and $FF);

    Line := Lines[FRow];
{$IF SizeOf(Line.Txt[0]) <> 1}
    Line.Txt[FCol] := WideChar(Ch);
{$ELSE}
    Line.Txt[FCol] := AnsiChar(Ch);
{$IFEND}
    Line.Att[FCol] := FAttribute;
    InvRect(Frow, FCol);

    if FCol < High(Line.Txt) then
        Inc(FCol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.SetAttr(Att : Char);
begin
     { Not implemented }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write a single character at current cursor location.                      }
{ Update cursor position.                                                   }
{$IFDEF COMPILER12_UP}
procedure TScreen.WriteChar(Ch : AnsiChar);
begin
    WriteChar(Char(Ch));
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write a single character at current cursor location.                      }
{ Update cursor position.                                                   }
procedure TScreen.WriteChar(Ch : Char);
var
    bProcess : Boolean;
    WCh      : Char;
begin
    if FCntLiteral > 0 then begin
        if (FCntLiteral and 1) <> 0 then
            WriteLiteralChar(Ch)
        else
            SetAttr(Ch);
        Dec(FCntLiteral);
        Exit;
    end;

    if FNoXlat then
        Ch := Char(FXlatInputTable^[Ord(Ansichar(Ch))]);

    WCh := Char(Ch);
    if FEscFLag then begin
        bProcess := FALSE;
        if (Length(FEscBuffer) = 0) and
           IsCharInSysCharSet(WCh, ['D', 'M', 'E', 'H', '7', '8', '=', '>', '<']) then
             bProcess := TRUE
        else if (Length(FEscBuffer) = 1) and
           ((FEscBuffer[1] = '(') or (FEscBuffer[1] = ')') or
            (FEscBuffer[1] = '*') or (FEscBuffer[1] = '+')) then
            bProcess := TRUE
        else if IsCharInSysCharSet(WCh, ['0'..'9', ';', '?', ' ']) or
                ((Length(FEscBuffer) = 0) and
                 IsCharInSysCharSet(WCh, ['[', '(', ')', '*', '+'])) then begin
            FEscBuffer := FEscBuffer + Ch;
{$IFDEF COMPILER12_UP}
            if Length(FEscBuffer) >= 80 then begin
{$ELSE}
            if Length(FEscBuffer) >= High(FEscBuffer) then begin
{$ENDIF}
                if FSound then              {drjohn}
                   MessageBeep(MB_ICONASTERISK);
                FEscBuffer := '';
                FEscFlag   := FALSE;
            end;
        end
        else
            bProcess := TRUE;

        if bProcess then begin
            ProcessEscape(Ch);
            FEscBuffer := '';
            FEscFlag   := FALSE;
        end;

        Exit;
    end;

    case Ch of
    #0:  ;
    #7:  if FSound then             {drjohn}
                  MessageBeep(MB_ICONASTERISK);
    #8:  BackSpace;
    #9:  begin
             repeat
                 Inc(FCol);
             until (FCol Mod 8) = 0;
         end;
    #10: begin
             CursorDown;
             if FAutoCR then
                 CarriageReturn;
         end;
    #13: begin
             CarriageReturn;
             if FAutoLF then
                 CursorDown;
         end;
    #14: begin
             FXlatInputTable  := @ibm_iso8859_1_G1;
             FXlatOutputTable := @ibm_iso8859_1_G1;
         end;
    #15: begin
             FXlatInputTable  := @ibm_iso8859_1_G0;
             FXlatOutputTable := @ibm_iso8859_1_G0;
         end;
    #27: begin
             FEscBuffer := '';
             FEscFlag   := TRUE;
         end;
    else
        WriteLiteralChar(Ch);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Write characters at current cursor location. Update cursor position.      }
procedure TScreen.WriteStr(Str : String);
var
    I : Integer;
begin
    for I := 1 to Length(Str) do
        WriteChar(Str[I]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Read characters from the cursor to end of line                            }
function TScreen.ReadStr : String;
var
    Line : TLine;
    Len  : Integer;
begin
    Line := Lines[FRow];
    Len  := FColCount - FCol;
    if Len <= 0 then
        Result := ''
    else begin
        SetLength(Result, Len);
        Move(Line.Txt[FCol], Result[1], Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TScreen.GotoXY(X, Y : Integer);
begin
    if X < 0 then
        FCol := 0
    else if X >= FColCount then
        FCol := FColCount - 1
    else
        FCol := X;

    if Y < 0 then
        FRow := 0
    else if Y >= FRowCount then
        FRow := FRowCount - 1
    else
        FRow := Y;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCaret;
begin
    if not FScreen.Focused then    { Jan 10, 2005 }
        Exit;

{$IFDEF CHAR_ZOOM}
    SetCaretPos(FCharPos[FScreen.FCol] + FLeftMargin + 2,
                FLinePos[FScreen.FRow - FTopLine] + FTopMargin + 3);
{$ELSE}
    SetCaretPos(FScreen.FCol * FCharWidth + FLeftMargin,
                (FScreen.FRow - FTopLine) * FLineHeight + FTopMargin);
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//     TVtColors = ( vtsBlack, vtsRed,     vtsGreen, vtsYellow,     {drjohn}
//                   vtsBlue,  vtsMagenta, vtsCyan,  vtsWhite);

procedure TScreen.SetFontColors;                {drjohn}
var
   Att : Byte;
begin
   case FFont.BackColor of
   vtsBlack:   Att := $00;
   vtsRed:     Att := $10;
   vtsGreen:   Att := $20;
   vtsYellow:  Att := $30;
   vtsBlue:    Att := $40;
   vtsMagenta: Att := $50;
   vtsCyan:    Att := $60;
   else
                Att := $70;   // vtclWhite
   end;

   case FFont.Color of
   vtsRed:     Att := Att + $01;
   vtsGreen:   Att := Att + $02;
   vtsYellow:  Att := Att + $03;
   vtsBlue:    Att := Att + $04;
   vtsMagenta: Att := Att + $05;
   vtsCyan:    Att := Att + $06;
   vtsWhite:   Att := Att + $07;
   else
                Att := Att;   // vtclBlack
   end;

   if fsBold in FFont.Style then
      Att := Att or F_INTENSE;;

   if fAttribute = fDefAttribute then
   begin
      fDefAttribute := att;
      fAttribute := att;
   end
   else
      fDefAttribute := att;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Adjusts the scrollbar properties to match the number of host and scrollback
  lines that we can scroll through. }
procedure TCustomEmulVT.AdjustScrollBar;
var
    VisibleLines    : Integer;
begin
    if not FVScrollBar.Visible then
        Exit;
    FVScrollBar.Min := FScreen.FBackEndRow;
{$IFDEF CHAR_ZOOM}
    VisibleLines := Trunc((Height - FTopMargin - FBottomMargin) / (LineHeight * FLineZoom));
{$ELSE}
    VisibleLines := Trunc(Height - FTopMargin - FBottomMargin) / LineHeight);              {drjohn}
{$ENDIF}
    if VisibleLines > FScreen.FRowCount then
        VisibleLines := FScreen.FRowCount;
    FVScrollBar.Max         := FScreen.FRowCount - VisibleLines;
    FVScrollBar.Position    := FTopLine;
    FVScrollBar.SmallChange := 1;
    FVScrollBar.LargeChange := VisibleLines;
    FVScrollBar.Enabled     := FVScrollBar.Max > FVScrollBar.Min;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.Clear;
begin
    FScreen.ClearScreen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCursor(Row, Col : Integer);
begin
    FScreen.GotoXY(Col - 1, Row - 1);
{    SetCaret; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}
procedure TCustomEmulVT.WriteChar(Ch : AnsiChar);
begin
    WriteChar(Char(Ch));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteChar(Ch : Char);
begin
    if FCaretCreated and FCaretShown then begin
        HideCaret(Handle);
        FCaretShown := FALSE;
    end;

    if FLog then
        Write(FFileHandle, Ch);
    FScreen.WriteChar(Ch);

    if FColCount <> FScreen.FColCount then             {drjohn}
    begin
       vResize(Self);
       FColCount := FScreen.FColCount;
    end;

    if FAutoRepaint then
        UpdateScreen;
{    SetCaret;   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteStr(Str : String);
var
    I : Integer;
begin
    if FCaretCreated and FCaretShown then begin
        HideCaret(Handle);
        FCaretShown := FALSE;
    end;

    for I := 1 to Length(Str) do begin
        if FLog then
            Write(FFileHandle, Str[I]);
        FScreen.WriteChar(Str[I]);
    end;
    if FColCount <> FScreen.FColCount then             {drjohn}
    begin
       vResize(Self);
       FColCount := FScreen.FColCount;
    end;
    if FAutoRepaint then
        UpdateScreen;
{    SetCaret; }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WriteBuffer(
    Buffer : PChar;
    Len    : Integer);
var
    I : Integer;
begin
    if FCaretCreated and FCaretShown then begin
        HideCaret(Handle);
        FCaretShown := FALSE;
    end;

    for I := 0 to Len - 1 do begin
        if FLog then
            Write(FFileHandle, Buffer[I]);
        FScreen.WriteChar(Buffer[I]);
    end;
    if FColCount <> FScreen.FColCount then             {drjohn}
    begin
       vResize(Self);
       FColCount := FScreen.FColCount;
    end;
    if FAutoRepaint then
        UpdateScreen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER12_UP}    { V8.03 }
procedure TCustomEmulVT.WriteBuffer(
    Buffer : PAnsiChar;
    Len    : Integer);
var
    I : Integer;
begin
    if FCaretCreated and FCaretShown then begin
        HideCaret(Handle);
        FCaretShown := FALSE;
    end;

    for I := 0 to Len - 1 do begin
        if FLog then
            Write(FFileHandle, PChar(Buffer)[I]);
        FScreen.WriteChar(Char(Buffer[I]));
    end;
    if FColCount <> FScreen.FColCount then             {drjohn}
    begin
       vResize(Self);
       FColCount := FScreen.FColCount;
    end;
    if FAutoRepaint then
        UpdateScreen;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.ReadStr : String;
begin
    Result := FScreen.ReadStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.CopyHostScreen;
begin
    FScreen.CopyScreenToBack;
    AdjustScrollBar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomEmulVT.Create(AOwner: TComponent);
type
    TMyLogPalette = record
        palVersion: Word;
        palNumEntries: Word;
        palPalEntry: array[0..NumPaletteEntries - 1] of TPaletteEntry;
    end;
    TPLogPalette = ^TLogPalette;
var
    plgpl      : ^TMyLogPalette;
    I          : Integer;
begin
    inherited Create(AOwner);
    ControlStyle := ControlStyle + [csOpaque];

    New(plgpl);
    plgpl^.palNumEntries := High(plgpl^.palPalEntry) + 1;
    plgpl^.palVersion    := $300;

    FTopMargin          := 4;
    FLeftMargin         := 6;
    FRightMargin        := 6;
    FBottomMargin       := 4;

    FPaletteEntries[0].peRed    := 0;                { Black }
    FPaletteEntries[0].peGreen  := 0;
    FPaletteEntries[0].peBlue   := 0;
    FPaletteEntries[1].peRed    := 168;              { Red }
    FPaletteEntries[1].peGreen  := 0;
    FPaletteEntries[1].peBlue   := 0;
    FPaletteEntries[2].peRed    := 0;                { Green  }
    FPaletteEntries[2].peGreen  := 168;
    FPaletteEntries[2].peBlue   := 0;
    FPaletteEntries[3].peRed    := 168;              { Yellow }
    FPaletteEntries[3].peGreen  := 168;
    FPaletteEntries[3].peBlue   := 0;
    FPaletteEntries[4].peRed    := 0;                { Dark Blue }
    FPaletteEntries[4].peGreen  := 0;
    FPaletteEntries[4].peBlue   := 168;
    FPaletteEntries[5].peRed    := 168;              { Magenta }
    FPaletteEntries[5].peGreen  := 0;
    FPaletteEntries[5].peBlue   := 168;
    FPaletteEntries[6].peRed    := 0;                { Cyan }
    FPaletteEntries[6].peGreen  := 112;
    FPaletteEntries[6].peBlue   := 216;
    FPaletteEntries[7].peRed    := 200;              { White }
    FPaletteEntries[7].peGreen  := 200;
    FPaletteEntries[7].peBlue   := 200;
    FPaletteEntries[8].peRed    := 84;               { Grey }
    FPaletteEntries[8].peGreen  := 84;
    FPaletteEntries[8].peBlue   := 84;
    FPaletteEntries[9].peRed    := 255;              { Red Highlight }
    FPaletteEntries[9].peGreen  := 84;
    FPaletteEntries[9].peBlue   := 212;
    FPaletteEntries[10].peRed   := 84;               { Green Highlight }
    FPaletteEntries[10].peGreen := 255;
    FPaletteEntries[10].peBlue  := 84;
    FPaletteEntries[11].peRed   := 255;              { Yellow Highlight }
    FPaletteEntries[11].peGreen := 255;
    FPaletteEntries[11].peBlue  := 84;
    FPaletteEntries[12].peRed   := 84;               { Blue Highlight }
    FPaletteEntries[12].peGreen := 84;
    FPaletteEntries[12].peBlue  := 255;
    FPaletteEntries[13].peRed   := 255;              { Magenta Highlight }
    FPaletteEntries[13].peGreen := 84;
    FPaletteEntries[13].peBlue  := 255;
    FPaletteEntries[14].peRed   := 84;               { Cyan highlight }
    FPaletteEntries[14].peGreen := 255;
    FPaletteEntries[14].peBlue  := 255;
    FPaletteEntries[15].peRed   := 255;              { White Highlight }
    FPaletteEntries[15].peGreen := 255;
    FPaletteEntries[15].peBlue  := 255;

    for I := 0 to High(plgpl^.palPalEntry) do begin
        plgpl^.PalPalEntry[I].peRed   := FPaletteEntries[I].peRed;
        plgpl^.PalPalEntry[I].peGreen := FPaletteEntries[I].peGreen;
        plgpl^.PalPalEntry[I].peBlue  := FPaletteEntries[I].peBlue;
        plgpl^.PalPalEntry[I].peFlags := PC_NOCOLLAPSE;
    end;

    FPal := CreatePalette(TPLogPalette(plgpl)^);
    Dispose(plgpl);

    FScreen             := TScreen.Create;
    FVScrollBar         := TScrollBar.Create(Self);
    FScreen.FFont       := TNFont.Create;             {drjohn}
    Font.Name           := 'Terminal';                {drjohn}
    Font.Size           := 12;                        {drjohn}
    Font.Style          := [];                        {drjohn}
    FCharZoom           := 1.0;
    FLineZoom           := 1.0;
    SetupFont;

    FScreen.FXlatInputTable     := @ibm_iso8859_1_G0;
    FScreen.FXlatOutputTable    := @ibm_iso8859_1_G0;
    FScreen.OnCursorVisible     := CursorVisibleEvent;

    FCursorVisible      := TRUE;
    Width               := 250;
    Height              := 100;
    FBorderStyle        := bsSingle;
    FBorderWidth        := 1;
    FAutoRepaint        := TRUE;
    FFkeys              := 1;
    FGraphicDraw        := FALSE;
    OnResize                  := VResize;              {drjohn}

    FLogFileName        := 'EMULVT.LOG';   // angus V6.01

    with FVScrollBar do begin
        Parent   := Self;
        Kind     := sbVertical;
        Width    := 16;
        Visible  := TRUE;
        Align    := alRight;
        OnScroll := VScrollBarScroll;
    end;
    AdjustScrollBar;

    with FScreen do begin
        GotoXY(0, 0);
        WriteStr('EmulVT');
        GotoXY(0, 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetRows(Value : Integer);
begin
    with FScreen do begin
        if FRowCount <> Value then begin
            SetRowCount(Value);
            AdjustScrollBar;
            ClearScreen;
            Repaint;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetRows : Integer;
begin
    Result := FScreen.FRowCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCols(Value : Integer);
begin
    with FScreen do begin
        if FColCount <> Value then begin
            FColCount := Value;
            ClearScreen;
            Repaint;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetCols : Integer;
begin
    Result := FScreen.FColCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.CursorVisibleEvent(Sender : TObject);
begin
    if FScreen.FCursorOff then begin
        if FCaretShown then begin
            HideCaret(Handle);
            FCaretShown := FALSE;
        end;
    end
    else begin
        if FScreen.Focused and not FCaretShown then begin
            ShowCaret(Handle);
            FCaretShown := TRUE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetAutoLF(Value : Boolean);
begin
    FScreen.FAutoLF := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetAutoCR(Value : Boolean);
begin
    FScreen.FAutoCR := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLog(Value : Boolean);
begin
    if FLog = Value then
        Exit;
    if Value then begin
        AssignFile(FFileHandle, FLogFileName);   // angus V6.01
{$I-}
        Append(FFileHandle);
{$I+}
        if IOResult <> 0 then
            Rewrite(FFileHandle);
        Write(FFileHandle, '<Open>');
        FLog := Value;
    end
    else begin
        Write(FFileHandle, '<Close>');
        CloseFile(FFileHandle);
        FLog := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetXlat(Value : Boolean);
begin
    FScreen.FNoXlat        := not Value;
    FScreen.FNoXlatInitial := not Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetXlat : Boolean;
begin
    Result := not FScreen.FNoXlatInitial;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetAutoLF : Boolean;
begin
    Result := FScreen.FAutoLF;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetAutoCR : Boolean;
begin
    Result := FScreen.FAutoCR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetSound(const Value: boolean );               {drjohn}
begin
   FScreen.FSound := Value;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TCustomEmulVT.GetSound : boolean;            {drjohn}
begin
   result := FScreen.FSound;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetResize(const Value: boolean );              {drjohn}
begin
   FResize := Value;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TCustomEmulVT.GetResize : boolean;               {drjohn}
begin
   result := FResize;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomEmulVT.Destroy;
begin
    if FLog then
        Log := FALSE;

    FScreen.FFont.Free;            {drjohn}
    FVScrollBar.Free;
    FScreen.Free;
    DeleteObject(FPal);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetBackRows(Value : Integer);
begin
    with FScreen do begin
        if FBackRowCount <> Value then begin
            SetBackRowCount(Value);
            AdjustScrollBar;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetTopLine(Value : Integer);
begin
    if Value < FVScrollBar.Min then
        Value := FVScrollBar.Min;
    if Value > FVScrollBar.Max then
        Value := FVScrollBar.Max;
    FTopLine := Value;
    FVScrollBar.Position := FTopLine;
    Repaint;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetBackRows : Integer;
begin
    Result := FScreen.FBackRowCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetBackColor(Value : TBackColors);
begin
    FScreen.FBackColor := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetBackColor : TBackColors;
begin
    Result := FScreen.FBackColor;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetMarginColor(const Value: Integer);
begin
    FMarginColor := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLeftMargin(const Value: Integer);
begin
    FLeftMargin := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetBottomMargin(const Value: Integer);
begin
    FBottomMargin := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetRightMargin(const Value: Integer);
begin
    FRightMargin := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetTopMargin(const Value: Integer);
begin
    FTopMargin := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetOptions(Value : TScreenOptions);
begin
    FScreen.FOptions := Value;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetOptions : TScreenOptions;
begin
    Result := FScreen.FOptions;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetupFont;
var
    DC      : HDC;
    Metrics : TTextMetric;
    hObject : THandle;
begin
    DC      := GetDC(0);
    hObject := SelectObject(DC, FScreen.FFont.Handle);             {drjohn}
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, hOBject);
    ReleaseDC(0, DC);

    SetCharWidth(Metrics.tmMaxCharWidth);
    SetLineHeight(Metrics.tmHeight);
    FScreen.SetFontColors;             {drjohn}
    FInternalLeading := Metrics.tmInternalLeading;
    Invalidate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCharWidth(newValue : Single);               {drjohn}
var
    nCol : Integer;
begin
    FCharWidth := newValue;
    for nCol := Low(FCharPos) to High(FCharPos) do
        FCharPos[nCol] := Trunc(FCharWidth * nCol * FCharZoom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetCharZoom(newValue : Single);
begin
    FCharZoom := newValue;
    SetCharWidth(FCharWidth);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLineHeight(Value : Single);             {drjohn}
var
    nRow : Integer;
begin
    FLineHeight := Value;
    for nRow := 0 to High(FLinePos) do  { Jan 10, 2002 }
        FLinePos[nRow] := Trunc(FLineHeight * nRow * FLineZoom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetLineZoom(newValue : Single);
begin
    FLineZoom := newValue;
    SetLineHeight(FLineHeight);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.SetFont(Value : TNFont);               {drjohn}
begin
    FScreen.FFont.Assign(Value);               {drjohn}
{$IFNDEF SINGLE_CHAR_PAINT}
    FFont.Pitch := fpFixed;
{$ENDIF}
    SetupFont;
    SetCaret;
    FScreen.SetFontColors;             {drjohn}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.GetFont : TNFont;               {drjohn}
begin
   Result := FScreen.FFont;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMLButtonDown(var Message: TWMLButtonDown);
begin
    inherited;
    SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
    FTopLine := ScrollPos;
    Repaint;
    SetFocus;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.VResize(Sender: TObject);              {drjohn}
begin
    if AutoResize then
    begin
        CharWidth := ((Width - 2 *(fLeftMargin + fRightMargin) ) / FScreen.FColCount);
        Font.Size := Trunc(CharWidth);
        LineHeight :=  (Height - (fTopMargin + fBottomMArgin)) / FScreen.FRowCount;
        FInternalLeading := -Trunc((LineHeight + Font.Height)/2) ;    { Font.Height is negative}
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.DoKeyBuffer(Buffer : PChar; Len : Integer);
var
    J : Integer;
    ch : Char;
begin
    if Assigned(FOnKeyBuffer) then
        FOnKeyBuffer(Self, Buffer, Len)
    else begin
        for J := 0 to Len - 1 do begin
            ch := Buffer[J];
            KeyPress(ch);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.FindFKeys(ScanCode: Char; Shift: TShiftState;
                                 Ext: Boolean) : PFuncKeyValue;
var
    I      : Integer;
    pFKeys : PFuncKeysTable;
begin
    Result := nil;
    case FKeys of
    0 : pFKeys := @FKeys1;
    1 : pFKeys := @FKeys2;
    2 : pFKeys := @FKeys3;
    else
        pFKeys := @FKeys2;
    end;

    for I := Low(pFKeys^) to High(pFKeys^) do begin
        if (pFKeys^[I].ScanCode <> #0) and (pFKeys^[I].ScanCode = ScanCode) and
           (pFKeys^[I].Shift = Shift) and
           (pFKeys^[I].Ext = Ext) then begin
            Result := @pFKeys^[I].Value;
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.ProcessFKeys(ScanCode: Char; Shift: TShiftState;
                                    Ext: Boolean) : Boolean;
var
    I      : Integer;
    pFKeys : PFuncKeysTable;
begin
    Result := FALSE;
    case FKeys of
    0 : pFKeys := @FKeys1;
    1 : pFKeys := @FKeys2;
    2 : pFKeys := @FKeys3;
    else
        pFKeys := @FKeys2;
    end;

    for I := Low(pFKeys^) to High(pFKeys^) do begin
        if (pFKeys^[I].ScanCode <> #0) and (pFKeys^[I].ScanCode = ScanCode) and
           (pFKeys^[I].Shift = Shift) and
           (pFKeys^[I].Ext = Ext) then begin
            Result := TRUE;
            DoKeyBuffer(@pFKeys^[I].Value[1], Length(pFKeys^[I].Value));
            Break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.AppMessageHandler(var Msg: TMsg; var Handled: Boolean);
const
    { Unicode and iso-8859-1 etc. }
    v1 : array[1..5] of Byte = ($61, $65, $69, $6F, $75); //'aeiou';
    v2 : array[1..5] of Byte = ($E2, $EA, $EE, $F4, $FB); //'âêîôû';
    v3 : array[1..5] of Byte = ($E4, $EB, $EF, $F6, $FC); //'äëïöü';
    SpyFlag : Boolean = FALSE;
var
    Shift     : TShiftState;
    ShiftLock : Boolean;
    VirtKey   : Integer;
    Key       : Char;
    I         : Integer;
    ScanCode  : Char;
    Ext       : Boolean;
    SpyBuffer : String;
    FnBuffer  : String;
    pFV       : PFuncKeyValue;
begin
    if (Msg.hWnd = Handle) and (Msg.Message = WM_KEYDOWN) then begin
        VirtKey   := Msg.wParam;
        Key       := chr(Msg.wParam and $FF);
{        DebugString('AppMessageHandler KEYDOWN ' + IntToHex(Msg.wParam, 4) + #13 + #10); }
        Shift     := KeyDataToShiftState(Msg.lParam);
        ShiftLock := ((GetKeyState(VK_CAPITAL) and 1) > 0);
        ScanCode  := Chr(LOBYTE(HIWORD(Msg.lParam)));
        Ext       := ((Msg.lParam and $1000000) <> 0);

        if Assigned(FOnKeyDown) then begin
            FOnKeyDown(Self, VirtKey, Shift, ShiftLock, ScanCode, Ext);
            if VirtKey = 0 then begin
                Handled := TRUE;
                Exit;
            end;
        end;

        if (Msg.wParam <> VK_SHIFT) and
           (Msg.wParam <> VK_CONTROL) and
           (Msg.wParam <> VK_MENU) then begin
            if (ScanCode = '7') and
               (Shift = [ssAlt, ssCtrl]) and (Ext = FALSE) then begin
                { This is CTRL-ALT-* (on num pad) }
                SpyFlag := TRUE;
                Handled := TRUE;
                Exit;
            end;

            if SpyFlag then begin
                SpyFlag   := FALSE;
                pFV       := FindFKeys(ScanCode, Shift, Ext);
                SpyBuffer := IntToHex(Ord(ScanCode), 2) + ', ' +
                             ShiftStateToString(Shift) + ', ';

                if Ext then
                    SpyBuffer := SpyBuffer + 'TRUE'
                else
                    SpyBuffer := SpyBuffer + 'FALSE';

                if pFV <> nil then
                    SpyBuffer := SpyBuffer + ', ''' +
                                 FuncKeyValueToString(pFV^) + '''';

                SpyBuffer := SpyBuffer + #0;
                ClipBoard.SetTextBuf(@SpyBuffer[1]);

                FnBuffer := 'Key definition from tnchrk' +
                            IntToStr(FKeys) + '.cfg' + #0;
                Application.MessageBox(@SpyBuffer[1], @FnBuffer[1], MB_OK);
                Handled := TRUE;
                Exit;
            end;

            if ProcessFKeys(ScanCode, Shift, Ext) then begin
                Handled := TRUE;
                Exit;
            end;
        end;

        case Msg.wParam of
        VK_SHIFT, VK_CONTROL, VK_MENU: ;

        VK_NEXT, VK_PRIOR, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_HOME, VK_END:
            begin
                if ProcessFKeys(ScanCode, Shift, TRUE) then begin
                    Handled := TRUE;
                    Exit;
                end;
            end;
        VK_TAB, VK_RETURN, VK_ESCAPE, VK_BACK:
            begin
                Handled := TRUE;
            end;

        $DD:
            begin
                if not (ssAlt in Shift) then begin
                    Key     := #0;
                    Handled := TRUE;
                    if (ssShift in Shift) then
                        FFlagTrema := TRUE
                    else
                        FFlagCirconflexe := TRUE;
                end;
            end;

        ord('A')..ord('Z') :
            begin
                if (ssCtrl in Shift) then
                    Key := chr(Word(Key) and $1F)
                else if not ShiftLock and not (ssShift in Shift) then
                    Key := chr(Word(Key) or $20);
                if (FFlagCirconflexe) then begin
                    for I := Length(v1) downto 1 do begin
                        if Key = Char(v1[I]) then begin
                            Key := Char(v2[I]);
                            Break;
                        end;
                    end;
                    FFlagCirconflexe := FALSE;
                end;
                if (FFlagTrema) then begin
                    for I := Length(v1) downto 1 do begin
                        if Key = Char(v1[I]) then begin
                            Key := Char(v3[I]);
                            Break;
                        end;
                    end;
                    FFlagTrema       := FALSE;
                end;
                Handled := TRUE;
            end;
        end;

{        DebugString('Char = ' + IntToHex(Integer(Key), 2) + #13 + #10); }
        if Handled and (Key <> #0) then
            KeyPress(Key);
    end;

    if not Handled and Assigned(FAppOnMessage) then
        FAppOnMessage(Msg, Handled);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.KeyPress(var Key: Char);
begin
{$IFDEF SizeOf(Char) <> 1}
    if Ord(Key) > 255 then
        raise Exception.Create('TCustomEmulVT.KeyPress detected a non-ansi char');
{$ENDIF}
    if not FScreen.FNoXlat then
        Key := Char(FScreen.FXlatOutputTable^[Byte(Key)]);

    inherited KeyPress(Key);
    if FLocalEcho then begin
        WriteChar(Key);
        if not FAutoRepaint then
            UpdateScreen;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMSetFocus(var Message: TWMSetFocus);
begin
{    inherited; }
    FScreen.Focused := TRUE;
{    SetupFont; }

    if not FCursorVisible then
        Exit;

    CreateCaret(Handle, 0, 2, Trunc(FLineHeight));             {drjohn}
    FCaretCreated := TRUE;
    SetCaret;
    if not FScreen.FCursorOff then begin
        ShowCaret(Handle);
        FCaretShown := TRUE;
    end;

    FAppOnMessage := Application.OnMessage;
    Application.OnMessage := AppMessageHandler;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMKillFocus(var Message: TWMKillFocus);
begin
{    inherited; }
    FScreen.Focused := FALSE;

    if not FCursorVisible then
        Exit;

    if FCaretShown then begin
        HideCaret(Handle);
        FCaretShown := FALSE;
    end;

    if FCaretCreated then begin
        DestroyCaret;
        FCaretCreated := FALSE;
    end;

    Application.OnMessage := FAppOnMessage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
begin
{$IFDEF CHAR_ZOOM}
    aRow := FScreen.FRowCount - 1;
    while (Y - FTopMargin) <= FLinePos[aRow] do
        Dec(aRow);
{$ELSE}
    aRow := Trunc((Y - FTopMargin) / FLineHeight);             {drjohn}
{$ENDIF}
    if aRow < 0 then
        aRow := 0
    else if aRow >= FScreen.FRowCount then
        aRow := FScreen.FRowCount - 1;

{$IFDEF CHAR_ZOOM}
    aCol := FScreen.FColCount - 1;
    while (X - FLeftMargin) <= FCharPos[aCol] do
        Dec(aCol);
{$ELSE}
    aCol := (X - FLeftMargin) div FCharWidth;
{$ENDIF}
    if aCol < 0 then
        aCol := 0
    else if aCol >= FScreen.FColCount then
        aCol := FScreen.FColCount - 1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.ShowCursor;
begin
    SetCaret;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMPaletteChanged(var Message : TMessage);
{var
    HandleDC : HDC;}
begin
{        if Message.wParam <> Handle then begin
            HandleDC := GetDC(Handle);
            SelectPalette(HandleDC, FPal, FALSE);
            if RealizePalette(HandleDC) <> 0 then begin
                InvalidateRect(Handle, nil, TRUE);
                MessageBeep(0);
            end;
            ReleaseDC(Handle, HandleDC);
        end;
}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.UpdateScreen;
var
    rc : TRect;
begin
    if FScreen.FAllInvalid then
        InvalidateRect(Handle, nil, FALSE)
    else begin
{$Q-}
            with FScreen.FInvRect do begin
{$IFDEF CHAR_ZOOM}
                if Left = 9999 then begin
                    rc.Top    := 0;
                    rc.Bottom := 0;
                    rc.Left   := 0;
                    rc.Right  := 0;
                end
                else begin
                    rc.Top    := FTopMargin  + FLinePos[Top - FTopLine] + FInternalLeading;
                    rc.Bottom := FTopMargin  + FLinePos[Bottom + 1 - FTopLine] + FInternalLeading;
                    rc.Left   := FLeftMargin + FCharPos[Left];
                    rc.Right  := FLeftMargin + FCharPos[Right + 1];
                end;
{$ELSE}
                rc.Top    := FTopMargin  + FLineHeight * (Top - FTopLine) + FInternalLeading;
                rc.Bottom := FTopMargin  + FLineHeight * (Bottom + 1 - FTopLine) + FInternalLeading;
                rc.Left   := FLeftMargin + FCharWidth * Left;
                rc.Right  := FLeftMargin + FCharWidth * (Right + 1);
{$ENDIF}
            end;
            InvalidateRect(Handle, @rc, FALSE);
{$Q+}
    end;

    { Invalidate the region where the caret is. I should'nt do that, but }
    { if I do'nt, the caret remains where it is ! Bug ?                  }
{$IFDEF CHAR_ZOOM}
    rc.Top    := FLinePos[FScreen.FRow - FTopLine] + FTopMargin;
    rc.Bottom := FLinePos[FScreen.FRow - FTopLine + 1] + FTopMargin;
    rc.Left   := FLeftMargin + FCharPos[FScreen.FCol];
    rc.Right  := FLeftMargin + FCharPos[FScreen.FCol + 1];
{$ELSE}
    rc.Top    := FTopMargin  + FLineHeight * (FScreen.FRow - FTopLine);
    rc.Bottom := rc.Top + FLineHeight;
    rc.Left   := FLeftMargin + FCharWidth * FScreen.FCol;
    rc.Right  := rc.Left + FCharWidth;
{$ENDIF}
    InvalidateRect(Handle, @rc, FALSE);

    FScreen.InvClear;

    if FCaretCreated then begin
        ShowCaret(Handle);
        FCaretShown := TRUE;
    end;
    SetCaret;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.SnapPixelToRow(Y : Integer) : Integer;
var
    nRow : Integer;
begin
    nRow := PixelToRow(Y);
{$IFDEF CHAR_ZOOM}
    Result := FTopMargin + FLinePos[nRow];
{$ELSE}
    Result := FTopMargin + nRow * FLineHeight;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.SnapPixelToCol(X : Integer) : Integer;
var
    nCol : Integer;
begin
    nCol := PixelToCol(X);
{$IFDEF CHAR_ZOOM}
    Result := FLeftMargin + FCharPos[nCol];
{$ELSE}
    Result := FLeftMargin + nCol * FCharWidth;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.PixelToRow(Y : Integer) : Integer;
var
    nRow : Integer;
begin
{$IFDEF CHAR_ZOOM}
    nRow := FScreen.FRowCount - 1;
    while (nRow > 0) and ((Y - FTopMargin) < FLinePos[nRow]) do
        Dec(nRow);
{$ELSE}
    nRow := (Y - FTopMargin) div FLineHeight;
{$ENDIF}
    if nRow < 0 then
        nRow := 0;
    Result := nRow;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomEmulVT.PixelToCol(X : Integer) : Integer;
var
    nCol : Integer;
begin
{$IFDEF CHAR_ZOOM}
    nCol := FScreen.FColCount - 1;
    while (X - FLeftMargin) < FCharPos[nCol] do
        Dec(nCol);
{$ELSE}
    nCol := (X - FLeftMargin) div FCharWidth;
{$ENDIF}
    if nCol < 0 then
        nCol := 0;
    Result := nCol;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure will paint graphic char from the OEM charset (lines,       }
{ corners, T and other like) using GDI functions. This will result in       }
{ autosized characters, necessary for example to draw a frame when zoom     }
{ affect character and line spacing.                                        }
procedure TCustomEmulVT.PaintGraphicChar(
    DC   : HDC;
    X, Y : Integer;
    rc   : PRect;
    ch   : Char);
const
    OneSpace : Char = ' ';
var
    X1, X2, X3 : Integer;
    Y1, Y2, Y3 : Integer;
    Co         : TColor;
begin
    ExtTextOut(DC,
               X, Y,
               ETO_OPAQUE or ETO_CLIPPED, rc,
               @OneSpace, 1, nil);
    X1 := X;
    X3 := rc^.Right;
    X2 := (X1 + X3) div 2;
    Y1 := rc^.Top;
    Y3 := rc^.Bottom;
    Y2 := (Y1 + Y3) div 2;
    case Ord(Byte(Ch)) of
    $C4: begin       { Horizontal single line }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
          end;
    $B3: begin       { Vertical single line }
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
          end;
    $DA: begin       { Upper Left Single Corner }
              Canvas.MoveTo(X3, Y2);
              Canvas.LineTo(X2, Y2);
              Canvas.LineTo(X2, Y3);
          end;
    $C0: begin       { Bottom Left Single Corner }
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y2);
              Canvas.LineTo(X3, Y2);
          end;
    $C1: begin       { Reverse T }
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y2);
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
          end;
    $C2: begin       { T }
              Canvas.MoveTo(X2, Y3);
              Canvas.LineTo(X2, Y2);
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
          end;
    $C3: begin       { Left T }
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
              Canvas.MoveTo(X2, Y2);
              Canvas.LineTo(X3, Y2);
          end;
    $B4: begin       { Right T }
              Canvas.MoveTo(X2,     Y1);
              Canvas.LineTo(X2,     Y3);
              Canvas.MoveTo(X2,     Y2);
              Canvas.LineTo(X1 - 1, Y2);
          end;
    $BF: begin       { Top Right Single Corner }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X2, Y2);
              Canvas.LineTo(X2, Y3);
          end;
    $D9: begin       { Bottom Right Single Corner }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X2, Y2);
              Canvas.LineTo(X2, Y1 - 1);
          end;
    $D6: begin       { Upper Left Single/Double Corner }
              Canvas.MoveTo(X3, Y2);
              Canvas.LineTo(X2 - 1, Y2);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y2);
              Canvas.LineTo(X2 + 1, Y3);
          end;
    $D3: begin       { Bottom Left Single/Double Corner }
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y2);
              Canvas.LineTo(X3, Y2);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2);
          end;
    $B7: begin       { Top Right Single/Double Corner }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X2 + 1, Y2);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y2);
              Canvas.LineTo(X2 - 1, Y3);
          end;
    $BD: begin       { Bottom Right Single/Double Corner }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2);
              Canvas.LineTo(X1 - 1, Y2);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y2);
          end;
    $D5: begin       { Upper Left Double/Single Corner }
              Canvas.MoveTo(X3, Y2 - 1);
              Canvas.LineTo(X2, Y2 - 1);
              Canvas.LineTo(X2, Y3);
              Canvas.MoveTo(X3, Y2 + 1);
              Canvas.LineTo(X2, Y2 + 1);
          end;
    $D4: begin       { Bottom Left Double/Single Corner }
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X2, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
          end;
    $B8: begin       { Top Right Double/Single Corner }
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X2, Y2 - 1);
              Canvas.LineTo(X2, Y3);
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X2, Y2 + 1);
          end;
    $BE: begin       { Bottom Right Double/Single Corner }
              Canvas.MoveTo(X2,     Y1);
              Canvas.LineTo(X2,     Y2 + 1);
              Canvas.LineTo(X1 - 1, Y2 + 1);
              Canvas.MoveTo(X1,     Y2 - 1);
              Canvas.LineTo(X2,     Y2 - 1);
          end;
    $CD: begin       { Horizontal Double line }
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
          end;
    $BA: begin       { Vertical Double line }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y3);
          end;
    $D1: begin       { T Top Horizontal Double line }
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
              Canvas.MoveTo(X2, Y2 + 1);
              Canvas.LineTo(X2, Y3);
          end;
    $CF: begin       { T Bottom Horizontal Double line }
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
              Canvas.MoveTo(X2, Y2 - 1);
              Canvas.LineTo(X2, Y1);
          end;
    $C6: begin       { T Left Horizontal Double line }
              Canvas.MoveTo(X2, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X2, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
          end;
    $B5: begin       { T Right Horizontal Double line }
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X2, Y2 + 1);
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X2, Y2 - 1);
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
          end;
    $C9: begin       { Upper Left Double Corner }
              Canvas.MoveTo(X3,     Y2 - 1);
              Canvas.LineTo(X2 - 1, Y2 - 1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X3,     Y2 + 1);
              Canvas.LineTo(X2 + 1, Y2 + 1);
              Canvas.LineTo(X2 + 1, Y3);
          end;
    $C8: begin       { Bottom Left Double Corner }
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y2 + 1);
              Canvas.LineTo(X3,     Y2 + 1);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2 - 1);
              Canvas.LineTo(X3,     Y2 - 1);
          end;
    $BB: begin       { Top Right Double Corner }
              Canvas.MoveTo(X1,     Y2 - 1);
              Canvas.LineTo(X2 + 1, Y2 - 1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X1,     Y2 + 1);
              Canvas.LineTo(X2 - 1, Y2 + 1);
              Canvas.LineTo(X2 - 1, Y3);
          end;
    $BC: begin       { Bottom Right Double Corner }
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y2 - 1);
              Canvas.LineTo(X1 - 1, Y2 - 1);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2 + 1);
              Canvas.LineTo(X1 - 1, Y2 + 1);
          end;
    $CC: begin       { Double left T }
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2 - 1);
              Canvas.LineTo(X3,     Y2 - 1);
              Canvas.MoveTo(X3,     Y2 + 1);
              Canvas.LineTo(X2 + 1, Y2 + 1);
              Canvas.LineTo(X2 + 1, Y3);
          end;
    $B9: begin       { Double Right T }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y2 - 1);
              Canvas.LineTo(X1 - 1, Y2 - 1);
              Canvas.MoveTo(X1,     Y2 + 1);
              Canvas.LineTo(X2 - 1, Y2 + 1);
              Canvas.LineTo(X2 - 1, Y3);
          end;
    $C7: begin       { Double T Single Left }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y2);
              Canvas.LineTo(X3,     Y2);
          end;
    $B6: begin       { Double T Single Right }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 - 1, Y2);
              Canvas.LineTo(X1 - 1, Y2);
          end;
    $D2: begin       { Single T Double Top }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
              Canvas.MoveTo(X2 - 1, Y2);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y2);
              Canvas.LineTo(X2 + 1, Y3);
          end;
    $D0: begin       { Single T Double Bottom }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
              Canvas.MoveTo(X2 - 1, Y2);
              Canvas.LineTo(X2 - 1, Y1);
              Canvas.MoveTo(X2 + 1, Y2);
              Canvas.LineTo(X2 + 1, Y1);
          end;
    $DB: begin       { Full Block }
              Canvas.Rectangle(X1, Y1, X3, Y3);
          end;
    $DC: begin       { Half Bottom Block }
              Canvas.Rectangle(X1, Y2, X3, Y3);
          end;
    $DD: begin       { Half Left Block }
              Canvas.Rectangle(X1, Y1, X2, Y3);
          end;
    $DE: begin       { Half Right Block }
              Canvas.Rectangle(X2, Y1, X3, Y3);
          end;
    $DF: begin       { Half Top Block }
              Canvas.Rectangle(X1, Y1, X2, Y2);
          end;
    $C5: begin       { Single Cross }
              Canvas.MoveTo(X1, Y2);
              Canvas.LineTo(X3, Y2);
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
          end;
    $CE: begin       { Double Cross }
              Canvas.MoveTo(X1,     Y2 - 1);
              Canvas.LineTo(X2 - 1, Y2 - 1);
              Canvas.LineTo(X2 - 1, Y1);
              Canvas.MoveTo(X1,     Y2 + 1);
              Canvas.LineTo(X2 - 1, Y2 + 1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2 - 1);
              Canvas.LineTo(X3,     Y2 - 1);
              Canvas.MoveTo(X2 + 1, Y3);
              Canvas.LineTo(X2 + 1, Y2 + 1);
              Canvas.LineTo(X3,     Y2 + 1);
          end;
    $D8: begin      { Cross Double Horizontal Single vertical }
              Canvas.MoveTo(X1, Y2 + 1);
              Canvas.LineTo(X3, Y2 + 1);
              Canvas.MoveTo(X1, Y2 - 1);
              Canvas.LineTo(X3, Y2 - 1);
              Canvas.MoveTo(X2, Y1);
              Canvas.LineTo(X2, Y3);
          end;
    $D7: begin      { Cross Single Horizontal Double Vertical }
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y3);
              Canvas.MoveTo(X2 - 1, Y1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X1,     Y2);
              Canvas.LineTo(X3,     Y2);
          end;
    $CA: begin      { Double T bottom }
              Canvas.MoveTo(X1,     Y2 - 1);
              Canvas.LineTo(X2 - 1, Y2 - 1);
              Canvas.LineTo(X2 - 1, Y1);
              Canvas.MoveTo(X2 + 1, Y1);
              Canvas.LineTo(X2 + 1, Y2 - 1);
              Canvas.LineTo(X3,     Y2 - 1);
              Canvas.MoveTo(X1,     Y2 + 1);
              Canvas.LineTo(X3,     Y2 + 1);
          end;
    $CB: begin      { Double T  }
              Canvas.MoveTo(X1,     Y2 + 1);
              Canvas.LineTo(X2 - 1, Y2 + 1);
              Canvas.LineTo(X2 - 1, Y3);
              Canvas.MoveTo(X2 + 1, Y3);
              Canvas.LineTo(X2 + 1, Y2 + 1);
              Canvas.LineTo(X3,     Y2 + 1);
              Canvas.MoveTo(X1,     Y2 - 1);
              Canvas.LineTo(X3,     Y2 - 1);
          end;
    $B0: begin
              Co := Canvas.Pen.Color;
              for Y := Y1 to Y3 do begin
                  X := X1 + (Y mod 3);
                  while X < X3 do begin
                      Canvas.Pixels[X, Y] := Co;
                      X := X + 3;
                  end;
              end;
          end;
    $B1: begin
              Co := Canvas.Pen.Color;
              for Y := Y1 to Y3 do begin
                  X := X1 + (Y and 1);
                  while X < X3 do begin
                      Canvas.Pixels[X, Y] := Co;
                      X := X + 2;
                  end;
              end;
          end;
    $B2: begin
              Co := Canvas.Pen.Color;
              for Y := Y1 to Y3 do begin
                  X := X1 + (Y mod 3);
                  while X < X3 do begin
                      Canvas.Pixels[X, Y] := Co;
                      Inc(X);
                      if X < X3 then
                          Canvas.Pixels[X, Y] := Co;
                      Inc(X);
                      Inc(X);
                  end;
              end;
          end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.PaintOneLine(
    DC         : HDC;
    Y, Y1      : Integer;
    const Line : TLine;
    nColFrom   : Integer;
    nColTo     : Integer;
    Blank      : Boolean);
var
    rc        : TRect;
    nCnt      : Integer;
    nAtt      : Byte;
    X         : Integer;
    nChr      : Integer;
    Ch        : Char;
const
    BlankLine : array [0..MAX_COL] of char = '';
begin
    nAtt := Line.Att[nColFrom];

{    if nAtt = $0B then
        X := 0; }
{
    SetBkColor(DC, PALETTEINDEX(nAtt div $0F));
    SetTextColor(DC, PALETTEINDEX(nAtt and $0F));
}
    if not FMonoChrome then begin
        with FPaletteEntries[(nAtt shr 4) and $0F] do
            SetBkColor(DC, PALETTERGB(peRed, peGreen, peBlue));
        with FPaletteEntries[nAtt and $0F] do begin
            SetTextColor(DC, PALETTERGB(peRed, peGreen, peBlue));
            Canvas.Pen.Color   := PALETTERGB(peRed, peGreen, peBlue);
            Canvas.Brush.Color := PALETTERGB(peRed, peGreen, peBlue);
        end;
    end
    else begin
        if (nAtt div $0F) <> 0 then
            SetBkColor(DC, RGB(127, 127, 127))
        else
            SetBkColor(DC, RGB(255, 255, 255));

        if (nAtt and $0F) <> 0 then
            SetTextColor(DC, RGB(0, 0, 0))
        else
            SetTextColor(DC, RGB(255, 255, 255));
    end;

    nCnt      := nColTo - nColFrom;
    nChr      := 0;
{$IFDEF SINGLE_CHAR_PAINT}
    while nChr < nCnt do begin
{$IFDEF CHAR_ZOOM}
        X         := FLeftMargin + FCharPos[nColFrom + nChr];
        rc.Top    := Y  + FInternalLeading;
        rc.Bottom := Y1 + FInternalLeading;
        rc.Left   := X;
        rc.Right  := FLeftMargin + FCharPos[nColFrom + nChr + 1];
{$ELSE}
        X         := FLeftMargin + (nColFrom + nChr) * FCharWidth;
        rc.Top    := Y  + FInternalLeading;
        rc.Bottom := Y1 + FInternalLeading;
        rc.Left   := X;
        rc.Right  := rc.Left + FCharWidth;
{$ENDIF}
        if (nColFrom + nChr) = 0 then
           rc.Left := rc.Left - FLeftMargin;
        if (nColFrom + nChr) >= FScreen.FColCount then
           rc.Right := rc.Right + FRightMargin;
        if Blank then
           Ch := ' '
        else
{$IF SizeOf(Line.Txt[0]) <> 1}
           Ch := Line.Txt[nColFrom + nChr];
{$ELSE}
           Ch := Char(Line.Txt[nColFrom + nChr]);
{$IFEND}
        if FGraphicDraw and
           (FScreen.FXlatOutputTable = @ibm_iso8859_1_G0) and
           (Byte(Ch) >= $B0) and (Byte(Ch) <= $DF) and
           (Byte(Ch) in
                  [$B3, $C4, $DA, $C0, $C1, $C2, $C3, $B4, $BF, $D9,
                   $DB, $DC, $DD, $DE, $DF,
                   $BA, $CD, $C9, $C8, $BB, $BC,
                   $CC, $B9, $C7, $B6, $D2, $D0,
                   $D5, $D4, $B8, $BE,
                   $C6, $D1, $B5, $CF,
                   $D6, $B7, $D3, $BD,
                   $C5, $CE, $D8, $D7, $CA, $CB,
                   $B0, $B1, $B2]) then
            PaintGraphicChar(DC, X, Y, @rc, Ch)
        else
            ExtTextOut(DC, X, Y, ETO_OPAQUE or ETO_CLIPPED, @rc, @Ch, 1, nil);
        Inc(nChr);
    end;
{$ELSE}
{$IFDEF CHAR_ZOOM}
    X         := LeftMargin + FCharPos[nColFrom];
    rc.Top    := Y  + FInternalLeading;
    rc.Bottom := Y1 + FInternalLeading;
    rc.Left   := X;
    rc.Right  := LeftMargin + FCharPos[nColFrom + nCnt];
{$ELSE}
    X         := LeftMargin + nColFrom * FCharWidth;
    rc.Top    := Y  + FInternalLeading;
    rc.Bottom := Y1 + FInternalLeading;
    rc.Left   := X;
    rc.Right  := rc.Left + nCnt * FCharWidth;
{$ENDIF}
    if nColFrom = 0 then
       rc.Left := rc.Left - LeftMargin;
    if nColTo >= FScreen.FColCount then
       rc.Right := rc.Right + RightMargin;
    if Blank then
        ExtTextOut(DC,
                   X, Y,
                   ETO_OPAQUE or ETO_CLIPPED, @rc,
                   @BlankLine[0], nCnt, nil)
    else
        ExtTextOut(DC,
                   X, Y,
                   ETO_OPAQUE or ETO_CLIPPED, @rc,
                   @Line.Txt[nColFrom], nCnt, nil);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WRectangle(DC: HDC; X1, Y1, X2, Y2 : Integer) : LongBool;
begin
{$IFDEF USEWINDOWS}
    Result := {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.Rectangle(DC, X1, Y1, X2, Y2);
{$ELSE}
    Result := Winprocs.Rectangle(DC, X1, Y1, X2, Y2);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomEmulVT.WMPaint(var Message: TWMPaint);
var
    DC        : HDC;
    PS        : TPaintStruct;
    Y, Y1     : Integer;
    rc        : TRect;
    OldPen    : THandle;
    OldBrush  : THandle;
    OldFont   : THandle;
    rcPaint   : TRect;
    DrawRct   : TRect;
    nRow      : Integer;
    nCol      : Integer;
    nColFrom  : Integer;
    Line      : TLine;
    BackBrush : HBrush;
    BackIndex : Integer;
begin
    { This may be a bit of overkill but we have to keep the scrollbar tracking
      with the number of lines visible on the screen.  The calling program can
      change the Height of the screen and we don't have a good way to know that.
      This routine will get called whenever the screen gets updated so it is a
      good time to update the scrollbar. }
    AdjustScrollBar;

    if not GetUpdateRect(WindowHandle, rc, FALSE) then
        Exit;

    BackBrush := 0;
    OldBrush  := 0;

    DC := Message.DC;
    if DC = 0 then
        DC := BeginPaint(WindowHandle, PS);
    try
        if not FMonoChrome then begin
            SelectPalette(DC, FPal, FALSE);
            RealizePalette(DC);
            if FMarginColor = -1 then
                { screen background is bits 4-6 (of 0-7) }
                BackIndex := (FScreen.FDefAttribute shr 4) and $07             {drjohn}
            else
                BackIndex := FMarginColor mod (NumPaletteEntries div 2);               {drjohn}
                                    { NumPaletteEntries includes Intense colors }
            with FPaletteEntries[BackIndex] do
                BackBrush := CreateSolidBrush(PALETTERGB(peRed, peGreen, peBlue));
            OldBrush := SelectObject(DC, BackBrush);
        end;

    {$IFDEF USEWINDOWS}
        {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetClientRect(WindowHandle, DrawRct);
    {$ELSE}
        WinProcs.GetClientRect(WindowHandle, DrawRct);
    {$ENDIF}
        rcPaint  := PS.rcPaint;
        rc.Left  := 2;
        rc.Right := DrawRct.Right - 2;
        nRow     := PixelToRow(rcPaint.top);
        nRow := nRow - 1;
        if nRow < 0 then
            nRow := 0;

{$IFDEF CHAR_ZOOM}
        Y  := FTopMargin + FLinePos[nRow];
        Y1 := FTopMargin + FLinePos[nRow + 1];
{$ELSE}
        Y  := FTopMargin + nRow * FLineHeight;
        Y1 := Y + FLineHeight;
{$ENDIF}

        if rcPaint.Top <= FTopMargin then begin
            OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
            WRectangle(DC, rcPaint.left, rcPaint.Top,
                       rcPaint.Right + 1,
                       FTopMargin - FInternalLeading + 1);        {drjohn}
            SelectObject(DC, OldPen);
        end;

        if (nRow = 0) and (FInternalLeading > 0) then begin
            OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
            WRectangle(DC, rcPaint.left, rcPaint.Top,
                       rcPaint.Right + 1,
                       Y - FInternalLeading + 1);              {drjohn}
            SelectObject(DC, OldPen);
        end;

        OldFont := SelectObject(DC, FScreen.FFont.Handle);             {drjohn}
        nRow    := nRow + FTopLine;
        while nRow < FScreen.FRowCount do begin
            rc.Top    := Y;
            rc.Bottom := TRunc(Y + FLineHeight);               {drjohn}
            if rc.Bottom > (DrawRct.Bottom - FBottomMargin) then begin
                OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
                WRectangle(DC, rc.Left - 2, rc.Top, rc.Right + 1,
                           DrawRct.Bottom - 1);
                SelectObject(DC, OldPen);
                Break;
            end;
            Line := FScreen.Lines[nRow];

            nCol     := 0;
            nColFrom := 0;
            while nCol < FScreen.FColCount do begin
                while (nCol < FScreen.FColCount) and
                      (Line.Att[nCol] = Line.Att[nColFrom]) do
                    Inc(nCol);

                PaintOneLine(DC, Y, Y1, Line, nColFrom, nCol, FALSE);
                nColFrom := nCol;
            end;

            nRow := nRow + 1;
{$IFDEF CHAR_ZOOM}
            Y    := FTopMargin + FLinePos[nRow - FTopLine];
            Y1   := FTopMargin + FLinePos[nRow + 1 - FTopLine];
{$ELSE}
            Y    := Y + FLineHeight;
            Y1   := Y + FLineHeight;
{$ENDIF}
            if Y > rcPaint.Bottom then
                Break;
        end;

        { Fill region between last text line and bottom of the window }
        OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
        if (FScreen.FRowCount - FTopLine) <= MAX_ROW then begin { WM + SE 09/08/00 }
            WRectangle(DC, rc.Left - 2,
                       FTopMargin + FLinePos[FScreen.FRowCount - FTopLine]{ + 1},
                       rc.Right + 3, DrawRct.Bottom + 1);
        end;
        SelectObject(DC, OldPen);

{$IFDEF CHAR_ZOOM}
        if (FLeftMargin + FCharPos[FScreen.FColCount]) < rc.Right then begin
            OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
            WRectangle(DC, FLeftMargin + FCharPos[FScreen.FColCount],
                       FTopMargin,   { 09/03/99 }
                       rcPaint.Right + 1, DrawRct.Bottom + 1);
            SelectObject(DC, OldPen);
        end;
{$ELSE}
        if (FLeftMargin + FScreen.FColCount * FCharWidth) < rc.Right then begin
            OldPen := SelectObject(DC, GetStockObject(NULL_PEN));
            WRectangle(DC, FLeftMargin + FScreen.FColCount * FCharWidth,
                       FTopMargin, rc.Right + 1, DrawRct.Bottom - 1);
            SelectObject(DC, OldPen);
        end;
{$ENDIF}

        if FSelectRect.Top <> -1 then begin
            SelectObject(DC, GetStockObject(NULL_BRUSH));
            SelectObject(DC, GetStockObject(BLACK_PEN));
            WRectangle(DC, FSelectRect.Left,
                       FSelectRect.Top,
                       FSelectRect.Right + 1,
                       FSelectRect.Bottom - 1);
            SelectObject(DC, GetStockObject(WHITE_PEN));
            WRectangle(DC, FSelectRect.Left - 1,
                       FSelectRect.Top - 1,
                       FSelectRect.Right + 2,
                       FSelectRect.Bottom);
        end;

        SelectObject(DC, OldFont);
        if OldBrush <> 0 then
            SelectObject(DC, OldBrush);
        if BackBrush <> 0 then
            DeleteObject(BackBrush);
    finally
        if Message.DC = 0 then
            EndPaint(WindowHandle, PS);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{* * * * * * * * * * * * * * * * * * * * * * ** * * * * * * * * * * * * * *}
function TCustomEmulVT.GetScreenText(Buffer: PChar; BufSize: Integer ): Integer;              {drjohn}
var
    StartRow : Integer;
    StopRow  : Integer;
    StartCol : Integer;
    StopCol  : Integer;
    nRow     : Integer;
    nCol     : Integer;
    Line     : TLine;
    nCnt     : Integer;
    redraw : boolean;
begin
    nCnt := 0;
    if (BufSize < 1) then begin
        if BufSize > 0 then
            Buffer[0] := #0;
        Result := nCnt;
        Exit;
    end;

  StartRow := 0;
  StopRow := 24;
  StartCol := 0;
  StopCol := 79;
  redraw := FALSE;

    for nRow := StartRow to StopRow do begin
        if BufSize < 2 then
            Break;
        Line := FScreen.FLines^[Rows - 1 - nRow];
        for nCol := StartCol to StopCol do
        begin
{$IF SizeOf(Line.Txt[0]) <> 1}
       if Line.Txt[nCol] = AnsiChar(#0) then
       begin
          Buffer[0] := ' ';
          Line.Txt[nCol] := ' ';
          redraw := TRUE;
       end
       else
          Buffer[0] := Line.Txt[nCol];
{$ELSE}
       if Line.Txt[nCol] = Char(#0) then
       begin
          Buffer[0] := ' ';
          Line.Txt[nCol] := ' ';
          redraw := TRUE;
       end
       else
          Buffer[0] := Char(Line.Txt[nCol]);
{$IFEND}
            Inc(Buffer);
            Dec(BufSize);
            Inc(nCnt);
            if BufSize < 2 then
                Break;
        end;
        if nRow < StopRow then begin
            if BufSize < 4 then
                Break;
            Buffer[0] := #13;
            Buffer[1] := #10;
            Inc(Buffer);
            Inc(Buffer);
            Dec(BufSize);
            Dec(BufSize);
            Inc(nCnt);
            Inc(nCnt);
        end;
    end;

  if redraw  then
     RePaint;

    Buffer[0] := #0;
    Result := nCnt;
end;


end.

