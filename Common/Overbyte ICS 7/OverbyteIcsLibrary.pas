{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:
Creation:     April 2004
Version:      1.16
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

History:
Aug 20, 2006 V1.01 Added IntToHex for .NET
Oct 28, 2006 V1.02 Added SysErrorMessage
Mar 10, 2008 V1.03 Made some changes to prepare code for Unicode
                   StrLen takes a PAnsiChar instead of Char argument
                   StrCopy takes a PAnsiChar instead of Char argument
                   StrPas takes a PAnsiChar instead of Char argument
Apr 15, 2008 V1.04 A. Garrels  - All Str.. functions take PAnsiChar argument
                   and are overloaded in D12 to also take a PWideChar argument
                   according to changes in SysUtils.pas.
Apr 20, 2008 V1.05 A. Garrels - Added own implementations for wide-versions of
                   StrLComp and StrLIComp. Added some other functions from
                   SysUtils.pas. Since the overloaded functions are included
                   you would have to reference the unit if both SysUtils.pas
                   and OverbyteIcsLibrary.pas are in the uses clause, i.e.
                   either SysUtils.StrLIComp() or OverbyteIcsLibrary.StrLIComp(),
                   not sure whether that breaks something.
Apr 20, 2008 V1.06 A. Garrels added GetACP and const CP_ACP.
Apr 30, 2008 V1.07 A. Garrels prefixed any Win32 functions with an underscore,
                   Added Ansi versions of Uppercase, Lowercase, Trim.
May 15, 2008 V1.08 A. Garrels optimized _UpperCase, _LowerCase, _Trim and
                   added IcsIntToStrA(), IcsIntToHexA() and _CompareText().
                   New define "USE_ICS_RTL" see below, added missing overloads
                   of _IntToStr.
May 23, 2008 V1.09 A. Garrels check for empty string in IcsLowerCaseA() and
                   IcsUpperCaseA().
Jul 01, 2008 V1.10 A. Garrels fixed a bug in IcsCompareTextA().
Jul 02, 2008 V1.11 A. Garrels optimized IcsCompareTextA() a bit.
Aug 03, 2008 V1.12 F. Piette made IcsUpperCaseA, IcsLowerCaseA, IcsTrimA and
                   IcsCompareTextA public. Added IcsSameTextA.
Aug 07, 2008 V1.13 F. Piette found a bug in some AnsiString-functions.
                   They always call SetLength() on the result now.
Aug 11, 2008 V1.14 A. Garrels added IcsCompareStrA() and an overload to
                   _CompareStr().
May 09, 2009 V1.15 Arno added const CP_UTF7, and procedure _ShowException
                   for future use.
May 15, 2009 V1.16 Arno added some EXTERNALSYM directives to make C++Builder
                   happy (Ambiguity between '_fastcall Application()' and
                   'Forms::Application')


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsLibrary;

interface

{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{ The following define works only for compiler versions < v120.               }
{ Comment next line if you want to use the original RTL including FastCode    }
{ patches. Otherwise ICS routines are called when they are faster either      }
{ faster than original RTL routines or not available in the RTL. Currently    }
{ only a few Ansi-versions are implemented. Should we use FastCode?           }
{#$DEFINE USE_ICS_RTL}

uses
{$IFDEF CLR}
  System.Collections,
  System.IO,
  System.Threading,
  System.Runtime.InteropServices,
  System.Reflection,
  System.Text,
{$ENDIF}
{$IFDEF WIN32}
  Windows, Classes, Messages,
{$IFNDEF NOFORMS}
  Forms,
{$ENDIF}
  SysUtils,
{$ENDIF}
  OverbyteIcsTypes;

const
  OverbyteIcsLibraryVersion = 115;
  CopyRight : String        = ' OverbyteIcsLibrary (c) 2004-2010 F. Piette V1.15 ';


{$IFDEF CLR}
type
  TRTLCriticalSection = class
  end;

  TList = class(ArrayList)  
    function  GetItems(Index: Integer): TObject;
    procedure SetItems(Index: Integer; const Value: TObject);
  public
    procedure Delete(Index : Integer);
    function  First: TObject;
    function  Last: TObject;
    procedure Pack;
    property Items[Index : Integer] : TObject read  GetItems
                                              write SetItems;
  end;
  TStrings = class
  public
    procedure SetStrings(nIndex: Integer; const Value: String); virtual; abstract;
    function  GetStrings(nIndex: Integer): String; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    procedure Add(const S: String); virtual; abstract;
    function  Count : Integer; virtual; abstract;
    property Strings[nIndex : Integer] : String read  GetStrings
                                                write SetStrings;
  end;
  TStringList = class(TStrings)
  protected
    FStrings : TList;
  public
    procedure SetStrings(nIndex: Integer; const Value: String); override;
    function  GetStrings(nIndex: Integer): String; override;
  public
    constructor Create;
    procedure Clear; override;
    procedure Add(const S: String); override;
    function  Count : Integer; override;
  end;
{$ENDIF}

{$IFNDEF VCL}
const
    fmCreate         = $FFFF;
    fmOpenRead       = $0000;
    fmOpenWrite      = $0001;
    fmOpenReadWrite  = $0002;
    fmShareCompat    = $0000 platform; // DOS compatibility mode is not portable
    fmShareExclusive = $0010;
    fmShareDenyWrite = $0020;
    fmShareDenyRead  = $0030 platform; // write-only not supported on all platforms
    fmShareDenyNone  = $0040;
    soFromBeginning  = 0;
    soFromCurrent    = 1;
    soFromEnd        = 2;
type
    TFileStream = class
    protected
        FFileName  : String;
        FClrStream : System.IO.Stream;
        function  GetSize : Int64;
        procedure SetSize(Value : Int64);
    public
        constructor Create(const AFileName : String; Mode : Integer);
        procedure Seek(Offset : Int64; Origin : Integer);
        procedure Write(Ch : Char); overload;
        property Size : Int64 read GetSize write SetSize;
    end;
{$ENDIF}

{$IFDEF CLR}
const
  WM_CREATE           = 1;
  WM_DESTROY          = 2;
  WM_CLOSE            = 16;
  WM_NCCREATE         = 129;
  WM_QUIT             = $0012;
  WM_TIMER            = $0113;
  WS_POPUP            = DWORD($80000000);
  WS_CAPTION          = $C00000;      { WS_BORDER or WS_DLGFRAME  }
  WS_CLIPSIBLINGS     = $4000000;
  WS_SYSMENU          = $80000;
  WS_MAXIMIZEBOX      = $10000;
  WS_MINIMIZEBOX      = $20000;
  WS_EX_TOOLWINDOW    = $80;
  WM_USER             = $0400;
  PM_NOREMOVE         = 0;
  PM_REMOVE           = 1;
  PM_NOYIELD          = 2;
  //WM_MY_MSG           = WM_USER + 1;
  //GWL_WNDPROC         = -4;

function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: IntPtr): IntPtr; overload;
function GetWindowLongIntPtr(hWnd: HWND; nIndex: Integer): IntPtr;
function GetMessage(out lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL;
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
function TranslateMessage(const lpMsg: TMsg): BOOL;
function DispatchMessage(const lpMsg: TMsg): Longint;
function PeekMessage(out lpMsg: TMsg; hWnd: HWND; wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL;
function RegisterClass(const lpWndClass: TWndClass): ATOM; overload;
function RegisterClass(const lpWndClassInfo: TWndClassInfo): ATOM; overload;
function UnregisterClass(lpClassName: String; hInstance: HINST): BOOL;
function GetClassInfo(hInstance: HINST; lpClassName: String;out lpWndClass: TWndClassInfo): BOOL;
function CreateWindowEx(dwExStyle: DWORD; lpClassName: String;
  lpWindowName: String; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: IntPtr): HWND; overload;
function DestroyWindow(hWnd: HWND): BOOL;
function KillTimer(hWnd: HWND; uIDEvent: Cardinal): BOOL;
function SetTimer(
    H           : HWND;
    uIDEvent    : Cardinal;
    uElapse     : Cardinal;
    lpTimerFunc : IntPtr): UINT;

function  HInstance: HINST;
function  GetCurrentThreadId: DWORD;
procedure Sleep(dwMilliseconds: DWORD);
function  GetTickCount: DWORD;
function FormatMessage(dwFlags: DWORD; lpSource: IntPtr;
  dwMessageId: DWORD; dwLanguageId: DWORD;
  lpBuffer: StringBuilder; nSize: DWORD; Arguments: IntPtr): DWORD;
function  SysErrorMessage(ErrCode: Integer): String;

procedure EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection);
procedure LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection);
procedure InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection);
procedure DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection);

function  IntToStr(N : Integer) : String;
function  IntToHex(N : Integer; Digits : Integer = 0) : String;
function  Trim(const S: String): String;
function  LowerCase(const S: String): String;
function  UpperCase(const S: String): String;
function  CompareStr(const S1, S2: String): Integer;
function  CompareText(const S1, S2 : String) : Integer;
function  FileExists(const FileName: String): Boolean;

{$ENDIF}

{$IFDEF WIN32}
const
  {$EXTERNALSYM fmOpenRead}
  fmOpenRead       = SysUtils.fmOpenRead;
  {$EXTERNALSYM fmShareDenyWrite}
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  {$EXTERNALSYM faAnyFile}
  faAnyFile        = SysUtils.faAnyFile;
  {$EXTERNALSYM faDirectory}
  faDirectory      = SysUtils.faDirectory;
//  {$EXTERNALSYM faVolumeID}
//  faVolumeID       = SysUtils.faVolumeID;  //deprecated;  // not used in Win32
  {$EXTERNALSYM faReadOnly}
  faReadOnly       = SysUtils.faReadOnly;
  {$EXTERNALSYM faSysFile}
  faSysFile        = SysUtils.faSysFile;
  {$EXTERNALSYM faHidden}
  faHidden         = SysUtils.faHidden;
  {$EXTERNALSYM rfReplaceAll}
  rfReplaceAll     = SysUtils.rfReplaceAll;
  {$EXTERNALSYM MinDateTime}
  MinDateTime      : TDateTime = -657434.0;
  {$EXTERNALSYM MaxDateTime}
  MaxDateTime      : TDateTime = 2958465.99999;
  {$EXTERNALSYM opRemove}
  opRemove         = Classes.opRemove;
  {$EXTERNALSYM csDesigning}
  csDesigning      = Classes.csDesigning;
  {$EXTERNALSYM csDestroying}
  csDestroying     = Classes.csDestroying;
  {$EXTERNALSYM lnDeleted}
  lnDeleted        = Classes.lnDeleted;
  {$EXTERNALSYM WM_QUIT}
  WM_QUIT          = Messages.WM_QUIT;
  {$EXTERNALSYM WM_USER}
  WM_USER          = Messages.WM_USER;
  {$EXTERNALSYM WM_TIMER}
  WM_TIMER         = Messages.WM_TIMER;
  {$EXTERNALSYM PM_REMOVE}
  PM_REMOVE        = Windows.PM_REMOVE;
  {$EXTERNALSYM WS_EX_TOOLWINDOW}
  WS_EX_TOOLWINDOW = Windows.WS_EX_TOOLWINDOW;
  {$EXTERNALSYM WS_POPUP}
  WS_POPUP         = Windows.WS_POPUP;
  {$EXTERNALSYM TIME_ZONE_ID_DAYLIGHT}
  TIME_ZONE_ID_DAYLIGHT = Windows.TIME_ZONE_ID_DAYLIGHT;
  {$EXTERNALSYM TIME_ZONE_ID_STANDARD}
  TIME_ZONE_ID_STANDARD = Windows.TIME_ZONE_ID_STANDARD;
  {$EXTERNALSYM CP_ACP}
  CP_ACP           = Windows.CP_ACP;
  {$EXTERNALSYM CP_UTF8}
  CP_UTF8          = Windows.CP_UTF8;
  {$EXTERNALSYM CP_UTF7}
  CP_UTF7          = Windows.CP_UTF7;

{#$EXTERNALSYM SysErrorMessage}
function  _SysErrorMessage(ErrCode: Integer): String;
procedure _ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
{#$EXTERNALSYM BoolToStr}
function  _BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): String;
{#$EXTERNALSYM IntToStr}
function  _IntToStr(const N : Integer) : String; overload;
function  _IntToStr(const N : Int64) : String; overload;
{$IFDEF COMPILER12_UP}
function  _IntToStr(const N : Cardinal) : String; overload;
{$ENDIF}
function IcsIntToStrA(N : Integer): AnsiString;
function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString;
{#$EXTERNALSYM IntToHex}
function  _IntToHex(Value: Integer; Digits: Integer): String;
{#$EXTERNALSYM StrToInt}
function  _StrToInt(const S: String): Integer;
function  _StrToInt64(const S: String): Int64;

function  _StrPas(const P : PAnsiChar) : AnsiString; {$IFDEF COMPILER12_UP} overload;
function  _StrPas(const P : PWideChar) : UnicodeString; overload;
{$ENDIF}
{#$EXTERNALSYM StrPas}

function  _StrLen(const P : PAnsiChar) : Cardinal; {$IFDEF COMPILER12_UP} overload;
function  _StrLen(const P : PWideChar) : Cardinal; overload;
{$ENDIF}
{#$EXTERNALSYM StrLen}

function  _StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function  _StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; overload;
{$ENDIF}
{#$EXTERNALSYM StrCopy}

{#$EXTERNALSYM FloatToStr}
function  _FloatToStr(Value: Extended): String;

function _Trim(const Str : AnsiString) : AnsiString; {$IFDEF COMPILER12_UP} overload;
function _Trim(const Str : UnicodeString) : UnicodeString; overload;
{$ENDIF}
{#$EXTERNALSYM Trim}

function _StrLower(Str: PAnsiChar): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function _StrLower(Str: PWideChar): PWideChar; overload;
{$ENDIF}
{#$EXTERNALSYM StrLower}

function _StrIComp(const Str1, Str2: PAnsiChar): Integer; {$IFDEF COMPILER12_UP} overload;
function _StrIComp(const Str1, Str2: PWideChar): Integer; overload;
{$ENDIF}
{#$EXTERNALSYM StrIComp}

function  _StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function  _StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar; overload;
{$ENDIF}
{#$EXTERNALSYM StrPCopy}

function  _StrComp(const Str1, Str2: PAnsiChar): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrComp(const Str1, Str2: PWideChar): Integer; overload;
{$ENDIF}
{#$EXTERNALSYM StrComp}

function  _StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; overload;
{$ENDIF}
{#$EXTERNALSYM StrLComp}

function  _StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;  overload;
{$ENDIF}
{#$EXTERNALSYM StrLIComp}

function _StrToDateTime(const S: String): TDateTime; overload;
function _StrToDateTime(const S: String; const FormatSettings: TFormatSettings): TDateTime; overload;
{#$EXTERNALSYM StrToDateTime}

function  _LowerCase(const S: AnsiString): AnsiString; {$IFDEF COMPILER12_UP} overload;
function  _LowerCase(const S: UnicodeString): UnicodeString; overload;
{$ENDIF}
{#$EXTERNALSYM LowerCase}

function  _UpperCase(const S: AnsiString): AnsiString; {$IFDEF COMPILER12_UP} overload;
function  _UpperCase(const S: UnicodeString): UnicodeString; overload;
{$ENDIF}
{#$EXTERNALSYM UpperCase}
function IcsUpperCaseA(const S: AnsiString): AnsiString;
function IcsLowerCaseA(const S: AnsiString): AnsiString;
function IcsCompareTextA(const S1, S2: AnsiString): Integer;
function IcsTrimA(const Str: AnsiString): AnsiString;
function IcsSameTextA(const S1, S2: AnsiString): Boolean;

{#$EXTERNALSYM CompareStr}
function  _CompareStr(const S1, S2: AnsiString): Integer; {$IFDEF COMPILER12_UP} overload;
function  _CompareStr(const S1, S2: UnicodeString): Integer; overload;
{$ENDIF}
{#$EXTERNALSYM CompareText}

function  _CompareText(const S1, S2: AnsiString): Integer;{$IFDEF COMPILER12_UP} overload;
function  _CompareText(const S1, S2: UnicodeString): Integer; overload;
{$ENDIF}

function _AnsiStrComp(S1, S2: PAnsiChar): Integer;
{#$EXTERNALSYM FileExists}
function  _FileExists(const FileName: String): Boolean;
{#$EXTERNALSYM DeleteFile}
function  _DeleteFile(const FileName: String): Boolean;
{#$EXTERNALSYM ExtractFileExt}
function  _ExtractFileExt(const FileName: String): String;
{#$EXTERNALSYM ExtractFilePath}
function  _ExtractFilePath(const FileName: String): String;
{#$EXTERNALSYM DateTimeToStr}
function  _DateTimeToStr(const DateTime: TDateTime): String;
{#$EXTERNALSYM DecodeDate}
procedure _DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
{#$EXTERNALSYM DecodeTime}
procedure _DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
{#$EXTERNALSYM EncodeTime}
function  _EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
{#$EXTERNALSYM DirectoryExists}
function  _DirectoryExists(const Name: String): Boolean;
{#$EXTERNALSYM ExcludeTrailingPathDelimiter}
function  _ExcludeTrailingPathDelimiter(const S: String): String;
{#$EXTERNALSYM IncludeTrailingPathDelimiter}
function  _IncludeTrailingPathDelimiter(const S: String): String;
{#$EXTERNALSYM Format}
function  _Format(const Fmt: String; const Args: array of const): String;
{#$EXTERNALSYM FindFirst}
function  _FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer;
{#$EXTERNALSYM FindNext}
function  _FindNext(var F: TSearchRec): Integer;
{#$EXTERNALSYM FindClose}
procedure _FindClose(var F: TSearchRec);
{#$EXTERNALSYM FileDateToDateTime}
function  _FileDateToDateTime(FileDate: Integer): TDateTime;
{#$EXTERNALSYM FreeAndNil}
procedure _FreeAndNil(var Obj);
{#$EXTERNALSYM Date}
function  _Date : TDateTime;
{#$EXTERNALSYM Now}
function  _Now: TDateTime;
{#$EXTERNALSYM StringReplace}
function  _StringReplace(const S: String; const OldPattern: String;
    const NewPattern: String; Flags: TReplaceFlags): String;
{#$EXTERNALSYM GetTimeZoneInformation}
function  _GetTimeZoneInformation(var lpTimeZoneInformation: TTimeZoneInformation): DWORD; stdcall;
{#$EXTERNALSYM GetWindowLong}
function  _GetWindowLong(H: HWND; nIndex: Integer): Longint; stdcall;
{#$EXTERNALSYM DefWindowProc}
function  _DefWindowProc(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT; stdcall;
{#$EXTERNALSYM GetCurrentThreadId}
function  _GetCurrentThreadId: DWORD; stdcall;
{#$EXTERNALSYM GetMessage}
function  _GetMessage(var lpMsg: TMsg; H: HWND;
                     wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
{#$EXTERNALSYM TranslateMessage}
function  _TranslateMessage(const lpMsg: TMsg): BOOL; stdcall;
{#$EXTERNALSYM DispatchMessage}
function  _DispatchMessage(const lpMsg: TMsg): LongInt; stdcall;
{#$EXTERNALSYM PeekMessage}
function  _PeekMessage(var lpMsg: TMsg; H: HWND;
               wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
{#$EXTERNALSYM PostMessage}
function  _PostMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): BOOL; stdcall;
{#$EXTERNALSYM EnterCriticalSection}
procedure _EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
{#$EXTERNALSYM LeaveCriticalSection}
procedure _LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
{#$EXTERNALSYM InitializeCriticalSection}
procedure _InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
{#$EXTERNALSYM DeleteCriticalSection}
procedure _DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
{#$EXTERNALSYM GetClassInfo}
function  _GetClassInfo(hInstance: HINST; lpClassName: PChar;
                       var lpWndClass: TWndClass): BOOL; stdcall;
{#$EXTERNALSYM RegisterClass}
function _RegisterClass(const lpWndClass: TWndClass): ATOM; stdcall;
{#$EXTERNALSYM UnregisterClass}
function _UnregisterClass(lpClassName: PChar; hInstance: HINST): BOOL; stdcall;
{#$EXTERNALSYM CreateWindowEx}
function _CreateWindowEx(dwExStyle: DWORD; lpClassName: PChar;
  lpWindowName: PChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; h_Menu: HMENU; hInstance: HINST; lpParam: Pointer): HWND;
{#$EXTERNALSYM DestroyWindow}
function _DestroyWindow(H: HWND): BOOL; stdcall;
{#$EXTERNALSYM SetWindowLong}
function _SetWindowLong(H: HWND; nIndex: Integer; dwNewLong: Longint): Longint; stdcall;
{#$EXTERNALSYM WM_USER}
function _LoadLibrary(lpLibFileName: PChar): HMODULE; stdcall;
{#$EXTERNALSYM LoadLibrary}
function _FreeLibrary(hLibModule: HMODULE): BOOL; stdcall;
{#$EXTERNALSYM GetProcAddress}
function _GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
{#$EXTERNALSYM GetTickCount}
function _GetTickCount: DWORD; stdcall;
{#$EXTERNALSYM Sleep}
procedure _Sleep(dwMilliseconds: DWORD); stdcall;
{#$EXTERNALSYM Sleep}
function _GetACP: Cardinal; stdcall;

{$IFNDEF NOFORMS}
{$EXTERNALSYM Application}
function Application : TApplication;
{$ENDIF}
{$ENDIF WIN32}

{$EXTERNALSYM MakeWord}
function MakeWord(a, b: Byte): Word;
{$EXTERNALSYM MakeLong}
function MakeLong(a, b: Word): Longint;
{$EXTERNALSYM HiWord}
function HiWord(L: DWORD): Word;

implementation

{$IFDEF CLR}
const
    user32   = 'user32.dll';
    kernel32 = 'kernel32.dll';

[DllImport(user32, CharSet = CharSet.Auto, SetLastError = False, EntryPoint = 'DefWindowProc')]
function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'SetWindowLong')]
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: IntPtr): IntPtr; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetWindowLong')]
function GetWindowLongIntPtr(hWnd: HWND; nIndex: Integer): IntPtr; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetMessage')]
function GetMessage; external;
[DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'TranslateMessage')]
function TranslateMessage; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'DispatchMessage')]
function DispatchMessage; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'PeekMessage')]
function PeekMessage; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'PostMessage')]
function PostMessage; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'RegisterClass')]
function RegisterClass(const lpWndClass: TWndClass): ATOM; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'RegisterClass')]
function RegisterClass(const lpWndClassInfo: TWndClassInfo): ATOM; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'UnregisterClass')]
function UnregisterClass; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'GetClassInfo')]
function GetClassInfo; external;
[DllImport(user32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'CreateWindowEx')]
function CreateWindowEx(dwExStyle: DWORD; lpClassName: String;
  lpWindowName: String; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; hMenu: HMENU; hInstance: HINST; lpParam: IntPtr): HWND; external;
[DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'DestroyWindow')]
function DestroyWindow; external;
[DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'SetTimer')]
function SetTimer; external;
[DllImport(user32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'KillTimer')]
function KillTimer; external;

[DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'GetTickCount')]
function GetTickCount; external;
[DllImport(kernel32, CharSet = CharSet.Ansi, SetLastError = True, EntryPoint = 'Sleep')]
procedure Sleep; external;
[DllImport(kernel32, CharSet = CharSet.Auto, SetLastError = True, EntryPoint = 'FormatMessage')]
function FormatMessage; external;

const
  FORMAT_MESSAGE_ALLOCATE_BUFFER = $100;
  FORMAT_MESSAGE_IGNORE_INSERTS  = $200;
  FORMAT_MESSAGE_FROM_STRING     = $400;
  FORMAT_MESSAGE_FROM_HMODULE    = $800;
  FORMAT_MESSAGE_FROM_SYSTEM     = $1000;
  FORMAT_MESSAGE_ARGUMENT_ARRAY  = $2000;
  FORMAT_MESSAGE_MAX_WIDTH_MASK  = 255;

function SysErrorMessage(ErrCode: Integer): String;
var
    Buffer  : StringBuilder;
    BufSize : Integer;
    Len     : Integer;
begin
    BufSize := 256;
    Buffer  := StringBuilder.Create(BufSize);
    Len     := FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
                             FORMAT_MESSAGE_IGNORE_INSERTS or
                             FORMAT_MESSAGE_ARGUMENT_ARRAY,
                             nil, ErrCode, 0, Buffer, BufSize, nil);
  while (Len > 0) and
        (AnsiChar(Buffer[Len - 1]) in [#0..#32, '.']) do   
      Dec(Len);
  Buffer.Length := Len;
  Result        := Buffer.ToString;
end;

function  GetCurrentThreadId: DWORD;
begin
    Result := Thread.CurrentThread.GetHashCode;
end;

function HInstance: HINST;
begin
    Result := HINST(Integer(Marshal.GetHInstance(Assembly.GetCallingAssembly.GetModules[0])));
end;

procedure EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Monitor.Enter(lpCriticalSection);
end;

procedure LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Monitor.Exit(lpCriticalSection);
end;

procedure InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    lpCriticalSection := TRTLCriticalSection.Create;
end;

procedure DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    lpCriticalSection.Free;
end;

function TList.GetItems(Index: Integer): TObject;
begin
    Result := Item[Index];
end;

procedure TList.SetItems(Index: Integer; const Value: TObject);
begin
    Item[Index] := Value;
end;

procedure TList.Delete(Index : Integer);
begin
    RemoveAt(Index);
end;

function TList.First: TObject;
begin
    Result := Item[0];
end;

function TList.Last: TObject;
begin
    Result := Item[Count - 1];
end;

procedure TList.Pack;
begin

end;

constructor TStringList.Create;
begin
    inherited Create;
    FStrings := TList.Create;
end;

procedure TStringList.Clear;
begin
    FStrings.Clear;
end;

procedure TStringList.Add(const S: String);
begin
    FStrings.Add(S);
end;

function TStringList.Count : Integer;
begin
    Result := FStrings.Count;
end;

procedure TStringList.SetStrings(nIndex: Integer; const Value: String);
begin
    FStrings.Items[nIndex] := Value;
end;

function TStringList.GetStrings(nIndex: Integer): String;
begin
    Result := String(FStrings.Items[nIndex]);
end;

{$IFNDEF VCL}
constructor TFileStream.Create(const AFileName: String; Mode: Integer);
var
    LMode   : System.IO.FileMode;
    LAccess : System.IO.FileAccess;
    LShare  : System.IO.FileShare;
begin
    inherited Create;
    if Mode = fmCreate then begin
        LMode   := System.IO.FileMode.Create;
        LAccess := System.IO.FileAccess.ReadWrite;
    end
    else begin
        LMode := System.IO.FileMode.Open;
        case Mode and $F of
        fmOpenReadWrite: LAccess := System.IO.FileAccess.ReadWrite;
        fmOpenWrite:     LAccess := System.IO.FileAccess.Write;
        else
            LAccess := System.IO.FileAccess.Read;
        end;
    end;
    case Mode and $F0 of
    fmShareDenyWrite: LShare := System.IO.FileShare.Read;
    fmShareDenyRead:  LShare := System.IO.FileShare.Write;
    fmShareDenyNone:  LShare := System.IO.FileShare.None;
    else
        LShare := System.IO.FileShare.ReadWrite;
    end;
    FClrStream := System.IO.FileStream.Create(AFileName, LMode, LAccess, LShare);
    FFileName  := AFileName;
end;

function TFileStream.GetSize: Int64;
begin
    Result := FClrStream.Length;
end;

procedure TFileStream.SetSize(Value: Int64);
begin
    FClrStream.SetLength(Value);
end;

procedure TFileStream.Seek(Offset : Int64; Origin: Integer);
var
    so : System.IO.SeekOrigin;
begin
    case Origin of
    soFromBeginning : so := System.IO.SeekOrigin.Begin;
    soFromCurrent   : so := System.IO.SeekOrigin.Current;
    soFromEnd       : so := System.IO.SeekOrigin.End;
    else
                      so := System.IO.SeekOrigin.Begin;
    end;
    FClrStream.Seek(Offset, so);
end;

procedure TFileStream.Write(Ch: Char);
begin
    FClrStream.WriteByte(Byte(Ch));
end;
{$ENDIF}

function IntToStr(N : Integer) : String;
begin
    Result := N.ToString;
end;

function IntToHex(N : Integer; Digits : Integer) : String;
begin
    if Digits = 0 then
        Result := System.String.Format('{0:X}', System.Object(N))
    else
        Result := System.String.Format('{0:X' + Digits.ToString() + '}',
                                       System.Object(N));
end;

function Trim(const S: String): String;
begin
    if S = '' then
        Result := ''
    else
        Result := S.Trim;
end;

function LowerCase(const S: String): String;
begin
    Result := S.ToLower;
end;

function UpperCase(const S: String): String;
begin
    Result := S.ToUpper;
end;

function CompareText(const S1, S2 : String) : Integer;
begin
    if S1.ToUpper = S2.ToUpper then
        Result := 0
    else if S1.ToUpper < S2.ToUpper then
        Result := -1
    else
        Result := 1;
end;

function CompareStr(const S1, S2 : String) : Integer;
begin
    Result := System.String.Compare(S1, S2);
end;


function FileExists(const FileName: String): Boolean;
begin
    Result := System.IO.File.Exists(FileName);
end;

{$ENDIF}

{$IFDEF WIN32}
function _SysErrorMessage(ErrCode: Integer): String;
begin
    Result := SysUtils.SysErrorMessage(ErrCode);
end;

procedure _ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

function _BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): String;
begin
    Result := SysUtils.BoolToStr(B, UseBoolStrs);
end;

function _IntToStr(const N : Integer) : String;
begin
    Result := SysUtils.IntToStr(N);
end;

function _IntToStr(const N : Int64) : String;
begin
    Result := SysUtils.IntToStr(N);
end;
{$IFDEF COMPILER12_UP}
function _IntToStr(const N : Cardinal) : String;
begin
    Result := SysUtils.IntToStr(N);
end;
{$ENDIF}

{ Author Arno Garrels - Needs optimization!      }
{ It's a bit slower that the RTL routine.        }
{ We should realy use a FastCode function here.  }
function IntToStrA(N : Integer) : AnsiString;
var
    I : Integer;
    Buf : array [0..11] of AnsiChar;
    Sign : Boolean;
begin
    if N >= 0 then
        Sign := FALSE
    else begin
        Sign := TRUE;
        if N = Low(Integer) then
        begin
            Result := '-2147483648';
            Exit;
        end
        else
            N := Abs(N);
    end;
    I := Length(Buf);
    repeat
        Dec(I);
        Buf[I] := AnsiChar(N mod 10 + $30);
        N := N div 10;
    until N = 0;
    if Sign then begin
        Dec(I);
        Buf[I] := '-';
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;

function IcsIntToStrA(N : Integer) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToStrA(N);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToStr(N);
{$ELSE}
    Result := IntToStrA(N);
{$ENDIF}
{$ENDIF}
end;

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IntToHexA(N : Integer; Digits: Byte) : AnsiString;
var
    Buf : array [0..7] of Byte;
    V : Cardinal;
    I : Integer;
begin
    V := Cardinal(N);
    I := Length(Buf);
    if Digits > I then Digits := I;
    repeat
        Dec(I);
        Buf[I] := V mod 16;
        if Buf[I] < 10 then
            Inc(Buf[I], $30)
        else
            Inc(Buf[I], $37);
        V := V div 16;
    until V = 0;
    while Digits > Length(Buf) - I do begin
       Dec(I);
       Buf[I] := $30;
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;

function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToHexA(N, Digits);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToHex(N, Digits);
{$ELSE}
    Result := IntToHexA(N, Digits);
{$ENDIF}
{$ENDIF}
end;

function _StrToInt(const S: String): Integer;
begin
    Result := SysUtils.StrToInt(S);
end;

function _StrToInt64(const S: String): Int64;
begin
    Result := SysUtils.StrToInt64(S);
end;

function _StrPas(const P : PAnsiChar) : AnsiString; // Unicode change
begin
    Result := SysUtils.StrPas(P);
end;

{$IFDEF COMPILER12_UP}
function _StrPas(const P : PWideChar): UnicodeString; // Unicode change
begin
    Result := SysUtils.StrPas(P);
end;
{$ENDIF}

function _StrLen(const P : PAnsiChar) : Cardinal;  // Unicode change
begin
    Result := SysUtils.StrLen(P);
end;

{$IFDEF COMPILER12_UP}
function _StrLen(const P : PWideChar) : Cardinal; // Unicode change
begin
    Result := SysUtils.StrLen(P);
end;
{$ENDIF}

function _StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; // Unicode change
begin
    Result := SysUtils.StrCopy(Dest, Source);
end;

{$IFDEF COMPILER12_UP}
function  _StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; // Unicode change
begin
    Result := SysUtils.StrCopy(Dest, Source);
end;
{$ENDIF}

{$IFDEF COMPILER12_UP}
function _TrimLeft(const Str: AnsiString): AnsiString;
var
    I, L : Integer;
begin
    L := Length(Str);
    I := 1;
    while (I <= L) and (Str[I] <= ' ') do
        Inc(I);
    Result := Copy(Str, I, Maxint);
end;

function _TrimRight(const Str: AnsiString): AnsiString;
var
    I : Integer;
begin
    I := Length(Str);
    while (I > 0) and (Str[I] <= ' ') do
        Dec(I);
    Result := Copy(Str, 1, I);
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's a bit faster than the RTL routine.                     }
function IcsTrimA(const Str: AnsiString): AnsiString;
var
    I, L : Integer;
begin
    L := Length(Str);
    I := 1;
    while (I <= L) and (Str[I] <= ' ') do
        Inc(I);
    if I > L then
        Result := ''
    else begin
        while Str[L] <= ' ' do
            Dec(L);
        SetLength(Result, L - I + 1);
        Move(Str[I], Pointer(Result)^, L - I + 1);
    end;
end;

function _Trim(const Str: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsTrimA(Str);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.Trim(Str);
{$ELSE}
    Result := IcsTrimA(Str);
{$ENDIF}
{$ENDIF}
end;


{$IFDEF COMPILER12_UP}
function _Trim(const Str : UnicodeString) : UnicodeString;
begin
    Result := SysUtils.Trim(Str);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function _StrLower(Str: PAnsiChar): PAnsiChar;
begin
    Result := SysUtils.StrLower(Str);
end;

{$IFDEF COMPILER12_UP}
function _StrLower(Str: PWideChar): PWideChar;
begin
    Result := SysUtils.StrLower(Str);
end;
{$ENDIF}

function _StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; // Unicode change
begin
    Result := SysUtils.StrPCopy(Dest, Source);
end;

{$IFDEF COMPILER12_UP}
function  _StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar;
begin
    Result := SysUtils.StrPCopy(Dest, Source);
end;
{$ENDIF}

function _StrComp(const Str1, Str2 : PAnsiChar): Integer;
begin
    Result := SysUtils.StrComp(Str1, Str2);
end;

{$IFDEF COMPILER12_UP}
function _StrComp(const Str1, Str2 : PWideChar): Integer;
begin
    Result := SysUtils.StrComp(Str1, Str2);
end;
{$ENDIF}

function _StrIComp(const Str1, Str2: PAnsiChar): Integer;
begin
    Result := SysUtils.StrIComp(Str1, Str2);
end;

{$IFDEF COMPILER12_UP}
function _StrIComp(const Str1, Str2: PWideChar): Integer;
begin
    Result := SysUtils.StrIComp(Str1, Str2);
end;
{$ENDIF}

function _StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := SysUtils.StrLComp(Str1, Str2, MaxLen);
end;

{$IFDEF COMPILER12_UP}
function  _StrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
(*
{ Currently buggy so use our own implementation // fixed in build 3047}
    function Min(I1, I2: Cardinal): Cardinal; inline;
    begin
        if I1 < I2 then
            Result := I1
        else
            Result := I2;
    end;
*)
begin
    Result :=  SysUtils.StrLComp(Str1, Str2, MaxLen);
  {  Result := CompareStringW(LOCALE_USER_DEFAULT,
                            NORM_IGNOREWIDTH,
                            Str1, Min(MaxLen, SysUtils.StrLen(Str1)),
                            Str2, Min(MaxLen, SysUtils.StrLen(Str2))) - 2; }
end;
{$ENDIF}

function _StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := SysUtils.StrLIComp(Str1, Str2, MaxLen);
end;

{$IFDEF COMPILER12_UP}
function _StrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
(*
{ Currently buggy so use our own implementation } // fixed in build 3047}
    function Min(I1, I2: Cardinal): Cardinal; inline;
    begin
        if I1 < I2 then
            Result := I1
        else
            Result := I2;
    end;
*)
begin
    Result :=  SysUtils.StrLIComp(Str1, Str2, MaxLen);
    { Result := CompareStringW(LOCALE_USER_DEFAULT,
                            NORM_IGNOREWIDTH or NORM_IGNORECASE,
                            Str1, Min(MaxLen, SysUtils.StrLen(Str1)),
                            Str2, Min(MaxLen, SysUtils.StrLen(Str2))) - 2;}
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsLowerCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I  : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['A'..'Z'] then Inc(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;

function _LowerCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsLowerCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.LowerCase(S);
{$ELSE}
    Result := IcsLowerCaseA(S);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _LowerCase(const S: UnicodeString): UnicodeString;
begin
    Result := SysUtils.LowerCase(S);
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsUpperCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['a'..'z'] then Dec(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;

function _UpperCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsUpperCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.UpperCase(S);
{$ELSE}
    Result := IcsUpperCaseA(S);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _UpperCase(const S: UnicodeString): UnicodeString;
begin
    Result := SysUtils.UpperCase(S);
end;
{$ENDIF}

function IcsSameTextA(const S1, S2: AnsiString): Boolean;
begin
    Result := (IcsCompareTextA(S1, S2) = 0);
end;

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsCompareTextA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen : Integer;
    Ch1, Ch2 : AnsiChar;
    P1, P2 : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 1 to MinLen do
    begin
        Ch1 := P1[I];
        Ch2 := P2[I];
        if (Ch1 <> Ch2) then
        begin
            { Strange, but this is how the original works, }
            { for instance, "a" is smaller than "[" .      }
            if (Ch1 > Ch2) then
            begin
                if Ch1 in ['a'..'z'] then
                    Dec(Byte(Ch1), 32);
            end
            else begin
                if Ch2 in ['a'..'z'] then
                    Dec(Byte(Ch2), 32);
            end;
        end;
        if (Ch1 <> Ch2) then
        begin
            Result := Byte(Ch1) - Byte(Ch2);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;

function _CompareText(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareTextA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareText(S1, S2);
{$ELSE}
    Result := IcsCompareTextA(S1, S2);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _CompareText(const S1, S2: UnicodeString): Integer;
begin
    Result := SysUtils.CompareText(S1, S2);
end;
{$ENDIF}

function _AnsiStrComp(S1, S2: PAnsiChar): Integer;
begin
    Result := SysUtils.AnsiStrComp(S1, S2);
end;

function IcsCompareStrA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen    : Integer;
    P1, P2    : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 1 to MinLen do
    begin
        if (P1[I] <> P2[I]) then
        begin
            Result := Ord(P1[I]) - Ord(P2[I]);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;

function _CompareStr(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareStrA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareStr(S1, S2);
{$ELSE}
    Result := IcsCompareStrA(S1, S2);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _CompareStr(const S1, S2: UnicodeString): Integer;
begin
    Result := SysUtils.CompareStr(S1, S2);
end;
{$ENDIF}

function _StringReplace(const S: String; const OldPattern: String;
    const NewPattern: String; Flags: TReplaceFlags): String;
begin
    Result := SysUtils.StringReplace(S, OldPattern, NewPattern, Flags);
end;

function _GetTimeZoneInformation(var lpTimeZoneInformation: TTimeZoneInformation): DWORD; stdcall;
begin
     Result := Windows.GetTimeZoneInformation(lpTimeZoneInformation);
end;

function _FileExists(const FileName: String): Boolean;
begin
    Result := SysUtils.FileExists(FileName);
end;

procedure _FreeAndNil(var Obj);
begin
    SysUtils.FreeAndNil(Obj);
end;

function _DeleteFile(const FileName: String): Boolean;
begin
    Result := SysUtils.DeleteFile(FileName);
end;

function _ExtractFileExt(const FileName: String): String;
begin
    Result := SysUtils.ExtractFileExt(FileName);
end;

function _DateTimeToStr(const DateTime: TDateTime): String;
begin
    Result := SysUtils.DateTimeToStr(DateTime);
end;

procedure _DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
    SysUtils.DecodeDate(Date, Year, Month, Day);
end;

procedure _DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
    SysUtils.DecodeTime(Time, Hour, Min, Sec, MSec);
end;

function _EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
    Result := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

function _StrToDateTime(const S: String): TDateTime;
begin
    Result := SysUtils.StrToDateTime(S);
end;

function _StrToDateTime(const S: String; const FormatSettings: TFormatSettings): TDateTime;
begin
    Result := SysUtils.StrToDateTime(S, FormatSettings);
end;

function _DirectoryExists(const Name: String): Boolean;
begin
    Result := SysUtils.DirectoryExists(Name);
end;

function _IncludeTrailingPathDelimiter(const S: String): String;
begin
    Result := SysUtils.IncludeTrailingPathDelimiter(S);
end;

function _ExcludeTrailingPathDelimiter(const S: String): String;
begin
    Result := SysUtils.ExcludeTrailingPathDelimiter(S);
end;

function _Now: TDateTime;
begin
    Result := SysUtils.Now;
end;

function _Format(const Fmt: String; const Args: array of const): String;
begin
    Result := SysUtils.Format(Fmt, Args);
end;

function _FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer;
begin
    Result := SysUtils.FindFirst(Path, Attr, F);
end;

function _FindNext(var F: TSearchRec): Integer;
begin
    Result := SysUtils.FindNext(F);
end;

procedure _FindClose(var F: TSearchRec);
begin
    SysUtils.FindClose(F);
end;

function _FileDateToDateTime(FileDate: Integer): TDateTime;
begin
    Result := SysUtils.FileDateToDateTime(FileDate);
end;

function _ExtractFilePath(const FileName: String): String;
begin
    Result := SysUtils.ExtractFilePath(FileName);
end;

function _Date : TDateTime;
begin
    Result := SysUtils.Date;
end;

function _IntToHex(Value: Integer; Digits: Integer): String;
begin
    Result := SysUtils.IntToHex(Value, Digits);
end;

function _FloatToStr(Value: Extended): String;
begin
    Result := SysUtils.FloatToStr(Value);
end;

function _GetWindowLong(H: HWND; nIndex: Integer): Longint;
begin
    Result := Windows.GetWindowLong(H, nIndex);
end;

function _DefWindowProc(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT;
begin
    Result := Windows.DefWindowProc(H, Msg, ParamW, ParamL);
end;

function _GetCurrentThreadId: DWORD;
begin
    Result := Windows.GetCurrentThreadId;
end;

function _GetMessage(
    var lpMsg: TMsg; H: HWND;
    wMsgFilterMin, wMsgFilterMax: UINT): BOOL;
begin
    Result := Windows.GetMessage(lpMsg, H, wMsgFilterMin, wMsgFilterMax);
end;

function _TranslateMessage(const lpMsg: TMsg): BOOL;
begin
    Result := Windows.TranslateMessage(lpMsg);
end;

function _DispatchMessage(const lpMsg: TMsg): LongInt;
begin
    Result := Windows.DispatchMessage(lpMsg);
end;

function _PeekMessage(
    var lpMsg: TMsg; H: HWND;
    wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL;
begin
    Result := Windows.PeekMessage(lpMsg, H,
                                  wMsgFilterMin, wMsgFilterMax, wRemoveMsg);
end;

function _PostMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): BOOL;
begin
    Result := Windows.PostMessage(H, Msg, ParamW, ParamL);
end;

procedure _EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.EnterCriticalSection(lpCriticalSection);
end;

procedure _LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.LeaveCriticalSection(lpCriticalSection);
end;

procedure _InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.InitializeCriticalSection(lpCriticalSection);
end;

procedure _DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.DeleteCriticalSection(lpCriticalSection);
end;

function _GetClassInfo(
    hInstance: HINST; lpClassName: PChar;
    var lpWndClass: TWndClass): BOOL;
begin
    Result := Windows.GetClassInfo(hInstance, lpClassName, lpWndClass);
end;

function _RegisterClass(const lpWndClass: TWndClass): ATOM; stdcall;
begin
    Result := Windows.RegisterClass(lpWndClass);
end;

function _CreateWindowEx(dwExStyle: DWORD; lpClassName: PChar;
  lpWindowName: PChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; h_Menu: HMENU; hInstance: HINST; lpParam: Pointer): HWND;
begin
    Result := Windows.CreateWindowEx(dwExStyle, lpClassName,
  lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, h_Menu, hInstance,
  lpParam);
end;

function _DestroyWindow(H: HWND): BOOL;
begin
    Result := Windows.DestroyWindow(H);
end;

function _SetWindowLong(H: HWND; nIndex: Integer; dwNewLong: Longint): LongInt;
begin
    Result := Windows.SetWindowLong(H, nIndex, dwNewLong);
end;

function _UnregisterClass(lpClassName: PChar; hInstance: HINST): BOOL;
begin
    Result := Windows.UnregisterClass(lpClassName, hInstance);
end;

function _FreeLibrary(hLibModule: HMODULE): BOOL;
begin
    Result := Windows.FreeLibrary(hLibModule);
end;

function _LoadLibrary(lpLibFileName: PChar): HMODULE;
begin
    Result := Windows.LoadLibrary(lpLibFileName);
end;

function _GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC;
begin
    Result := Windows.GetProcAddress(hModule, lpProcName);
end;

function _GetTickCount: DWORD;
begin
    Result := Windows.GetTickCount;
end;

procedure _Sleep(dwMilliseconds: DWORD);
begin
    Windows.Sleep(dwMilliseconds);
end;

function _GetACP: Cardinal; stdcall;
begin
    Result := Windows.GetACP;
end;

{$IFNDEF NOFORMS}
function Application : TApplication;
begin
    Result := Forms.Application;
end;
{$ENDIF}
{$ENDIF WIN32}

function MakeWord(a, b: Byte): Word;
begin
    Result := A or B shl 8;
end;

function MakeLong(a, b: Word): Longint;
begin
    Result := A or B shl 16;
end;

function HiWord(L: DWORD): Word;
begin
  Result := L shr 16;
end;

end.
