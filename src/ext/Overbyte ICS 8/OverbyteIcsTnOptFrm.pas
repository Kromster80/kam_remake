{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  User interface for TnEmulVT component options
Creation:     May, 1996
Version:      8.02
Author:       François PIETTE
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org See website for details.
Legal issues: Copyright (C) 1996-2010 by François PIETTE
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

History:
Mar 18, 1999  V1.01 Removed FormPos dependency
Mar 26, 2006  V6.00 started from previous version
Mar 24, 2008  V6.01 Francois Piette made some changes to prepare code
              for Unicode.
May 11, 2008  V6.02 USchuster removed local atoi implementation (atoi is now in OverbyteIcsUtils.pas)
Sept 4, 2008  V7.00 Angus don't save form size since it's fixed, font now Sans Serif
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 11, 2013  V8.01 Angus changed font to Courer New 8 from System and ensure only fixed
                      fonts allowed, since character spacing otherwise set to width of widest
                      character in a proportional spaced font and looks horrible
                    Changed form to Arial 8 which looks more modern
May 28, 2014  v8.02 DrJohn fixed problem with (border) colours
                    AutoResize property added with improved font resizing
                    SoundOn property added
                    GetScreenText function added

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTnOptFrm;

interface

{$R-}           { Range Check off                     }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
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

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
{$IFDEF USEWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Graphics{$ELSE}Graphics{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Controls{$ELSE}Controls{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Dialogs{$ELSE}Dialogs{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Forms{$ELSE}Forms{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.StdCtrls{$ELSE}StdCtrls{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Vcl.Buttons{$ELSE}Buttons{$ENDIF},
    OverbyteIcsUtils,
    OverbyteIcsEmulVT;                                      {drjohn}

const
  TnOptFrmVersion      = 802;
  CopyRight : String = ' TnOptFrm (c) 1996-2013 F. Piette V8.02 ';

type
  TOptForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    AutoCRCheckBox: TCheckBox;
    AutoLFCheckBox: TCheckBox;
    LocalEchoCheckBox: TCheckBox;
    MonoChromeCheckBox: TCheckBox;
    RowsEdit: TEdit;
    ColsEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    LaboButton: TButton;
    RDVButton: TButton;
    USUSButton: TButton;
    XlatCheckBox: TCheckBox;
    FontDialog1: TFontDialog;
    FontButton: TButton;
    LineHeightEdit: TEdit;
    Label3: TLabel;
    NamesButton: TButton;
    UpperLockCheckBox: TCheckBox;
    A11Button: TButton;
    GroupBox1: TGroupBox;
    FKeys1RadioButton: TRadioButton;
    FKeys2RadioButton: TRadioButton;
    FKeys3RadioButton: TRadioButton;
    Label4: TLabel;
    Label5: TLabel;
    LineZoomEdit: TEdit;
    CharZoomEdit: TEdit;
    GraphicDrawCheckBox: TCheckBox;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LaboButtonClick(Sender: TObject);
    procedure RDVButtonClick(Sender: TObject);
    procedure USUSButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure NamesButtonClick(Sender: TObject);
    procedure A11ButtonClick(Sender: TObject);
  public
    FIniFilename  : String;
    FInitialized  : Boolean;
    FSectionName  : String;
    FKeyName      : String;
    FHostName     : String;
    FFont         : TNFont;                                  {drjohn}
    FOnNamesClick : TNotifyEvent;
    function  GetLocalEcho   : Boolean;
    function  GetAutoCr      : Boolean;
    function  GetAutoLF      : Boolean;
    function  GetAltKeys     : Boolean;
    function  GetMonoChrome  : Boolean;
    function  GetUpperLock   : Boolean;
    function  GetXlat        : Boolean;
    function  GetGraphicDraw : Boolean;
    function  GetRows        : Integer;
    function  GetCols        : integer;
    function  GetLineHeight  : Single;              {drjohn}
    function  GetLineZoom    : Single;
    function  GetCharZoom    : Single;
    function  GetFKeys       : Integer;
    procedure SetLocalEcho(Value : Boolean);
    procedure SetAutoCr(Value : Boolean);
    procedure SetAutoLF(Value : Boolean);
    procedure SetAltKeys(Value : Boolean);
    procedure SetMonoChrome(Value : Boolean);
    procedure SetUpperLock(Value : Boolean);
    procedure SetXlat(Value : Boolean);
    procedure SetGraphicDraw(Value : Boolean);
    procedure SetRows(Value : Integer);
    procedure SetCols(Value : Integer);
    procedure SetHostName(Value : String);
    procedure SetLineHeight(Value : Single);       {drjohn}
    procedure SetLineZoom(Value : Single);
    procedure SetCharZoom(Value : Single);
    procedure SetFKeys(Value : Integer);

    property IniFilename : String  read FIniFileName   write FIniFileName;
    property SectionName : String  read FSectionName   write FSectionName;
    property KeyName     : String  read FKeyName       write FKeyName;
    property HostName    : String  read FHostName      write SetHostName;
    property LocalEcho   : Boolean read GetLocalEcho   write SetLocalEcho;
    property AutoCR      : Boolean read GetAutoCr      write SetAutoCR;
    property AutoLF      : Boolean read GetAutoLF      write SetAutoLF;
    property AltKeys     : Boolean read GetAltKeys     write SetAltKeys;
    property FKeys       : Integer read GetFKeys       write SetFkeys;
    property MonoChrome  : Boolean read GetMonoChrome  write SetMonoChrome;
    property UpperLock   : Boolean read GetUpperLock   write SetUpperLock;
    property Xlat        : Boolean read GetXlat        write SetXlat;
    property GraphicDraw : Boolean read GetGraphicDraw write SetGraphicDraw;
    property Rows        : Integer read GetRows        write SetRows;
    property Cols        : Integer read GetCols        write SetCols;
    property AFont       : TNFont  read FFont          write FFont;           {drjohn}
    property LineHeight  : Single  read GetLineHeight  write SetLineHeight;   {drjohn}
    property LineZoom    : Single  read GetLineZoom    write SetLineZoom;
    property CharZoom    : Single  read GetCharZoom    write SetCharZoom;

    property OnNamesClick : TNotifyEvent read FOnNamesClick write FOnNamesClick;
  end;

var
  OptForm: TOptForm;

implementation

{$R *.DFM}

const
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetFKeys(Value : Integer);
begin
    case Value of
    0 : FKeys1RadioButton.Checked := TRUE;
    1 : FKeys2RadioButton.Checked := TRUE;
    2 : FKeys3RadioButton.Checked := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TOptForm.GetFKeys : Integer;
begin
   if FKeys1RadioButton.Checked then
       Result := 0
   else if FKeys2RadioButton.Checked then
       Result := 1
   else
       Result := 2;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetHostName(Value : String);
begin
    FHostName := Value;
    Caption := 'Options for ' + HostName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLocalEcho  : Boolean;
begin
    Result := LocalEchoCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAutoCr     : Boolean;
begin
    Result := AutoCrCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAutoLF     : Boolean;
begin
    Result := AutoLFCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetXlat     : Boolean;
begin
    Result := XlatCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetGraphicDraw : Boolean;
begin
    Result := GraphicDrawCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetAltKeys    : Boolean;
begin
    Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetMonoChrome : Boolean;
begin
    Result := MonoChromeCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetUpperLock : Boolean;
begin
    Result := UpperLockCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLocalEcho(Value : Boolean);
begin
    LocalEchoCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAutoCr(Value : Boolean);
begin
    AutoCRCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAutoLF(Value : Boolean);
begin
    AutoLFCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetXlat(Value : Boolean);
begin
    XlatCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetGraphicDraw(Value : Boolean);
begin
    GraphicDrawCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetAltKeys(Value : Boolean);
begin
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetMonoChrome(Value : Boolean);
begin
    MonoChromeCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetUpperLock(Value : Boolean);
begin
    UpperLockCheckBox.Checked := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetRows : Integer;
begin
    Result := atoi(RowsEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetCols : integer;
begin
    Result := atoi(ColsEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLineZoom : Single;
begin
    try
        Result := StrToFloat(LineZoomEdit.Text);
    except
        Result := 1.0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetCharZoom : Single;
begin
    try
        Result := StrToFloat(CharZoomEdit.Text);
    except
        Result := 1.0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  TOptForm.GetLineHeight : single;              {drjohn}
var
    DC      : HDC;
    Metrics : TTextMetric;
    hObject : THandle;
begin
    Result := atoi(LineHeightEdit.Text);
    if Result = 0 then begin
        DC      := GetDC(0);
        hObject := SelectObject(DC, FFont.Handle);
        GetTextMetrics(DC, Metrics);
        SelectObject(DC, hOBject);
        ReleaseDC(0, DC);

        Result := Metrics.tmHeight;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetRows(Value : Integer);
begin
    RowsEdit.Text := IntToStr(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetCols(Value : Integer);
begin
    ColsEdit.Text := IntToStr(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLineHeight(Value : Single);                         {drjohn}
begin
    LineHeightEdit.Text := IntToStr(Trunc(Value));                  {drjohn}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetLineZoom(Value : Single);
begin
    LineZoomEdit.Text := Format('%5.3f', [Value]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.SetCharZoom(Value : Single);
begin
    CharZoomEdit.Text := Format('%5.3f', [Value]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.OkButtonClick(Sender: TObject);
begin
    ModalResult := IDOK;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.CancelButtonClick(Sender: TObject);
begin
    ModalResult := IDCANCEL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
    {    Width        := IniFile.ReadInteger(SectionName, KeyName + KeyWidth,
                                            Width);
        Height       := IniFile.ReadInteger(SectionName, KeyName + KeyHeight,
                                            Height);  }
        Top          := IniFile.ReadInteger(SectionName, KeyName + KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionName, KeyName + KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
    end;
    NamesButton.Visible := Assigned(FOnNamesClick);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionName, KeyName + KeyTop,         Top);
    IniFile.WriteInteger(SectionName, KeyName + KeyLeft,        Left);
 {   IniFile.WriteInteger(SectionName, KeyName + KeyWidth,       Width);
    IniFile.WriteInteger(SectionName, KeyName + KeyHeight,      Height);  }
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.A11ButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := TRUE;
    LocalEcho  := FALSE;
    MonoChrome := TRUE;
    UpperLock  := TRUE;
    Xlat       := FALSE;
    FKeys3RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.LaboButtonClick(Sender: TObject);
begin
    Rows       := 24;
    Cols       := 80;
    AutoCr     := TRUE;
    AutoLF     := FALSE;
    AltKeys    := TRUE;
    LocalEcho  := FALSE;
    MonoChrome := TRUE;
    UpperLock  := FALSE;
    Xlat       := FALSE;
    FKeys2RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.RDVButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := FALSE;
    LocalEcho  := FALSE;
    MonoChrome := FALSE;
    UpperLock  := FALSE;
    Xlat       := TRUE;
    FKeys1RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.USUSButtonClick(Sender: TObject);
begin
    Rows       := 25;
    Cols       := 80;
    AutoCr     := FALSE;
    AutoLF     := FALSE;
    AltKeys    := FALSE;
    LocalEcho  := FALSE;
    MonoChrome := FALSE;
    UpperLock  := FALSE;
    Xlat       := TRUE;
    FKeys2RadioButton.Checked := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FormCreate(Sender: TObject);
begin
    FFont       := TNFont.Create;             {drjohn}
    SectionName := 'Windows';
    KeyName     := 'Options';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.FontButtonClick(Sender: TObject);
begin
    FontDialog1.Font := FFont;
    if FontDialog1.Execute then
        FFont := TNFont(FontDialog1.Font);            {drjohn}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TOptForm.NamesButtonClick(Sender: TObject);
begin
    if Assigned(FOnNamesClick) then
        FOnNamesClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

