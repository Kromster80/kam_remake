{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  TIcsCharsetComboBox provides easy MIME charset selection.
Creation:     May 10, 2009
Version:      V8.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009 by François PIETTE
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
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
May  21, 2009 V1.00a Preserve custom alias names.
June 27, 2009 V1.01 Added the CodeGear-fix of QC #41940 for compilers below
              D2007 UPD 3.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsCharsetComboBox;

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
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}Vcl.StdCtrls{$ELSE}StdCtrls{$ENDIF},
{$IFDEF COMPILER7_UP}
  {$IFDEF RTL_NAMESPACES}Vcl.Themes{$ELSE}Themes{$ENDIF},
{$ENDIF}  
  OverbyteIcsCharsetUtils;

type
  TIcsCharsetComboBox = class(TCustomComboBox)
  protected
    FCharset      : String;
    FUserFriendly : Boolean;
    FOnChange     : TNotifyEvent;
    FIncludeList  : TMimeCharsets;
    procedure PopulateItems; virtual;
    function  GetCharSet: String; virtual;
    procedure SetCharset(const Value: String); virtual;
    procedure SetIncludeList(const Value: TMimeCharsets); virtual;
    procedure SetUserFriendly(const Value: Boolean); virtual;
    procedure CreateWnd; override;
    procedure Change; override;
    procedure TriggerChange; virtual;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;    
    function IsCharsetSupported: Boolean;
    function GetCodePageDef: LongWord;
    function GetCodePage: LongWord;
    property IncludeList: TMimeCharsets read FIncludeList write SetIncludeList;
  published
    { Doesn't check whether a MIME charset string is supported in non-DropDownList style }
    property CharSet: String read GetCharSet write SetCharset;
    property UserFriendly : Boolean read FUserFriendly write SetUserFriendly default True;    
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind {default bkNone};
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    // property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted default True;
    property TabOrder;
    property TabStop;
    // property Text;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    //property Items; { Must be published after OnMeasureItem }
  end;

implementation


{ TIcsCharsetCombobox }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsCharsetComboBox.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Sorted        := True;
    FUserFriendly := True;
    FIncludeList  := [
    WIN_1250, WIN_1251, WIN_1252, WIN_1253, WIN_1254, WIN_1255, WIN_1256,
    WIN_1257, WIN_1258, ISO_8859_1, ISO_8859_2, ISO_8859_4, ISO_8859_5,
    ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_8_i, ISO_8859_9, ISO_8859_13,
    ISO_8859_15, BIG_5, EUC_KR, GB_18030, GB_2312, HZ_GB_2312, KOI8_R, KOI8_U,
    KOREAN_HANGUL, SHIFT_JIS, UTF_8, WIN_874];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER11_UP} // Fixed in D2007 UPD 3
// custom combobox control messages
const
  {$EXTERNALSYM CBM_FIRST}
  CBM_FIRST               = $1700;      { Combobox control messages }
  {$EXTERNALSYM CB_SETMINVISIBLE}
  CB_SETMINVISIBLE        = CBM_FIRST + 1;
{$ENDIF}

procedure TIcsCharsetComboBox.CreateWnd;
begin
    inherited CreateWnd;
{$IFNDEF COMPILER11_UP} // Fixed in D2007 UPD 3
{$IFDEF COMPILER7_UP}
    if CheckWin32Version(5, 1) and ThemeServices.ThemesEnabled then
        SendMessage(Handle, CB_SETMINVISIBLE, WPARAM(DropDownCount), 0);
{$ENDIF}
{$ENDIF}
    PopulateItems;
    SetCharset(FCharset);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.PopulateItems;
begin
    Items.BeginUpdate;
    try
        Clear;
        if FUserFriendly then
            GetFriendlyCharsetList(Items, FIncludeList, False)
        else
            GetMimeCharsetList(Items, FIncludeList, False);
    finally
        Items.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.Change;
var
    ACharSet: String;
begin
    inherited Change;
    ACharSet := GetCharSet;
    if FCharset <> ACharSet then
    begin
        FCharset := ACharSet;
        TriggerChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.Click;
begin
    if ItemIndex >= 0 then
        Charset := CodePageToMimeCharsetString(PULONG(Items.Objects[ItemIndex])^);
    inherited Click;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCharsetComboBox.GetCharSet: String;
var
    I : Integer;
begin
    Result := FCharset;
    if (Style <> csDropDownList) and (ItemIndex < 0) then
    begin
        I := Items.IndexOf(inherited Text);
        if I >= 0 then
            Result := CodePageToMimeCharsetString(PULONG(Items.Objects[I])^)
        else
            Result := inherited Text;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.SetCharset(const Value: String);
var
    Item         : Integer;
    CurCodePage  : LongWord;
    PCsInfo      : PCharsetInfo;
begin
    if (ItemIndex < 0) or (Value <> FCharset) then
    begin
        if Value = '' then
            PCsInfo := GetMimeInfo(IcsSystemCodePage)
        else
            PCsInfo := GetMimeInfo(Value);
        if Assigned(PCsInfo) then
        begin
            if Value <> '' then    // Preserve alias names
                FCharSet := Value
            else
                FCharSet := ExtractMimeName(PCsInfo);
            CurCodePage := PCsInfo^.CodePage;
            { Change selected }
            for Item := 0 to Items.Count -1 do
            begin
                if CurCodePage = PULONG(Items.Objects[Item])^ then
                begin
                    if (ItemIndex <> Item) then
                        ItemIndex := Item;
                    TriggerChange;
                    Exit;
                end;
            end;
        end
        else
            FCharset := Value;
        if Style = csDropDownList then
            ItemIndex := -1
        else
            inherited Text := FCharset;
        TriggerChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.SetUserFriendly(const Value: Boolean);
begin
    if Value <> FUserFriendly then
    begin
        FUserFriendly := Value;
        if Handle <> 0 then
        begin
            ItemIndex := -1; // Important!
            RecreateWnd;
        end;    
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.SetIncludeList(const Value: TMimeCharsets);
begin
    if Value <> FIncludeList then
    begin
        FIncludeList := Value;
        if Handle <> 0 then
        begin
            ItemIndex := -1; // Important!
            RecreateWnd;
        end;    
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsCharsetComboBox.TriggerChange;
begin
    if not (csLoading in ComponentState) then
        if Assigned(FOnChange) then FOnChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Returns the code page ID < $FFFF on success or ERR_CP_NOTMAPPED or        }
{ ERR_CP_NOTAVAILABLE on failure.                                           }
function TIcsCharsetComboBox.GetCodePage: LongWord;
begin
    MimeCharsetToCodePage(FCharset, Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCharsetComboBox.GetCodePageDef: LongWord;
begin
    Result := MimeCharsetToCodePageDef(FCharset);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsCharsetComboBox.IsCharsetSupported: Boolean;
var
    CodePage : LongWord;
begin
    Result := MimeCharsetToCodePage(FCharset, CodePage);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
