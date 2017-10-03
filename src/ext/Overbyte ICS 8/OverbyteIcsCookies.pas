{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Client Cookie Handling, see RFC2109/RFC6265 (RFC2965 is obsolete)
Creation:     19 March 2012
Updated:      5 Nov 2015
Version:      8.03
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2011 by François PIETTE
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

Overview
--------

Provides a memory store for persistent and non-persisent cookies captured from the
HTTP SetCookie header, and builds a Cookie header for any stored cookies for a URL.
Persistent cookies are saved and re-read from a simple CSV file.
OverbyteIcsHttpTst1.pas shows how to handle cookies, very few new lines

Updates:
19 Mar 2012 - 1.00 baseline Angus
28 Mar 2012 - 1.01 Arno: StrictDelimiter doesn't exist in Delphi 7
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Oct 8, 2012  V8.01 remove compiler warning
Dec 15, 2012 V8.02 Arno removed Windows.pas from uses clause and fixed a small bug
                   in GetCookies() when path was empty. Also removed use of "with"
                   in GetCookies() for better debugging.
Nov 5, 2015 V8.03  restored Windows to remove compiler warning 

Note - needs more testing for domain and path matching
Pending - not yet thread safe

}

unit OverbyteIcsCookies;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
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
{$IFDEF MSWINDOWS}
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
  OverbyteIcsUrl, OverbyteIcsUtils;

type
    TCookie = record       // RFC6265 says save this lot (except URL)
        CName: string;
        CValue: string;
        CExpireDT: TDateTime;
        CDomain: string;
        CPath: string;
        CCreateDT: TDateTime;
        CAccessDT: TDateTime;
        CHostOnly: boolean;
        CSecureOnly: boolean;
        CHttpOnly: boolean;
        CUrl: string;
        CPersist: boolean;  // don't save following fields
        CDelete: boolean;
        CRawCookie: string; // temp for debugging
    end;

    TNewCookieEvent   = procedure(Sender: TObject;
                                 ACookie: TCookie;
                                 var Save: Boolean) of object;

const
    ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;

    PerCookDomain = 0; PerCookPath = 1; PerCookName = 2;
    PerCookValue = 3; PerCookExpireDT = 4; PerCookCreateDT = 5;
    PerCookAccessDT = 6; PerCookHostOnly = 7; PerCookSecureOnly = 8;
    PerCookHttpOnly = 9; PerCookUrl = 10; PerCookTotal = 11;

    PerCookTitles: array [0..PerCookTotal-1] of string = (
        'Domain','Path','Name','Value','ExpireDT','CreateDT',
        'AccessDT','HostOnly','SecureOnly','HttpOnly','URL');

type

    TIcsCookies = class(TComponent)
    private
     { Private declarations }
        FCookieIdx: THashedStringList;
        FCookies: array of TCookie;
        FTotCookies: integer;
        FNewCookieEvent: TNewCookieEvent;
        FLoadFileName: string;
        FAutoSave: boolean;
    //    FLockList: TRTLCriticalSection;
    protected
      { Protected declarations }
        function GetCount: integer;
    public
      { Public declarations }
        constructor Create(Aowner:TComponent); override;
        destructor Destroy; override;
{ clear all cookies }
        procedure ClearAllCookies;
{ Get a single cookie by sorted index }
        function Get1Cookie (Idx: integer): TCookie;
{ rebuild cookie index without expired cookies }
        procedure RebuildIndex;
{ add a single cookie, may replace or delete }
        procedure AddCookie (ACookie: TCookie);
{ parse a cookie from SetCookie HTTP header }
        function ParseCookie (const ACookieHdr, AURL: string): TCookie;
{ add a cookie from SetCookie HTTP header }
        procedure SetCookie (const ACookieHdr, AURL: string);
{ find cookies for a domain and path }
        function GetCookies (const AURL: string): string;
{ load persistent cookies from string list as CSV }
        procedure LoadFromList (AList: TStrings);
{ load persistent cookies from a file as CSV }
        function LoadFromFile (const AFileName: string): boolean;
{ save persistent cookies to string list as CSV }
        procedure SaveToList (AList: TStrings);
{ save persistent cookies to a file as CSV }
        function SaveToFile (const AFileName: string; Repl: boolean = true): boolean;
    published
      { Published declarations }
        property Count: integer                    read GetCount;
        property AutoSave: boolean                 read FAutoSave write FAutoSave;
        property OnNewCookie: TNewCookieEvent      read FNewCookieEvent write FNewCookieEvent;
    end;

{ convert ASCII ISO time string to Date/Time - no quotes }
{ yyyy-mm-ddThh:nn:ss (ISODateTimeMask) (time may be missing) }
function PackedISOToDT (const ISO: string): TDateTime ;
{ convert Date/Time to ASCII ISO time string - no quotes }
{ yyyy-mm-ddThh:nn:ss (ISODateTimeMask) }
function DTToPackedISO (D: TDateTime): string ;
{ convert ASCII cookies date and time to Date/Time - similar to RFC1123 but may have two digit year }
{ note this function does not currently process the date exactly according to RFC6265 }
{ Sun, 16-Mar-2014 12:31:40 GMT or  Mon, 18-Mar-13 18:53:09 GMT }
function Cookie_StrToDate (aDate: String): TDateTime;

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert ASCII ISO time string to Date/Time - no quotes }
{ yyyy-mm-ddThh:nn:ss (ISODateTimeMask) (time may be missing) }
function PackedISOToDT (const ISO: string): TDateTime ;
var
    yy, mm, dd: word ;
    hh, nn, ss: word ;
    timeDT: TDateTime ;
    info: string;
begin
    result := 0 ;
    info := trim (ISO) ;
    if length (info) < 10 then exit ;
    if info [5] <> '-' then exit ;
    yy :=  atoi (copy (info, 1, 4)) ;
    mm := atoi (copy (info, 6, 2)) ;
    dd := atoi (copy (info, 9, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then     // D6 only
    begin
        result := -1 ;
        exit ;
    end ;
    if length (info) <> 19 then exit ;
    if info [14] <> ':' then exit ;
    hh := atoi (copy (info, 12, 2)) ;
    nn := atoi (copy (info, 15, 2)) ;
    ss := atoi (copy (info, 18, 2)) ;
    if NOT TryEncodeTime (hh, nn, ss, 0, timeDT) then exit ;   // D6 only
    result := result + timeDT ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert Date/Time to ASCII ISO time string - no quotes }
{ yyyy-mm-ddThh:nn:ss (ISODateTimeMask) }
function DTToPackedISO (D: TDateTime): string ;
begin
    result := FormatDateTime (ISODateTimeMask, D) ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ convert ASCII cookies date and time to Date/Time - similar to RFC1123 but may have two digit year }
{ note this function does not currently process the date exactly according to RFC6265 }
{ Sun, 16-Mar-2014 12:31:40 GMT or  Mon, 18-Mar-13 18:53:09 GMT }
function Cookie_StrToDate (aDate: String): TDateTime;
const
   RFC1123_StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
    Year, Month, Day: Word;
    Hour, Min,   Sec: Word;
    offset: integer;
    newDT: TDateTime;
begin
    result := 0;
    if Length (aDate) < 25 then exit;
    offset := 6;
    Day    := StrToIntDef(Copy(aDate, offset, 2), 0);
    offset := offset + 3;
    Month  := (Pos(Copy(aDate, offset, 3), RFC1123_StrMonth) + 2) div 3;
    offset := offset + 4;
    if aDate [offset + 2] <> ' ' then begin
        Year   := StrToIntDef(Copy(aDate, offset, 4), 0);
        offset := offset + 5;
    end
    else begin
        Year   := StrToIntDef(Copy(aDate, offset, 2), 0);
        if Year >= 70 then
            Year := Year + 1900
        else
            Year := Year + 2000;
        offset := offset + 3;
    end;
    Hour   := StrToIntDef(Copy(aDate, offset, 2), 0);
    offset := offset + 3;
    Min    := StrToIntDef(Copy(aDate, offset, 2), 0);
    offset := offset + 3;
    Sec    := StrToIntDef(Copy(aDate, offset, 2), 0);
    TryEncodeDate(Year, Month, Day, Result);
    TryEncodeTime(Hour, Min, Sec, 0, newDT);
    Result := Result + newDT;
    if Pos ('GMT', aDate) > 10 then Result := IcsUTCToDateTime (Result) ;  // convert GMT to local time
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsCookies.Create(Aowner:TComponent);
begin
    inherited Create(AOwner);
    SetLength (FCookies, 100);
    FTotCookies := 0;
    FCookieIdx := THashedStringList.Create;
    FCookieIdx.Sorted := true;
    FCookieIdx.Duplicates := dupAccept;
    FLoadFileName := '';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsCookies.Destroy;
begin
    if FAutoSave and (FLoadFileName <> '') then
        SaveToFile(FLoadFileName, true);
    SetLength (FCookies, 0);
    FTotCookies := 0;
    FCookieIdx.Free;
    inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ clear all cookies }
procedure TIcsCookies.ClearAllCookies;
begin
    SetLength (FCookies, 0);
    SetLength (FCookies, 100);
    FTotCookies := 0;
    FCookieIdx.Clear;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ how many saved cookies }
function TIcsCookies.GetCount: integer;
begin
    result := FCookieIdx.Count;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ get a single cookie by sorted index }
function TIcsCookies.Get1Cookie (Idx: integer): TCookie;
begin
    Result.CName := '';
    if Idx >= FCookieIdx.Count then exit;
    Result := FCookies [Integer (FCookieIdx.Objects [Idx])];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ rebuild cookie index without expired cookies }
procedure TIcsCookies.RebuildIndex;
var
    cnr: integer ;
    curDT: TDateTime;
    key: string;
begin
    FCookieIdx.Clear;
    curDT := Now ;
 // ?? should we removed expired records from the array as well?
    for cnr := 0 to FTotCookies - 1 do begin
        with FCookies [cnr] do begin
            if (CName = '') or CDelete then continue;
            if CPersist and (CExpireDT < curDT) then begin
                CName := '';
                CDelete := true;
                continue;
            end;
            key := CDomain + CPath + CName;
        end;
        FCookieIdx.AddObject (key, TObject (cnr));
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ add a single cookie, may replace or delete }
procedure TIcsCookies.AddCookie (ACookie: TCookie);
var
    idx, cnr, I: integer ;
    key: string;
begin
    if ACookie.CName = '' then exit;
    if ACookie.CDomain = '' then exit;

// see if we have this cookie
    key := ACookie.CDomain + ACookie.CPath + ACookie.CName;
    idx := -1;
    if FCookieIdx.Find (key, idx) then begin
        cnr := Integer (FCookieIdx.Objects [idx]);
        FCookies [cnr] := ACookie;  // replace old cookie
        if ACookie.CDelete then     // delete old cookie
        begin
            FCookieIdx.Delete (idx);
            FCookies [cnr].CName := '';
        end;
        exit;
    end;

// add a new cookie, find first free record
    cnr := -1;
    if FTotCookies > 0 then
    begin
        for I := 0 to FTotCookies - 1 do
        begin
            if (FCookies [I].CName = '') or FCookies [I].CDelete then
            begin
                cnr := I ;
                break;
            end;
        end;
    end;
    if cnr = -1  then
    begin
        cnr := FTotCookies;
        inc (FTotCookies);
    end;
    if FTotCookies >= Length (FCookies) then SetLength (FCookies, FTotCookies+100);
    FCookies [cnr] := ACookie;
    FCookieIdx.AddObject (key, TObject (cnr));
end;

// Set-Cookie: SID=31d4d96e407aad42; Max-Age=3600; Path=/; Secure; HttpOnly
// Set-Cookie: lang=en-US; Expires=Wed, 09 Jun 2021 10:18:14 GMT; Path=/; Domain=example.com
// Set-Cookie: PREF=ID=8fe7e287d7a86143:FF=0:TM=1331901100:LM=1331901100:S=C2ELFTWvpWggPxhO; expires=Sun, 16-Mar-2014 12:31:40 GMT; path=/; domain=.google.com
// Set-Cookie: NID=57=SDHXnvNgjSETCB5vRTJ7JbuqVKjC5dh-L8GGQdgDkhelL85yl6WOB14pc0vlJ09krTzw_4vFWRnoCq0PADUwSF0ztLX1ZgJQt9WHU0UTrnmitxCyyc91Snya2AR9I78k; expires=Sat, 15-Sep-2012 12:31:40 GMT; path=/; domain=.google.co.uk; HttpOnly
// Set-Cookie: WebAppTelecom-CodeLook=OwnCode%3D%2526CallTariff%3D100173%2526TariffName%3D24Talk%2BEvening%2B; EXPIRES=Sun, 17 Mar 2013 00:00:00; PATH=/codelook.htm

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF COMPILER10_UP}
procedure SetStrictDelimitedText(ALines: TStrings; const AText: string; const ADelimiter: Char);
var
    I, J: Integer;
    SLen: Integer;
begin
    ALines.Clear;
    SLen := Length(AText);
    if SLen = 0 then Exit;
    J := 1;
    for I := 1 to SLen do
    begin
        if AText[I] = ADelimiter then
        begin
            ALines.Add(Copy(AText, J, I - J));
            J := I + 1;
        end
        else if (I = SLen) and (J < I) then
            ALines.Add(Copy(AText, J, I - J + 1));
    end;
end;
{$ENDIF}

{ parse a cookie from SetCookie HTTP header }
function TIcsCookies.ParseCookie (const ACookieHdr, AURL: string): TCookie;
var
    Fields: THashedStringList;
    S, Proto, User, Pass, Host, Port, Path: string;
    curDT: TDateTime;
    I, P1, P2: integer;
begin
    with result do
    begin
        CRawCookie := '';
        CName :=  '';
        CValue := '';
        CExpireDT := 0;
        CCreateDT := 0;
        CAccessDT := 0;
        CPersist := false;
        CDelete :=  false;
    end;
    Fields := THashedStringList.Create;
    curDT := Now ;
    try
        ParseURL (AnsiLowercase (AURL), Proto, User, Pass, Host, Port, Path);
        if Length (Path) = 0 then Path := '/';
        P1 := Posn('/', Path, -1); // last /
        P2 := Pos('.', Path);      // first .
        if P2 > P1 then Path := Copy (Path, 1, P1);  // remove file name
        P2 := Pos('?', Path);  // first .
        if P2 > 0 then Path := Copy (Path, 1, P2 - 1);  // remove query
        Fields.Delimiter := ';';
        Fields.CaseSensitive := false;
    {$IFDEF COMPILER10_UP}
        Fields.StrictDelimiter := true;
        Fields.DelimitedText := ACookieHdr;
    {$ELSE}
        SetStrictDelimitedText(Fields, ACookieHdr, ';');
    {$ENDIF}    
        if Fields.Count = 0 then exit;
        if Pos ('=', Fields [0]) = 0 then exit;
        for I := 0 to Fields.Count - 1 do
              Fields [I] := Trim (Fields [I]);
        with result do
        begin
            CRawCookie := ACookieHdr;  // TEMP for debugging
            CUrl := AURL;
            CDelete := false;
            CName :=  Fields.Names [0];
            CValue := Fields.ValueFromIndex [0];
            CPath := AnsiLowerCase (Fields.Values ['Path']);
            CDomain := AnsiLowerCase (Fields.Values ['Domain']);
            S := Fields.Values ['Expires'];
            if S <> '' then
            begin
             // cookies dates may have 2 or 4 digit years so RFC1123_StrToDate does not work
                CExpireDT := Cookie_StrToDate (S);
            end;
            S := Fields.Values ['Max-Age'];  // Max-Age take precedence over Expires
            if S <> '' then CExpireDT := curDT + ((1 / SecsPerDay) * atoi (S));
            if CExpireDT >= curDT then CPersist := true;
            if (CExpireDT > 0) and (CExpireDT < curDT) then
            begin
                CValue := '';  // cookie value expired
                CDelete := true;
            end
            else
            begin
                CCreateDT := curDT;
            end;
            CSecureOnly := (Fields.IndexOf('Secure') >= 1);
            CHttpOnly := (Fields.IndexOf('HttpOnly') >= 1);

         // check we have a domain and it not third party, or get it from host
            if CDomain <> '' then
            begin
                if CDomain [1] = '.' then Delete (CDomain, 1, 1);
                if Pos (CDomain, Host) < 0 then  // cookie is for our host, not somewhere else
                begin
                    CName :=  '';   // ignore cookie
                    exit ;
                end;
            end
            else
            begin
                CDomain := Host;
                CHostOnly := true;
            end;
         // should check public suffix list to prevent cookies being set for com, co.uk, etc, over 5,000 entries

         // clean path of quotes, use path from URL if missing
            if Length (CPath) = 0 then CPath := Path;
            if CPath [1] = '"' then Delete (CPath, 1, 1);
            if CPath [Length (CPath)] = '"' then SetLength (CPath, Length (CPath)-1);
            if CPath [1] <> '/' then CPath := '/';
        end;
    finally
        Fields.Free;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ add a cookie from SetCookie HTTP header }
procedure TIcsCookies.SetCookie (const ACookieHdr, AURL: string);
var
    ACookie: TCookie;
    Save: boolean;
begin
    ACookie :=  ParseCookie (ACookieHdr, AURL);
    if ACookie.CName = '' then exit;
    Save := true;
    if Assigned (FNewCookieEvent) then FNewCookieEvent (Self, ACookie, Save);
    if NOT Save then exit;
    AddCookie (ACookie);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ find cookies for a domain and path }
function TIcsCookies.GetCookies (const AURL: string): string;
var
    idx, cnr, I: integer;
    Proto, User, Pass, Host, Port, Path: string;
    curDT: TDateTime;
    LCookie: TCookie;
    secure, expireflag: boolean;

// pending cookies are supposed to be sorted by name, currently by domain then by name
    procedure FindCookies (const domain: string);
    begin
        if FCookieIdx.Count = 0 then exit;
        idx := -1;
        FCookieIdx.Find (domain, idx);  // partial match
        while idx < FCookieIdx.Count do begin
            cnr := Integer (FCookieIdx.Objects [idx]);
            LCookie := FCookies [cnr];
            if LCookie.CDomain <> domain then exit;  // finished matching domains
            if (LCookie.CExpireDT <= curDT) then expireflag := true;  // need to remove cookie in a moment
            if (Pos (LCookie.CPath, path) = 1) and ((NOT LCookie.CPersist) or
                                    (LCookie.CExpireDT > curDT)) then begin // if persistend, check not expired
                if (NOT LCookie.CSecureOnly) or (LCookie.CSecureOnly = secure) then
                begin
                    if result <> '' then result := result + '; ';
                    result := result + LCookie.CName + '=' + LCookie.CValue;
                    LCookie.CAccessDT := curDT;
                end;
            end;
            inc (idx); // look for next name
        end;
    end;

begin
    result := '';
    expireflag := false;
    curDT := Now;
    ParseURL (AnsiLowercase (AURL), Proto, User, Pass, Host, Port, Path);
    if Path = '' then
        Path := '/';

    secure := (Proto = 'https');

// now build cookie string, removing one node of host name at a time
// suggestions for any better algorithm welcome!!!
    FindCookies (Host);
    I := Pos ('.', Host);
    if I >= 2 then begin
        Host := Copy (Host, I + 1, 999);  // remove first node
        FindCookies (Host);
        I := Pos ('.', Host);
        if I >= 2 then begin
            Host := Copy (Host, I + 1, 999);  // remove second node
            FindCookies (Host);
        end;
    end;
    if expireflag then RebuildIndex;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ load persistent cookies from string list as CSV }
procedure TIcsCookies.LoadFromList (AList: TStrings);
var
    I: integer;
    Fields: TStringList;
    ACookie: TCookie;
    curDT: TDateTime;
begin
    ClearAllCookies;
    if AList.Count <= 1 then exit;
    curDT := Now ;
    Fields := TStringList.Create;
    try
        Fields.CommaText := AList [0];
      // check we have a column name row, ideally we should parse the column names
        if Fields [PerCookDomain] <> PerCookTitles [PerCookDomain] then exit;
        if Fields.Count < PerCookTotal then exit;
        for I := 1 to AList.Count - 1 do begin
            Fields.CommaText := AList [I];
            if Fields.Count < PerCookTotal then continue;
            with ACookie do begin
                CDomain := Fields [PerCookDomain];
                CPath := Fields [PerCookPath];
                CName := Fields [PerCookName];
                CValue := Fields [PerCookValue];
                CExpireDT := PackedISOToDT (Fields [PerCookExpireDT]);
                if CExpireDT < curDT then continue;  // ignore expired cookie
                CCreateDT := PackedISOToDT (Fields [PerCookCreateDT]);
                CAccessDT := PackedISOToDT (Fields [PerCookAccessDT]);
                CHostOnly := (Fields [PerCookHostOnly]='1');
                CSecureOnly := (Fields [PerCookSecureOnly]='1');
                CHttpOnly := (Fields [PerCookHttpOnly]='1');
                CUrl := Fields [PerCookUrl];
                CPersist := true;
                CDelete := false;
                CRawCookie := '';
            end;
            AddCookie (ACookie);
        end;
    finally
        Fields.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ load persistent cookies from a file as CSV }
function TIcsCookies.LoadFromFile (const AFileName: string): boolean;
var
    Lines: TStringList;
begin
    result := false ;
    FLoadFileName := AFileName;
    if NOT FileExists (AFileName) then exit ;
    Lines := TStringList.Create;
    try
        try
            Lines.LoadFromFile (AFileName);
            LoadFromList (Lines);
            Result := true;
        except
        end;
    finally
       Lines.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ save persistent cookies to string list as CSV }
procedure TIcsCookies.SaveToList (AList: TStrings);
var
    I, J, cnr: integer;
    Fields: TStringList;
    curDT: TDateTime;
begin
    if NOT Assigned (AList) then AList := TStringList.Create;
    curDT := Now ;
    AList.Clear;
    Fields := TStringList.Create;
    try
        for  J := 0 to PerCookTotal - 1 do Fields.Add (PerCookTitles [J]);
        AList.Add (Fields.CommaText);
        if FCookieIdx.Count = 0 then exit;
        for I := 0 to FCookieIdx.Count - 1 do begin
            Fields.Clear;
            for  J := 0 to PerCookTotal - 1 do Fields.Add ('');
            cnr := Integer (FCookieIdx.Objects [I]);
            with FCookies [cnr] do begin
                if NOT CPersist then continue;
                if CExpireDT < curDT then continue;  // ignore expired cookie
                Fields [PerCookDomain] := CDomain;
                Fields [PerCookPath] := CPath;
                Fields [PerCookName] := CName;
                Fields [PerCookValue] := CValue;
                Fields [PerCookExpireDT] := DTToPackedISO (CExpireDT);
                Fields [PerCookCreateDT] := DTToPackedISO (CCreateDT);
                Fields [PerCookAccessDT] := DTToPackedISO (CAccessDT);
                Fields [PerCookHostOnly] := IntToStr (Ord (CHostOnly));
                Fields [PerCookSecureOnly] := IntToStr (Ord (CSecureOnly));
                Fields [PerCookHttpOnly] := IntToStr (Ord (CHttpOnly));
                Fields [PerCookUrl] := CUrl;
            end;
            AList.Add (Fields.CommaText);
        end ;
    finally
        Fields.Free;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ save persistent cookies to a file as CSV }
function TIcsCookies.SaveToFile (const AFileName: string; Repl: boolean = true): boolean;
var
    Lines: TStringList;
begin
    result := false ;
    if FileExists (AFileName) then
    begin
        if NOT Repl then exit;
        {$IFDEF RTL_NAMESPACES}System.{$ENDIF}SysUtils.DeleteFile (AFileName);
    end;
    Lines := TStringList.Create;
    try
        try
            SaveToList (Lines);
            Lines.SaveToFile (AFileName);
            Result := True;
        except
        end;
    finally
       Lines.Free;
    end;
end;

end.
