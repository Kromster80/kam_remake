{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TSnmpCli class encapsulate the SNMP client paradigm
Creation:     March 2011
Version:      1.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              The Initial Developer of the Original Code is Lukas Gebauer.

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
May 11, 2012 V1.01 Arno - Unit did not compile with Ansi Delphi

|==============================================================================|
| Copyright (c)1999-2007, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

unit OverbyteIcsSnmpMsgs;
{$I Include\OverbyteIcsDefs.inc}

interface

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  OverbyteIcsMD5,
  OverbyteIcsSHA1,
  OverbyteIcsAsn1Utils;

const
  SnmpMsgsVersion       = 101;
  CopyRight    : String = ' AnmpMsgs (c) 2011 Francois Piette V1.01 ';

const
  cSnmpProtocol         = '161';
  cSnmpTrapProtocol     = '162';

  SNMP_V1               = 0;
  SNMP_V2C              = 1;
  SNMP_V3               = 3;

  //PDU type
  PDUGetRequest         = $A0;
  PDUGetNextRequest     = $A1;
  PDUGetResponse        = $A2;
  PDUSetRequest         = $A3;
  PDUTrap               = $A4; //Obsolete
  //for SNMPv2
  PDUGetBulkRequest     = $A5;
  PDUInformRequest      = $A6;
  PDUTrapV2             = $A7;
  PDUReport             = $A8;

  //errors
  ENoError              = 0;
  ETooBig               = 1;
  ENoSuchName           = 2;
  EBadValue             = 3;
  EReadOnly             = 4;
  EGenErr               = 5;
  //errors SNMPv2
  ENoAccess             = 6;
  EWrongType            = 7;
  EWrongLength          = 8;
  EWrongEncoding        = 9;
  EWrongValue           = 10;
  ENoCreation           = 11;
  EInconsistentValue    = 12;
  EResourceUnavailable  = 13;
  ECommitFailed         = 14;
  EUndoFailed           = 15;
  EAuthorizationError   = 16;
  ENotWritable          = 17;
  EInconsistentName     = 18;

  SnmpErrorStrings : array [ENoError..EInconsistentName] of String = (
    // EnoError
    'No error occurred',
    // ETooBig
    'The size of the Response-PDU would be too large to transport',
    // ENoSuchName
    'The name of a requested object was not found',
    // EBadValue
    'A value in the request didn''t match the structure that the ' +
    'recipient of the request had for the object',
    // EReadOnly
    'An attempt was made to set a variable that has an Access value ' +
    'indicating that it is read-only',
    // EGenErr
    'An error occurred other than one indicated by a more specific error code',
    // ENoAccess
    'Access was denied to the object for security reasons',
    // EWrongType
    'The object type in a variable binding is incorrect for the object',
    // EWrongLength
    'A variable binding specifies a length incorrect for the object',
    // EWrongEncoding
    'A variable binding specifies an encoding incorrect for the object',
    // EWrongValue
    'The value given in a variable binding is not possible for the object',
    // ENoCreation
    'A specified variable does not exist and cannot be created',
    // EInconsistentValue
    'A variable binding specifies a value that could be held by the ' +
    'variable but cannot be assigned to it at this time',
    // EResourceUnavailable
    'An attempt to set a variable required a resource that is not available',
    // ECommitFailed
    'An attempt to set a particular variable failed',
    // EUndoFailed
    'An attempt to set a particular variable as part of a group of ' +
    'variables failed, and the attempt to then undo the setting of ' +
    'other variables was not successful',
    // EAuthorizationError
    'A problem occurred in authorization',
    // ENotWritable
    'The variable cannot be written or created',
    // EInconsistentName
    'The name in a variable binding specifies a variable that does not exist');

type

  {:@abstract(Possible values for SNMPv3 flags.)
   This flags specify level of authorization and encryption.}
  TV3Flags = (
    NoAuthNoPriv,
    AuthNoPriv,
    AuthPriv);

  {:@abstract(Type of SNMPv3 authorization)}
  TV3Auth = (
    AuthMD5,
    AuthSHA1);

  {:@abstract(Data object with one record of MIB OID and corresponding values.)}
  TSNMPMib = class(TObject)
  protected
    FOID: AnsiString;
    FValue: AnsiString;
    FValueType: Integer;
  public
    {:OID number in string format.}
    property OID: AnsiString read FOID write FOID;

    {:Value of OID object in string format.}
    property Value: AnsiString read FValue write FValue;

    {:Define type of Value. Supported values are defined in @link(asn1util).
     For queries use ASN1_NULL, becouse you don't know type in response!}
    property ValueType: Integer read FValueType write FValueType;
  end;

  {:@abstract(It holding all information for SNMPv3 agent synchronization)
   Used internally.}
  TV3Sync = record
    EngineID: AnsiString;
    EngineBoots: integer;
    EngineTime: integer;
    EngineStamp: Cardinal;
  end;

  {:@abstract(Data object abstracts SNMP data packet)}
  TSNMPRec = class(TObject)
  protected
    FVersion: Integer;
    FPDUType: Integer;
    FID: Integer;
    FErrorStatus: Integer;
    FErrorIndex: Integer;
    FCommunity: AnsiString;
    FSNMPMibList: TList;
    FMaxSize: Integer;
    FFlags: TV3Flags;
    FFlagReportable: Boolean;
    FContextEngineID: AnsiString;
    FContextName: AnsiString;
    FAuthMode: TV3Auth;
    FAuthEngineID: AnsiString;
    FAuthEngineBoots: integer;
    FAuthEngineTime: integer;
    FAuthEngineTimeStamp: cardinal;
    FUserName: AnsiString;
    FPassword: AnsiString;
    FAuthKey: AnsiString;
    FPrivKey: AnsiString;
    FOldTrapEnterprise: AnsiString;
    FOldTrapHost: AnsiString;
    FOldTrapGen: Integer;
    FOldTrapSpec: Integer;
    FOldTrapTimeTicks: Integer;
    function Pass2Key(const Value: AnsiString): AnsiString;
  public
    constructor Create;
    destructor Destroy; override;

    {:Decode SNMP packet in buffer to object properties.}
    function DecodeBuf(const Buffer: AnsiString): Boolean;

    {:Encode obeject properties to SNMP packet.}
    function EncodeBuf: AnsiString;

    {:Clears all object properties to default values.}
    procedure Clear;

    {:Add entry to @link(SNMPMibList). For queries use value as empty string,
     and ValueType as ASN1_NULL.}
    procedure MIBAdd(const MIB, Value: AnsiString; ValueType: Integer);

    {:Delete entry from @link(SNMPMibList).}
    procedure MIBDelete(Index: Integer);

    {:Search @link(SNMPMibList) list for MIB and return correspond value.}
    function MIBGet(const MIB: AnsiString): AnsiString;

    {:return number of entries in MIB array.}
    function MIBCount: integer;

    {:Return MIB information from given row of MIB array.}
    function MIBByIndex(Index: Integer): TSNMPMib;

    {:List of @link(TSNMPMib) objects.}
    property SNMPMibList: TList read FSNMPMibList;
  public
    {:Version of SNMP packet. Default value is 0 (SNMP ver. 1). You can use
     value 1 for SNMPv2c or value 3 for SNMPv3.}
    property Version: Integer read FVersion write FVersion;

    {:Community string for autorize access to SNMP server. (Case sensitive!)
     Community string is not used in SNMPv3! Use @link(Username) and
     @link(password) instead!}
    property Community: AnsiString read FCommunity write FCommunity;

    {:Define type of SNMP operation.}
    property PDUType: Integer read FPDUType write FPDUType;

    {:Contains ID number. Not need to use.}
    property ID: Integer read FID write FID;

    {:When packet is reply, contains error code. Supported values are defined by
     E* constants.}
    property ErrorStatus: Integer read FErrorStatus write FErrorStatus;

    {:Point to error position in reply packet. Not usefull for users. It only
     good for debugging!}
    property ErrorIndex: Integer read FErrorIndex write FErrorIndex;

    {:special value for GetBulkRequest of SNMPv2 and v3.}
    property NonRepeaters: Integer read FErrorStatus write FErrorStatus;

    {:special value for GetBulkRequest of SNMPv2 and v3.}
    property MaxRepetitions: Integer read FErrorIndex write FErrorIndex;

    {:Maximum message size in bytes for SNMPv3. For sending is default 1472 bytes.}
    property MaxSize: Integer read FMaxSize write FMaxSize;

    {:Specify if message is authorised or encrypted. Used only in SNMPv3, and
     encryption is not yet supported!}
    property Flags: TV3Flags read FFlags write FFlags;

    {:For SNMPv3.... If is @true, SNMP agent must send reply (at least with some
     error).}
    property FlagReportable: Boolean read FFlagReportable write FFlagReportable;

    {:For SNMPv3. If not specified, is used value from @link(AuthEngineID)}
    property ContextEngineID: AnsiString read FContextEngineID write FContextEngineID;

    {:For SNMPv3.}
    property ContextName: AnsiString read FContextName write FContextName;

    {:For SNMPv3. Specify Authorization mode. (specify used hash for
     authorization)}
    property AuthMode: TV3Auth read FAuthMode write FAuthMode;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineID: AnsiString read FAuthEngineID write FAuthEngineID;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineBoots: Integer read FAuthEngineBoots write FAuthEngineBoots;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineTime: Integer read FAuthEngineTime write FAuthEngineTime;

    {:value used by SNMPv3 authorisation for synchronization with SNMP agent.}
    property AuthEngineTimeStamp: Cardinal read FAuthEngineTimeStamp Write FAuthEngineTimeStamp;

    {:SNMPv3 authorization username}
    property UserName: AnsiString read FUserName write FUserName;

    {:SNMPv3 authorization password}
    property Password: AnsiString read FPassword write FPassword;

    {:For SNMPv3. Computed Athorization key from @link(password).}
    property AuthKey: AnsiString read FAuthKey write FAuthKey;

    {:For SNMPv3. Encryption key for message encryption. Not yet used!}
    property PrivKey: AnsiString read FPrivKey write FPrivKey;

    {:MIB value to identify the object that sent the TRAPv1.}
    property OldTrapEnterprise: AnsiString read FOldTrapEnterprise write FOldTrapEnterprise;

    {:Address of TRAPv1 sender (IP address).}
    property OldTrapHost: AnsiString read FOldTrapHost write FOldTrapHost;

    {:Generic TRAPv1 identification.}
    property OldTrapGen: Integer read FOldTrapGen write FOldTrapGen;

    {:Specific TRAPv1 identification.}
    property OldTrapSpec: Integer read FOldTrapSpec write FOldTrapSpec;

    {:Number of 1/100th of seconds since last reboot or power up. (for TRAPv1)}
    property OldTrapTimeTicks: Integer read FOldTrapTimeTicks write FOldTrapTimeTicks;
  end;

function SnmpErrorToString(ErrCode : Integer) : String;

implementation

{==============================================================================}

constructor TSNMPRec.Create;
begin
  inherited Create;
  FSNMPMibList := TList.Create;
  Clear;
  FID := 1;
  FMaxSize := 1472;
end;

destructor TSNMPRec.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Clear;
  FSNMPMibList.Free;
  inherited Destroy;
end;


{==============================================================================}
// Extractect from synacode, ported to ICS-MD5
{:Returns a binary string with a RSA-MD5 hashing of string what is constructed
 by repeating "value" until length is "Len".}
function MD5LongHash(const Value: AnsiString; Len: integer): AnsiString;
var
  cnt, rest: integer;
  l: integer;
  n: integer;
  MDContext: TMD5Context;
  MD5Digest  : TMD5Digest;
  I          : Integer;
begin
  l := length(Value);
  cnt := Len div l;
  rest := Len mod l;
  MD5DigestInit(MD5Digest);
  MD5Init(MDContext);
  for n := 1 to cnt do
    MD5UpdateBuffer(MDContext, Value);
  if rest > 0 then
    MD5UpdateBuffer(MDContext, Copy(Value, 1, rest));
  MD5Final(MD5Digest, MDContext);
  MD5DigestToHex(MD5Digest);
  SetLength(Result, SizeOf(MD5Digest));
    for i := 0 to SizeOf(MD5Digest) - 1 do
      Result := Result + AnsiChar(MD5Digest[i]);

end;


{==============================================================================}
// Extractect from synacode, ported to ICS-SHA1
{:Returns a binary string with a SHA-1 hashing of string what is constructed
 by repeating "value" until length is "Len".}
function SHA1LongHash(const Value: AnsiString; Len: integer): AnsiString;
var
  cnt, rest: integer;
  l: integer;
  n: integer;
  SHAContext: SHA1Context;
  digest : SHA1Digest;
  I : Integer;
begin
  l := length(Value);
  cnt := Len div l;
  rest := Len mod l;
  SHA1Reset(SHAContext);
  for n := 1 to cnt do
    SHA1Input(SHAContext, PAnsiChar(Value), Length(Value));
  if rest > 0 then
    SHA1Input(SHAContext, PAnsiChar(Copy(Value, 1, rest)), rest);
  SHA1Result(SHAContext, digest);
  SetLength( Result, sizeof(digest) );
  for I := 1 to sizeof(digest) do
    Result[I] := AnsiChar(digest[I - 1]);
end;

{==============================================================================}
function TSNMPRec.Pass2Key(const Value: AnsiString): AnsiString;
var
  key: AnsiString;
begin
  case FAuthMode of
    AuthMD5:
      begin
        key := MD5LongHash(Value, 1048576);
        Result := StrMD5(key + FAuthEngineID + key);  // Not sure it must be hex !
      end;
    AuthSHA1:
      begin
        key := SHA1LongHash(Value, 1048576);
        Result := SHA1ofStr(key + FAuthEngineID + key);
      end;
  else
    Result := '';
  end;
end;

// Extracted from Synautil
{:Return current value of system timer with precizion 1 millisecond. Good for
 measure time difference.}
function GetTick: LongWord;
var
  tick, freq: TLargeInteger;
begin
  if {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.QueryPerformanceFrequency(freq) then begin
    {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.QueryPerformanceCounter(tick);
    Result := Trunc((tick / freq) * 1000) and High(LongWord)
  end
  else
    Result := {$IFDEF RTL_NAMESPACES}Winapi.{$ENDIF}Windows.GetTickCount;
end;

// Extracted from Synautil
{:Return difference between two timestamps. It working fine only for differences
 smaller then maxint. (difference must be smaller then 24 days.)}
function TickDelta(TickOld, TickNew: LongWord): LongWord;
begin
//if DWord is signed type (older Deplhi),
// then it not work properly on differencies larger then maxint!
  Result := 0;
  if TickOld <> TickNew then
  begin
    if TickNew < TickOld then
    begin
      TickNew := TickNew + LongWord(MaxInt) + 1;
      TickOld := TickOld + LongWord(MaxInt) + 1;
    end;
    Result := TickNew - TickOld;
    if TickNew < TickOld then
      if Result > 0 then
        Result := 0 - Result;
  end;
end;

{==============================================================================}

function TSNMPRec.DecodeBuf(const Buffer: AnsiString): Boolean;
var
  Pos: Integer;
  EndPos: Integer;
  sm, sv: AnsiString;
  Svt: Integer;
  s: AnsiString;
  Spos: integer;
  x: Byte;
begin
  Clear;
  Result := False;
  if Length(Buffer) < 2 then
    Exit;
  if (Ord(Buffer[1]) and $20) = 0 then
    Exit;
  Pos := 2;
  EndPos := ASNDecLen(Pos, Buffer);
  if Length(Buffer) < (EndPos + 2) then
    Exit;
  Self.FVersion := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);

  if FVersion = 3 then
  begin
    ASNItem(Pos, Buffer, Svt);  //header data seq
    ASNItem(Pos, Buffer, Svt);  //ID
    FMaxSize := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    s := ASNItem(Pos, Buffer, Svt);
    x := 0;
    if s <> '' then
      x := Ord(s[1]);
    FFlagReportable := (x and 4) > 0;
    x := x and 3;
    case x of
      1:
        FFlags := AuthNoPriv;
      3:
        FFlags := AuthPriv;
    else
      FFlags := NoAuthNoPriv;
    end;

    x := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    s := ASNItem(Pos, Buffer, Svt); //SecurityParameters
    //if SecurityModel is USM, then try to decode SecurityParameters
    if (x = 3) and (s <> '') then
    begin
      spos := 1;
      ASNItem(SPos, s, Svt);
      FAuthEngineID := ASNItem(SPos, s, Svt);
      FAuthEngineBoots := StrToIntDef(String(ASNItem(SPos, s, Svt)), 0);
      FAuthEngineTime := StrToIntDef(String(ASNItem(SPos, s, Svt)), 0);
      FAuthEngineTimeStamp := GetTick;
      FUserName := ASNItem(SPos, s, Svt);
      FAuthKey := ASNItem(SPos, s, Svt);
      FPrivKey := ASNItem(SPos, s, Svt);
    end;
    //scopedPDU
    s := ASNItem(Pos, Buffer, Svt);
    if Svt = ASN1_OCTSTR then
    begin
      //decrypt!
    end;
    FContextEngineID := ASNItem(Pos, Buffer, Svt);
    FContextName := ASNItem(Pos, Buffer, Svt);
  end
  else
  begin
    //old packet
    Self.FCommunity := ASNItem(Pos, Buffer, Svt);
  end;

  ASNItem(Pos, Buffer, Svt);
  Self.FPDUType := Svt;
  if Self.FPDUType = PDUTrap then
  begin
    FOldTrapEnterprise := ASNItem(Pos, Buffer, Svt);
    FOldTrapHost := ASNItem(Pos, Buffer, Svt);
    FOldTrapGen := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    FOldTrapSpec := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    FOldTrapTimeTicks := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
  end
  else
  begin
    Self.FID := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    Self.FErrorStatus := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
    Self.FErrorIndex := StrToIntDef(String(ASNItem(Pos, Buffer, Svt)), 0);
  end;
  ASNItem(Pos, Buffer, Svt);
  while Pos < EndPos do
  begin
    ASNItem(Pos, Buffer, Svt);
    Sm := ASNItem(Pos, Buffer, Svt);
    Sv := ASNItem(Pos, Buffer, Svt);
    Self.MIBAdd(sm, sv, Svt);
  end;
  Result := True;
end;

{==============================================================================}
// Extracted from Synautil
{:Returns a portion of the "Value" string located to the left of the "Delimiter"
 string. If a delimiter is not found, results is original string.}
function SeparateLeft(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x < 1 then
    Result := Value
  else
    Result := Copy(Value, 1, x - 1);
end;

{==============================================================================}
// Extracted from Synautil
{:Returns the portion of the "Value" string located to the right of the
 "Delimiter" string. If a delimiter is not found, results is original string.}
function SeparateRight(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x > 0 then
    x := x + Length(Delimiter) - 1;
  Result := Copy(Value, x + 1, Length(Value) - x);
end;

{==============================================================================}
// Extracted from Synautil
{:Like TrimLeft, but remove only spaces, not control characters!}
function TrimSPLeft(const S: string): string;
var
  I, L: Integer;
begin
  Result := '';
  if S = '' then
    Exit;
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do
    Inc(I);
  Result := Copy(S, I, Maxint);
end;

{==============================================================================}
// Extracted from Synautil
{:Like TrimRight, but remove only spaces, not control characters!}
function TrimSPRight(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  if S = '' then
    Exit;
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

{==============================================================================}
// Extracted from Synautil
{:Like Trim, but remove only spaces, not control characters!}
function TrimSP(const S: string): string;
begin
  Result := TrimSPLeft(s);
  Result := TrimSPRight(Result);
end;

{==============================================================================}
// Extracted from Synautil
function FetchBin(var Value: string; const Delimiter: string): string;
var
  s: string;
begin
  Result := SeparateLeft(Value, Delimiter);
  s := SeparateRight(Value, Delimiter);
  if s = Value then
    Value := ''
  else
    Value := s;
end;

{==============================================================================}
// Extracted from Synautil
function Fetch(var Value: string; const Delimiter: string): string;
begin
  Result := FetchBin(Value, Delimiter);
  Result := TrimSP(Result);
  Value := TrimSP(Value);
end;

{==============================================================================}
// Extracted from synaip
function IPToID(Host: string): Ansistring; {$IFDEF UNICODE} overload; {$ENDIF}
var
  s: string;
  i, x: Integer;
begin
  Result := '';
  for x := 0 to 3 do begin
    s := Fetch(Host, '.');
    i := StrToIntDef(s, 0);
    Result := Result + AnsiChar(Chr(i));
  end;
end;
{$IFDEF UNICODE}
function IPToID(Host: AnsiString): Ansistring; overload;
begin
    Result := IPToId(String(Host));
end;
{$ENDIF}
{==============================================================================}

function My_HMAC_MD5(Text, Key: AnsiString): AnsiString;
var
    Digest: TMD5Digest;
    I     : Integer;
begin
    HMAC_MD5(PAnsiChar(Text)^, Length(Text), PAnsiChar(Key)^, Length(Key), Digest);
    SetLength(Result, SizeOf(Digest));
    for I := 0 to SizeOf(Digest) - 1 do
      Result[I + 1] := AnsiChar(Digest[I]);
end;

{==============================================================================}
function My_HMAC_SHA1(Text, Key: AnsiString): AnsiString;
var
    Digest: SHA1Digest;
    I     : Integer;
begin
    HMAC_SHA1(PAnsiChar(Text)^, Length(Text), PAnsiChar(Key)^, Length(Key), Digest);
    SetLength(Result, SizeOf(Digest));
    for I := 0 to SizeOf(Digest) - 1 do
      Result[I + 1] := AnsiChar(Digest[I]);
end;

{==============================================================================}
function TSNMPRec.EncodeBuf: AnsiString;
var
  s: AnsiString;
  SNMPMib: TSNMPMib;
  n: Integer;
  pdu, head, auth, authbeg: AnsiString;
  x: Byte;
begin
  pdu := '';
  for n := 0 to FSNMPMibList.Count - 1 do
  begin
    SNMPMib := TSNMPMib(FSNMPMibList[n]);
    case SNMPMib.ValueType of
      ASN1_INT:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncInt(StrToIntDef(String(SNMPMib.Value), 0)), SNMPMib.ValueType);
      ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncUInt(StrToIntDef(String(SNMPMib.Value), 0)), SNMPMib.ValueType);
      ASN1_OBJID:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(MibToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_IPADDR:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(IPToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_NULL:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject('', ASN1_NULL);
    else
      s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
        ASNObject(SNMPMib.Value, SNMPMib.ValueType);
    end;
    pdu := pdu + ASNObject(s, ASN1_SEQ);
  end;
  pdu := ASNObject(pdu, ASN1_SEQ);

  if Self.FPDUType = PDUTrap then
    pdu := ASNObject(MibToID(FOldTrapEnterprise), ASN1_OBJID) +
      ASNObject(IPToID(FOldTrapHost), ASN1_IPADDR) +
      ASNObject(ASNEncInt(FOldTrapGen), ASN1_INT) +
      ASNObject(ASNEncInt(FOldTrapSpec), ASN1_INT) +
      ASNObject(ASNEncUInt(FOldTrapTimeTicks), ASN1_TIMETICKS) +
      pdu
  else
    pdu := ASNObject(ASNEncInt(Self.FID), ASN1_INT) +
      ASNObject(ASNEncInt(Self.FErrorStatus), ASN1_INT) +
      ASNObject(ASNEncInt(Self.FErrorIndex), ASN1_INT) +
      pdu;
  pdu := ASNObject(pdu, Self.FPDUType);

  if FVersion = 3 then
  begin
    if FContextEngineID = '' then
      FContextEngineID := FAuthEngineID;
    //complete PDUv3...
    pdu := ASNObject(FContextEngineID, ASN1_OCTSTR)
      + ASNObject(FContextName, ASN1_OCTSTR)
      + pdu;
    //maybe encrypt pdu... in future
    pdu := ASNObject(pdu, ASN1_SEQ);

    //prepare flags
    case FFlags of
      AuthNoPriv:
        x := 1;
      AuthPriv:
        x := 3;
    else
      x := 0;
    end;
    if FFlagReportable then
      x := x or 4;
    head := ASNObject(ASNEncInt(Self.FVersion), ASN1_INT);
    s := ASNObject(ASNEncInt(FID), ASN1_INT)
      + ASNObject(ASNEncInt(FMaxSize), ASN1_INT)
      + ASNObject(AnsiChar(x), ASN1_OCTSTR)
    //encode security model USM
      + ASNObject(ASNEncInt(3), ASN1_INT);
    head := head + ASNObject(s, ASN1_SEQ);

    //compute engine time difference
    x := TickDelta(FAuthEngineTimeStamp, GetTick) div 1000;

    authbeg := ASNObject(FAuthEngineID, ASN1_OCTSTR)
      + ASNObject(ASNEncInt(FAuthEngineBoots), ASN1_INT)
      + ASNObject(ASNEncInt(FAuthEngineTime + x), ASN1_INT)
      + ASNObject(FUserName, ASN1_OCTSTR);


    case FFlags of
      AuthNoPriv,
      AuthPriv:
        begin
          s := authbeg + ASNObject(StringOfChar(AnsiChar(#0), 12), ASN1_OCTSTR)
             + ASNObject(FPrivKey, ASN1_OCTSTR);
          s := ASNObject(s, ASN1_SEQ);
          s := head + ASNObject(s, ASN1_OCTSTR);
          s := ASNObject(s + pdu, ASN1_SEQ);
          //in s is entire packet without auth info...
          case FAuthMode of
            AuthMD5:
              begin
                s := My_HMAC_MD5(s, Pass2Key(FPassword) + StringOfChar(AnsiChar(#0), 48));
                //strip to HMAC-MD5-96
                delete(s, 13, 4);
              end;
            AuthSHA1:
              begin
                s := My_HMAC_SHA1(s, Pass2Key(FPassword) + StringOfChar(AnsiChar(#0), 44));
                //strip to HMAC-SHA-96
                delete(s, 13, 8);
              end;
          else
            s := '';
          end;
          FAuthKey := s;
        end;
    end;

    auth := authbeg + ASNObject(FAuthKey, ASN1_OCTSTR)
     + ASNObject(FPrivKey, ASN1_OCTSTR);
    auth := ASNObject(auth, ASN1_SEQ);

    head := head + ASNObject(auth, ASN1_OCTSTR);
    Result := ASNObject(head + pdu, ASN1_SEQ);
  end
  else
  begin
    head := ASNObject(ASNEncInt(Self.FVersion), ASN1_INT) +
      ASNObject(Self.FCommunity, ASN1_OCTSTR);
    Result := ASNObject(head + pdu, ASN1_SEQ);
  end;
  // inc(self.FID);  // F.Piette: Bug: encoding should not change the fields
end;

procedure TSNMPRec.Clear;
var
  i: Integer;
begin
  FVersion := SNMP_V1;
  FCommunity := 'public';
  FUserName := '';
  FPassword := '';
  FPDUType := 0;
  FErrorStatus := 0;
  FErrorIndex := 0;
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Clear;
  FOldTrapEnterprise := '';
  FOldTrapHost := '';
  FOldTrapGen := 0;
  FOldTrapSpec := 0;
  FOldTrapTimeTicks := 0;
  FFlags := NoAuthNoPriv;
  FFlagReportable := false;
  FContextEngineID := '';
  FContextName := '';
  FAuthMode := AuthMD5;
  FAuthEngineID := '';
  FAuthEngineBoots := 0;
  FAuthEngineTime := 0;
  FAuthEngineTimeStamp := 0;
  FAuthKey := '';
  FPrivKey := '';
end;

{==============================================================================}
procedure TSNMPRec.MIBAdd(const MIB, Value: AnsiString; ValueType: Integer);
var
  SNMPMib: TSNMPMib;
begin
  SNMPMib := TSNMPMib.Create;
  SNMPMib.OID := MIB;
  SNMPMib.Value := Value;
  SNMPMib.ValueType := ValueType;
  FSNMPMibList.Add(SNMPMib);
end;

{==============================================================================}
procedure TSNMPRec.MIBDelete(Index: Integer);
begin
  if (Index >= 0) and (Index < MIBCount) then
  begin
    TSNMPMib(FSNMPMibList[Index]).Free;
    FSNMPMibList.Delete(Index);
  end;
end;

{==============================================================================}
function TSNMPRec.MIBCount: integer;
begin
  Result := FSNMPMibList.Count;
end;

{==============================================================================}
function TSNMPRec.MIBByIndex(Index: Integer): TSNMPMib;
begin
  Result := nil;
  if (Index >= 0) and (Index < MIBCount) then
    Result := TSNMPMib(FSNMPMibList[Index]);
end;

{==============================================================================}
function TSNMPRec.MIBGet(const MIB: AnsiString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to MIBCount - 1 do
  begin
    if ((TSNMPMib(FSNMPMibList[i])).OID = MIB) then
    begin
      Result := (TSNMPMib(FSNMPMibList[i])).Value;
      Break;
    end;
  end;
end;

{==============================================================================}
// Extracted from synautil
{:If string is binary string (contains non-printable characters), then is
 returned true.}
function IsBinaryString(const Value: string): Boolean;
var
  n: integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
  {$IFDEF UNICODE}
    if CharInSet(Value[n], [#0..#8, #10..#31]) then
  {$ELSE}
    if Value[n] in [#0..#8, #10..#31] then
  {$ENDIF}
      //ignore null-terminated strings
      if not ((n = Length(value)) and (Value[n] = #0)) then
      begin
        Result := True;
        Break;
      end;
end;

{==============================================================================}
// Extracted from synautil
{:Returns a string with hexadecimal digits representing the corresponding values
 of the bytes found in "Value" string.}
function StrToHex(const Value: Ansistring): string;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Value) do
    Result := Result + IntToHex(Byte(Value[n]), 2);
  Result := LowerCase(Result);
end;

{==============================================================================}
function SnmpErrorToString(ErrCode : Integer) : String;
begin
    if (ErrCode >= Low(SnmpErrorStrings)) or
       (ErrCode <= High(SnmpErrorStrings)) then
        Result := SnmpErrorStrings[Errcode]
    else
        Result := 'SNMP error #' + IntToStr(ErrCode);

end;

{==============================================================================}

end.


