{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
              Code copied from OverbyteIcsHttpSrv.pas by
              Fastream Technologies (www.fastream.com) coders SubZero
              (G. I. Ates) and PeterS (Peter Nikolow), Luke (Boris Evstatiev)
Creation:     January 7, 2009
Version:      8.00
Description:  HTTP Digest Access Authentication, RFC 2617.
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2009-2012 by François PIETTE
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
Sep 20, 2010 V1.01 Uses OverbyteIcsMD5.MD5DigestToLowerHexA.
Feb 26, 2011 V1.02 Some small changes in generating the digest request string,
                   enabled algorithm "MD5-sess" which is untested so far.
Jan 09, 2012 V1.03 Fixed backward compatibility with RFC 2069.
                   Handle more than one qop and algorithm in server challenge.
                   Faster checks for empty strings.
Feb 29, 2012 V1.04 Use IcsRandomInt
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDigestAuth;

interface

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$J+}             { Allow typed constant to be modified }

uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsMD5, OverbyteIcsMimeUtils,
    OverbyteIcsTypes, OverbyteIcsUtils;

type
    EIcsAuthenticationError = class(Exception);
    THashHex                = type AnsiString;
    TIcsNonceString         = type AnsiString;
    TIcsAuthDigestAlgorithm = (adalgMD5, adalgMD5sess);
    TIcsAuthDigestQop       = (adqopAuth, adqopAuthInt);

    TAuthDigestNonceRec = record
        DT    : TDateTime;
        Hash  : TMd5Digest;
    end;
    PAuthDigestNonceRec = ^TAuthDigestNonceRec;

    TAuthDigestMethod   = (daAuth, daAuthInt, daBoth);
   (*
    TAuthDigestRequestInfo = record
        UserName    : String;
        Realm       : String;
        Qop         : String;
        Algorithm   : String;
        Nonce       : String;
        Nc          : String;
        Uri         : String;
        Cnonce      : String;
        Opaque      : String;
        Response    : String;
    end;
    *)
    { Data required by clients to preemtively create a authorization header }
    TAuthDigestResponseInfo = record
        Nc              : Integer;// + must be filled
        // from server //
        Realm       : String; // from Challenge, the protection space
        Qop         : String; // from Challenge, either auth or auth-int or not specified
        Algorithm   : String; // from Challenge, optional defaults to 'MD5'
        Nonce       : String; // from Challenge, MUST
        Opaque      : String; // from Challenge, MUST
        Domain      : String; // from Challenge, optional space separated list of URIs
        Stale       : Boolean;// from Challenge, optional, no need to pop up a login dialog
    end;


procedure AuthDigestGetBodyHash(const EntityBody: RawByteString;
  var EntityHash: THashHex);

function AuthDigestGenerateRequest(
    const UserName, Password, HttpMethod, Uri : String;
    const EntityHash : THashHex;
    var Info: TAuthDigestResponseInfo): String; overload;
    
function AuthDigestGenerateRequest(
    const UserName, Password, HttpMethod, Uri, Realm, Qop, Nonce, Opaque,
    Algorithm : String; const EntityHash: THashHex;
    var Cnonce: String; var Nc: Integer): String; overload;

function AuthDigestGenerateChallenge(
    DigestMethod: TAuthDigestMethod; Secret: Int64; const Realm,
    Domain : String; Stale: Boolean; var Nonce, Opaque: String): String;

function AuthDigestValidateResponse(var Info: TAuthDigestResponseInfo;
    APreferredAlgo: TIcsAuthDigestAlgorithm = adalgMD5;
    APreferredQop: TIcsAuthDigestQop = adqopAuth): Boolean;


procedure AuthDigestParseChallenge(
    const ALine   : String;
    var   Info    : TAuthDigestResponseInfo);
    {var   Realm     : String;
    var   Domain    : String;
    var   Nonce     : String;
    var   Opaque    : String;
    var   Stale     : Boolean;
    var   Algorithm : String;
    var   Qop       : String);}

function AuthDigestGetRequest(
    const ALine     : String;
    var   NonceTime   : TDateTime;
    var   UserName    : String;
    var   Realm       : String;
    var   Qop         : String;
    var   Algorithm   : String;
    var   Nonce       : String;
    var   Nc          : String;
    var   DigestUri   : String;
    var   Cnonce      : String;
    var   Opaque      : String;
    var   Response    : String): Boolean;


function AuthDigestGenerateIcsNonce(TimeStamp: TDateTime; Secret: Int64;
    const Opaque, Realm: AnsiString): TIcsNonceString;

procedure AuthDigestCalcResponse(
    const HA1           : THashHex;    { H(A1)                             }
    const Nonce         : AnsiString;  { nonce from server                 }
    const NonceCount    : AnsiString;  { 8 hex digits                      }
    const CNonce        : AnsiString;  { client nonce                      }
    const Qop           : AnsiString;  { qop-value: "", "auth", "auth-int" }
    const Method        : AnsiString;  { method from the request           }
    const DigestUri     : AnsiString;  { requested URL                     }
    const HEntity       : THashHex;    { H(entity body) if qop="auth-int"  }
    out   Response      : THashHex);   { request-digest or response-digest }

procedure AuthDigestCalcHA1(
    const Algorithm       : String;
    const UserName        : AnsiString;
    const Realm           : AnsiString;
    const Password        : AnsiString;
    const Nonce           : AnsiString;
    const CNonce          : AnsiString;
    out   SessionKey      : THashHex);

implementation

uses
    {$IFDEF RTL_NAMESPACES}System.StrUtils{$ELSE}StrUtils{$ENDIF}; // For PosEx(), it uses a FastCode function in newer RTLs.

const
    AUTH_DIGEST_DELIM : AnsiChar = ':';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AuthDigestGetBodyHash(const EntityBody: RawByteString;
  var EntityHash: THashHex);
var
    Md5Ctx : TMD5Context;
    HA1    : TMD5Digest;
begin
    MD5Init(Md5Ctx);
    MD5Update(Md5Ctx, Pointer(EntityBody)^, Length(EntityBody));
    MD5Final(HA1, Md5Ctx);
    EntityHash := MD5DigestToLowerHexA(HA1);  { V1.01 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AuthDigestGenerateIcsNonce(TimeStamp: TDateTime; Secret: Int64;
    const Opaque, Realm: AnsiString): TIcsNonceString;
var
    Nonce    : TAuthDigestNonceRec;
    NonceCtx : TMD5Context;
begin
    Nonce.DT := TimeStamp;
    MD5Init(NonceCtx);
    MD5Update(NonceCtx, Secret, SizeOf(Secret));
    MD5Update(NonceCtx, Pointer(Realm)^, Length(Realm));
    MD5Update(NonceCtx, TimeStamp, SizeOf(TimeStamp));
    MD5Update(NonceCtx, Pointer(Opaque)^, Length(Opaque));
    MD5Final(Nonce.Hash, NonceCtx);
    Result := Base64Encode(PAnsiChar(@Nonce), SizeOf(Nonce));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AuthDigestCalcResponse(
    const HA1           : THashHex;       { H(A1)                             }
    const Nonce         : AnsiString;     { nonce from server                 }
    const NonceCount    : AnsiString;     { 8 hex digits                      }
    const CNonce        : AnsiString;     { client nonce                      }
    const Qop           : AnsiString;     { qop-value: "", "auth", "auth-int" }
    const Method        : AnsiString;     { method from the request           }
    const DigestUri     : AnsiString;     { requested URL                     }
    const HEntity       : THashHex;       { H(entity body) if qop="auth-int"  }
    out   Response      : THashHex);      { request-digest or response-digest }
var
    Md5Ctx   : TMD5Context;
    HA2      : TMD5Digest;
    RespHash : TMD5Digest;
    HA2Hex   : THashHex;
begin
    { calculate H(A2) }
    MD5Init(Md5Ctx);
    MD5UpdateBuffer(Md5Ctx, Method);
    MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
    MD5UpdateBuffer(Md5Ctx, DigestUri);

    if Qop = 'auth-int' then begin
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, HEntity);
    end;

    MD5Final(HA2, Md5Ctx);
    HA2Hex := MD5DigestToLowerHexA(HA2);  { V1.01 }

    { calculate response }
    MD5Init(Md5Ctx);
    MD5UpdateBuffer(Md5Ctx, HA1);
    MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
    MD5UpdateBuffer(Md5Ctx, Nonce);
    if Qop <> '' then begin // (if auth or auth-int) rfc2617 3.2.2.1 Request-Digest
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, NonceCount);
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, CNonce);
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, Qop);
    end;
    MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
    MD5UpdateBuffer(Md5Ctx, HA2Hex);
    MD5Final(RespHash, Md5Ctx);
    Response := MD5DigestToLowerHexA(RespHash);  { V1.01 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AuthDigestCalcHA1(
    const Algorithm       : String;
    const UserName        : AnsiString;
    const Realm           : AnsiString;
    const Password        : AnsiString;
    const Nonce           : AnsiString;
    const CNonce          : AnsiString;
    out   SessionKey      : THashHex);
var
    Md5Ctx : TMD5Context;
    HA1    : TMD5Digest;
begin
    MD5Init(Md5Ctx);
    MD5UpdateBuffer(Md5Ctx, UserName);
    MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
    MD5UpdateBuffer(Md5Ctx, Realm);
    MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
    MD5UpdateBuffer(Md5Ctx, Password);
    MD5Final(HA1, Md5Ctx);

    if Algorithm = 'MD5-sess' then
    begin
        MD5Init(Md5Ctx);
        MD5UpdateBuffer(Md5Ctx, HA1);
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, Nonce);
        MD5UpdateBuffer(Md5Ctx, AUTH_DIGEST_DELIM);
        MD5UpdateBuffer(Md5Ctx, CNonce);
        MD5Final(HA1, Md5Ctx);
    end;
   
    SessionKey := MD5DigestToLowerHexA(HA1);  { V1.01 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AuthDigestGetRequest(
    const ALine       : String;
    var   NonceTime   : TDateTime;
    var   UserName    : String;
    var   Realm       : String;
    var   Qop         : String;
    var   Algorithm   : String;
    var   Nonce       : String;
    var   Nc          : String;
    var   DigestUri   : String;
    var   Cnonce      : String;
    var   Opaque      : String;
    var   Response    : String): Boolean;
var
    Pos1, Pos2 : Integer;
    Buf : THashHex;
begin
    { Authorization: Digest username="Mufasa",
                 realm="testrealm@host.com",
                 nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093",
                 uri="/dir/index.html",
                 qop=auth,
                 nc=00000001,
                 cnonce="0a4f113b",
                 response="6629fae49393a05397450978507c4ef1",
                 opaque="5ccc069c403ebaf9f0171e9517f40e41" }

    Result := FALSE;
    UserName := ''; Realm := ''; Qop := ''; Algorithm := ''; Nonce := '';
    Nc := ''; DigestUri := ''; Cnonce := ''; Opaque := ''; Response := '';

    Pos1 := PosEx('username="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('username="'));
        Pos2 := PosEx('"', ALine, Pos1);
        UserName := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Exit;

    Pos1 := PosEx('realm="', ALine, 1);
    if Pos1 > 0 then
    begin
        Inc(Pos1, Length('realm="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Realm := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Exit;
{
   qop
     Indicates what "quality of protection" the client has applied to
     the message. If present, its value MUST be one of the alternatives
     the server indicated it supports in the WWW-Authenticate header.
     This directive is optional in order to
     preserve backward compatibility with a minimal implementation of
     RFC 2069 [6], but SHOULD be used if the server indicated that qop
     is supported by providing a qop directive in the WWW-Authenticate
     header field.
}

    Pos1 := PosEx('qop="', ALine, 1);
    if Pos1 = 0 then begin
        Pos1 := PosEx('qop=', ALine, 1);
        if Pos1 > 0 then
        begin
            Inc(Pos1, Length('qop='));
            Pos2 := PosEx(',', ALine, Pos1);
            Qop  := Copy(ALine, Pos1, Pos2 - Pos1);
        end;
    end
    else begin
        Inc(Pos1, Length('qop="'));
        Pos2  := PosEx('"', ALine, Pos1);
        Qop   := Copy(ALine, Pos1, Pos2 - Pos1);
    end;
    (*
    case Method of
      daAuth:
          if Qop <> 'auth' then
              Exit;
      daAuthInt:
          if Qop <> 'auth-int' then
              Exit;
      daBoth:;
        { whatever it is }
    end;
    *)

    Pos1 := PosEx('nonce="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('nonce="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Nonce := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Exit;

 {cnonce
     This MUST be specified if a qop directive is sent (see above), and
     MUST NOT be specified if the server did not send a qop directive in
     the WWW-Authenticate header field. }

    Pos1 := PosEx('cnonce="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('cnonce="'));
        Pos2 := PosEx('"', ALine, Pos1);
        CNonce := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else if Qop <> '' then
        Exit;

  {nonce-count
     This MUST be specified if a qop directive is sent (see above), and
     MUST NOT be specified if the server did not send a qop directive in
     the WWW-Authenticate header field. }

    Pos1 := PosEx('nc=', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('nc='));
        Pos2 := PosEx(',', ALine, Pos1);
        Nc := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else if Qop <> '' then
        Exit;

    Pos1 := PosEx('algorithm="', ALine, 1);
    if Pos1 = 0 then begin
        Pos1 := PosEx('algorithm=', ALine, 1);
        if Pos1 = 0 then
            Algorithm := 'MD5'
        else begin
            Inc(Pos1, Length('algorithm='));
            Pos2 := PosEx(',', ALine, Pos1);
            Algorithm := Copy(ALine, Pos1, Pos2 - Pos1);
        end;
    end
    else begin
        Inc(Pos1, Length('algorithm="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Algorithm := Copy(ALine, Pos1, Pos2 - Pos1);
    end;

    Pos1 := PosEx('uri="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('uri="'));
        Pos2 := PosEx('"', ALine, Pos1);
        DigestUri := Copy(ALine, Pos1, Pos2 - Pos1);
    end;

    Pos1 := PosEx('response="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('response="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Response := Copy(ALine, Pos1, Pos2 - Pos1);
    end;

    Pos1 := PosEx('opaque="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('opaque="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Opaque := Copy(ALine, Pos1, Pos2 - Pos1);
    end;

    Buf := Base64Decode(AnsiString(Nonce));
    if Length(Buf) <> SizeOf(TAuthDigestNonceRec) then
        Exit;

    NonceTime := PAuthDigestNonceRec(Pointer(Buf))^.DT;

    Result := (NonceTime > MinDateTime) and (NonceTime <= Now);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure AuthDigestParseChallenge(
    const ALine   : String;
    var   Info    : TAuthDigestResponseInfo);
var
    Pos1, Pos2 : Integer;
begin
    { WWW-Authenticate: Digest
                 realm="testrealm@host.com",
                 qop="auth,auth-int",
                 nonce="dcd98b7102dd2f0e8b11d0f600bfb0c093",
                 opaque="5ccc069c403ebaf9f0171e9517f40e41" }

    Info.Nc         := 0; // initialize to zero;

    Pos1 := PosEx('realm="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('realm="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Realm := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Info.Realm := '';

    Pos1 := PosEx('domain="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('domain="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Domain := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Info.Domain := '';

    Pos1 := PosEx('nonce="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('nonce="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Nonce := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Info.Nonce := '';


    Pos1 := PosEx('opaque="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('opaque="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Opaque := Copy(ALine, Pos1, Pos2 - Pos1);
    end
    else
        Info.Opaque := '';

    Pos1 := PosEx('stale="', ALine, 1);
    if Pos1 > 0 then begin
        Inc(Pos1, Length('stale="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Stale := CompareText(Copy(ALine, Pos1, Pos2 - Pos1), 'true') = 0;
    end
    else
        Info.Stale := FALSE;

    Pos1 := PosEx('algorithm="', ALine, 1);
    if Pos1 = 0 then begin
        Pos1 := PosEx('algorithm=', ALine, 1);
        if Pos1 = 0 then
            Info.Algorithm := ''
        else begin
            Inc(Pos1, Length('algorithm='));
            Pos2 := PosEx(',', ALine, Pos1);
            Info.Algorithm := Copy(ALine, Pos1, Pos2 - Pos1);
        end;
    end
    else begin
        Inc(Pos1, Length('algorithm="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Algorithm := Copy(ALine, Pos1, Pos2 - Pos1);
    end;

    Pos1 := PosEx('qop="', ALine, 1);
    if Pos1 = 0 then begin
        Pos1 := PosEx('qop=', ALine, 1);
        if Pos1 > 0 then begin
            Inc(Pos1, Length('qop='));
            Pos2 := PosEx(',', ALine, Pos1);
            Info.Qop := Copy(ALine, Pos1, Pos2 - Pos1);
        end
        else
            Info.Qop := '';
    end
    else begin
        Inc(Pos1, Length('qop="'));
        Pos2 := PosEx('"', ALine, Pos1);
        Info.Qop := Copy(ALine, Pos1, Pos2 - Pos1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Don't change the defaults for now in production, only adalgMD5 and        }
{ adqopAuth are known working params so far.                                }
function AuthDigestValidateResponse(
    var Info        : TAuthDigestResponseInfo;
    APreferredAlgo  : TIcsAuthDigestAlgorithm = adalgMD5;
    APreferredQop   : TIcsAuthDigestQop = adqopAuth): Boolean;
const
    sAlgs : array [TIcsAuthDigestAlgorithm] of String = ('MD5', 'MD5-sess');
    sQops : array [TIcsAuthDigestQop] of String = ('auth', 'auth-int');
var
    LPos: Integer;
    LStr: String;
begin
    { Check for more than one algorithm, we have to pick one }
    LPos := Pos(',', Info.Algorithm);
    if LPos > 0 then begin
       { Assume two possible values, "MD5" and "MD5-sess" }
        LStr := Copy(Info.Algorithm, 1, LPos - 1);
        if LStr = sAlgs[APreferredAlgo] then
            Info.Algorithm := LStr
        else if Copy(Info.Algorithm, LPos + 1, MaxInt) = sAlgs[APreferredAlgo] then
            Info.Algorithm := sAlgs[APreferredAlgo]
        else
            Info.Algorithm := LStr; // Pick first, it should be "MD5"
    end;

    { Check for more than one Qop, we have to pick one }
    LPos := Pos(',', Info.Qop);
    if LPos > 0 then begin
       { Assume two possible values, "auth" and "auth-int" }
        LStr := Copy(Info.Qop, 1, LPos - 1);
        if LStr = sQops[APreferredQop] then
            Info.Qop := LStr
        else if Copy(Info.Qop, LPos + 1, MaxInt) = sQops[APreferredQop] then
            Info.Qop := sQops[APreferredQop]
        else
            Info.Qop := LStr; // Pick first, it should be "auth"
    end;

    Result := (Info.Realm <> '') and
              ((Info.Algorithm = '') or (Info.Algorithm = sAlgs[adalgMD5]) or
               (Info.Algorithm = sAlgs[adalgMD5sess])) and
              { "auth-int" is currently not supported, "MD5-sess" is untested }
              ((Info.qop = '') or (Info.qop = sQops[adqopAuth]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AuthDigestGenerateRequest(
    const UserName, Password, HttpMethod, Uri, Realm, Qop, Nonce, Opaque,
    Algorithm : String; const EntityHash: THashHex;
    var Cnonce: String; var Nc: Integer): String;
var
    HA1       : THashHex;
    Response  : THashHex;
    NcHex     : String;
begin
    if Qop <> '' then begin
        NcHex  := IntToHex(Nc, 8);
        Cnonce := IntToHex(IcsRandomInt(MaxInt), 8);
    end
    else
        CNonce := '';
    AuthDigestCalcHA1(Algorithm,
                      AnsiString(UserName),
                      AnsiString(Realm),
                      AnsiString(Password), // UTF-8 cast ?
                      AnsiString(Nonce),
                      AnsiString(CNonce),
                      HA1);
    AuthDigestCalcResponse(HA1,
                           AnsiString(Nonce),
                           AnsiString(NcHex),
                           AnsiString(CNonce),
                           AnsiString(Qop),
                           AnsiString(HttpMethod),
                           AnsiString(Uri),
                           EntityHash, // used only with auth-int!
                           Response);
    Result := 'username="'   + UserName     + '"' +
              ',realm="'     + Realm        + '"' +
              ',nonce="'     + Nonce        + '"' +
              ',uri="'       + Uri          + '"' +
              ',response="'  + String(Response)  + '"';
    if Opaque <> '' then
        Result := Result +
              ',opaque="'    + Opaque       + '"';
    if Algorithm = 'MD5-sess' then
        Result := Result +
             ',algorithm='   + Algorithm;
    if Qop <> '' then
        Result := Result +
              ',qop='        + Qop          +
              ',nc='         + NcHex        +
              ',cnonce="'    + CNonce       + '"';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AuthDigestGenerateRequest(
    const UserName, Password, HttpMethod, Uri : String;
    const EntityHash : THashHex;
    var Info: TAuthDigestResponseInfo): String;
var
    HA1       : THashHex;
    Response  : THashHex;
    NcHex     : String;
    CNonce    : String;
begin
    if Info.qop <> '' then begin
        NcHex  := IntToHex(Info.Nc, 8);
        CNonce := IntToHex(IcsRandomInt(MaxInt), 8);
    end;
    AuthDigestCalcHA1(Info.Algorithm,
                      AnsiString(UserName),
                      AnsiString(Info.Realm),
                      AnsiString(Password),   // UTF-8 cast ?
                      AnsiString(Info.Nonce),
                      AnsiString(CNonce),
                      HA1);
    AuthDigestCalcResponse(HA1,
                           AnsiString(Info.Nonce),
                           AnsiString(NcHex),
                           AnsiString(CNonce),
                           AnsiString(Info.Qop),
                           AnsiString(HttpMethod),
                           AnsiString(Uri),
                           EntityHash, // used only with auth-int!
                           Response);
    Result := 'username="'   + UserName          + '"' +
              ',realm="'     + Info.Realm        + '"' +
              ',nonce="'     + Info.Nonce        + '"' +
              ',uri="'       + Uri               + '"' +
              ',response="'  + String(Response)  + '"';
    if Info.Opaque <> '' then
        Result := Result +
              ',opaque="'    + Info.Opaque       + '"';
    if Info.Algorithm = 'MD5-sess' then
        Result := Result +
             ',algorithm='   + Info.Algorithm;
    if Info.Qop <> '' then
        Result := Result +
              ',qop='        + Info.Qop          +
              ',nc='         + NcHex             +
              ',cnonce="'    + CNonce            + '"' ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AuthDigestGenerateChallenge(
    DigestMethod: TAuthDigestMethod; Secret: Int64; const Realm,
    Domain : String; Stale: Boolean; var Nonce, Opaque: String): String;
var
    I     : Integer;
    iCh   : Integer;
    Qop   : String;
begin
    { Generate the opaque, we use to generate the nonce hash }
    SetLength(Opaque, 34);
    for I := 1 to Length(Opaque) do begin
        while TRUE do begin
            iCh := IcsRandomInt(123);
            case iCh of
                48..57, 65..90, 97..122 :
                    begin
                        Opaque[I] := Char(iCh);
                        Break;
                    end;
            end
        end;
    end;

    Nonce := String(AuthDigestGenerateIcsNonce(Now, Secret,
                                         AnsiString(Opaque),
                                         AnsiString(Realm)));

    case DigestMethod of
        daAuth:    Qop := 'auth';
        daAuthInt: Qop := 'auth-int';
        daBoth:    Qop := 'auth,auth-int';
    end;

    Result := 'realm="'   + Realm          + '"' +
              ',qop="'    + Qop            + '"' +
              ',nonce="'  + Nonce          + '"' +
              ',opaque="' + Opaque         + '"';
    if Stale then
        Result := Result + ', stale="true"';
    if Domain <> '' then
        Result := Result + ', domain="' + Domain + '"';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
