{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 01, 2004
Version:      1.00
Description:  This is an implementation of the NTLM authentification
              messages used within HTTP protocol (client side).
              NTLM protocol documentation can be found at:
              http://davenport.sourceforge.net/ntlm.html
Credit:       This code is based on a work by Diego Ariel Degese
              <crapher@utenet.com.ar>. The code was not working at all but
              helped me a lot starting with NTLM.
              Csonka Tibor <bee@rawbite.ro> worked a lot on my original code,
              fixing it and making it work properly.
EMail:        francois.piette@overbyte.be     http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2007 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsNtlmMsgs;

{$I ICSDEFS.INC}
{$R-}
{$Q-}
interface

uses
    SysUtils, IcsDES, IcsMD4, MimeUtil;

const
    IcsNtlmMsgsVersion     = 100;
    CopyRight : String     = ' IcsNtlmMsgs (c) 2004-2007 F. Piette V1.00 ';

const
    Flags_Negotiate_Unicode               = $00000001;
    Flags_Negotiate_OEM                   = $00000002;
    Flags_Request_Target                  = $00000004;
    Flags_Negotiate_8                     = $00000008;  // unknown
    Flags_Negotiate_Sign                  = $00000010;
    Flags_Negotiate_Seal                  = $00000020;
    Flags_Negotiate_Datagram_Style        = $00000040;
    Flags_Negotiate_LAN_Manager_Key       = $00000080;
    Flags_Negotiate_Netware               = $00000100;
    Flags_Negotiate_NTLM                  = $00000200;
    Flags_Negotiate_400                   = $00000400;  // unknown
    Flags_Negotiate_800                   = $00000800;  // unknown
    Flags_Negotiate_Domain_Supplied       = $00001000;
    Flags_Negotiate_Workstation_Supplied  = $00002000;
    Flags_Negotiate_Local_Call            = $00004000;
    Flags_Negotiate_Allways_Sign          = $00008000;
    Flags_Target_Type_Domain              = $00010000;
    Flags_Target_Type_Server              = $00020000;
    Flags_Target_Type_Share               = $00040000;
    Flags_Negotiate_NTLM2_Key             = $00080000;
    Flags_Request_Init_Response           = $00100000;
    Flags_Request_Accept_Response         = $00200000;
    Flags_Request_Non_NT_Session_Key      = $00400000;
    Flags_Negotiate_Target_Info           = $00800000;
    Flags_Negotiate_1000000               = $01000000;  // unknown
    Flags_Negotiate_2000000               = $02000000;  // unknown
    Flags_Negotiate_4000000               = $04000000;  // unknown
    Flags_Negotiate_8000000               = $08000000;  // unknown
    Flags_Negotiate_10000000              = $10000000;  // unknown
    Flags_Negotiate_128_Bit_Encryption    = $20000000;
    Flags_Negotiate_Key_Exchange          = $40000000;
    Flags_Negotiate_56_Bit_Encryption     = $80000000;

    // target information block types
    TIB_Type_Server                       = 1;
    TIB_Type_Domain                       = 2;
    TIB_Type_DNS_Full                     = 3;
    TIB_Type_DNS_Domain                   = 4;
    TIB_Type_ask_microsoft_or_god         = 5;

type
    // security buffer
    TNTLM_SecBuff   = record
        Length        : Word;
        Space         : Word;
        Offset        : Cardinal;
    end;

    // first message
    TNTLM_Message1  = record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        Flags         : Cardinal;
        Domain        : TNTLM_SecBuff;
        Host          : TNTLM_SecBuff;
    end;

    // second message
    TNTLM_Message2  = record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        TargetName    : TNTLM_SecBuff;
        Flags         : Cardinal;
        Challenge     : TArrayOf8Bytes;
        Context       : TArrayOf8Bytes;   // reserved, not used
        TargetInfo    : TNTLM_SecBuff;
    end;

    // interesting information from message 2
    TNTLM_Msg2_Info = record
        SrvRespOk     : boolean;                // server response was ok ?
        Target        : WideString;
        Domain        : WideString;
        Server        : WideString;
        Challenge     : TArrayOf8Bytes;
    end;

    // third message
    TNTLM_Message3  = record
        Protocol      : TArrayOf8Bytes;
        MsgType       : Cardinal;
        LM            : TNTLM_SecBuff;
        NTLM          : TNTLM_SecBuff;
        Domain        : TNTLM_SecBuff;
        User          : TNTLM_SecBuff;
        Host          : TNTLM_SecBuff;
        SessionKey    : TNTLM_SecBuff;
        Flags         : Cardinal;
    end;

function NtlmGetMessage1(const AHost, ADomain: String): String;
function NtlmGetMessage2(const AServerReply: String): TNTLM_Msg2_Info;
function NtlmGetMessage3(const ADomain, AHost, AUser, APassword: String; AChallenge: TArrayOf8Bytes): String;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert an text to a unicode text stored }
function Unicode(const AData: String): String;
var
    I, J : Integer;
begin
    SetLength(Result, Length(AData) * 2);
    J := 1;
    for I := 1 to Length(AData) do begin
        Result[J] := AData[I];
        Inc(J);
        Result[J] := #0;
        Inc(J);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DesEcbEncrypt(
    const AKey  : String;                  // Must be exactly 8 characters
    const AData : TArrayOf8Bytes): String;
var
    i, j, t, bit : Integer;
    XKey         : TArrayOf8Bytes;
begin
    XKey[0] := Byte(AKey[1]);
    XKey[1] := ((Byte(AKey[1]) shl 7) and $FF) or (Byte(AKey[2]) shr 1);
    XKey[2] := ((Byte(AKey[2]) shl 6) and $FF) or (Byte(AKey[3]) shr 2);
    XKey[3] := ((Byte(AKey[3]) shl 5) and $FF) or (Byte(AKey[4]) shr 3);
    XKey[4] := ((Byte(AKey[4]) shl 4) and $FF) or (Byte(AKey[5]) shr 4);
    XKey[5] := ((Byte(AKey[5]) shl 3) and $FF) or (Byte(AKey[6]) shr 5);
    XKey[6] := ((Byte(AKey[6]) shl 2) and $FF) or (Byte(AKey[7]) shr 6);
    XKey[7] := ((Byte(AKey[7]) shl 1) and $FF);

    for i := 0 to 7 do begin
        for j := 1 to 7 do begin
            bit := 0;
            t   := XKey[i] shl j;
            bit := (t xor bit) and 1;
        end;
        XKey[i] := Byte((XKey[i] and $FE) or bit);
    end;
    SetLength(Result, 8);
    DES(AData, PArrayOf8Bytes(@Result[1])^, XKey, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetLMHash(
    const APassword : String;
    const ANonce    : TArrayOf8Bytes): String;
const
    Magic: TArrayOf8Bytes = ($4B, $47, $53, $21, $40, $23, $24, $25);
var
    I        : Integer;
    Pass     : String;
    PassHash : String;
begin
    Pass := Copy(UpperCase(APassword), 1, 14);
    for I := Length(APassword) to 14 do
        Pass := Pass + #0;

    PassHash := DesEcbEncrypt(Copy(Pass, 1, 7),  Magic) +
                DesEcbEncrypt(Copy(Pass, 8, 7), Magic) +
                #0#0#0#0#0;

    Result := DesEcbEncrypt(Copy(PassHash,  1, 7), ANonce) +
              DesEcbEncrypt(Copy(PassHash,  8, 7), ANonce) +
              DesEcbEncrypt(Copy(PassHash, 15, 7), ANonce);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetNTHash(
    const APassword : String;
    const ANonce    : TArrayOf8Bytes): String;
var
    PassHash: String;
begin
    PassHash := MD4String(Unicode(APassword)) + #0#0#0#0#0;
    Result   := DesEcbEncrypt(Copy(PassHash,  1, 7), ANonce) +
                DesEcbEncrypt(Copy(PassHash,  8, 7), ANonce) +
                DesEcbEncrypt(Copy(PassHash, 15, 7), ANonce);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage1(const AHost, ADomain: String): String;
var
    Msg         : TNTLM_Message1;
    MessageAux  : String;
    Host        : String;
    Domain      : String;
begin
    Host   := UpperCase(AHost);
    Domain := UpperCase(ADomain);

    FillChar(Msg, SizeOf(Msg), #0);
    // signature
    Move('NTLMSSP' + #0, Msg.Protocol, 8);

    // message type (negotiate)
    Msg.MsgType := 1;

    // prepare flags
    Msg.Flags := Flags_Negotiate_Unicode or
                 Flags_Negotiate_OEM or
                 Flags_Request_Target or
                 Flags_Negotiate_NTLM or
                 Flags_Negotiate_Allways_Sign { or
                 Flags_Negotiate_NTLM2_Key};

    // host and/or domain supplied ?
    // host
    if Length(Host) > 0 then
         Msg.Flags := Msg.Flags or Flags_Negotiate_Workstation_Supplied;

    // domain
    if Length(Domain) > 0 then
         Msg.Flags := Msg.Flags or Flags_Negotiate_Domain_Supplied;

    // host
    Msg.Host.Length := Length(Host);
    Msg.Host.Space  := Msg.Host.Length;

    if Msg.Host.Length > 0 then
        Msg.Host.Offset := $20
    else
        Msg.Host.Offset := 0;

    // domain
    Msg.Domain.Length := Length(Domain);
    Msg.Domain.Space  := Msg.Domain.Length;

    if Msg.Domain.Length > 0 then
         Msg.Domain.Offset := Msg.Host.Offset + Msg.Domain.Length
    else
         Msg.Domain.Offset := 0;

    SetLength(MessageAux, SizeOf(Msg));
    Move(Msg, MessageAux[1], SizeOf(Msg));
    MessageAux := MessageAux + Host + Domain;

    Result := Base64Encode(MessageAux);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage2(const AServerReply: String): TNTLM_Msg2_Info;
var
    Msg         : TNTLM_Message2;
    MsgInfo     : TNTLM_Msg2_Info;
    InfoType    : Word;
    InfoLength  : Word;
    InfoStr     : WideString;
    I           : Integer;
    NTLMReply   : String;
begin
    NTLMReply  := Base64Decode(AServerReply);

    if Length(AServerReply) > 0 then begin
        // we have a response
        MsgInfo.SrvRespOk := TRUE;
        Move(NTLMReply[1], Msg, SizeOf(Msg));
        // extract target
        MsgInfo.Target := Copy(NTLMReply, Msg.TargetName.Offset + 1,
                               Msg.TargetName.Length);
        // extract challenge
        Move(Msg.Challenge, MsgInfo.Challenge, SizeOf(Msg.Challenge));
        // let's extract the other information
        I := Msg.TargetInfo.Offset + 1;
        // loop through target information blocks
        while I < Length(NTLMReply) do begin
            // extract type
            Move(NTLMReply[I], InfoType, SizeOf(InfoType));
            I := I + SizeOf(InfoType);
            // extract length
            Move(NTLMReply[I], InfoLength, SizeOf(InfoLength));
            I := I + SizeOf(InfoLength);
            // terminator block ?
            if (InfoType = 0) and (InfoLength = 0) then
                break
            else begin
                // extract information
                InfoStr := Copy(NTLMReply, I, InfoLength);
                if InfoType = TIB_Type_Server then
                    MsgInfo.Server := InfoStr
                else if InfoType = TIB_Type_Domain then
                    MsgInfo.Domain := InfoStr;
                // jump to next block
                I := I + InfoLength;
            end;
        end;
    end
    else begin
        // no response from server
        MsgInfo.SrvRespOk := FALSE;
    end;
    Result := MsgInfo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function NtlmGetMessage3(const ADomain, AHost, AUser,
  APassword: String; AChallenge: TArrayOf8Bytes): String;
var
    Msg        : TNTLM_Message3;
    MessageAux : String;
    LM_Resp    : String[30];
    NT_Resp    : String[30];
    UDomain    : String;
    UHost      : String;
    UUser      : String;
begin
    UDomain    := Unicode(ADomain);
    UHost      := Unicode(AHost);
    UUser      := Unicode(AUser);
    FillChar(Msg, SizeOf(Msg), #0);
    Move('NTLMSSP' + #0, Msg.Protocol, 8);
    Msg.MsgType := 3;

    // prepare domain
    Msg.Domain.Length := Length(UDomain);
    Msg.Domain.Space  := Msg.Domain.Length;
    Msg.Domain.Offset := $40;

    // prepare user
    Msg.User.Length   := Length(UUser);
    Msg.User.Space    := Msg.User.Length;
    Msg.User.Offset   := Msg.Domain.Offset + Msg.Domain.Length;

    // preapre host
    Msg.Host.Length   := Length(UHost);
    Msg.Host.Space    := Msg.Host.Length;
    Msg.Host.Offset   := Msg.User.Offset + Msg.User.Length;

    // prepare LM and NTLM responses
    Msg.LM.Length     := $18;
    Msg.LM.Space      := Msg.LM.Length;
    Msg.LM.Offset     := Msg.Host.Offset + Msg.Host.Length;

    Msg.NTLM.Length   := $18;
    Msg.NTLM.Space    := Msg.LM.Length;
    Msg.NTLM.Offset   := Msg.LM.Offset + Msg.LM.Length;

    // no session key
    Msg.SessionKey.Length := 0;
    Msg.SessionKey.Space  := 0;
    Msg.SessionKey.Offset := 0;

    // prepare flags
    Msg.Flags := Flags_Negotiate_Unicode or
                 Flags_Request_Target or
                 Flags_Negotiate_NTLM or
                 Flags_Negotiate_Allways_Sign {or
                 Flags_Negotiate_NTLM2_Key};

    LM_Resp := NtlmGetLMHash(APassword, AChallenge);
    NT_Resp := NtlmGetNTHash(APassword, AChallenge);

    SetLength(MessageAux, SizeOf(Msg));
    Move(Msg, MessageAux[1], SizeOf(Msg));

    MessageAux := MessageAux + UDomain + UUser + UHost + LM_Resp + NT_Resp;

    Result := Base64Encode(MessageAux);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
