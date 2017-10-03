{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Server-side NTLM, validation of user credentials using Windows SSPI.
Creation:     Sep 04, 2006
Version:      8.00
Legal issues: Copyright (C) 2005-2011 by Arno Garrels, Berlin, Germany,
              contact: <arno.garrels@gmx.de>

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

              Development of this unit has been sponsored by Fastream
              Technologies (www.fastream.com) and donated to ICS, thanks.

              **If you compile with BCB personality define SECURITY_WIN32
             in the project options**.
History:
Sep 11, 2006 V1.01 A. Garrels added func ValidateUserCredentials() which allows
             validation of user credentials locally. Also added func
             ImpersonateContext as well as RevertContext. ImpersonateContext
             will make the calling thread run in the security context of the
             authenticated user.
Apr 25, 2008 V1.02 A. Garrels, some changes to prepare code for Unicode.
Apr 30, 2008 V1.03 A. Garrels moved the call to LoadSecPackage from
             initialization section to TNtlmAuthSession's constructor.
Nov 15, 2010 V1.0.4 Fix in function UCS2ToString that is used by ANSI compilers.
Jun 16, 2011 V1.05 PatchINT3 function adjusted since signature of
             Windows.WriteProcessMemory has changed.
Jul 22, 2011 V1.06 Arno - OEM NTLM changes.
Feb 17, 2012 V1.07 Arno added NTLMv2 and NTLMv2 session security (basics),
             to method ValidateUserCredintials read comment "HowTo NTLMv2" in
             OverbyteIcsNtlmMsgs.pas.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsNtlmSsp;
{$B-}                                 { Enable partial Boolean evaluation   }
{$T-}                                 { Untyped Pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long Strings                    }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$IFDEF COMPILER12_UP}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$WARN SYMBOL_PLATFORM   OFF}
{#$DEFINE DEBUG_EXCEPTIONS}

interface

{$IFDEF MSWINDOWS}

uses
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsMimeUtils,
    OverbyteIcsNtlmMsgs,
    OverbyteIcsSspi;
const
    {$EXTERNALSYM WC_NO_BEST_FIT_CHARS}
    WC_NO_BEST_FIT_CHARS = $00000400; // do not use best fit chars

type
    UCS2String = AnsiString;
    PNTLM_Message3  = ^TNTLM_Message3;
    PNTLM_Message1  = ^TNTLM_Message1;
    TNtlmState  = (lsNone, lsInAuth, lsDoneOK, lsDoneErr);
    TNtlmSessionBeforeValidate = procedure(Sender: TObject; var Allow: Boolean) of object;
    TNtlmAuthSession = class(TObject)
    private
        FPSFT              : PSecurityFunctionTable;
        FHCred             : TCredHandle;
        FHCtx              : TSecHandle;
        FHaveCredHandle    : Boolean;
        FHaveCtxHandle     : Boolean;
        FState             : TNtlmState;
        FUsername          : String;
        FDomain            : String;
        FHost              : String;
        FNtlmMessage       : String;
        FAuthError         : Integer;
        FLmCompatLevel     : LongWord;  { V1.07 }
        FOnBeforeValidate  : TNtlmSessionBeforeValidate;
   protected
        procedure   NtlmMsg3GetAttributes(const NtlmMsg3: AnsiString);
        function    NtlmMsgGetType(const NtlmMsg: AnsiString): Integer;
        function    NtlmAccept(const InBuffer: AnsiString): AnsiString;
        function    NtlmErrorDesc(ErrCode: Integer): String;
   public
        constructor Create;
        destructor  Destroy; override;
        function    AuthErrorDesc: String;
        function    GetUserFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
        function    GetAuthorityFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
        function    ProcessNtlmMsg(const InBuffer: String): Boolean;
        function    ValidateUserCredentials(const AUser, APassword, ADomain: String;
                                            CleanUpSession: Boolean): Boolean;
        function    ImpersonateContext: Boolean;
        function    RevertContext: Boolean;
        procedure   CleanUpLogonSession;
        property    NtlmMessage : String read FNtlmMessage write FNtlmMessage;
        property    Username : String read FUsername;
        property    Domain : String read FDomain;
        property    Host : String read FHost;
        property    HCread : TCredHandle read FHCred;
        property    HCtx  : TSecHandle  read  FHCtx;
        property    State : TNtlmState read FState;
        property    LmCompatLevel : LongWord read FLmCompatLevel write FLmCompatLevel; { V1.07 }
        property    OnBeforeValidate: TNtlmSessionBeforeValidate read FOnBeforeValidate write FOnBeforeValidate;
    end;

//procedure LoadSecPackage;
//procedure UnloadSecPackage;

{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

var
    SecPackageLock      : TRtlCriticalSection;
    cbMaxMessage        : Cardinal;
    PSFT                : PSecurityFunctionTable;
    SecPackageHandle    : THandle;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get rid of some ntdll.DbgBreakPoints M$ forgot to remove from their DLLs  }
{ popping up the CPU window. Written by Francois Piette, published 2002 in  }
{ HowToDoThings website, based on code written by Pete Morris.              }
{ Tiny change by myself - to be very correct ;-)                            }
procedure PatchINT3;
var
    NOP          : Byte;
    NTDLL        : THandle;
{$IFDEF COMPILER16_UP}
    BytesWritten : NativeUint;
{$ELSE}
    BytesWritten : DWORD;
{$ENDIF}
    Address      : Pointer;
begin
    if Win32Platform <> VER_PLATFORM_WIN32_NT then Exit;
    NTDLL := GetModuleHandle('NTDLL.DLL');
    if NTDLL = 0 then Exit;
    Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
    if Address = nil then Exit;
    try
        if Byte(Address^) <> $CC then Exit;
        NOP := $90;
        if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
          (BytesWritten = 1) then
            FlushInstructionCache(GetCurrentProcess, Address, 1);
    except
        //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
        on EAccessViolation do ;
        else
            raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadSecPackage;
type
    INIT_SECURITY_ENTRYPOINT_FN = function : PSecurityFunctionTable;
var
    pInit   : INIT_SECURITY_ENTRYPOINT_FN;
    pkgInfo : PSecPkgInfo;
    sStatus : TSecurityStatus;
begin
    EnterCriticalSection(SecPackageLock);
    try
        if SecPackageHandle <> 0 then
            Exit;

        SecPackageHandle := LoadLibrary('security.dll');     // Winnt up
        if SecPackageHandle = 0 then
            SecPackageHandle := LoadLibrary('secur32.dll');  // Win9x
        if SecPackageHandle <> 0 then
        try
            pInit := GetProcAddress(SecPackageHandle, SECURITY_ENTRYPOINT);

            if not Assigned(pInit) then
                raise Exception.Create('Couldn''t init security package');

            PSFT := pInit;
{$IFNDEF UNICODE}
            sStatus := PSFT^.QuerySecurityPackageInfoA(NTLMSP_NAME_A, pkgInfo);
{$ELSE}
            sStatus := PSFT^.QuerySecurityPackageInfoW(NTLMSP_NAME,  pkgInfo);
{$ENDIF}
            if sStatus < 0 then
                raise Exception.CreateFmt('Couldn''t find package info for ' +
                                          'NTLM, error 0x%x', [sStatus]);

            cbMaxMessage := pkgInfo^.cbMaxToken;

            PSFT^.FreeContextBuffer(pkgInfo);
        except
            if SecPackageHandle <> 0 then
                FreeLibrary(SecPackageHandle);
            raise
        end
    finally
        LeaveCriticalSection(SecPackageLock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnloadSecPackage;
begin
    if SecPackageHandle = 0 then
        Exit;
    EnterCriticalSection(SecPackageLock);
    try
        FreeLibrary(SecPackageHandle);
        SecPackageHandle := 0;
    finally
        LeaveCriticalSection(SecPackageLock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TNtlmAuthSession.Create;
begin
    inherited Create;
    LoadSecPackage;
    FPSFT := PSFT;
    CleanUpLogonSession;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TNtlmAuthSession.Destroy;
begin
    CleanUpLogonSession;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ValidateUserCredentials(
  const AUser, APassword, ADomain: String;
  CleanUpSession: Boolean): Boolean;
var
    NtlmMsg2Info : TNTLM_Msg2_Info;
begin
    Result := ProcessNtlmMsg(NtlmGetMessage1('', ADomain, FLmCompatLevel)); { V1.07 }
    if not (FState in [lsDoneOk, lsDoneErr]) then begin
        NtlmMsg2Info := NtlmGetMessage2(FNtlmMessage);
        Result := ProcessNtlmMsg(NtlmGetMessage3(ADomain, '',
                                                 AUser, APassword,
                                                 NtlmMsg2Info,     { V1.07 }
                                                 CP_ACP,
                                                 FLmCompatLevel)); { V1.07 }
        if CleanUpSession then
            CleanUpLogonSession;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNtlmAuthSession.CleanUpLogonSession;
begin
    if FHaveCtxHandle then
        FPSFT^.DeleteSecurityContext(@FHCtx);
    if FHaveCredHandle then
        FPSFT^.FreeCredentialHandle(@FHCred);
    FHaveCredHandle := FALSE;
    FHaveCtxHandle  := FALSE;
    FUserName       := '';
    FDomain         := '';
    FHost           := '';
    FNtlmMessage    := '';
    FAuthError      := 0;
    FState          := lsNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UCS2ToString(const S: UCS2String): AnsiString;
var
    Len   : Integer;
    WcCnt : Integer;
begin
    Result := '';
    if S <> '' then
    begin
      WcCnt := Length(S) div SizeOf(WideChar);
      if WcCnt > 0 then
      begin
          Len := WideCharToMultiByte(CP_ACP, 0, Pointer(S), WcCnt, nil, 0,
                                     nil, nil);
          SetLength(Result, Len);
          WideCharToMultiByte(CP_ACP, 0, Pointer(S), WcCnt, Pointer(Result), Len,
                              nil, nil);
      end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The name of a server or domain that authenticated the connection.         }
function TNtlmAuthSession.GetAuthorityFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
var
    CtxAutority: TSecPkgContextAuthority;
    Sec: Integer;
begin
    if not FHaveCtxHandle then
        Exit;
{$IFNDEF UNICODE}
    Sec := FPSFT^.QueryContextAttributesA(@FHCtx, SECPKG_ATTR_AUTHORITY, @CtxAutority);
{$ELSE}
    Sec := FPSFT^.QueryContextAttributesW(@FHCtx, SECPKG_ATTR_AUTHORITY, @CtxAutority);
{$ENDIF}
    if Sec = 0 then
        Result := CtxAutority.sAuthorityName;
    FPSFT^.FreeContextBuffer(@CtxAutority);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.GetUserFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
var
    ContextNames : TSecPkgContextNames;
    Sec: Integer;
begin
    if not FHaveCtxHandle then
        Exit;
{$IFNDEF UNICODE}
    Sec := FPSFT^.QueryContextAttributesA(@FHCtx, SECPKG_ATTR_NAMES, @ContextNames);
{$ELSE}
    Sec := FPSFT^.QueryContextAttributesW(@FHCtx, SECPKG_ATTR_NAMES, @ContextNames);
{$ENDIF}
    if Sec = 0 then
        Result := ContextNames.sUserName;
    FPSFT^.FreeContextBuffer(@ContextNames);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNtlmAuthSession.NtlmMsg3GetAttributes(const NtlmMsg3: AnsiString);
var
    PMsg : PNTLM_Message3;
    Len, Offs : Integer;
begin
    if Length(NtlmMsg3) < SizeOf(TNTLM_Message3) then
        Exit;
    PMsg := @NtlmMsg3[1];

    { Extract user name }
    Len  := PMsg^.User.Length;
    Offs := PMsg^.User.Offset;
    if (Len > 0) and (Len <= 256 * 2) and  // ? Max. length Win logon names = 104
       (Offs + 1 + Len <= Length(NtlmMsg3)) then begin
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then begin
        {$IFDEF UNICODE}
            SetLength(FUsername, Len div 2);
            Move(NtlmMsg3[Offs + 1], FUsername[1], Len);
        {$ELSE}
            FUsername := UCS2ToString(Copy(NtlmMsg3, Offs + 1, Len));
        {$ENDIF}
        end
        else
            FUsername := Copy(NtlmMsg3, Offs + 1, Len);
    end;

    { Extract host }
    Len  := PMsg^.Host.Length;
    Offs := PMsg^.Host.Offset;
    if (Len > 0) and (Len <= 256 * 2) and  // Max. host name length ?
       (Offs + 1 + Len <= Length(NtlmMsg3)) then begin
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then begin
        {$IFDEF UNICODE}
            SetLength(FHost, Len div 2);
            Move(NtlmMsg3[Offs + 1], FHost[1], Len);
        {$ELSE}
            FHost := UCS2ToString(Copy(NtlmMsg3, Offs + 1, Len));
        {$ENDIF}
        end
        else
            FHost := Copy(NtlmMsg3, Offs + 1, Len);
    end;

    { Extract Domain }
    Len  := PMsg^.Domain.Length;
    Offs := PMsg^.Domain.Offset;
    if (Len > 0) and (Len <= 256 * 2) and  // Max. domain name length ?
       (Offs + 1 + Len <= Length(NtlmMsg3)) then begin
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then begin
        {$IFDEF UNICODE}
            SetLength(FDomain, Len div 2);
            Move(NtlmMsg3[Offs + 1], FDomain[1], Len);
        {$ELSE}
            FDomain := UCS2ToString(Copy(NtlmMsg3, Offs + 1, Len));
        {$ENDIF}
        end
        else
            FDomain := Copy(NtlmMsg3, Offs + 1, Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmMsgGetType(const NtlmMsg: AnsiString): Integer;
var
    PMsg : PNTLM_Message1;
begin
    Result := -1;
    if Length(NtlmMsg) < SizeOf(TNTLM_Message1) then
        Exit;
    PMsg   := @NtlmMsg[1];
    Result := PMsg^.MsgType;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ProcessNtlmMsg(const InBuffer: String): Boolean;
begin
    Result := False;
    if Length(InBuffer) = 0 then begin
        CleanupLogonSession;
        Exit;
    end;
    FNtlmMessage := String(NtlmAccept(Base64Decode(AnsiString(InBuffer))));
    Result       := FState = lsDoneOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmAccept(const InBuffer: AnsiString): AnsiString;
var
    Sec                 : TSecurityStatus;
    Lifetime            : LARGE_INTEGER;
    OutBuffDesc         : TSecBufferDesc;
    InBuffDesc          : TSecBufferDesc;
    InSecBuff           : TSecBuffer;
    OutSecBuff          : TSecBuffer;
    ContextAttr         : Cardinal;
    pHCtx               : PCtxtHandle;
    Allow               : Boolean;
    MsgType             : Integer;
begin
    try
        //InBufDec := Base64Decode(InBuffer);
        if Length(InBuffer) = 0 then begin
            FAuthError := Integer(SEC_E_INVALID_TOKEN);
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('InBuffer empty');
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
        end;
        MsgType := NtlmMsgGetType(InBuffer);
        if not ((MsgType = 3) and (FState = lsInAuth)) then
            CleanupLogonSession;

        if FState = lsNone then begin
{$IFNDEF UNICODE}
            Sec := FPSFT^.AcquireCredentialsHandleA(nil,
                                                   NTLMSP_NAME_A,
                                                   SECPKG_CRED_INBOUND,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   FHCred,
                                                   Lifetime);
{$ELSE}
            Sec := FPSFT^.AcquireCredentialsHandleW(nil,
                                                   NTLMSP_NAME,
                                                   SECPKG_CRED_INBOUND,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   FHCred,
                                                   Lifetime);
{$ENDIF}
            if Sec < 0 then begin
                FAuthError := Sec;
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.CreateFmt('AcquireCredentials failed 0x%x', [Sec]);
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
            FHaveCredHandle := TRUE
        end;

        // prepare output buffer
        OutBuffDesc.ulVersion := 0;
        OutBuffDesc.cBuffers  := 1;
        OutBuffDesc.pBuffers  := @OutSecBuff;

        SetLength(Result, cbMaxMessage);
        OutSecBuff.cbBuffer   := Length(Result);
        OutSecBuff.BufferType := SECBUFFER_TOKEN;
        OutSecBuff.pvBuffer   := @Result[1];

        // prepare input buffer
        InBuffDesc.ulVersion := 0;
        InBuffDesc.cBuffers  := 1;
        InBuffDesc.pBuffers  := @InSecBuff;

        InSecBuff.cbBuffer   := Length(InBuffer);
        InSecBuff.BufferType := SECBUFFER_TOKEN;
        InSecBuff.pvBuffer   := @InBuffer[1];

        if FState = lsNone then
            pHCtx := nil
        else begin
            pHCtx := @FHCtx;
            { We received NTLMMsg3 }
            NtlmMsg3GetAttributes(InBuffer);
            Allow := TRUE;
            if Assigned(FOnBeforeValidate) then
                FOnBeforeValidate(Self, Allow);
            if not Allow then begin
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('User canceled the session');
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
        end;
        Sec := FPSFT^.AcceptSecurityContext(@FHCred,
                                           pHCtx,
                                           @InBuffDesc,
                                           ASC_REQ_SEQUENCE_DETECT{ASC_REQ_DELEGATE}, // context requirements
                                           SECURITY_NATIVE_DREP,
                                           @FHCtx,
                                           @OutBuffDesc,
                                           ContextAttr,
                                           Lifetime);
        if Sec < 0 then begin
            FAuthError := Sec;
        {$IFDEF DEBUG_EXCEPTIONS}
            raise Exception.CreateFmt('Init context failed: 0x%x', [Sec]);
        {$ELSE}
            Result := '';
            FState := lsDoneErr;
            Exit;
        {$ENDIF}
        end;

        FHaveCtxHandle := TRUE;
        if(Sec = SEC_I_COMPLETE_NEEDED) or
          (Sec = SEC_I_COMPLETE_AND_CONTINUE) then begin
            if Assigned(FPSFT^.CompleteAuthToken) then begin
                Sec := FPSFT^.CompleteAuthToken(@FHCtx, @OutBuffDesc);
                if Sec < 0 then begin
                    FAuthError := Sec;
                {$IFDEF DEBUG_EXCEPTIONS}
                    raise Exception.CreateFmt('Complete failed: 0x%x', [Sec]);
                {$ELSE}
                    Result := '';
                    FState := lsDoneErr;
                    Exit;
                {$ENDIF}
                end;
            end
            else begin
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('CompleteAuthToken not supported.');
            {$ELSE}
                Result := '';
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
        end;

        if (Sec <> SEC_I_CONTINUE_NEEDED) and
                   (Sec <> SEC_I_COMPLETE_AND_CONTINUE) then
            FState := lsDoneOK
        else
            if FState = lsNone then
                FState := lsInAuth;

        if FState = lsInAuth then begin
            SetLength(Result, OutSecBuff.cbBuffer);
            Result := Base64Encode(Result);
        end;
    except
        FState := lsDoneErr;
        Result := '';
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.AuthErrorDesc: String;
begin
    Result := NtlmErrorDesc(FAuthError) + ' Error: 0x' + IntToHex(FAuthError, 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmErrorDesc(ErrCode: Integer): String;
begin
    case Cardinal(ErrCode) of
        SEC_E_INCOMPLETE_MESSAGE : Result := 'The supplied message is incomplete.  The signature was not verified.';
        SEC_E_INSUFFICIENT_MEMORY: Result := 'Not enough memory is available to complete this request';
        SEC_E_INTERNAL_ERROR : Result := 'The Local Security Authority cannot be contacted';
        SEC_E_INVALID_HANDLE : Result := 'The handle specified is invalid';
        SEC_E_INVALID_TOKEN : Result := 'The token supplied to the function is invalid';
        SEC_E_LOGON_DENIED : Result := 'The logon attempt failed';
        SEC_E_NO_AUTHENTICATING_AUTHORITY : Result := 'No authority could be contacted for authentication';
        SEC_E_NO_CREDENTIALS : Result := 'No credentials are available in the security package';
        SEC_E_OK : Result := 'Call completed successfully';
        SEC_E_SECURITY_QOS_FAILED : Result := 'The security context could not be established due to a failure in the requested quality of service (e.g. mutual authentication or delegation).';
        SEC_E_UNSUPPORTED_FUNCTION : Result := 'The function requested is not supported';
        else
            Result := 'Unknow error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ImpersonateContext: Boolean;
begin
    if FHaveCtxHandle then
        Result := FPSFT^.ImpersonateSecurityContext(@FHCtx) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.RevertContext: Boolean;
begin
    if FHaveCtxHandle then
        Result := FPSFT^.RevertSecurityContext(@FHCtx) = 0
    else
        Result := FALSE;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    InitializeCriticalSection(SecPackageLock);
    SecPackageHandle := 0;
    //LoadSecPackage;
    if DebugHook <> 0 then
        PatchINT3;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
finalization
    UnloadSecPackage;
    DeleteCriticalSection(SecPackageLock);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF MSWINDOWS}
end.
