{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Windows API interface Unit for Object Pascal.
              Includes portions of the original file sspi.h as well as security.h.
Creation:     Jan 11, 2006
Version:      8.00
Legal issues: Portions Copyright (C) 2006 by Arno Garrels, Berlin, Germany,
              contact: <arno.garrels@gmx.de>
              Portions created by Microsoft are Copyright (C) 1995-2001
              Microsoft Corporation. All Rights Reserved.
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
History:
Sep 04, 2006 V1.01 Reworked by A.Garrels in order to support BCB as well as
             Unicode. Development of this version has been sponsored by Fastream
             Technologies (www.fastream.com) and donated to ICS, thanks.
             **If you compile with BCB personality define SECURITY_WIN32
             in the project options**
Apr 25, 2008 V1.0.2 A.Garrels some changes to prepare code for Unicode;
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSspi;

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

interface

{$IFDEF MSWINDOWS}

uses
  {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF};
  
{$HPPEMIT '#include <security.h>'}
{$HPPEMIT '#include <sspi.h>'}

{ security.h }
const
  {$EXTERNALSYM NTLMSP_NAME_A}
  NTLMSP_NAME_A  : PChar     = 'NTLM';
  {$EXTERNALSYM NTLMSP_NAME}
  NTLMSP_NAME    : PWideChar = 'NTLM';

  { Some from Winerror.h }
  {$EXTERNALSYM SEC_I_CONTINUE_NEEDED}
  SEC_I_CONTINUE_NEEDED         = $00090312;
  {$EXTERNALSYM SEC_I_COMPLETE_NEEDED}
  SEC_I_COMPLETE_NEEDED         = $00090313;
  {$EXTERNALSYM SEC_I_COMPLETE_AND_CONTINUE}
  SEC_I_COMPLETE_AND_CONTINUE   = $00090314;

  {$EXTERNALSYM SEC_E_INCOMPLETE_MESSAGE}
  SEC_E_INCOMPLETE_MESSAGE      = $80090318;
  {$EXTERNALSYM SEC_E_INSUFFICIENT_MEMORY}
  SEC_E_INSUFFICIENT_MEMORY     = $80090300;
  {$EXTERNALSYM SEC_E_INTERNAL_ERROR}
  SEC_E_INTERNAL_ERROR          = $80090304;
  {$EXTERNALSYM SEC_E_INVALID_HANDLE}
  SEC_E_INVALID_HANDLE          = $80100003;
  {$EXTERNALSYM SEC_E_INVALID_TOKEN}
  SEC_E_INVALID_TOKEN           = $80090308;
  {$EXTERNALSYM SEC_E_LOGON_DENIED}
  SEC_E_LOGON_DENIED            = $8009030C;
  {$EXTERNALSYM SEC_E_NO_AUTHENTICATING_AUTHORITY}
  SEC_E_NO_AUTHENTICATING_AUTHORITY = $80090311;
  {$EXTERNALSYM SEC_E_NO_CREDENTIALS}
  SEC_E_NO_CREDENTIALS          = $8009030E;
  {$EXTERNALSYM SEC_E_OK}
  SEC_E_OK                      = $00000000;
  {$EXTERNALSYM SEC_E_SECURITY_QOS_FAILED}
  SEC_E_SECURITY_QOS_FAILED     = $80090332;
  {$EXTERNALSYM SEC_E_UNSUPPORTED_FUNCTION}
  SEC_E_UNSUPPORTED_FUNCTION    = $80090302;
  {$EXTERNALSYM SEC_E_OUT_OF_SEQUENCE}
  SEC_E_OUT_OF_SEQUENCE         = $80090310;

  {$EXTERNALSYM STATUS_LOGON_FAILURE}
  STATUS_LOGON_FAILURE          = $C000006D;

{ sspi.h }

const
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_ANSI}
  SEC_WINNT_AUTH_IDENTITY_ANSI  = $01;
  {$EXTERNALSYM SECPKG_CRED_INBOUND}
  SECPKG_CRED_INBOUND           = $00000001;
  {$EXTERNALSYM SECPKG_CRED_OUTBOUND}
  SECPKG_CRED_OUTBOUND          = $00000002;
  {$EXTERNALSYM SECPKG_CRED_BOTH}
  SECPKG_CRED_BOTH              = $00000003;
  {$EXTERNALSYM SECPKG_CRED_DEFAULT}
  SECPKG_CRED_DEFAULT           = $00000004;
  {$EXTERNALSYM SECPKG_CRED_RESERVED}
  SECPKG_CRED_RESERVED          = $F0000000;

  {$EXTERNALSYM SECBUFFER_VERSION}
  SECBUFFER_VERSION             = 0;
  {$EXTERNALSYM SECBUFFER_EMPTY}
  SECBUFFER_EMPTY               = 0;            // Undefined, replaced by provider
  {$EXTERNALSYM SECBUFFER_DATA}
  SECBUFFER_DATA                = 1;            // Packet data
  {$EXTERNALSYM SECBUFFER_TOKEN}
  SECBUFFER_TOKEN               = 2;            // Security token
  {$EXTERNALSYM SECBUFFER_PKG_PARAMS}
  SECBUFFER_PKG_PARAMS          = 3;            // Package specific parameters
  {$EXTERNALSYM SECBUFFER_MISSING}
  SECBUFFER_MISSING             = 4;            // Missing Data indicator
  {$EXTERNALSYM SECBUFFER_EXTRA}
  SECBUFFER_EXTRA               = 5;            // Extra data
  {$EXTERNALSYM SECBUFFER_STREAM_TRAILER}
  SECBUFFER_STREAM_TRAILER      = 6;            // Security Trailer
  {$EXTERNALSYM SECBUFFER_STREAM_HEADER}
  SECBUFFER_STREAM_HEADER       = 7;            // Security Header
  {$EXTERNALSYM SECBUFFER_NEGOTIATION_INFO}
  SECBUFFER_NEGOTIATION_INFO    = 8;            // Hints from the negotiation pkg
  {$EXTERNALSYM SECBUFFER_PADDING}
  SECBUFFER_PADDING             = 9;            // non-data padding
  {$EXTERNALSYM SECBUFFER_STREAM}
  SECBUFFER_STREAM              = 10;           // whole encrypted message
  {$EXTERNALSYM SECBUFFER_ATTRMASK}
  SECBUFFER_ATTRMASK            = $F0000000;
  {$EXTERNALSYM SECBUFFER_READONLY}
  SECBUFFER_READONLY            = $80000000;    // Buffer is read-only
  {$EXTERNALSYM SECBUFFER_RESERVED}
  SECBUFFER_RESERVED            = $40000000;

  {$EXTERNALSYM SECURITY_NATIVE_DREP}
  SECURITY_NATIVE_DREP          = $00000010;
  {$EXTERNALSYM SECURITY_NETWORK_DREP}
  SECURITY_NETWORK_DREP         = $00000000;


  {$EXTERNALSYM SECPKG_ATTR_NAMES}
  SECPKG_ATTR_NAMES            = 1;
  {$EXTERNALSYM SECPKG_ATTR_AUTHORITY}
  SECPKG_ATTR_AUTHORITY        = 6;

  {$EXTERNALSYM ASC_REQ_DELEGATE}
  ASC_REQ_DELEGATE              = $00000001;
  {$EXTERNALSYM ASC_REQ_MUTUAL_AUTH}
  ASC_REQ_MUTUAL_AUTH           = $00000002;
  {$EXTERNALSYM ASC_REQ_REPLAY_DETECT}
  ASC_REQ_REPLAY_DETECT         = $00000004;
  {$EXTERNALSYM ASC_REQ_SEQUENCE_DETECT}
  ASC_REQ_SEQUENCE_DETECT       = $00000008;
  {$EXTERNALSYM ASC_REQ_CONFIDENTIALITY}
  ASC_REQ_CONFIDENTIALITY       = $00000010;
  {$EXTERNALSYM ASC_REQ_USE_SESSION_KEY}
  ASC_REQ_USE_SESSION_KEY       = $00000020;
  {$EXTERNALSYM ASC_REQ_ALLOCATE_MEMORY}
  ASC_REQ_ALLOCATE_MEMORY       = $00000100;
  {$EXTERNALSYM ASC_REQ_USE_DCE_STYLE}
  ASC_REQ_USE_DCE_STYLE         = $00000200;
  {$EXTERNALSYM ASC_REQ_DATAGRAM}
  ASC_REQ_DATAGRAM              = $00000400;
  {$EXTERNALSYM ASC_REQ_CONNECTION}
  ASC_REQ_CONNECTION            = $00000800;
  {$EXTERNALSYM ASC_REQ_CALL_LEVEL}
  ASC_REQ_CALL_LEVEL            = $00001000;
  {$EXTERNALSYM ASC_REQ_EXTENDED_ERROR}
  ASC_REQ_EXTENDED_ERROR        = $00008000;
  {$EXTERNALSYM ASC_REQ_STREAM}
  ASC_REQ_STREAM                = $00010000;
  {$EXTERNALSYM ASC_REQ_INTEGRITY}
  ASC_REQ_INTEGRITY             = $00020000;
  {$EXTERNALSYM ASC_REQ_LICENSING}
  ASC_REQ_LICENSING             = $00040000;
  {$EXTERNALSYM ASC_REQ_IDENTIFY}
  ASC_REQ_IDENTIFY              = $00080000;
  {$EXTERNALSYM ASC_REQ_ALLOW_NULL_SESSION}
  ASC_REQ_ALLOW_NULL_SESSION    = $00100000;
  {$EXTERNALSYM ASC_REQ_ALLOW_NON_USER_LOGONS}
  ASC_REQ_ALLOW_NON_USER_LOGONS = $00200000;
  {$EXTERNALSYM ASC_REQ_ALLOW_CONTEXT_REPLAY}
  ASC_REQ_ALLOW_CONTEXT_REPLAY  = $00400000;
  {$EXTERNALSYM ASC_REQ_FRAGMENT_TO_FIT}
  ASC_REQ_FRAGMENT_TO_FIT       = $00800000;
  {$EXTERNALSYM ASC_REQ_FRAGMENT_SUPPLIED}
  ASC_REQ_FRAGMENT_SUPPLIED     = $00002000;
  {$EXTERNALSYM ASC_REQ_NO_TOKEN}
  ASC_REQ_NO_TOKEN              = $01000000;



type
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR = Longword;

  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_A}
  _SEC_WINNT_AUTH_IDENTITY_A = record
    User            : PAnsiChar;
    UserLength      : Cardinal;
    Domain          : PAnsiChar;
    DomainLength    : Cardinal;
    Password        : PAnsiChar;
    PasswordLength  : Cardinal;
    Flags           : Cardinal;
  end;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_A}
  SEC_WINNT_AUTH_IDENTITY_A = _SEC_WINNT_AUTH_IDENTITY_A;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_A}
  PSEC_WINNT_AUTH_IDENTITY_A = ^SEC_WINNT_AUTH_IDENTITY_A;
  TSecWinntAuthIdentityA = SEC_WINNT_AUTH_IDENTITY_A;
  PSecWinntAuthIdentityA = PSEC_WINNT_AUTH_IDENTITY_A;

  {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY_W}
  _SEC_WINNT_AUTH_IDENTITY_W = record
    User: PWideChar;
    UserLength: Cardinal;
    Domain: PWideChar;
    DomainLength: Cardinal;
    Password: PWideChar;
    PasswordLength: Cardinal;
    Flags: Cardinal;
  end;
  {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY_W}
  SEC_WINNT_AUTH_IDENTITY_W = _SEC_WINNT_AUTH_IDENTITY_W;
  {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY_W}
  PSEC_WINNT_AUTH_IDENTITY_W = ^SEC_WINNT_AUTH_IDENTITY_W;
  TSecWinNTAuthIdentityW = SEC_WINNT_AUTH_IDENTITY_W;
  PSecWinNTAuthIdentityW = PSEC_WINNT_AUTH_IDENTITY_W;

{$IFDEF UNICODE}
    TSecWinNTAuthIdentity = TSecWinNTAuthIdentityW;
    PSecWinNTAuthIdentity = PSecWinNTAuthIdentityW;
    {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY}
    SEC_WINNT_AUTH_IDENTITY = SEC_WINNT_AUTH_IDENTITY_W;
    {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY}
    PSEC_WINNT_AUTH_IDENTITY = PSEC_WINNT_AUTH_IDENTITY_W;
    {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY}
    _SEC_WINNT_AUTH_IDENTITY = _SEC_WINNT_AUTH_IDENTITY_W;
{$ELSE}
    TSecWinNTAuthIdentity = TSecWinNTAuthIdentityA;
    PSecWinNTAuthIdentity = PSecWinNTAuthIdentityA;
    {$EXTERNALSYM SEC_WINNT_AUTH_IDENTITY}
    SEC_WINNT_AUTH_IDENTITY = SEC_WINNT_AUTH_IDENTITY_A;
    {$EXTERNALSYM PSEC_WINNT_AUTH_IDENTITY}
    PSEC_WINNT_AUTH_IDENTITY = PSEC_WINNT_AUTH_IDENTITY_A;
    {$EXTERNALSYM _SEC_WINNT_AUTH_IDENTITY}
    _SEC_WINNT_AUTH_IDENTITY = _SEC_WINNT_AUTH_IDENTITY_A;
{$ENDIF}

  {$EXTERNALSYM _SecHandle}
  _SecHandle = record
    dwLower : ULONG_PTR;
    dwUpper : ULONG_PTR;
  end;
  {$EXTERNALSYM SecHandle}
  SecHandle = _SecHandle;
  {$EXTERNALSYM PSecHandle}
  PSecHandle = ^SecHandle;
  TSecHandle = SecHandle;

  {$EXTERNALSYM _SecBuffer}
  _SecBuffer = record
    cbBuffer    : Cardinal;     // Size of the buffer, in bytes
    BufferType  : Cardinal;     // Type of the buffer (below)
    pvBuffer    : Pointer;      // Pointer to the buffer
  end;
  {$EXTERNALSYM SecBuffer}
  SecBuffer = _SecBuffer;
  {$EXTERNALSYM PSecBuffer}
  PSecBuffer = ^SecBuffer;
  TSecBuffer = SecBuffer;

    {$EXTERNALSYM _SecBufferDesc}
  _SecBufferDesc = record
    ulVersion   : Cardinal;     // Version number
    cBuffers    : Cardinal;     // Number of buffers
    pBuffers    : PSecBuffer;   // Pointer to array of buffers
  end;
  {$EXTERNALSYM SecBufferDesc}
  SecBufferDesc = _SecBufferDesc;
  {$EXTERNALSYM PSecBufferDesc}
  PSecBufferDesc = ^SecBufferDesc;
  TSecBufferDesc = SecBufferDesc;

  TCredHandle = TSecHandle;
  {$EXTERNALSYM CredHandle}
  CredHandle = SecHandle; 
  {$EXTERNALSYM PCredHandle}
  PCredHandle = PSecHandle;

  TCtxtHandle = TSecHandle;
  {$EXTERNALSYM PCtxtHandle}
  PCtxtHandle = PSecHandle;

  {$EXTERNALSYM _SecPkgInfoA}
  _SecPkgInfoA = record
    fCapabilities   : Cardinal;    // Capability bitmask
    wVersion        : Word;        // Version of driver
    wRPCID          : Word;        // ID for RPC Runtime
    cbMaxToken      : Cardinal;    // Size of authentication token (max)
    Name            : PAnsiChar;   // Text name
    Comment         : PAnsiChar;   // Comment
  end;
  {$EXTERNALSYM SecPkgInfoA}
  SecPkgInfoA = _SecPkgInfoA;
  {$EXTERNALSYM PSecPkgInfoA}
  PSecPkgInfoA = ^SecPkgInfoA;
  TSecPkgInfoA = SecPkgInfoA;

  {$EXTERNALSYM _SecPkgInfoW}
  _SecPkgInfoW = record
    fCapabilities   : Cardinal;    // Capability bitmask
    wVersion        : Word;        // Version of driver
    wRPCID          : Word;        // ID for RPC Runtime
    cbMaxToken      : Cardinal;    // Size of authentication token (max)
    Name            : PWideChar;   // Text name
    Comment         : PWideChar;   // Comment
  end;
  {$EXTERNALSYM SecPkgInfoW}
  SecPkgInfoW = _SecPkgInfoW;
  {$EXTERNALSYM PSecPkgInfoW}
  PSecPkgInfoW = ^SecPkgInfoW;
  TSecPkgInfoW = SecPkgInfoW;

{$IFDEF UNICODE}
  TSecPkgInfo = TSecPkgInfoW;
  {$EXTERNALSYM SecPkgInfo}
  SecPkgInfo  = TSecPkgInfoW;
  {$EXTERNALSYM PSecPkgInfo}
  PSecPkgInfo = PSecPkgInfoW;
{$ELSE}
  TSecPkgInfo = TSecPkgInfoA;
  {$EXTERNALSYM SecPkgInfo}
  SecPkgInfo  = TSecPkgInfoA;
  {$EXTERNALSYM PSecPkgInfo}
  PSecPkgInfo = PSecPkgInfoA;
{$ENDIF}

  {$EXTERNALSYM SECURITY_STATUS}
  SECURITY_STATUS = LongInt;
  TSecurityStatus = SECURITY_STATUS;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN_A}
  ENUMERATE_SECURITY_PACKAGES_FN_A  = function(var cPackages: Cardinal; var PackageInfo: PSecPkgInfo): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENUMERATE_SECURITY_PACKAGES_FN_W}
  ENUMERATE_SECURITY_PACKAGES_FN_W  = function(var cPackages: Cardinal; var PackageInfo: PSecPkgInfoW): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN_A}
  QUERY_SECURITY_PACKAGE_INFO_FN_A  = function(packageName: PAnsiChar; var info: PSecPkgInfo): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_PACKAGE_INFO_FN_W}
  QUERY_SECURITY_PACKAGE_INFO_FN_W  = function(packageName: PWideChar; var info: PSecPkgInfo): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN_A}
  QUERY_CREDENTIALS_ATTRIBUTES_FN_A = function(phCredential: pCredHandle; ulAttribute: Cardinal; buffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CREDENTIALS_ATTRIBUTES_FN_W}
  QUERY_CREDENTIALS_ATTRIBUTES_FN_W = function(phCredential: pCredHandle; ulAttribute: Cardinal; buffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM EXPORT_SECURITY_CONTEXT_FN}
  EXPORT_SECURITY_CONTEXT_FN        = function(hContext: pCtxtHandle; flags: Cardinal; pPackedContext: PSecBuffer; var token: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM SEC_GET_KEY_FN}
  SEC_GET_KEY_FN                    = procedure(Arg, Principal: Pointer; KeyVer: Cardinal; var Key: Pointer; var status: SECURITY_STATUS); stdcall;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN_A}
  ACQUIRE_CREDENTIALS_HANDLE_FN_A   = function(pszPrincipal: PAnsiChar; pszPackage: PAnsiChar; fCredentialUse: Cardinal; pvLogonID: Pointer; pAuthData: Pointer; pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; var phCredential: CredHandle; var ptsExpiry : LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ACQUIRE_CREDENTIALS_HANDLE_FN_W}
  ACQUIRE_CREDENTIALS_HANDLE_FN_W   = function(pszPrincipal: PWideChar; pszPackage: PWideChar; fCredentialUse: Cardinal; pvLogonID: Pointer; pAuthData: Pointer; pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; var phCredential: CredHandle; var ptsExpiry : LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM FREE_CREDENTIALS_HANDLE_FN}
  FREE_CREDENTIALS_HANDLE_FN        = function(credHandle: PCredHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN_A}
  INITIALIZE_SECURITY_CONTEXT_FN_A  = function(phCredential: PCredHandle; phContent: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq, Reserved1, TargetDataRe : Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry : LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM INITIALIZE_SECURITY_CONTEXT_FN_W}
  INITIALIZE_SECURITY_CONTEXT_FN_W  = function(phCredential: PCredHandle; phContent: PCtxtHandle; pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: Cardinal; pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry : LARGE_INTEGER) : SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ACCEPT_SECURITY_CONTEXT_FN}
  ACCEPT_SECURITY_CONTEXT_FN        = function(phCredential: PCredHandle; phContext: PCtxtHandle; pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry: LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM COMPLETE_AUTH_TOKEN_FN}
  COMPLETE_AUTH_TOKEN_FN           = function(phContext: PCtxtHandle; pToken: PSecBufferDesc): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM DELETE_SECURITY_CONTEXT_FN}
  DELETE_SECURITY_CONTEXT_FN       = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM APPLY_CONTROL_TOKEN_FN}
  APPLY_CONTROL_TOKEN_FN           = function(phContext: PCtxtHandle; pInput: PSecBufferDesc) : SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN_A}
  QUERY_CONTEXT_ATTRIBUTES_FN_A    = function(phContext: PCtxtHandle; alAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_CONTEXT_ATTRIBUTES_FN_W}
  QUERY_CONTEXT_ATTRIBUTES_FN_W    = function(phContext: PCtxtHandle; alAttribute: Cardinal; pBuffer: Pointer): SECURITY_STATUS; stdcall;

  {$EXTERNALSYM IMPERSONATE_SECURITY_CONTEXT_FN}
  IMPERSONATE_SECURITY_CONTEXT_FN  = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM REVERT_SECURITY_CONTEXT_FN}
  REVERT_SECURITY_CONTEXT_FN       = function(phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM MAKE_SIGNATURE_FN}
  MAKE_SIGNATURE_FN                = function(phContext: PCtxtHandle; fQOP: DWORD; pMessage: PSecBufferDesc;  MessageSeqNo: Cardinal): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM VERIFY_SIGNATURE_FN}
  VERIFY_SIGNATURE_FN              = function(phContext: PCtxtHandle; pMessage: PSecBufferDesc; MessageSeqNo: Cardinal; var fQOP: Cardinal):SECURITY_STATUS; stdcall;
  {$EXTERNALSYM FREE_CONTEXT_BUFFER_FN}
  FREE_CONTEXT_BUFFER_FN           = function(contextBuffer: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN_A}
  IMPORT_SECURITY_CONTEXT_FN_A     = function(pszPackage: PAnsiChar; pPackedContext: PSecBuffer; Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM IMPORT_SECURITY_CONTEXT_FN_W}
  IMPORT_SECURITY_CONTEXT_FN_W     = function(pszPackage: PWideChar; pPackedContext: PSecBuffer; Token: Pointer; phContext: PCtxtHandle): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN_A}
  ADD_CREDENTIALS_FN_A             = function(hCredentials: PCredHandle; pszPrincipal, pszPackage: PAnsiChar; fCredentialUse: Cardinal; pAuthData: Pointer; pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; var ptsExpiry: LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ADD_CREDENTIALS_FN_W}
  ADD_CREDENTIALS_FN_W             = function(hCredentials: PCredHandle; pszPrincipal, pszPackage : PWideChar; fCredentialUse: Cardinal; pAuthData: Pointer; pGetKeyFn: SEC_GET_KEY_FN; pvGetKeyArgument: Pointer; var ptsExpiry: LARGE_INTEGER): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM QUERY_SECURITY_CONTEXT_TOKEN_FN}
  QUERY_SECURITY_CONTEXT_TOKEN_FN  = function(phContext: PCtxtHandle; var token: Pointer): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM ENCRYPT_MESSAGE_FN}
  ENCRYPT_MESSAGE_FN               = function(phContext: PCtxtHandle; fQOP: DWORD; pMessage: PSecBufferDesc; MessageSeqNo: DWORD): SECURITY_STATUS; stdcall;
  {$EXTERNALSYM DECRYPT_MESSAGE_FN}
  DECRYPT_MESSAGE_FN               = function(phContext: PCtxtHandle; pMessage: PSecBufferDesc; MessageSeqNo: Cardinal; fQOP: DWORD): SECURITY_STATUS; stdcall;

const
  {$EXTERNALSYM SECURITY_ENTRYPOINTW}
  SECURITY_ENTRYPOINTW  = 'InitSecurityInterfaceW';
  {$EXTERNALSYM SECURITY_ENTRYPOINTA}
  SECURITY_ENTRYPOINTA  = 'InitSecurityInterfaceA';

{$IFDEF UNICODE}
  {$EXTERNALSYM SECURITY_ENTRYPOINT}
  SECURITY_ENTRYPOINT = SECURITY_ENTRYPOINTW;
{$ELSE}
  {$EXTERNALSYM SECURITY_ENTRYPOINT}
  SECURITY_ENTRYPOINT = SECURITY_ENTRYPOINTA;
{$ENDIF}


type
   {$EXTERNALSYM _SECURITY_FUNCTION_TABLE_A}
  _SECURITY_FUNCTION_TABLE_A = record
    dwVersion : LongInt;
    EnumerateSecurityPackagesA  : ENUMERATE_SECURITY_PACKAGES_FN_A;
    QueryCredentialsAttributesA : QUERY_CREDENTIALS_ATTRIBUTES_FN_A;
    AcquireCredentialsHandleA   : ACQUIRE_CREDENTIALS_HANDLE_FN_A;
    FreeCredentialHandle        : FREE_CREDENTIALS_HANDLE_FN;
    Reserved2                   : FARPROC;
    InitializeSecurityContextA  : INITIALIZE_SECURITY_CONTEXT_FN_A;
    AcceptSecurityContext       : ACCEPT_SECURITY_CONTEXT_FN;
    CompleteAuthToken           : COMPLETE_AUTH_TOKEN_FN;
    DeleteSecurityContext       : DELETE_SECURITY_CONTEXT_FN;
    ApplyControlToken           : APPLY_CONTROL_TOKEN_FN;
    QueryContextAttributesA     : QUERY_CONTEXT_ATTRIBUTES_FN_A;
    ImpersonateSecurityContext  : IMPERSONATE_SECURITY_CONTEXT_FN;
    RevertSecurityContext       : REVERT_SECURITY_CONTEXT_FN;
    MakeSignature               : MAKE_SIGNATURE_FN;
    VerifySignature             : VERIFY_SIGNATURE_FN;
    FreeContextBuffer           : FREE_CONTEXT_BUFFER_FN;
    QuerySecurityPackageInfoA   : QUERY_SECURITY_PACKAGE_INFO_FN_A;
    Reserved3                   : FARPROC;
    Reserved4                   : FARPROC;
    ExportSecurityContext       : EXPORT_SECURITY_CONTEXT_FN;
    ImportSecurityContextA      : IMPORT_SECURITY_CONTEXT_FN_A;
    AddCredentialsA             : ADD_CREDENTIALS_FN_A;
    Reserved8                   : FARPROC;
    QuerySecurityContextToken   : QUERY_SECURITY_CONTEXT_TOKEN_FN;
    EncryptMessage              : ENCRYPT_MESSAGE_FN;
    DecryptMessage              : DECRYPT_MESSAGE_FN;
  end;
  {$EXTERNALSYM PSecurityFunctionTableA}
  PSecurityFunctionTableA = ^TSecurityFunctionTableA;
  {$EXTERNALSYM SecurityFunctionTableA}
  SecurityFunctionTableA = _SECURITY_FUNCTION_TABLE_A;
  TSecurityFunctionTableA = SecurityFunctionTableA;

  {$EXTERNALSYM _SECURITY_FUNCTION_TABLE_W}
  _SECURITY_FUNCTION_TABLE_W = record
    dwVersion : LongInt;
    EnumerateSecurityPackagesW  : ENUMERATE_SECURITY_PACKAGES_FN_W;
    QueryCredentialsAttributesW : QUERY_CREDENTIALS_ATTRIBUTES_FN_W;
    AcquireCredentialsHandleW   : ACQUIRE_CREDENTIALS_HANDLE_FN_W;
    FreeCredentialHandle        : FREE_CREDENTIALS_HANDLE_FN;
    Reserved2                   : FARPROC;
    InitializeSecurityContextW  : INITIALIZE_SECURITY_CONTEXT_FN_W;
    AcceptSecurityContext       : ACCEPT_SECURITY_CONTEXT_FN;
    CompleteAuthToken           : COMPLETE_AUTH_TOKEN_FN;
    DeleteSecurityContext       : DELETE_SECURITY_CONTEXT_FN;
    ApplyControlToken           : APPLY_CONTROL_TOKEN_FN;
    QueryContextAttributesW     : QUERY_CONTEXT_ATTRIBUTES_FN_W;
    ImpersonateSecurityContext  : IMPERSONATE_SECURITY_CONTEXT_FN;
    RevertSecurityContext       : REVERT_SECURITY_CONTEXT_FN;
    MakeSignature               : MAKE_SIGNATURE_FN;
    VerifySignature             : VERIFY_SIGNATURE_FN;
    FreeContextBuffer           : FREE_CONTEXT_BUFFER_FN;
    QuerySecurityPackageInfoW   : QUERY_SECURITY_PACKAGE_INFO_FN_W;
    Reserved3                   : FARPROC;
    Reserved4                   : FARPROC;
    ExportSecurityContext       : EXPORT_SECURITY_CONTEXT_FN;
    ImportSecurityContextW      : IMPORT_SECURITY_CONTEXT_FN_W;
    AddCredentialsW             : ADD_CREDENTIALS_FN_W;
    Reserved8                   : FARPROC;
    QuerySecurityContextToken   : QUERY_SECURITY_CONTEXT_TOKEN_FN;
    EncryptMessage              : ENCRYPT_MESSAGE_FN;
    DecryptMessage              : DECRYPT_MESSAGE_FN;
  end;
  {$EXTERNALSYM PSecurityFunctionTableW}
  PSecurityFunctionTableW = ^TSecurityFunctionTableW;
  {$EXTERNALSYM SecurityFunctionTableW}
  SecurityFunctionTableW = _SECURITY_FUNCTION_TABLE_W;
  TSecurityFunctionTableW = SecurityFunctionTableW;

{$IFDEF UNICODE}
  TSecurityFunctionTable  = TSecurityFunctionTableW;
  {$EXTERNALSYM PSecurityFunctionTable}
  PSecurityFunctionTable  = PSecurityFunctionTableW;
{$ELSE}
  TSecurityFunctionTable = TSecurityFunctionTableA;
  {$EXTERNALSYM PSecurityFunctionTable}
  PSecurityFunctionTable = PSecurityFunctionTableA;
{$ENDIF} 

  {$EXTERNALSYM _SecPkgContext_NamesA}
  _SecPkgContext_NamesA = record
    sUserName: PAnsiChar;
  end;
  {$EXTERNALSYM SecPkgContext_NamesA}
  SecPkgContext_NamesA = _SecPkgContext_NamesA;
  {$EXTERNALSYM PSecPkgContext_NamesA}
  PSecPkgContext_NamesA = ^SecPkgContext_NamesA;
  TSecPkgContextNamesA = SecPkgContext_NamesA;
  PSecPkgContextNamesA = PSecPkgContext_NamesA;


  {$EXTERNALSYM _SecPkgContext_NamesW}
  _SecPkgContext_NamesW = record
    sUserName: PWideChar;
  end;
  {$EXTERNALSYM SecPkgContext_NamesW}
  SecPkgContext_NamesW = _SecPkgContext_NamesW;
  {$EXTERNALSYM PSecPkgContext_NamesW}
  PSecPkgContext_NamesW = ^SecPkgContext_NamesW;
  TSecPkgContextNamesW = SecPkgContext_NamesW;
  PSecPkgContextNamesW = PSecPkgContext_NamesW;

{$IFDEF UNICODE}
  {$EXTERNALSYM SecPkgContext_Names}
  SecPkgContext_Names = SecPkgContext_NamesW;
  {$EXTERNALSYM PSecPkgContext_Names}
  PSecPkgContext_Names = PSecPkgContext_NamesW;
  TSecPkgContextNames = TSecPkgContextNamesW;
  PSecPkgContextNames = PSecPkgContextNamesW;
{$ELSE}
  {$EXTERNALSYM SecPkgContext_Names}
  SecPkgContext_Names = SecPkgContext_NamesA;
  {$EXTERNALSYM PSecPkgContext_Names}
  PSecPkgContext_Names = PSecPkgContext_NamesA;
  TSecPkgContextNames = TSecPkgContextNamesA;
  PSecPkgContextNames = PSecPkgContextNamesA;
{$ENDIF}

   {$EXTERNALSYM _SecPkgContext_AuthorityA}
  _SecPkgContext_AuthorityA = record
    sAuthorityName: PAnsiChar;
  end;
  {$EXTERNALSYM SecPkgContext_AuthorityA}
  SecPkgContext_AuthorityA = _SecPkgContext_AuthorityA;
  {$EXTERNALSYM PSecPkgContext_AuthorityA}
  PSecPkgContext_AuthorityA = ^SecPkgContext_AuthorityA;
  TSecPkgContextAuthorityA = SecPkgContext_AuthorityA;
  PSecPkgContextAuthorityA = PSecPkgContext_AuthorityA;

  {$EXTERNALSYM _SecPkgContext_AuthorityW}
  _SecPkgContext_AuthorityW = record
    sAuthorityName: PWideChar;
  end;
  {$EXTERNALSYM SecPkgContext_AuthorityW}
  SecPkgContext_AuthorityW = _SecPkgContext_AuthorityW;
  {$EXTERNALSYM PSecPkgContext_AuthorityW}
  PSecPkgContext_AuthorityW = ^SecPkgContext_AuthorityW;
  TSecPkgContextAuthorityW = SecPkgContext_AuthorityW;
  PSecPkgContextAuthorityW = PSecPkgContext_AuthorityW;

{$IFDEF UNICODE}
  {$EXTERNALSYM SecPkgContext_Authority}
  SecPkgContext_Authority  = SecPkgContext_AuthorityW;
  {$EXTERNALSYM PSecPkgContext_Authority}
  PSecPkgContext_Authority = PSecPkgContext_AuthorityW;
  TSecPkgContextAuthority = TSecPkgContextAuthorityW;
  PSecPkgContextAuthority = PSecPkgContextAuthorityW;
{$ELSE}
  {$EXTERNALSYM SecPkgContext_Authority}
  SecPkgContext_Authority  = SecPkgContext_AuthorityA;
  {$EXTERNALSYM PSecPkgContext_Authority}
  PSecPkgContext_Authority = PSecPkgContext_AuthorityA;
  TSecPkgContextAuthority = SecPkgContext_AuthorityA;
  PSecPkgContextAuthority = PSecPkgContext_AuthorityA;
{$ENDIF}

{$ENDIF MSWINDOWS}
implementation

end.
 