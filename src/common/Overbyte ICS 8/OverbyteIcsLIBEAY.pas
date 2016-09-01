{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for LIBEAY32.DLL (OpenSSL)
              This is only the subset needed by ICS.
Creation:     Jan 12, 2003
Version:      8.12
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2014 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added. New version check,
             see comments in source. In order to disable version check uncomment
             define NO_OSSL_VERSION_CHECK below and rebuild all. New functions
             OpenSslVersion, OpenSslCompilerFlags, OpenSslBuiltOn,
             OpenSslPlatForm, OpenSslDir all return a string type.
Jan 27, 2006 A. Garrels, made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2006 A. Garrels: Added functions f_Ics_X509_get_notBefore,
             f_Ics_X509_get_notAfter, Asn1ToUTDateTime.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
May 24, 2007 A.Garrels: Added code to handle ASN1 BMPString and Utf8 string
             types.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
Jul 18, 2008 A. Garrels made some changes to get rid of some string cast
             warnings.
Jun 05, 2008 A.Garrels revised Asn1ToString(), made some string casts explicit.
Aug 19, 2008 A.Garrels checked against OpenSSL v0.9.8h and added that version
             as maximum version.
Nov 17, 2008 A.Garrels checked against OpenSSL v0.9.8i and added that version
             as maximum version.
Apr 10, 2009 A.Garrels checked against OpenSSL v0.9.8k and made it the maximum
             supported version.
Sep 24, 2009 Arno - Use OverbyteIcsUtils.IcsBufferToHex()
Nov 05, 2009 A.Garrels checked against OpenSSL v0.9.8L and made it the maximum
             supported version. OpenSSL V0.9.8L disables session renegotiation
             due to TLS renegotiation vulnerability.
Dec 20, 2009 A.Garrels added plenty of stuff. Some is not yet used some is, like
             Server Name Indication (SNI) and an option to let OpenSSL use the
             default Delphi memory manager (both needs to be turned on in
             OverbyteIcsSslDefs.inc).
May 07, 2010 A. Garrels moved declaration of size_t to OverbyteIcsTypes,
             changed user type CRYPTO_dynlock_value to use TRTLCriticalSection.
May 08, 2010 Arno Garrels added support for OpenSSL 0.9.8n.
             In OSSL v0.9.8L and v0.9.8m renegotiation support was disabled
             due to vulnerability of the SSL protocol. In v0.9.8n renegotiation
             support was re-enabled and RFC5746 implemented but require the
             extension as needed. It's also possible to enable unsafe legacy
             renegotiation explicitly by setting new option
             sslOpt_ALLOW_UNSAFE_LEGACY_RENEGOTIATION of TSslContext.
Apr 15, 2011 Arno prepared for 64-bit.
Apr 23, 2011 Arno added support for OpenSSL 0.9.8r and 1.0.0d.
Apr 24, 2011 Arno added some helper rountines since record TEVP_PKEY_st
             changed in 1.0.0 and had to be declared as dummy.
May 03, 2011 Arno added some function declarations.
May 08, 2011 Arno added function f_ERR_remove_thread_state new in v1.0.0+.
May 17, 2011 Arno made one hack thread-safe and got rid of another hack with
             OSSL v1.0.0+.
May 31, 2011 Arno changed the 64-bit hack in Ics_Ssl_EVP_PKEY_GetKey.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Feb 13, 2014 V8.01 - Angus added more NID_xx literals
Apr 19, 2014 V8.02 - Arno allow load of OSSL 1.0.1g (untested so far)
Jul 07, 2014 V8.03 - Angus allow load of OSSL 1.0.1h (briefly tested)
Aug 08, 2014 V8.04 - Angus allow load of OSSL 1.0.1i (briefly tested)
Aug 08, 2014 V8.04 - Angus allow load of OSSL 1.0.1i (briefly tested)
Dec 09, 2014 V8.05 - Angus allow load of OSSL 1.0.1j (untested)
                     Added Ics_Ssl_ERR_GET_LIB, Ics_Ssl_ERR_GET_FUNC and Ics_Ssl_ERR_GET_REASON
                     Added error literals for library, function and reason
Jan 19, 2015 V8.06 - Angus allow load of OSSL 1.0.1k (briefly tested)
Mar 13, 2015 V8.07 - Angus allow load of OSSL 1.0.2 (briefly tested)
                     Note, only OpenSSL 1.0.1 and later are now supported, removed some old conditionals and code
                     Added functions and literals for DH and EC key support
Mar 17, 2015 V8.08 - Angus allow load of OSSL 1.0.2a (untested)
Mar 26, 2015 V8.09   Angus, the OpenSSL version check is relaxed so minor versions with a letter suffix
                      are now supported up to the next major version, so now support up to 1.0.2z
Oct 23, 2015 V8.10   Angus, another NID literal
Nov 20, 2015 V8.11   Eugene Kotlyarov added RSA key related stuff
Nov 23, 2015 V8.12   Angus added f_PEM_write_bio_RSAPrivateKey and f_PEM_write_bio_RSAPublicKey


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

{$WARN SYMBOL_DEPRECATED OFF}
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$IFDEF COMPILER12_UP}
    { These are usefull for debugging !}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}

unit OverbyteIcsLIBEAY;

interface

{$IFDEF USE_SSL}

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
    OverbyteIcsTypes, // size_t
{$IFDEF POSIX}
    Posix.SysTypes,
    Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsUtils,
    OverbyteIcsSSLEAY;

const
    IcsLIBEAYVersion   = 812;
    CopyRight : String = ' IcsLIBEAY (c) 2003-2015 F. Piette V8.12 ';

type
    EIcsLibeayException = class(Exception);

    TStatLockLockCallback = procedure(Mode : Integer; N : Integer; const _File : PAnsiChar; Line : Integer); cdecl;
    TStatLockIDCallback   = function : Longword; cdecl;
    TCryptoThreadIDCallback = procedure (ID : PCRYPTO_THREADID); cdecl;

    TCRYPTO_dynlock_value_st = record
        Mutex : TIcsCriticalSection;
    end;
    PCRYPTO_dynlock_value = ^TCRYPTO_dynlock_value_st;
    CRYPTO_dynlock_value  = TCRYPTO_dynlock_value_st;

    TDynLockCreateCallback  = function(const _file : PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
    TDynLockLockCallback    = procedure(Mode : Integer; L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;
    TDynLockDestroyCallback = procedure(L : PCRYPTO_dynlock_value; _File : PAnsiChar; Line: Integer); cdecl;

const
    V_ASN1_UNIVERSAL                    = $00;
    V_ASN1_APPLICATION                  = $40;
    V_ASN1_CONTEXT_SPECIFIC             = $80;
    V_ASN1_PRIVATE                      = $c0;

    V_ASN1_CONSTRUCTED                  = $20;
    V_ASN1_PRIMITIVE_TAG                = $1f;

    V_ASN1_UNDEF                        = -1;
    V_ASN1_EOC                          = 0;
    V_ASN1_BOOLEAN                      = 1;
    V_ASN1_INTEGER                      = 2;
    V_ASN1_BIT_STRING                   = 3;
    V_ASN1_OCTET_STRING                 = 4;
    V_ASN1_NULL                         = 5;
    V_ASN1_OBJECT                       = 6;
    V_ASN1_OBJECT_DESCRIPTOR            = 7;
    V_ASN1_EXTERNAL                     = 8;
    V_ASN1_REAL                         = 9;
    V_ASN1_ENUMERATED                   = 10;
    V_ASN1_UTF8STRING                   = 12;
    V_ASN1_SEQUENCE                     = 16;
    V_ASN1_SET                          = 17;
    V_ASN1_NUMERICSTRING                = 18;
{ An ASN.1 NumericString object may represent any arbitrary string of numeric }
{ characters including the space character: 0,1,2,...,9,SPACE                 }
    V_ASN1_PRINTABLESTRING              = 19;
{ An ASN.1 PrintableString may represent any arbitrary string of  printable  }
{ characters (A,B,...,Z; a,b,...,z; 0,1,...,9; space ' () + , - . / : = ?)   }
    V_ASN1_T61STRING                    = 20;
    V_ASN1_TELETEXSTRING                = 20;  (* alias *)
    V_ASN1_VIDEOTEXSTRING               = 21;
    V_ASN1_IA5STRING                    = 22;
{ An ASN.1 IA5String object may represent any arbitrary string of ASCII   }
{ characters. The term IA5 denotes International Alphabet 5 (= ASCII).    }
    V_ASN1_UTCTIME                      = 23;
    V_ASN1_GENERALIZEDTIME              = 24;
    V_ASN1_GRAPHICSTRING                = 25;
    V_ASN1_ISO64STRING                  = 26;
    V_ASN1_VISIBLESTRING                = 26;  (* alias *)
    V_ASN1_GENERALSTRING                = 27;
{ The ASN.1 character string type GeneralString encompasses all registered }
{ graphic and character sets (see ISO 2375) plus SPACE and DELETE.         }
    V_ASN1_UNIVERSALSTRING              = 28;
{ UniversalString is defined in ASN.1:1993.                                }
    V_ASN1_BMPSTRING                    = 30;
{ BMPString is a subtype of the UniversalString type and models the Basic  }
{ Multilingual Plane of ISO/IEC/ITU 10646-1, a two-octet (USC-2) encoding  }
{ form, which is identical to Unicode 1.1.                                 }

    {ERR_NUM_ERRORS   = 10;
    ERR_TXT_MALLOCED = 1; }
    ERR_TXT_STRING   = 2;
    { Changed from 32 in v0.9.7 up }
    ERR_R_FATAL      = 64;

  { V8.05 Library codes for SSLErr() }
    ERR_LIB_NONE     = 1;
    ERR_LIB_SYS      = 2;
    ERR_LIB_BN       = 3;
    ERR_LIB_RSA      = 4;
    ERR_LIB_DH       = 5;
    ERR_LIB_EVP      = 6;
    ERR_LIB_BUF      = 7;
    ERR_LIB_OBJ      = 8;
    ERR_LIB_PEM      = 9;
    ERR_LIB_DSA      = 10;
    ERR_LIB_X509     = 11;
    ERR_LIB_METH     = 12;
    ERR_LIB_ASN1     = 13;
    ERR_LIB_CONF     = 14;
    ERR_LIB_CRYPTO   = 15;
    ERR_LIB_SSL      = 20;
    ERR_LIB_SSL23    = 21;
    ERR_LIB_SSL2     = 22;
    ERR_LIB_SSL3     = 23;
    ERR_LIB_RSAREF   = 30;
    ERR_LIB_PROXY    = 31;
    ERR_LIB_BIO      = 32;
    ERR_LIB_PKCS7    = 33;
    ERR_LIB_X509V3   = 34;
    ERR_LIB_PKCS12   = 35;
    ERR_LIB_RAND     = 36;
    ERR_LIB_DSO      = 37;
    ERR_LIB_ENGINE   = 38;
    ERR_LIB_OCSP     = 39;
    ERR_LIB_UI       = 40;
    ERR_LIB_COMP     = 41;
    ERR_LIB_ECDSA    = 42;
    ERR_LIB_ECDH     = 43;
    ERR_LIB_STORE    = 44;
    ERR_LIB_FIPS     = 45;
    ERR_LIB_CMS      = 46;
    ERR_LIB_TS       = 47;
    ERR_LIB_HMAC     = 48;
    ERR_LIB_JPAKE    = 49;
    ERR_LIB_USER     = 128;

  { V8.05 function codes for SSLErr() }
    SSL_F_CLIENT_CERTIFICATE = 100;
    SSL_F_CLIENT_FINISHED = 167;
    SSL_F_CLIENT_HELLO = 101;
    SSL_F_CLIENT_MASTER_KEY = 102;
    SSL_F_D2I_SSL_SESSION = 103;
    SSL_F_DO_DTLS1_WRITE = 245;
    SSL_F_DO_SSL3_WRITE = 104;
    SSL_F_DTLS1_ACCEPT = 246;
    SSL_F_DTLS1_ADD_CERT_TO_BUF = 295;
    SSL_F_DTLS1_BUFFER_RECORD = 247;
    SSL_F_DTLS1_CHECK_TIMEOUT_NUM = 316;
    SSL_F_DTLS1_CLIENT_HELLO = 248;
    SSL_F_DTLS1_CONNECT = 249;
    SSL_F_DTLS1_ENC = 250;
    SSL_F_DTLS1_GET_HELLO_VERIFY = 251;
    SSL_F_DTLS1_GET_MESSAGE = 252;
    SSL_F_DTLS1_GET_MESSAGE_FRAGMENT = 253;
    SSL_F_DTLS1_GET_RECORD = 254;
    SSL_F_DTLS1_HANDLE_TIMEOUT = 297;
    SSL_F_DTLS1_HEARTBEAT = 305;
    SSL_F_DTLS1_OUTPUT_CERT_CHAIN = 255;
    SSL_F_DTLS1_PREPROCESS_FRAGMENT = 288;
    SSL_F_DTLS1_PROCESS_OUT_OF_SEQ_MESSAGE = 256;
    SSL_F_DTLS1_PROCESS_RECORD = 257;
    SSL_F_DTLS1_READ_BYTES = 258;
    SSL_F_DTLS1_READ_FAILED = 259;
    SSL_F_DTLS1_SEND_CERTIFICATE_REQUEST = 260;
    SSL_F_DTLS1_SEND_CLIENT_CERTIFICATE = 261;
    SSL_F_DTLS1_SEND_CLIENT_KEY_EXCHANGE = 262;
    SSL_F_DTLS1_SEND_CLIENT_VERIFY = 263;
    SSL_F_DTLS1_SEND_HELLO_VERIFY_REQUEST = 264;
    SSL_F_DTLS1_SEND_SERVER_CERTIFICATE = 265;
    SSL_F_DTLS1_SEND_SERVER_HELLO = 266;
    SSL_F_DTLS1_SEND_SERVER_KEY_EXCHANGE = 267;
    SSL_F_DTLS1_WRITE_APP_DATA_BYTES = 268;
    SSL_F_GET_CLIENT_FINISHED = 105;
    SSL_F_GET_CLIENT_HELLO = 106;
    SSL_F_GET_CLIENT_MASTER_KEY = 107;
    SSL_F_GET_SERVER_FINISHED = 108;
    SSL_F_GET_SERVER_HELLO = 109;
    SSL_F_GET_SERVER_VERIFY = 110;
    SSL_F_I2D_SSL_SESSION = 111;
    SSL_F_READ_N = 112;
    SSL_F_REQUEST_CERTIFICATE = 113;
    SSL_F_SERVER_FINISH = 239;
    SSL_F_SERVER_HELLO = 114;
    SSL_F_SERVER_VERIFY = 240;
    SSL_F_SSL23_ACCEPT = 115;
    SSL_F_SSL23_CLIENT_HELLO = 116;
    SSL_F_SSL23_CONNECT = 117;
    SSL_F_SSL23_GET_CLIENT_HELLO = 118;
    SSL_F_SSL23_GET_SERVER_HELLO = 119;
    SSL_F_SSL23_PEEK = 237;
    SSL_F_SSL23_READ = 120;
    SSL_F_SSL23_WRITE = 121;
    SSL_F_SSL2_ACCEPT = 122;
    SSL_F_SSL2_CONNECT = 123;
    SSL_F_SSL2_ENC_INIT = 124;
    SSL_F_SSL2_GENERATE_KEY_MATERIAL = 241;
    SSL_F_SSL2_PEEK = 234;
    SSL_F_SSL2_READ = 125;
    SSL_F_SSL2_READ_INTERNAL = 236;
    SSL_F_SSL2_SET_CERTIFICATE = 126;
    SSL_F_SSL2_WRITE = 127;
    SSL_F_SSL3_ACCEPT = 128;
    SSL_F_SSL3_ADD_CERT_TO_BUF = 296;
    SSL_F_SSL3_CALLBACK_CTRL = 233;
    SSL_F_SSL3_CHANGE_CIPHER_STATE = 129;
    SSL_F_SSL3_CHECK_CERT_AND_ALGORITHM = 130;
    SSL_F_SSL3_CHECK_CLIENT_HELLO = 304;
    SSL_F_SSL3_CLIENT_HELLO = 131;
    SSL_F_SSL3_CONNECT = 132;
    SSL_F_SSL3_CTRL = 213;
    SSL_F_SSL3_CTX_CTRL = 133;
    SSL_F_SSL3_DIGEST_CACHED_RECORDS = 293;
    SSL_F_SSL3_DO_CHANGE_CIPHER_SPEC = 292;
    SSL_F_SSL3_ENC = 134;
    SSL_F_SSL3_GENERATE_KEY_BLOCK = 238;
    SSL_F_SSL3_GET_CERTIFICATE_REQUEST = 135;
    SSL_F_SSL3_GET_CERT_STATUS = 289;
    SSL_F_SSL3_GET_CERT_VERIFY = 136;
    SSL_F_SSL3_GET_CLIENT_CERTIFICATE = 137;
    SSL_F_SSL3_GET_CLIENT_HELLO = 138;
    SSL_F_SSL3_GET_CLIENT_KEY_EXCHANGE = 139;
    SSL_F_SSL3_GET_FINISHED = 140;
    SSL_F_SSL3_GET_KEY_EXCHANGE = 141;
    SSL_F_SSL3_GET_MESSAGE = 142;
    SSL_F_SSL3_GET_NEW_SESSION_TICKET = 283;
    SSL_F_SSL3_GET_NEXT_PROTO = 306;
    SSL_F_SSL3_GET_RECORD = 143;
    SSL_F_SSL3_GET_SERVER_CERTIFICATE = 144;
    SSL_F_SSL3_GET_SERVER_DONE = 145;
    SSL_F_SSL3_GET_SERVER_HELLO = 146;
    SSL_F_SSL3_HANDSHAKE_MAC = 285;
    SSL_F_SSL3_NEW_SESSION_TICKET = 287;
    SSL_F_SSL3_OUTPUT_CERT_CHAIN = 147;
    SSL_F_SSL3_PEEK = 235;
    SSL_F_SSL3_READ_BYTES = 148;
    SSL_F_SSL3_READ_N = 149;
    SSL_F_SSL3_SEND_CERTIFICATE_REQUEST = 150;
    SSL_F_SSL3_SEND_CLIENT_CERTIFICATE = 151;
    SSL_F_SSL3_SEND_CLIENT_KEY_EXCHANGE = 152;
    SSL_F_SSL3_SEND_CLIENT_VERIFY = 153;
    SSL_F_SSL3_SEND_SERVER_CERTIFICATE = 154;
    SSL_F_SSL3_SEND_SERVER_HELLO = 242;
    SSL_F_SSL3_SEND_SERVER_KEY_EXCHANGE = 155;
    SSL_F_SSL3_SETUP_KEY_BLOCK = 157;
    SSL_F_SSL3_SETUP_READ_BUFFER = 156;
    SSL_F_SSL3_SETUP_WRITE_BUFFER = 291;
    SSL_F_SSL3_WRITE_BYTES = 158;
    SSL_F_SSL3_WRITE_PENDING = 159;
    SSL_F_SSL_ADD_CLIENTHELLO_RENEGOTIATE_EXT = 298;
    SSL_F_SSL_ADD_CLIENTHELLO_TLSEXT = 277;
    SSL_F_SSL_ADD_CLIENTHELLO_USE_SRTP_EXT = 307;
    SSL_F_SSL_ADD_DIR_CERT_SUBJECTS_TO_STACK = 215;
    SSL_F_SSL_ADD_FILE_CERT_SUBJECTS_TO_STACK = 216;
    SSL_F_SSL_ADD_SERVERHELLO_RENEGOTIATE_EXT = 299;
    SSL_F_SSL_ADD_SERVERHELLO_TLSEXT = 278;
    SSL_F_SSL_ADD_SERVERHELLO_USE_SRTP_EXT = 308;
    SSL_F_SSL_BAD_METHOD = 160;
    SSL_F_SSL_BYTES_TO_CIPHER_LIST = 161;
    SSL_F_SSL_CERT_DUP = 221;
    SSL_F_SSL_CERT_INST = 222;
    SSL_F_SSL_CERT_INSTANTIATE = 214;
    SSL_F_SSL_CERT_NEW = 162;
    SSL_F_SSL_CHECK_PRIVATE_KEY = 163;
    SSL_F_SSL_CHECK_SERVERHELLO_TLSEXT = 280;
    SSL_F_SSL_CHECK_SRVR_ECC_CERT_AND_ALG = 279;
    SSL_F_SSL_CIPHER_PROCESS_RULESTR = 230;
    SSL_F_SSL_CIPHER_STRENGTH_SORT = 231;
    SSL_F_SSL_CLEAR = 164;
    SSL_F_SSL_COMP_ADD_COMPRESSION_METHOD = 165;
    SSL_F_SSL_CREATE_CIPHER_LIST = 166;
    SSL_F_SSL_CTRL = 232;
    SSL_F_SSL_CTX_CHECK_PRIVATE_KEY = 168;
    SSL_F_SSL_CTX_MAKE_PROFILES = 309;
    SSL_F_SSL_CTX_NEW = 169;
    SSL_F_SSL_CTX_SET_CIPHER_LIST = 269;
    SSL_F_SSL_CTX_SET_CLIENT_CERT_ENGINE = 290;
    SSL_F_SSL_CTX_SET_PURPOSE = 226;
    SSL_F_SSL_CTX_SET_SESSION_ID_CONTEXT = 219;
    SSL_F_SSL_CTX_SET_SSL_VERSION = 170;
    SSL_F_SSL_CTX_SET_TRUST = 229;
    SSL_F_SSL_CTX_USE_CERTIFICATE = 171;
    SSL_F_SSL_CTX_USE_CERTIFICATE_ASN1 = 172;
    SSL_F_SSL_CTX_USE_CERTIFICATE_CHAIN_FILE = 220;
    SSL_F_SSL_CTX_USE_CERTIFICATE_FILE = 173;
    SSL_F_SSL_CTX_USE_PRIVATEKEY = 174;
    SSL_F_SSL_CTX_USE_PRIVATEKEY_ASN1 = 175;
    SSL_F_SSL_CTX_USE_PRIVATEKEY_FILE = 176;
    SSL_F_SSL_CTX_USE_PSK_IDENTITY_HINT = 272;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY = 177;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY_ASN1 = 178;
    SSL_F_SSL_CTX_USE_RSAPRIVATEKEY_FILE = 179;
    SSL_F_SSL_DO_HANDSHAKE = 180;
    SSL_F_SSL_GET_NEW_SESSION = 181;
    SSL_F_SSL_GET_PREV_SESSION = 217;
    SSL_F_SSL_GET_SERVER_SEND_CERT = 182;
    SSL_F_SSL_GET_SERVER_SEND_PKEY = 317;
    SSL_F_SSL_GET_SIGN_PKEY = 183;
    SSL_F_SSL_INIT_WBIO_BUFFER = 184;
    SSL_F_SSL_LOAD_CLIENT_CA_FILE = 185;
    SSL_F_SSL_NEW = 186;
    SSL_F_SSL_PARSE_CLIENTHELLO_RENEGOTIATE_EXT = 300;
    SSL_F_SSL_PARSE_CLIENTHELLO_TLSEXT = 302;
    SSL_F_SSL_PARSE_CLIENTHELLO_USE_SRTP_EXT = 310;
    SSL_F_SSL_PARSE_SERVERHELLO_RENEGOTIATE_EXT = 301;
    SSL_F_SSL_PARSE_SERVERHELLO_TLSEXT = 303;
    SSL_F_SSL_PARSE_SERVERHELLO_USE_SRTP_EXT = 311;
    SSL_F_SSL_PEEK = 270;
    SSL_F_SSL_PREPARE_CLIENTHELLO_TLSEXT = 281;
    SSL_F_SSL_PREPARE_SERVERHELLO_TLSEXT = 282;
    SSL_F_SSL_READ = 223;
    SSL_F_SSL_RSA_PRIVATE_DECRYPT = 187;
    SSL_F_SSL_RSA_PUBLIC_ENCRYPT = 188;
    SSL_F_SSL_SESSION_NEW = 189;
    SSL_F_SSL_SESSION_PRINT_FP = 190;
    SSL_F_SSL_SESSION_SET1_ID_CONTEXT = 312;
    SSL_F_SSL_SESS_CERT_NEW = 225;
    SSL_F_SSL_SET_CERT = 191;
    SSL_F_SSL_SET_CIPHER_LIST = 271;
    SSL_F_SSL_SET_FD = 192;
    SSL_F_SSL_SET_PKEY = 193;
    SSL_F_SSL_SET_PURPOSE = 227;
    SSL_F_SSL_SET_RFD = 194;
    SSL_F_SSL_SET_SESSION = 195;
    SSL_F_SSL_SET_SESSION_ID_CONTEXT = 218;
    SSL_F_SSL_SET_SESSION_TICKET_EXT = 294;
    SSL_F_SSL_SET_TRUST = 228;
    SSL_F_SSL_SET_WFD = 196;
    SSL_F_SSL_SHUTDOWN = 224;
    SSL_F_SSL_SRP_CTX_INIT = 313;
    SSL_F_SSL_UNDEFINED_CONST_FUNCTION = 243;
    SSL_F_SSL_UNDEFINED_FUNCTION = 197;
    SSL_F_SSL_UNDEFINED_VOID_FUNCTION = 244;
    SSL_F_SSL_USE_CERTIFICATE = 198;
    SSL_F_SSL_USE_CERTIFICATE_ASN1 = 199;
    SSL_F_SSL_USE_CERTIFICATE_FILE = 200;
    SSL_F_SSL_USE_PRIVATEKEY = 201;
    SSL_F_SSL_USE_PRIVATEKEY_ASN1 = 202;
    SSL_F_SSL_USE_PRIVATEKEY_FILE = 203;
    SSL_F_SSL_USE_PSK_IDENTITY_HINT = 273;
    SSL_F_SSL_USE_RSAPRIVATEKEY = 204;
    SSL_F_SSL_USE_RSAPRIVATEKEY_ASN1 = 205;
    SSL_F_SSL_USE_RSAPRIVATEKEY_FILE = 206;
    SSL_F_SSL_VERIFY_CERT_CHAIN = 207;
    SSL_F_SSL_WRITE = 208;
    SSL_F_TLS1_CERT_VERIFY_MAC = 286;
    SSL_F_TLS1_CHANGE_CIPHER_STATE = 209;
    SSL_F_TLS1_CHECK_SERVERHELLO_TLSEXT = 274;
    SSL_F_TLS1_ENC = 210;
    SSL_F_TLS1_EXPORT_KEYING_MATERIAL = 314;
    SSL_F_TLS1_HEARTBEAT = 315;
    SSL_F_TLS1_PREPARE_CLIENTHELLO_TLSEXT = 275;
    SSL_F_TLS1_PREPARE_SERVERHELLO_TLSEXT = 276;
    SSL_F_TLS1_PRF = 284;
    SSL_F_TLS1_SETUP_KEY_BLOCK = 211;
    SSL_F_WRITE_PENDING = 212;

  { V8.05 reason codes for SSLErr() }
    SSL_R_APP_DATA_IN_HANDSHAKE = 100;
    SSL_R_ATTEMPT_TO_REUSE_SESSION_IN_DIFFERENT_CONTEXT = 272;
    SSL_R_BAD_ALERT_RECORD = 101;
    SSL_R_BAD_AUTHENTICATION_TYPE = 102;
    SSL_R_BAD_CHANGE_CIPHER_SPEC = 103;
    SSL_R_BAD_CHECKSUM = 104;
    SSL_R_BAD_DATA_RETURNED_BY_CALLBACK = 106;
    SSL_R_BAD_DECOMPRESSION = 107;
    SSL_R_BAD_DH_G_LENGTH = 108;
    SSL_R_BAD_DH_PUB_KEY_LENGTH = 109;
    SSL_R_BAD_DH_P_LENGTH = 110;
    SSL_R_BAD_DIGEST_LENGTH = 111;
    SSL_R_BAD_DSA_SIGNATURE = 112;
    SSL_R_BAD_ECC_CERT = 304;
    SSL_R_BAD_ECDSA_SIGNATURE = 305;
    SSL_R_BAD_ECPOINT = 306;
    SSL_R_BAD_HANDSHAKE_LENGTH = 332;
    SSL_R_BAD_HELLO_REQUEST = 105;
    SSL_R_BAD_LENGTH = 271;
    SSL_R_BAD_MAC_DECODE = 113;
    SSL_R_BAD_MAC_LENGTH = 333;
    SSL_R_BAD_MESSAGE_TYPE = 114;
    SSL_R_BAD_PACKET_LENGTH = 115;
    SSL_R_BAD_PROTOCOL_VERSION_NUMBER = 116;
    SSL_R_BAD_PSK_IDENTITY_HINT_LENGTH = 316;
    SSL_R_BAD_RESPONSE_ARGUMENT = 117;
    SSL_R_BAD_RSA_DECRYPT = 118;
    SSL_R_BAD_RSA_ENCRYPT = 119;
    SSL_R_BAD_RSA_E_LENGTH = 120;
    SSL_R_BAD_RSA_MODULUS_LENGTH = 121;
    SSL_R_BAD_RSA_SIGNATURE = 122;
    SSL_R_BAD_SIGNATURE = 123;
    SSL_R_BAD_SRP_A_LENGTH = 347;
    SSL_R_BAD_SRP_B_LENGTH = 348;
    SSL_R_BAD_SRP_G_LENGTH = 349;
    SSL_R_BAD_SRP_N_LENGTH = 350;
    SSL_R_BAD_SRP_PARAMETERS = 371;
    SSL_R_BAD_SRP_S_LENGTH = 351;
    SSL_R_BAD_SRTP_MKI_VALUE = 352;
    SSL_R_BAD_SRTP_PROTECTION_PROFILE_LIST = 353;
    SSL_R_BAD_SSL_FILETYPE = 124;
    SSL_R_BAD_SSL_SESSION_ID_LENGTH = 125;
    SSL_R_BAD_STATE = 126;
    SSL_R_BAD_WRITE_RETRY = 127;
    SSL_R_BIO_NOT_SET = 128;
    SSL_R_BLOCK_CIPHER_PAD_IS_WRONG = 129;
    SSL_R_BN_LIB = 130;
    SSL_R_CA_DN_LENGTH_MISMATCH = 131;
    SSL_R_CA_DN_TOO_LONG = 132;
    SSL_R_CCS_RECEIVED_EARLY = 133;
    SSL_R_CERTIFICATE_VERIFY_FAILED = 134;
    SSL_R_CERT_LENGTH_MISMATCH = 135;
    SSL_R_CHALLENGE_IS_DIFFERENT = 136;
    SSL_R_CIPHER_CODE_WRONG_LENGTH = 137;
    SSL_R_CIPHER_OR_HASH_UNAVAILABLE = 138;
    SSL_R_CIPHER_TABLE_SRC_ERROR = 139;
    SSL_R_CLIENTHELLO_TLSEXT = 226;
    SSL_R_COMPRESSED_LENGTH_TOO_LONG = 140;
    SSL_R_COMPRESSION_DISABLED = 343;
    SSL_R_COMPRESSION_FAILURE = 141;
    SSL_R_COMPRESSION_ID_NOT_WITHIN_PRIVATE_RANGE = 307;
    SSL_R_COMPRESSION_LIBRARY_ERROR = 142;
    SSL_R_CONNECTION_ID_IS_DIFFERENT = 143;
    SSL_R_CONNECTION_TYPE_NOT_SET = 144;
    SSL_R_COOKIE_MISMATCH = 308;
    SSL_R_DATA_BETWEEN_CCS_AND_FINISHED = 145;
    SSL_R_DATA_LENGTH_TOO_LONG = 146;
    SSL_R_DECRYPTION_FAILED = 147;
    SSL_R_DECRYPTION_FAILED_OR_BAD_RECORD_MAC = 281;
    SSL_R_DH_PUBLIC_VALUE_LENGTH_IS_WRONG = 148;
    SSL_R_DIGEST_CHECK_FAILED = 149;
    SSL_R_DTLS_MESSAGE_TOO_BIG = 334;
    SSL_R_DUPLICATE_COMPRESSION_ID = 309;
    SSL_R_ECC_CERT_NOT_FOR_KEY_AGREEMENT = 317;
    SSL_R_ECC_CERT_NOT_FOR_SIGNING = 318;
    SSL_R_ECC_CERT_SHOULD_HAVE_RSA_SIGNATURE = 322;
    SSL_R_ECC_CERT_SHOULD_HAVE_SHA1_SIGNATURE = 323;
    SSL_R_ECGROUP_TOO_LARGE_FOR_CIPHER = 310;
    SSL_R_EMPTY_SRTP_PROTECTION_PROFILE_LIST = 354;
    SSL_R_ENCRYPTED_LENGTH_TOO_LONG = 150;
    SSL_R_ERROR_GENERATING_TMP_RSA_KEY = 282;
    SSL_R_ERROR_IN_RECEIVED_CIPHER_LIST = 151;
    SSL_R_EXCESSIVE_MESSAGE_SIZE = 152;
    SSL_R_EXTRA_DATA_IN_MESSAGE = 153;
    SSL_R_GOT_A_FIN_BEFORE_A_CCS = 154;
    SSL_R_GOT_NEXT_PROTO_BEFORE_A_CCS = 355;
    SSL_R_GOT_NEXT_PROTO_WITHOUT_EXTENSION = 356;
    SSL_R_HTTPS_PROXY_REQUEST = 155;
    SSL_R_HTTP_REQUEST = 156;
    SSL_R_ILLEGAL_PADDING = 283;
    SSL_R_INAPPROPRIATE_FALLBACK = 373;
    SSL_R_INCONSISTENT_COMPRESSION = 340;
    SSL_R_INVALID_CHALLENGE_LENGTH = 158;
    SSL_R_INVALID_COMMAND = 280;
    SSL_R_INVALID_COMPRESSION_ALGORITHM = 341;
    SSL_R_INVALID_PURPOSE = 278;
    SSL_R_INVALID_SRP_USERNAME = 357;
    SSL_R_INVALID_STATUS_RESPONSE = 328;
    SSL_R_INVALID_TICKET_KEYS_LENGTH = 325;
    SSL_R_INVALID_TRUST = 279;
    SSL_R_KEY_ARG_TOO_LONG = 284;
    SSL_R_KRB5 = 285;
    SSL_R_KRB5_C_CC_PRINC = 286;
    SSL_R_KRB5_C_GET_CRED = 287;
    SSL_R_KRB5_C_INIT = 288;
    SSL_R_KRB5_C_MK_REQ = 289;
    SSL_R_KRB5_S_BAD_TICKET = 290;
    SSL_R_KRB5_S_INIT = 291;
    SSL_R_KRB5_S_RD_REQ = 292;
    SSL_R_KRB5_S_TKT_EXPIRED = 293;
    SSL_R_KRB5_S_TKT_NYV = 294;
    SSL_R_KRB5_S_TKT_SKEW = 295;
    SSL_R_LENGTH_MISMATCH = 159;
    SSL_R_LENGTH_TOO_SHORT = 160;
    SSL_R_LIBRARY_BUG = 274;
    SSL_R_LIBRARY_HAS_NO_CIPHERS = 161;
    SSL_R_MESSAGE_TOO_LONG = 296;
    SSL_R_MISSING_DH_DSA_CERT = 162;
    SSL_R_MISSING_DH_KEY = 163;
    SSL_R_MISSING_DH_RSA_CERT = 164;
    SSL_R_MISSING_DSA_SIGNING_CERT = 165;
    SSL_R_MISSING_EXPORT_TMP_DH_KEY = 166;
    SSL_R_MISSING_EXPORT_TMP_RSA_KEY = 167;
    SSL_R_MISSING_RSA_CERTIFICATE = 168;
    SSL_R_MISSING_RSA_ENCRYPTING_CERT = 169;
    SSL_R_MISSING_RSA_SIGNING_CERT = 170;
    SSL_R_MISSING_SRP_PARAM = 358;
    SSL_R_MISSING_TMP_DH_KEY = 171;
    SSL_R_MISSING_TMP_ECDH_KEY = 311;
    SSL_R_MISSING_TMP_RSA_KEY = 172;
    SSL_R_MISSING_TMP_RSA_PKEY = 173;
    SSL_R_MISSING_VERIFY_MESSAGE = 174;
    SSL_R_MULTIPLE_SGC_RESTARTS = 346;
    SSL_R_NON_SSLV2_INITIAL_PACKET = 175;
    SSL_R_NO_CERTIFICATES_RETURNED = 176;
    SSL_R_NO_CERTIFICATE_ASSIGNED = 177;
    SSL_R_NO_CERTIFICATE_RETURNED = 178;
    SSL_R_NO_CERTIFICATE_SET = 179;
    SSL_R_NO_CERTIFICATE_SPECIFIED = 180;
    SSL_R_NO_CIPHERS_AVAILABLE = 181;
    SSL_R_NO_CIPHERS_PASSED = 182;
    SSL_R_NO_CIPHERS_SPECIFIED = 183;
    SSL_R_NO_CIPHER_LIST = 184;
    SSL_R_NO_CIPHER_MATCH = 185;
    SSL_R_NO_CLIENT_CERT_METHOD = 331;
    SSL_R_NO_CLIENT_CERT_RECEIVED = 186;
    SSL_R_NO_COMPRESSION_SPECIFIED = 187;
    SSL_R_NO_GOST_CERTIFICATE_SENT_BY_PEER = 330;
    SSL_R_NO_METHOD_SPECIFIED = 188;
    SSL_R_NO_PRIVATEKEY = 189;
    SSL_R_NO_PRIVATE_KEY_ASSIGNED = 190;
    SSL_R_NO_PROTOCOLS_AVAILABLE = 191;
    SSL_R_NO_PUBLICKEY = 192;
    SSL_R_NO_RENEGOTIATION = 339;
    SSL_R_NO_REQUIRED_DIGEST = 324;
    SSL_R_NO_SHARED_CIPHER = 193;
    SSL_R_NO_SRTP_PROFILES = 359;
    SSL_R_NO_VERIFY_CALLBACK = 194;
    SSL_R_NULL_SSL_CTX = 195;
    SSL_R_NULL_SSL_METHOD_PASSED = 196;
    SSL_R_OLD_SESSION_CIPHER_NOT_RETURNED = 197;
    SSL_R_OLD_SESSION_COMPRESSION_ALGORITHM_NOT_RETURNED = 344;
    SSL_R_ONLY_TLS_ALLOWED_IN_FIPS_MODE = 297;
    SSL_R_OPAQUE_PRF_INPUT_TOO_LONG = 327;
    SSL_R_PACKET_LENGTH_TOO_LONG = 198;
    SSL_R_PARSE_TLSEXT = 227;
    SSL_R_PATH_TOO_LONG = 270;
    SSL_R_PEER_DID_NOT_RETURN_A_CERTIFICATE = 199;
    SSL_R_PEER_ERROR = 200;
    SSL_R_PEER_ERROR_CERTIFICATE = 201;
    SSL_R_PEER_ERROR_NO_CERTIFICATE = 202;
    SSL_R_PEER_ERROR_NO_CIPHER = 203;
    SSL_R_PEER_ERROR_UNSUPPORTED_CERTIFICATE_TYPE = 204;
    SSL_R_PRE_MAC_LENGTH_TOO_LONG = 205;
    SSL_R_PROBLEMS_MAPPING_CIPHER_FUNCTIONS = 206;
    SSL_R_PROTOCOL_IS_SHUTDOWN = 207;
    SSL_R_PSK_IDENTITY_NOT_FOUND = 223;
    SSL_R_PSK_NO_CLIENT_CB = 224;
    SSL_R_PSK_NO_SERVER_CB = 225;
    SSL_R_PUBLIC_KEY_ENCRYPT_ERROR = 208;
    SSL_R_PUBLIC_KEY_IS_NOT_RSA = 209;
    SSL_R_PUBLIC_KEY_NOT_RSA = 210;
    SSL_R_READ_BIO_NOT_SET = 211;
    SSL_R_READ_TIMEOUT_EXPIRED = 312;
    SSL_R_READ_WRONG_PACKET_TYPE = 212;
    SSL_R_RECORD_LENGTH_MISMATCH = 213;
    SSL_R_RECORD_TOO_LARGE = 214;
    SSL_R_RECORD_TOO_SMALL = 298;
    SSL_R_RENEGOTIATE_EXT_TOO_LONG = 335;
    SSL_R_RENEGOTIATION_ENCODING_ERR = 336;
    SSL_R_RENEGOTIATION_MISMATCH = 337;
    SSL_R_REQUIRED_CIPHER_MISSING = 215;
    SSL_R_REQUIRED_COMPRESSSION_ALGORITHM_MISSING = 342;
    SSL_R_REUSE_CERT_LENGTH_NOT_ZERO = 216;
    SSL_R_REUSE_CERT_TYPE_NOT_ZERO = 217;
    SSL_R_REUSE_CIPHER_LIST_NOT_ZERO = 218;
    SSL_R_SCSV_RECEIVED_WHEN_RENEGOTIATING = 345;
    SSL_R_SERVERHELLO_TLSEXT = 275;
    SSL_R_SESSION_ID_CONTEXT_UNINITIALIZED = 277;
    SSL_R_SHORT_READ = 219;
    SSL_R_SIGNATURE_ALGORITHMS_ERROR = 360;
    SSL_R_SIGNATURE_FOR_NON_SIGNING_CERTIFICATE = 220;
    SSL_R_SRP_A_CALC = 361;
    SSL_R_SRTP_COULD_NOT_ALLOCATE_PROFILES = 362;
    SSL_R_SRTP_PROTECTION_PROFILE_LIST_TOO_LONG = 363;
    SSL_R_SRTP_UNKNOWN_PROTECTION_PROFILE = 364;
    SSL_R_SSL23_DOING_SESSION_ID_REUSE = 221;
    SSL_R_SSL2_CONNECTION_ID_TOO_LONG = 299;
    SSL_R_SSL3_EXT_INVALID_ECPOINTFORMAT = 321;
    SSL_R_SSL3_EXT_INVALID_SERVERNAME = 319;
    SSL_R_SSL3_EXT_INVALID_SERVERNAME_TYPE = 320;
    SSL_R_SSL3_SESSION_ID_TOO_LONG = 300;
    SSL_R_SSL3_SESSION_ID_TOO_SHORT = 222;
    SSL_R_SSLV3_ALERT_BAD_CERTIFICATE = 1042;
    SSL_R_SSLV3_ALERT_BAD_RECORD_MAC = 1020;
    SSL_R_SSLV3_ALERT_CERTIFICATE_EXPIRED = 1045;
    SSL_R_SSLV3_ALERT_CERTIFICATE_REVOKED = 1044;
    SSL_R_SSLV3_ALERT_CERTIFICATE_UNKNOWN = 1046;
    SSL_R_SSLV3_ALERT_DECOMPRESSION_FAILURE = 1030;
    SSL_R_SSLV3_ALERT_HANDSHAKE_FAILURE = 1040;
    SSL_R_SSLV3_ALERT_ILLEGAL_PARAMETER = 1047;
    SSL_R_SSLV3_ALERT_NO_CERTIFICATE = 1041;
    SSL_R_SSLV3_ALERT_UNEXPECTED_MESSAGE = 1010;
    SSL_R_SSLV3_ALERT_UNSUPPORTED_CERTIFICATE = 1043;
    SSL_R_SSL_CTX_HAS_NO_DEFAULT_SSL_VERSION = 228;
    SSL_R_SSL_HANDSHAKE_FAILURE = 229;
    SSL_R_SSL_LIBRARY_HAS_NO_CIPHERS = 230;
    SSL_R_SSL_SESSION_ID_CALLBACK_FAILED = 301;
    SSL_R_SSL_SESSION_ID_CONFLICT = 302;
    SSL_R_SSL_SESSION_ID_CONTEXT_TOO_LONG = 273;
    SSL_R_SSL_SESSION_ID_HAS_BAD_LENGTH = 303;
    SSL_R_SSL_SESSION_ID_IS_DIFFERENT = 231;
    SSL_R_TLSV1_ALERT_ACCESS_DENIED = 1049;
    SSL_R_TLSV1_ALERT_DECODE_ERROR = 1050;
    SSL_R_TLSV1_ALERT_DECRYPTION_FAILED = 1021;
    SSL_R_TLSV1_ALERT_DECRYPT_ERROR = 1051;
    SSL_R_TLSV1_ALERT_EXPORT_RESTRICTION = 1060;
    SSL_R_TLSV1_ALERT_INAPPROPRIATE_FALLBACK = 1086;
    SSL_R_TLSV1_ALERT_INSUFFICIENT_SECURITY = 1071;
    SSL_R_TLSV1_ALERT_INTERNAL_ERROR = 1080;
    SSL_R_TLSV1_ALERT_NO_RENEGOTIATION = 1100;
    SSL_R_TLSV1_ALERT_PROTOCOL_VERSION = 1070;
    SSL_R_TLSV1_ALERT_RECORD_OVERFLOW = 1022;
    SSL_R_TLSV1_ALERT_UNKNOWN_CA = 1048;
    SSL_R_TLSV1_ALERT_USER_CANCELLED = 1090;
    SSL_R_TLSV1_BAD_CERTIFICATE_HASH_VALUE = 1114;
    SSL_R_TLSV1_BAD_CERTIFICATE_STATUS_RESPONSE = 1113;
    SSL_R_TLSV1_CERTIFICATE_UNOBTAINABLE = 1111;
    SSL_R_TLSV1_UNRECOGNIZED_NAME = 1112;
    SSL_R_TLSV1_UNSUPPORTED_EXTENSION = 1110;
    SSL_R_TLS_CLIENT_CERT_REQ_WITH_ANON_CIPHER = 232;
    SSL_R_TLS_HEARTBEAT_PEER_DOESNT_ACCEPT = 365;
    SSL_R_TLS_HEARTBEAT_PENDING = 366;
    SSL_R_TLS_ILLEGAL_EXPORTER_LABEL = 367;
    SSL_R_TLS_INVALID_ECPOINTFORMAT_LIST = 157;
    SSL_R_TLS_PEER_DID_NOT_RESPOND_WITH_CERTIFICATE_LIST = 233;
    SSL_R_TLS_RSA_ENCRYPTED_VALUE_LENGTH_IS_WRONG = 234;
    SSL_R_TRIED_TO_USE_UNSUPPORTED_CIPHER = 235;
    SSL_R_UNABLE_TO_DECODE_DH_CERTS = 236;
    SSL_R_UNABLE_TO_DECODE_ECDH_CERTS = 313;
    SSL_R_UNABLE_TO_EXTRACT_PUBLIC_KEY = 237;
    SSL_R_UNABLE_TO_FIND_DH_PARAMETERS = 238;
    SSL_R_UNABLE_TO_FIND_ECDH_PARAMETERS = 314;
    SSL_R_UNABLE_TO_FIND_PUBLIC_KEY_PARAMETERS = 239;
    SSL_R_UNABLE_TO_FIND_SSL_METHOD = 240;
    SSL_R_UNABLE_TO_LOAD_SSL2_MD5_ROUTINES = 241;
    SSL_R_UNABLE_TO_LOAD_SSL3_MD5_ROUTINES = 242;
    SSL_R_UNABLE_TO_LOAD_SSL3_SHA1_ROUTINES = 243;
    SSL_R_UNEXPECTED_MESSAGE = 244;
    SSL_R_UNEXPECTED_RECORD = 245;
    SSL_R_UNINITIALIZED = 276;
    SSL_R_UNKNOWN_ALERT_TYPE = 246;
    SSL_R_UNKNOWN_CERTIFICATE_TYPE = 247;
    SSL_R_UNKNOWN_CIPHER_RETURNED = 248;
    SSL_R_UNKNOWN_CIPHER_TYPE = 249;
    SSL_R_UNKNOWN_DIGEST = 368;
    SSL_R_UNKNOWN_KEY_EXCHANGE_TYPE = 250;
    SSL_R_UNKNOWN_PKEY_TYPE = 251;
    SSL_R_UNKNOWN_PROTOCOL = 252;
    SSL_R_UNKNOWN_REMOTE_ERROR_TYPE = 253;
    SSL_R_UNKNOWN_SSL_VERSION = 254;
    SSL_R_UNKNOWN_STATE = 255;
    SSL_R_UNSAFE_LEGACY_RENEGOTIATION_DISABLED = 338;
    SSL_R_UNSUPPORTED_CIPHER = 256;
    SSL_R_UNSUPPORTED_COMPRESSION_ALGORITHM = 257;
    SSL_R_UNSUPPORTED_DIGEST_TYPE = 326;
    SSL_R_UNSUPPORTED_ELLIPTIC_CURVE = 315;
    SSL_R_UNSUPPORTED_PROTOCOL = 258;
    SSL_R_UNSUPPORTED_SSL_VERSION = 259;
    SSL_R_UNSUPPORTED_STATUS_TYPE = 329;
    SSL_R_USE_SRTP_NOT_NEGOTIATED = 369;
    SSL_R_WRITE_BIO_NOT_SET = 260;
    SSL_R_WRONG_CIPHER_RETURNED = 261;
    SSL_R_WRONG_MESSAGE_TYPE = 262;
    SSL_R_WRONG_NUMBER_OF_KEY_BITS = 263;
    SSL_R_WRONG_SIGNATURE_LENGTH = 264;
    SSL_R_WRONG_SIGNATURE_SIZE = 265;
    SSL_R_WRONG_SIGNATURE_TYPE = 370;
    SSL_R_WRONG_SSL_VERSION = 266;
    SSL_R_WRONG_VERSION_NUMBER = 267;
    SSL_R_X509_LIB = 268;
    SSL_R_X509_VERIFICATION_SETUP_PROBLEMS = 269;

    NID_undef                       =  0;  //AG
    NID_rsaEncryption               =  6;  //AG
    NID_commonName                  = 13;  //AG
    NID_countryName                 = 14;  //AG
    NID_localityName                = 15;  //AG
    NID_stateOrProvinceName         = 16;  //AG
    NID_organizationName            = 17;  //AG
    NID_organizationalUnitName      = 18;  //AG
    NID_pkcs7                       = 20;
    NID_pkcs7_data                  = 21;
    NID_pkcs7_signed                = 22;
    NID_pkcs7_enveloped             = 23;
    NID_pkcs7_signedAndEnveloped    = 24;
    NID_pkcs7_digest                = 25;
    NID_pkcs7_encrypted             = 26;
    NID_dhKeyAgreement              = 28;  // Angus
    NID_pkcs9_emailAddress          = 48;  //AG
    NID_netscape                    = 57;
    NID_netscape_cert_extension     = 58;
    NID_netscape_data_type          = 59;
    NID_netscape_base_url           = 72;
    NID_netscape_ca_revocation_url  = 74;
    NID_netscape_cert_type          = 71;
    NID_netscape_revocation_url     = 73;
    NID_netscape_renewal_url        = 75;
    NID_netscape_ca_policy_url      = 76;
    NID_netscape_ssl_server_name    = 77;
    NID_netscape_comment            = 78;
    NID_netscape_cert_sequence      = 79;
    NID_subject_key_identifier      = 82;
    NID_key_usage                   = 83;
    NID_private_key_usage_period    = 84;
    NID_subject_alt_name            = 85;  //AG
    NID_issuer_alt_name             = 86;
    NID_basic_constraints           = 87;
    NID_certificate_policies        = 89;
    NID_givenName                   = 99;  // Angus
    NID_surname                     = 100; // Angus
    NID_initials                    = 101; // Angus
    NID_x500UniqueIdentifier        = 102; // Angus
    NID_crl_distribution_points     = 103; //AG
    NID_serialNumber                = 105; // Angus
    NID_title                       = 106; // Angus
    NID_description                 = 107; // Angus
    NID_dsa                         = 116;
    NID_ext_key_usage               = 126;
    NID_X9_62_id_ecPublicKey        = 408;
    NID_X9_62_prime192v1            = 409; // Angus elliptic curves
    NID_X9_62_prime192v2            = 410; // Angus
    NID_X9_62_prime192v3            = 411; // Angus
    NID_X9_62_prime239v1            = 412; // Angus
    NID_X9_62_prime239v2            = 413; // Angus
    NID_X9_62_prime239v3            = 414; // Angus
    NID_X9_62_prime256v1            = 415; // Angus  NIST Prime-Curve P-256
    NID_secp224k1                   = 713; // Angus  NIST Prime-Curve P-224
    NID_secp256k1                   = 714; // Angus  NID_X9_62_prime256v1 NIST Prime-Curve P-256
    NID_secp384r1                   = 715; // Angus  NIST Prime-Curve P-384
    NID_secp521r1                   = 716; // Angus  NIST Prime-Curve P-521

    { Asn1.h - For use with ASN1_mbstring_copy() } //AG
    MBSTRING_FLAG  = $1000;               //AG
    MBSTRING_ASC   = MBSTRING_FLAG or 1;  //AG
    MBSTRING_BMP   = MBSTRING_FLAG or 2;  //AG
    { 0.9.7 }
    MBSTRING_UNIV  : Longword = MBSTRING_FLAG or 3;
    MBSTRING_UTF8  : Longword = MBSTRING_FLAG or 4;
    (*
    { 0.9.8 they are set dynamically on load }
    MBSTRING_UNIV  = MBSTRING_FLAG or 4;  //AG
    MBSTRING_UTF8  = MBSTRING_FLAG;       //AG
    *)

    RSA_F4         = $10001;              //AG
    EVP_PKEY_RSA   = NID_rsaEncryption;   //AG
    EVP_PKEY_DSA   = NID_dsa;
    EVP_PKEY_EC    = NID_X9_62_id_ecPublicKey;


    EVP_MAX_MD_SIZE                   = 64; //* longest known is SHA512 */

    { Crypto.h - params for f_SSLeay_version() }
    SSLEAY_VERSION      = 0;
    SSLEAY_OPTIONS      = 1; //no longer supported
    SSLEAY_CFLAGS       = 2;
    SSLEAY_BUILT_ON     = 3;
    SSLEAY_PLATFORM     = 4;
    SSLEAY_DIR          = 5; // since 0.9.7


    X509_V_OK                                           = 0;
    // illegal error (for uninitialized values, to avoid X509_V_OK): 1
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT                = 2;
    X509_V_ERR_UNABLE_TO_GET_CRL                        = 3;
    X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE         = 4;
    X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE          = 5;
    X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY       = 6;
    X509_V_ERR_CERT_SIGNATURE_FAILURE                   = 7;
    X509_V_ERR_CRL_SIGNATURE_FAILURE                    = 8;
    X509_V_ERR_CERT_NOT_YET_VALID                       = 9;
    X509_V_ERR_CERT_HAS_EXPIRED                         = 10;
    X509_V_ERR_CRL_NOT_YET_VALID                        = 11;
    X509_V_ERR_CRL_HAS_EXPIRED                          = 12;
    X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD           = 13;
    X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD            = 14;
    X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD           = 15;
    X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD           = 16;
    X509_V_ERR_OUT_OF_MEM                               = 17;
    X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT              = 18;
    X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN                = 19;
    X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY        = 20;
    X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE          = 21;
    X509_V_ERR_CERT_CHAIN_TOO_LONG                      = 22;
    X509_V_ERR_CERT_REVOKED                             = 23;
    X509_V_ERR_INVALID_CA                               = 24;
    X509_V_ERR_PATH_LENGTH_EXCEEDED                     = 25;
    X509_V_ERR_INVALID_PURPOSE                          = 26;
    X509_V_ERR_CERT_UNTRUSTED                           = 27;
    X509_V_ERR_CERT_REJECTED                            = 28;
    // These are 'informational' when looking for issuer cert
    X509_V_ERR_SUBJECT_ISSUER_MISMATCH                  = 29;
    X509_V_ERR_AKID_SKID_MISMATCH                       = 30;
    X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH              = 31;
    X509_V_ERR_KEYUSAGE_NO_CERTSIGN                     = 32;

    X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER                 = 33;
    X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION             = 34;
    X509_V_ERR_KEYUSAGE_NO_CRL_SIGN                     = 35;
    X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION         = 36;
    X509_V_ERR_INVALID_NON_CA                           = 37;
    X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED               = 38;
    X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE            = 39;
    X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED           = 40;

    X509_V_ERR_INVALID_EXTENSION                        = 41;
    X509_V_ERR_INVALID_POLICY_EXTENSION                 = 42;
    X509_V_ERR_NO_EXPLICIT_POLICY                       = 43;
    X509_V_ERR_UNNESTED_RESOURCE                        = 44;

    // The application is not happy
    X509_V_ERR_APPLICATION_VERIFICATION                 = 50;

{$IFDEF OPENSSL_USE_RESOURCE_STRINGS}
resourcestring
  { Verify error strings from x509_txt.c }
  sX509_V_OK                                      = 'ok';
  sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT           = 'unable to get issuer certificate';
  sX509_V_ERR_UNABLE_TO_GET_CRL                   = 'unable to get certificate CRL';
  sX509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE    = 'unable to decrypt certificate''s signature';
  sX509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE     = 'unable to decrypt CRL''s signature';
  sX509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY  = 'unable to decode issuer public key';
  sX509_V_ERR_CERT_SIGNATURE_FAILURE              = 'certificate signature failure';
  sX509_V_ERR_CRL_SIGNATURE_FAILURE               = 'CRL signature failure';
  sX509_V_ERR_CERT_NOT_YET_VALID                  = 'certificate is not yet valid';
  sX509_V_ERR_CRL_NOT_YET_VALID                   = 'CRL is not yet valid';
  sX509_V_ERR_CERT_HAS_EXPIRED                    = 'certificate has expired';
  sX509_V_ERR_CRL_HAS_EXPIRED                     = 'CRL has expired';
  sX509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD      = 'format error in certificate''s notBefore field';
  sX509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD       = 'format error in certificate''s notAfter field';
  sX509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD      = 'format error in CRL''s lastUpdate field';
  sX509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD      = 'format error in CRL''s nextUpdate field';
  sX509_V_ERR_OUT_OF_MEM                          = 'out of memory';
  sX509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT         = 'self signed certificate';
  sX509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN           = 'self signed certificate in certificate chain';
  sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY   = 'unable to get local issuer certificate';
  sX509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE     = 'unable to verify the first certificate';
  sX509_V_ERR_CERT_CHAIN_TOO_LONG                 = 'certificate chain too long';
  sX509_V_ERR_CERT_REVOKED                        = 'certificate revoked';
  sX509_V_ERR_INVALID_CA                          = 'invalid CA certificate';
  sX509_V_ERR_INVALID_NON_CA                      = 'invalid non-CA certificate (has CA markings)';
  sX509_V_ERR_PATH_LENGTH_EXCEEDED                = 'path length constraint exceeded';
  sX509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED          = 'proxy path length constraint exceeded';
  sX509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED      = 'proxy certificates not allowed, please set the appropriate flag';
  sX509_V_ERR_INVALID_PURPOSE                     = 'unsupported certificate purpose';
  sX509_V_ERR_CERT_UNTRUSTED                      = 'certificate not trusted';
  sX509_V_ERR_CERT_REJECTED                       = 'certificate rejected';
  sX509_V_ERR_APPLICATION_VERIFICATION            = 'application verification failure';
  sX509_V_ERR_SUBJECT_ISSUER_MISMATCH             = 'subject issuer mismatch';
  sX509_V_ERR_AKID_SKID_MISMATCH                  = 'authority and subject key identifier mismatch';
  sX509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH         = 'authority and issuer serial number mismatch';
  sX509_V_ERR_KEYUSAGE_NO_CERTSIGN                = 'key usage does not include certificate signing';
  sX509_V_ERR_UNABLE_TO_GET_CRL_ISSUER            = 'unable to get CRL issuer certificate';
  sX509_V_ERR_UNHANDLED_CRITICAL_EXTENSION        = 'unhandled critical extension';
  sX509_V_ERR_KEYUSAGE_NO_CRL_SIGN                = 'key usage does not include CRL signing';
  sX509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE       = 'key usage does not include digital signature';
  sX509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION    = 'unhandled critical CRL extension';
  sX509_V_ERR_INVALID_EXTENSION                   = 'invalid or inconsistent certificate extension';
  sX509_V_ERR_INVALID_POLICY_EXTENSION            = 'invalid or inconsistent certificate policy extension';
  sX509_V_ERR_NO_EXPLICIT_POLICY                  = 'no explicit policy';
  sX509_V_ERR_UNNESTED_RESOURCE                   = 'RFC 3779 resource not subset of parent''s resources';
  sX509_V_ERR_NUMBER                              = 'Error number ';

const
{$ENDIF}

  { Lock IDs for use with CRYPTO_lock() }
  CRYPTO_LOCK_ERR                     = 1;
  CRYPTO_LOCK_EX_DATA                 = 2;
  CRYPTO_LOCK_X509                    = 3;
  CRYPTO_LOCK_X509_INFO               = 4;
  CRYPTO_LOCK_X509_PKEY               = 5;
  CRYPTO_LOCK_X509_CRL                = 6;
  CRYPTO_LOCK_X509_REQ                = 7;
  CRYPTO_LOCK_DSA                     = 8;
  CRYPTO_LOCK_RSA                     = 9;
  CRYPTO_LOCK_EVP_PKEY                = 10;
  CRYPTO_LOCK_X509_STORE              = 11;
  CRYPTO_LOCK_SSL_CTX                 = 12;
  CRYPTO_LOCK_SSL_CERT                = 13;
  CRYPTO_LOCK_SSL_SESSION             = 14;
  CRYPTO_LOCK_SSL_SESS_CERT           = 15;
  CRYPTO_LOCK_SSL                     = 16;
  CRYPTO_LOCK_SSL_METHOD              = 17;
  CRYPTO_LOCK_RAND                    = 18;
  CRYPTO_LOCK_RAND2                   = 19;
  CRYPTO_LOCK_MALLOC                  = 20;
  CRYPTO_LOCK_BIO                     = 21;
  CRYPTO_LOCK_GETHOSTBYNAME           = 22;
  CRYPTO_LOCK_GETSERVBYNAME           = 23;
  CRYPTO_LOCK_READDIR                 = 24;
  CRYPTO_LOCK_RSA_BLINDING            = 25;
  CRYPTO_LOCK_DH                      = 26;
  CRYPTO_LOCK_MALLOC2                 = 27;
  CRYPTO_LOCK_DSO                     = 28;
  CRYPTO_LOCK_DYNLOCK                 = 29;
  CRYPTO_LOCK_ENGINE                  = 30;
  CRYPTO_LOCK_UI                      = 31;
  CRYPTO_LOCK_ECDSA                   = 32;
  CRYPTO_LOCK_EC                      = 33;
  CRYPTO_LOCK_ECDH                    = 34;
  CRYPTO_LOCK_BN                      = 35;
  CRYPTO_LOCK_EC_PRE_COMP             = 36;
  CRYPTO_LOCK_STORE                   = 37;
  CRYPTO_LOCK_COMP                    = 38;
  CRYPTO_LOCK_FIPS                    = 39;
  CRYPTO_LOCK_FIPS2                   = 40;
  CRYPTO_NUM_LOCKS                    = 41;

  { mode param of CRYPTO_lock()                                              }
  { These values are pairwise exclusive, with undefined behaviour if misused }
  {(for example, CRYPTO_READ and CRYPTO_WRITE should not be used together):  }
  CRYPTO_LOCK                         = 1;
  CRYPTO_UNLOCK                       = 2;
  CRYPTO_READ                         = 4;
  CRYPTO_WRITE                        = 8;

    // Certificate verify flags

    // Send issuer+subject checks to verify_cb
    X509_V_FLAG_CB_ISSUER_CHECK                         = $1;
    // Use check time instead of current time
    X509_V_FLAG_USE_CHECK_TIME                          = $2;
    // Lookup CRLs
    X509_V_FLAG_CRL_CHECK                               = $4;
    // Lookup CRLs for whole chain
    X509_V_FLAG_CRL_CHECK_ALL                           = $8;
    // Ignore unhandled critical extensions
    X509_V_FLAG_IGNORE_CRITICAL                         = $10;
    // Disable workarounds for broken certificates
    X509_V_FLAG_X509_STRICT                             = $20;
    // Enable proxy certificate validation
    X509_V_FLAG_ALLOW_PROXY_CERTS                       = $40;

    //Purposes
    X509_PURPOSE_SSL_CLIENT                             = 1;
    X509_PURPOSE_SSL_SERVER                             = 2;
    X509_PURPOSE_NS_SSL_SERVER                          = 3;
    X509_PURPOSE_SMIME_SIGN                             = 4;
    X509_PURPOSE_SMIME_ENCRYPT                          = 5;
    X509_PURPOSE_CRL_SIGN                               = 6;
    X509_PURPOSE_ANY                                    = 7;
    X509_PURPOSE_OCSP_HELPER                            = 8;

    X509_PURPOSE_MIN                                    = 1;
    X509_PURPOSE_MAX                                    = 8;

{$IFNDEF OPENSSL_NO_ENGINE}
//const
    // engine.h //

    //* These flags are used to control combinations of algorithm (methods)
    //* by bitwise "OR"ing.
    ENGINE_METHOD_RSA                   = $0001;
    ENGINE_METHOD_DSA                   = $0002;
    ENGINE_METHOD_DH                    = $0004;
    ENGINE_METHOD_RAND                  = $0008;
    ENGINE_METHOD_ECDH                  = $0010;
    ENGINE_METHOD_ECDSA                 = $0020;
    ENGINE_METHOD_CIPHERS               = $0040;
    ENGINE_METHOD_DIGESTS               = $0080;
    ENGINE_METHOD_STORE                 = $0100;
    //* Obvious all-or-nothing cases. */
    ENGINE_METHOD_ALL                   = $FFFF;
    ENGINE_METHOD_NONE                  = $0000;

    //* Error codes for the ENGINE functions. */

    //* Function codes. */
    {
    ENGINE_F_DYNAMIC_CTRL                 = 180;
    ENGINE_F_DYNAMIC_GET_DATA_CTX         = 181;
    ENGINE_F_DYNAMIC_LOAD                 = 182;
    ENGINE_F_DYNAMIC_SET_DATA_CTX         = 183;
    ENGINE_F_ENGINE_ADD                   = 105;
    ENGINE_F_ENGINE_BY_ID                 = 106;
    ENGINE_F_ENGINE_CMD_IS_EXECUTABLE     = 170;
    ENGINE_F_ENGINE_CTRL                  = 142;
    ENGINE_F_ENGINE_CTRL_CMD              = 178;
    ENGINE_F_ENGINE_CTRL_CMD_STRING       = 171;
    ENGINE_F_ENGINE_FINISH                = 107;
    ENGINE_F_ENGINE_FREE_UTIL             = 108;
    ENGINE_F_ENGINE_GET_CIPHER            = 185;
    ENGINE_F_ENGINE_GET_DEFAULT_TYPE      = 177;
    ENGINE_F_ENGINE_GET_DIGEST            = 186;
    ENGINE_F_ENGINE_GET_NEXT              = 115;
    ENGINE_F_ENGINE_GET_PREV              = 116;
    ENGINE_F_ENGINE_INIT                  = 119;
    ENGINE_F_ENGINE_LIST_ADD              = 120;
    ENGINE_F_ENGINE_LIST_REMOVE           = 121;
    ENGINE_F_ENGINE_LOAD_PRIVATE_KEY      = 150;
    ENGINE_F_ENGINE_LOAD_PUBLIC_KEY       = 151;
    ENGINE_F_ENGINE_LOAD_SSL_CLIENT_CERT  = 192;
    ENGINE_F_ENGINE_NEW                   = 122;
    ENGINE_F_ENGINE_REMOVE                = 123;
    ENGINE_F_ENGINE_SET_DEFAULT_STRING    = 189;
    ENGINE_F_ENGINE_SET_DEFAULT_TYPE      = 126;
    ENGINE_F_ENGINE_SET_ID                = 129;
    ENGINE_F_ENGINE_SET_NAME              = 130;
    ENGINE_F_ENGINE_TABLE_REGISTER        = 184;
    ENGINE_F_ENGINE_UNLOAD_KEY            = 152;
    ENGINE_F_ENGINE_UNLOCKED_FINISH       = 191;
    ENGINE_F_ENGINE_UP_REF                = 190;
    ENGINE_F_INT_CTRL_HELPER              = 172;
    ENGINE_F_INT_ENGINE_CONFIGURE         = 188;
    ENGINE_F_INT_ENGINE_MODULE_INIT       = 187;
    ENGINE_F_LOG_MESSAGE                  = 141;
    }
    //* Reason codes. */
    {
    ENGINE_R_ALREADY_LOADED               = 100;
    ENGINE_R_ARGUMENT_IS_NOT_A_NUMBER     = 133;
    ENGINE_R_CMD_NOT_EXECUTABLE           = 134;
    ENGINE_R_COMMAND_TAKES_INPUT          = 135;
    ENGINE_R_COMMAND_TAKES_NO_INPUT       = 136;
    ENGINE_R_CONFLICTING_ENGINE_ID        = 103;
    ENGINE_R_CTRL_COMMAND_NOT_IMPLEMENTED = 119;
    ENGINE_R_DH_NOT_IMPLEMENTED           = 139;
    ENGINE_R_DSA_NOT_IMPLEMENTED          = 140;
    ENGINE_R_DSO_FAILURE                  = 104;
    ENGINE_R_DSO_NOT_FOUND                = 132;
    ENGINE_R_ENGINES_SECTION_ERROR        = 148;
    ENGINE_R_ENGINE_IS_NOT_IN_LIST        = 105;
    ENGINE_R_ENGINE_SECTION_ERROR         = 149;
    ENGINE_R_FAILED_LOADING_PRIVATE_KEY   = 128;
    ENGINE_R_FAILED_LOADING_PUBLIC_KEY    = 129;
    ENGINE_R_FINISH_FAILED                = 106;
    ENGINE_R_GET_HANDLE_FAILED            = 107;
    ENGINE_R_ID_OR_NAME_MISSING           = 108;
    ENGINE_R_INIT_FAILED                  = 109;
    ENGINE_R_INTERNAL_LIST_ERROR          = 110;
    ENGINE_R_INVALID_ARGUMENT             = 143;
    ENGINE_R_INVALID_CMD_NAME             = 137;
    ENGINE_R_INVALID_CMD_NUMBER           = 138;
    ENGINE_R_INVALID_INIT_VALUE           = 151;
    ENGINE_R_INVALID_STRING               = 150;
    ENGINE_R_NOT_INITIALISED              = 117;
    ENGINE_R_NOT_LOADED                   = 112;
    ENGINE_R_NO_CONTROL_FUNCTION          = 120;
    ENGINE_R_NO_INDEX                     = 144;
    ENGINE_R_NO_LOAD_FUNCTION             = 125;
    ENGINE_R_NO_REFERENCE                 = 130;
    ENGINE_R_NO_SUCH_ENGINE               = 116;
    ENGINE_R_NO_UNLOAD_FUNCTION           = 126;
    ENGINE_R_PROVIDE_PARAMETERS           = 113;
    ENGINE_R_RSA_NOT_IMPLEMENTED          = 141;
    ENGINE_R_UNIMPLEMENTED_CIPHER         = 146;
    ENGINE_R_UNIMPLEMENTED_DIGEST         = 147;
    ENGINE_R_VERSION_INCOMPATIBILITY      = 145;
    }
{$ENDIF}

//const
    BIO_CTRL_RESET         = 1;  // opt - rewind/zero etc
    BIO_CTRL_EOF           = 2;  // opt - are we at the eof
    BIO_CTRL_INFO          = 3;  // opt - extra tit-bits
    BIO_CTRL_SET           = 4;  // man - set the 'IO' type
    BIO_CTRL_GET           = 5;  // man - get the 'IO' type
    BIO_CTRL_PUSH          = 6;  // opt - internal, used to signify change
    BIO_CTRL_POP           = 7;  // opt - internal, used to signify change
    BIO_CTRL_GET_CLOSE     = 8;  // man - set the 'close' on free
    BIO_CTRL_SET_CLOSE     = 9;  // man - set the 'close' on free
    BIO_CTRL_PENDING       = 10; // opt - is their more data buffered
    BIO_CTRL_FLUSH         = 11; // opt - 'flush' buffered output
    BIO_CTRL_DUP           = 12; // man - extra stuff for 'duped' BIO
    BIO_CTRL_WPENDING      = 13; // opt - number of bytes still to write
    BIO_CTRL_SET_CALLBACK  = 14; // opt - set callback function
    BIO_CTRL_GET_CALLBACK  = 15; // opt - set callback function
    BIO_CTRL_SET_FILENAME  = 30; // BIO_s_file special

    BIO_C_SET_CONNECT                       = 100;
    BIO_C_DO_STATE_MACHINE                  = 101;
    BIO_C_SET_NBIO                          = 102;
    BIO_C_SET_PROXY_PARAM                   = 103;
    BIO_C_SET_FD                            = 104;
    BIO_C_GET_FD                            = 105;
    BIO_C_SET_FILE_PTR                      = 106;
    BIO_C_GET_FILE_PTR                      = 107;
    BIO_C_SET_FILENAME                      = 108;
    BIO_C_SET_SSL                           = 109;
    BIO_C_GET_SSL                           = 110;
    BIO_C_SET_MD                            = 111;
    BIO_C_GET_MD                            = 112;
    BIO_C_GET_CIPHER_STATUS                 = 113;
    BIO_C_SET_BUF_MEM                       = 114;
    BIO_C_GET_BUF_MEM_PTR                   = 115;
    BIO_C_GET_BUFF_NUM_LINES                = 116;
    BIO_C_SET_BUFF_SIZE                     = 117;
    BIO_C_SET_ACCEPT                        = 118;
    BIO_C_SSL_MODE                          = 119;
    BIO_C_GET_MD_CTX                        = 120;
    BIO_C_GET_PROXY_PARAM                   = 121;
    BIO_C_SET_BUFF_READ_DATA                = 122; // data to read first
    BIO_C_GET_CONNECT                       = 123;
    BIO_C_GET_ACCEPT                        = 124;
    BIO_C_SET_SSL_RENEGOTIATE_BYTES         = 125;
    BIO_C_GET_SSL_NUM_RENEGOTIATES          = 126;
    BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT       = 127;
    BIO_C_FILE_SEEK                         = 128;
    BIO_C_GET_CIPHER_CTX                    = 129;
    BIO_C_SET_BUF_MEM_EOF_RETURN            = 130; // return end of input value
    BIO_C_SET_BIND_MODE                     = 131;
    BIO_C_GET_BIND_MODE                     = 132;
    BIO_C_FILE_TELL                         = 133;
    BIO_C_GET_SOCKS                         = 134;
    BIO_C_SET_SOCKS                         = 135;

    BIO_C_SET_WRITE_BUF_SIZE                = 136; // for BIO_s_bio
    BIO_C_GET_WRITE_BUF_SIZE                = 137;
    BIO_C_MAKE_BIO_PAIR                     = 138;
    BIO_C_DESTROY_BIO_PAIR                  = 139;
    BIO_C_GET_WRITE_GUARANTEE               = 140;
    BIO_C_GET_READ_REQUEST                  = 141;
    BIO_C_SHUTDOWN_WR                       = 142;
    BIO_C_NREAD0                            = 143;
    BIO_C_NREAD                             = 144;
    BIO_C_NWRITE0                           = 145;
    BIO_C_NWRITE                            = 146;
    BIO_C_RESET_READ_REQUEST                = 147;

    BIO_NOCLOSE                             = 0;
    BIO_CLOSE                               = 1;

//const
    BIO_FLAGS_READ                          = 1;
    BIO_FLAGS_WRITE                         = 2;
    BIO_FLAGS_IO_SPECIAL                    = 4;
    BIO_FLAGS_RWS                           = (BIO_FLAGS_READ or
                                               BIO_FLAGS_WRITE or
                                               BIO_FLAGS_IO_SPECIAL);
    BIO_FLAGS_SHOULD_RETRY                  = 8;

//const
    // These are passed by the BIO callback //
    BIO_CB_FREE     = $01;
    BIO_CB_READ     = $02;
    BIO_CB_WRITE    = $03;
    BIO_CB_PUTS     = $04;
    BIO_CB_GETS     = $05;
    BIO_CB_CTRL     = $06;

    // The callback is called before and after the underling operation,
    // The BIO_CB_RETURN flag indicates if it is after the call
    BIO_CB_RETURN   = $80;

//const
    X509V3_EXT_DYNAMIC      = $1;
    X509V3_EXT_CTX_DEP      = $2;
    X509V3_EXT_MULTILINE    = $4;

{$IFNDEF OPENSSL_NO_ENGINE}
type
    TUi_method_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_METHOD = ^TUi_method_st;

    TUi_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI = ^TUi_st;

    TUi_string_st = record
        Dummy : array [0..0] of Byte;
    end;
    PUI_STRING = ^TUi_string_st;

    TPinCallBack = function(ui: PUI; uis: PUI_STRING): Integer; cdecl; //AG
{$ENDIF}
{$IFDEF OPENSSL_USE_DELPHI_MM}
type
    TCryptoMallocFunc   = function(Size: size_t): Pointer; cdecl;
    TCryptoReallocFunc  = function(P: Pointer; Size: size_t): Pointer; cdecl;
    TCryptoFreeMemFunc  = procedure(P: Pointer); cdecl;
{$ENDIF}

const
    f_SSLeay :                                 function: Longword; cdecl = nil; //AG
    f_SSLeay_version :                         function(t: Integer): PAnsiChar; cdecl = nil; //AG
    f_ERR_get_error_line_data :                function(const FileName: PPAnsiChar; Line: PInteger; const Data: PPAnsiChar; Flags: PInteger): Cardinal; cdecl = nil;
    f_ERR_peek_error :                         function : Cardinal; cdecl = nil;
    f_ERR_peek_last_error :                    function : Cardinal; cdecl = nil;
    f_ERR_get_error :                          function: Cardinal; cdecl = nil;
    f_ERR_error_string :                       function(Err: Cardinal; Buf: PAnsiChar): PAnsiChar; cdecl = nil;
    f_ERR_error_string_n :                     procedure(Err: Cardinal; Buf: PAnsiChar; Len: size_t); cdecl = nil;
    f_ERR_clear_error :                        procedure; cdecl = nil; //empties the current thread's error queue
    f_ERR_lib_error_string:                    function (e: Cardinal): PAnsiChar; cdecl = nil;   { V8.11 }
    f_ERR_func_error_string:                   function (e: Cardinal): PAnsiChar; cdecl = nil;
    f_ERR_reason_error_string:                 function (e: Cardinal): PAnsiChar; cdecl = nil;
    f_ERR_load_crypto_strings:                 procedure; cdecl = nil;

    { Note that ERR_remove_state() is now deprecated, because it is tied
     to the assumption that thread IDs are numeric.  ERR_remove_state(0)
     to free the current thread's error state should be replaced by
     ERR_remove_thread_state(nil). }
    f_ERR_remove_state :                       procedure(ThreadID: Longword); cdecl = nil;
    { Next is v1.0.0+ ** check for nil ** }
    f_ERR_remove_thread_state :                procedure(tid: PCRYPTO_THREADID); cdecl = nil;
    f_ERR_free_strings :                       procedure; cdecl = nil; //"Brutal" (thread-unsafe) Application-global cleanup functions
    f_RAND_seed :                              procedure(Buf: Pointer; Num: Integer); cdecl = nil;

    f_BIO_new :                                function(BioMethods: PBIO_METHOD): PBIO; cdecl = nil;
    f_BIO_new_socket :                         function(Sock: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_new_fd :                             function(Fd: Integer; CloseFlag: Integer): PBIO; cdecl = nil;
    f_BIO_new_file :                           function(FileName: PAnsiChar; Mode: PAnsiChar): PBIO; cdecl = nil;
    f_BIO_new_mem_buf :                        function(Buf : Pointer; Len : Integer): PBIO; cdecl = nil;
    f_BIO_new_bio_pair :                       function(Bio1: PPBIO; WriteBuf1: size_t; Bio2: PPBIO; WriteBuf2: size_t): Integer; cdecl = nil;

    f_BIO_ctrl :                               function(bp: PBIO; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt; cdecl = nil;
    f_BIO_ctrl_pending :                       function(b: PBIO): size_t; cdecl = nil;
    f_BIO_ctrl_get_write_guarantee :           function(b: PBIO): size_t; cdecl = nil;
    f_BIO_ctrl_get_read_request :              function(b: PBIO): size_t; cdecl = nil;

    f_BIO_s_mem :                              function : PBIO_METHOD; cdecl = nil;
    f_BIO_get_retry_BIO :                      function(B: PBIO; Reason : PInteger): PBIO; cdecl = nil;
    f_BIO_get_retry_reason :                   function(B: PBIO): Integer; cdecl = nil;
    f_BIO_free :                               function(B: PBIO): Integer; cdecl = nil;
    f_BIO_read :                               function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;
    f_BIO_nread :                              function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nread0 :                             function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_nwrite :                             function(B: PBIO; PBuf: PPAnsiChar; Num: Integer): Integer; cdecl = nil;
    f_BIO_nwrite0 :                            function(B: PBIO; PBuf: PPAnsiChar): Integer; cdecl = nil;
    f_BIO_gets :                               function(B: PBIO; Buf: PAnsiChar; Size: Integer): Integer; cdecl = nil;
    f_BIO_puts :                               function(B: PBIO; Buf: PAnsiChar): Integer; cdecl = nil;
    f_BIO_push :                               function(B: PBIO; B_Append: PBIO): PBIO; cdecl = nil;
    f_BIO_write :                              function(B: PBIO; Buf: Pointer; Len: Integer): Integer; cdecl = nil;

    f_BN_new:                                  function : PBIGNUM; cdecl = nil;                 { V8.11 }
    f_BN_free:                                 procedure (a: PBIGNUM); cdecl = nil;             
    f_BN_set_word:                             function (a: PBIGNUM; w: BN_ULONG): Integer; cdecl = nil;

    f_d2i_X509_bio :                           function(B: PBIO; X509: PPX509): PX509; cdecl = nil;
    f_i2d_X509_bio :                           function(B: PBIO; X509: PX509): Integer; cdecl = nil;
    f_d2i_PrivateKey_bio :                     function(B: PBIO; A: PPEVP_PKEY): PEVP_PKEY; cdecl = nil;//AG
    f_i2d_PrivateKey_bio :                     function(B: PBIO; pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_d2i_X509 :                               function(C509: PPX509; Buf: PPAnsiChar; Len: Integer): PX509; cdecl = nil;
    f_d2i_PKCS12_bio :                         function(B: PBIO; p12: PPPKCS12): PPKCS12; cdecl = nil; //AG
    f_i2d_PKCS12_bio :                         function(B: PBIO; p12: PPKCS12): Integer; cdecl = nil;
    f_d2i_PKCS7_bio:                           function(B: PBIO; p7: PPKCS7): PPKCS7; cdecl = nil; //AG

    f_CRYPTO_lock :                            procedure(mode, n: Longint; file_: PAnsiChar; line: Longint); cdecl = nil; //AG
    f_CRYPTO_add_lock :                        procedure(IntPtr: PInteger; amount: Integer; type_: Integer; const file_ : PAnsiChar; line: Integer); cdecl = nil;
    f_CRYPTO_num_locks :                       function: Integer; cdecl = nil;
    f_CRYPTO_set_id_callback :                 procedure(CB : TStatLockIDCallback); cdecl = nil;
    { Next three functions are v1.0.0+ only. ** Check for nil at runtime ** }
    f_CRYPTO_THREADID_set_callback :           function(CB : TCryptoThreadIDCallback) : Integer; cdecl = nil;
    // Only use CRYPTO_THREADID_set_[numeric|pointer]() within callbacks
    f_CRYPTO_THREADID_set_numeric :            procedure(id : PCRYPTO_THREADID; val: LongWord); cdecl = nil;
    f_CRYPTO_THREADID_set_pointer :            procedure(id : PCRYPTO_THREADID; ptr: Pointer); cdecl = nil;

    f_CRYPTO_set_locking_callback :            procedure(CB : TStatLockLockCallback); cdecl = nil;
    f_CRYPTO_set_dynlock_create_callback :     procedure(CB : TDynLockCreateCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_lock_callback :       procedure(CB : TDynLockLockCallBack); cdecl = nil;
    f_CRYPTO_set_dynlock_destroy_callback :    procedure(CB : TDynLockDestroyCallBack); cdecl = nil;
{$IFDEF OPENSSL_USE_DELPHI_MM}
    f_CRYPTO_set_mem_functions :               function(M: TCryptoMallocFunc; R: TCryptoReallocFunc; F: TCryptoFreeMemFunc): Integer; cdecl = nil; //AG
{$ENDIF}
    f_CRYPTO_cleanup_all_ex_data :             procedure; cdecl = nil;

    f_X509_dup :                               function(X: PX509): PX509; cdecl = nil;//AG;
    f_X509_check_ca :                          function(X: PX509): Integer; cdecl = nil;//AG;
    f_X509_STORE_new :                         function: PX509_STORE; cdecl = nil;//AG;
    f_X509_STORE_free :                        procedure(Store: PX509_STORE); cdecl = nil;//AG;
    f_X509_STORE_add_cert :                    function(Store: PX509_STORE; Cert: PX509): Integer; cdecl = nil;//AG;
    f_X509_STORE_add_crl :                     function(Store: PX509_STORE; CRL: PX509_CRL): Integer; cdecl = nil;//AG;
    f_X509_STORE_add_lookup :                  function(Store: PX509_STORE; Meth: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl = nil;//AG;
    f_X509_STORE_set_flags :                   procedure(Store: PX509_STORE; Flags: Longword); cdecl = nil;//AG;

    f_X509_STORE_CTX_new :                     function: PX509_STORE_CTX; cdecl = nil;//AG;
    f_X509_STORE_CTX_free :                    procedure(Ctx: PX509_STORE_CTX); cdecl = nil;//AG;
    f_X509_STORE_CTX_init :                    function(Ctx: PX509_STORE_CTX; Store: PX509_STORE; Cert: PX509; UnTrustedChain: PSTACK_OF_X509): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_cleanup :                 procedure(Ctx: PX509_STORE_CTX); cdecl = nil;//AG;
    f_X509_STORE_CTX_get_ex_data :             function(Ctx: PX509_STORE_CTX; Idx: Integer): Pointer; cdecl = nil;
    f_X509_STORE_CTX_get_current_cert :        function(Ctx: PX509_STORE_CTX): PX509; cdecl = nil;
    f_X509_STORE_CTX_get_error :               function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_set_error :               procedure(Ctx: PX509_STORE_CTX; s: Integer); cdecl = nil;
    f_X509_STORE_CTX_get_error_depth :         function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;
    f_X509_STORE_CTX_get_chain :               function(Ctx: PX509_STORE_CTX): PSTACK_OF_X509; cdecl = nil;//AG;
    f_X509_STORE_CTX_trusted_stack :           procedure(Ctx: PX509_STORE_CTX; STACK_OF_X509: PSTACK_OF_X509); cdecl = nil;//AG;
    f_X509_STORE_CTX_set_purpose :             function(Ctx: PX509_STORE_CTX; Purpose: Integer): Integer; cdecl = nil;//AG;
    f_X509_STORE_CTX_set_verify_cb :           procedure(Ctx: PX509_STORE_CTX; Cb: TSetVerify_cb); cdecl = nil;//AG;
    f_X509_STORE_CTX_set_ex_data :             function(Ctx: PX509_STORE_CTX; Idx: Integer; Data: Pointer): Integer; cdecl = nil;//AG;

    f_X509_load_crl_file :                     function(Ctx: PX509_LOOKUP; const Filename: PAnsiChar; type_: Integer): Integer; cdecl = nil;//AG;

    f_X509_LOOKUP_file :                       function: PX509_LOOKUP_METHOD; cdecl = nil;//AG;
    f_X509_LOOKUP_hash_dir :                   function: PX509_LOOKUP_METHOD; cdecl = nil;//AG;
    f_X509_LOOKUP_new :                        function(Method: PX509_LOOKUP_METHOD): PX509_LOOKUP; cdecl = nil;//AG;
    f_X509_LOOKUP_free :                       procedure(Ctx: PX509_LOOKUP); cdecl = nil;//AG;
    f_X509_LOOKUP_by_issuer_serial :           function(Ctx: PX509_LOOKUP; Typ_: Integer; Name: PX509_NAME; Serial: PASN1_INTEGER; Ret: PX509_OBJECT): Integer; cdecl = nil;//AG;
    f_X509_LOOKUP_by_fingerprint :             function(Ctx: PX509_LOOKUP; Typ_: Integer; Bytes: PAnsiChar; Len: Integer; Ret: PX509_OBJECT ): Integer; cdecl = nil;//AG;
    f_X509_LOOKUP_ctrl :                       function(Ctx: PX509_LOOKUP; Cmd: Integer; Argc: PAnsiChar; Argl: Cardinal; Ret: PPAnsiChar): Integer; cdecl = nil;//AG;

    f_X509_check_issued :                      function(Issuer: PX509; Subject: PX509): Integer; cdecl = nil;//AG;
    f_X509_verify_cert :                       function(Ctx: PX509_STORE_CTX): Integer; cdecl = nil;//AG;
    f_X509_verify_cert_error_string :          function(ErrCode : Integer): PAnsiChar; cdecl = nil;

    f_X509_get_issuer_name :                   function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_get_subject_name :                  function(Cert: PX509): PX509_NAME; cdecl = nil;
    f_X509_get_serialNumber :                  function(Cert: PX509): PASN1_INTEGER; cdecl = nil;
    f_X509_NAME_oneline :                      function(CertName: PX509_NAME; Buf: PAnsiChar; BufSize: Integer): PAnsiChar; cdecl = nil;
    f_X509_NAME_get_text_by_NID :              function(CertName: PX509_NAME; Nid: Integer; Buf : PAnsiChar; Len : Integer): Integer; cdecl = nil;
    f_X509_NAME_get_index_by_NID:              function(CertName: PX509_NAME; Nid: Integer; LastPos: Integer): Integer; cdecl = nil; //AG

    f_X509_NAME_free :                         procedure(AName: PX509_NAME); cdecl = nil;//AG;
    f_X509_NAME_cmp :                          function(const a: PX509_NAME; const b: PX509_NAME): Integer; cdecl = nil;//AG;

    f_X509_get_ext :                           function(Cert: PX509; Loc : Integer): PX509_EXTENSION; cdecl = nil;
    f_X509_get_ext_count :                     function(Cert: PX509): Integer; cdecl = nil;
    f_X509_free :                              procedure(Cert: PX509); cdecl = nil;
    f_X509_CRL_free :                          procedure(CRL: PX509_CRL); cdecl = nil;//AG
    f_X509V3_EXT_get :                         function(Ext: PX509_EXTENSION): PX509V3_EXT_METHOD; cdecl = nil;
    f_X509V3_EXT_print :                       function(B: PBIO; Ext: PX509_EXTENSION; Flag: Integer; Indent: Integer):Integer; cdecl = nil;//AG;
    f_X509V3_EXT_d2i :                         function(Ext: PX509_EXTENSION): Pointer; cdecl = nil;//AG;
    f_X509V3_conf_free :                       procedure(Val: PCONF_VALUE); cdecl = nil;//AG
    f_X509_EXTENSION_get_object :              function(Ext: PX509_EXTENSION): PASN1_OBJECT; cdecl = nil;
    f_X509_EXTENSION_get_data :                function(Ext : PX509_EXTENSION): PASN1_OCTET_STRING; cdecl = nil;//AG;
    f_X509_EXTENSION_get_critical :            function(Ext: PX509_EXTENSION): Integer; cdecl = nil;//AG;
    f_X509_subject_name_hash :                 function(Cert: PX509): Cardinal; cdecl = nil;
    f_X509_print :                             function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_X509_digest :                            function(Cert: PX509; Type_: PEVP_MD; Buf: PAnsiChar; BufSize: PInteger): Integer; cdecl = nil; //AG
    f_X509_check_private_key :                 function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil; //AG

    f_EVP_sha1 :                               function: PEVP_MD; cdecl = nil;//AG
    f_EVP_sha256 :                             function: PEVP_MD; cdecl = nil;//AG
    f_EVP_md5 :                                function: PEVP_MD; cdecl = nil;//AG
    f_EVP_PKEY_free :                          procedure(PKey: PEVP_PKEY); cdecl = nil;//AG
    { Next is v1.0.0+ ** check for nil ** }
    f_EVP_PKEY_get0 :                          function(PKey: PEVP_PKEY): Pointer; cdecl = nil;//AG
    f_EVP_PKEY_new :                           function: PEVP_PKEY; cdecl = nil;//AG
    f_EVP_PKEY_assign :                        function(PKey: PEVP_PKEY; Type_: Integer; Key: PAnsiChar): Integer; cdecl = nil;//AG
    f_EVP_PKEY_size :                          function(Pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_EVP_PKEY_bits :                          function(Pkey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_EVP_get_cipherbyname :                   function(name: PAnsiChar): PEVP_CIPHER; cdecl = nil;//AG
    f_EVP_des_ede3_cbc :                       function: PEVP_CIPHER; cdecl = nil;//AG
    f_EVP_cleanup :                            procedure; cdecl = nil;

    f_RSA_generate_key :                       function(Num: Integer; E: Cardinal; CallBack: TRSA_genkey_cb; cb_arg: Pointer): PRSA; cdecl = nil;//AG
    f_RSA_print :                              function(B: PBIO; Rsa: PRSA; Offset: Integer): Integer; cdecl = nil;//AG;
    f_DSA_print :                              function(B: PBIO; Dsa: PDSA; Offset: Integer): Integer; cdecl = nil;//AG;
    f_EC_KEY_print :                           function(B: PBIO; const EC: PEC_KEY; Offset: Integer): Integer; cdecl = nil;//AG;
    f_OBJ_nid2sn :                             function(N: Integer): PAnsiChar; cdecl = nil;
    f_OBJ_nid2ln :                             function(N: Integer): PAnsiChar; cdecl = nil;
    f_OBJ_obj2nid :                            function(O: PASN1_OBJECT): Integer; cdecl = nil;
    f_sk_num :                                 function(Stack: PSTACK): Integer; cdecl = nil;
    f_sk_value :                               function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;
    f_sk_new_null:                             function: PSTACK; cdecl = nil;//AG;
    { This function free()'s a stack structure.  The elements in the stack will not be freed }
    f_sk_free :                                procedure(Stack: PSTACK); cdecl = nil;//AG;
    { This function calls 'func' for each element on the stack, passing the element as the argument.  sk_free() is then called to free the 'stack' structure.}
    f_sk_pop_free :                            procedure(Stack: PSTACK; PFreeProc: Pointer); cdecl = nil;//AG;
    { Append 'data' to the stack.  0 is returned if there is a failure (due to a malloc failure), else 1 }
    f_sk_push :                                function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    { Remove the item at location 'loc' from the stack and returns it. Returns NULL if the 'loc' is out of range }
    f_sk_delete :                              function(Stack: PSTACK; Item: Integer): PAnsiChar; cdecl = nil;//AG;
    { Return and delete the last element on the stack }
    f_sk_pop :                                 function(Stack: PSTACK): PAnsiChar; cdecl = nil;//AG;
    f_sk_find :                                function(Stack: PSTACK; Data: PAnsiChar): Integer; cdecl = nil;//AG;
    f_sk_insert :                              function(Stack: PSTACK; Data: PAnsiChar; Index: Integer): Integer; cdecl = nil;//AG;
    f_sk_dup :                                 function(Stack: PSTACK): PSTACK; cdecl = nil;//AG;
    f_sk_set :                                 function(Stack: PSTACK; Index: Integer; value: PAnsiChar): PAnsiChar; cdecl = nil;//AG;
    f_PEM_write_bio_X509 :                     function(B: PBIO; Cert: PX509): Integer; cdecl = nil;
    f_PEM_write_bio_X509_REQ :                 function(B: PBIO; Cert_Req: PX509_REQ) : Integer; cdecl = nil;
    f_PEM_write_bio_X509_CRL :                 function(B: PBIO; CRL: PX509_CRL) : Integer; cdecl = nil;
    f_PEM_read_bio_X509_CRL :                  function(B: PBIO; CRL: PPX509_CRL; CallBack: TPem_password_cb; UData: PAnsiChar): PX509_CRL; cdecl = nil;//AG
    f_PEM_read_bio_X509 :                      function(B: PBIO; C509: PPX509; CallBack: TPem_password_cb; UData: PAnsiChar): PX509; cdecl = nil;
    f_PEM_read_bio_PKCS7 :                     function(B: PBIO; X: PPPKCS7; CallBack: TPem_password_cb; UData: PAnsiChar): PPKCS7; cdecl = nil;//AG;
    f_PEM_read_bio_DHparams :                  function(B: PBIO; X: PDH; CallBack: TPem_password_cb; UData: PAnsiChar): PDH; cdecl = nil;  { V8.07 }

    f_PEM_write_bio_PKCS7 :                    function(B: PBIO; P7: PPKCS7): Integer; cdecl = nil;
    f_PEM_do_header :                          function(cipher: PEVP_CIPHER_INFO; data: PAnsiChar; var len: Integer; callback: TPem_password_cb; u: Pointer): Integer; cdecl = nil;//AG;

    f_PEM_X509_INFO_read_bio :                 function(B: PBIO; Stack: PSTACK_OF_X509_INFO; CallBack: TPem_password_cb; UData: PAnsiChar): PSTACK_OF_X509_INFO; cdecl = nil;//AG;

    f_PEM_read_bio_RSA_PUBKEY:                 function(B: PBIO; x: PPRSA; cb: TPem_password_cb; u: pointer): PRSA; cdecl = nil;        { V8.11 }
    f_PEM_read_bio_RSAPrivateKey:              function(B: PBIO; x: PPRSA; cb: TPem_password_cb; u: pointer): PRSA; cdecl = nil;
    f_d2i_RSAPrivateKey:                       function(a: PPRSA; var pp: PByte; length: Integer): PRSA; cdecl = nil;
    f_i2d_RSAPublicKey:                        function(a: PRSA; var pp: PByte): Integer; cdecl = nil;
    f_i2d_RSA_PUBKEY:                          function(a: PRSA; var pp: PByte): Integer; cdecl = nil;
    f_PEM_write_bio_RSAPrivateKey:             function(B: PBIO; X: PRSA; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil; { V8.12 }
    f_PEM_write_bio_RSAPublicKey:              function(B: PBIO; X: PRSA): Integer; cdecl = nil; { V8.12 }

    f_CRYPTO_free :                            procedure(P: Pointer); cdecl = nil;//AG
    f_X509_NAME_ENTRY_get_object :             function(Ne: PX509_NAME_ENTRY): PASN1_OBJECT; cdecl = nil;//AG
    f_X509_NAME_get_entry :                    function(Name: PX509_NAME; Loc: Integer): PX509_NAME_ENTRY; cdecl = nil;//AG
    f_X509_NAME_entry_count :                  function(Name: PX509_NAME) : Integer; cdecl = nil; //AG
    f_X509_NAME_ENTRY_get_data :               function(Ne: PX509_NAME_ENTRY) : PASN1_STRING; cdecl = nil;//AG
    f_X509_set_version :                       function(Cert: PX509; Version: LongInt): Integer; cdecl = nil;//AG
    f_ASN1_INTEGER_get :                       function(Asn1_Int : PASN1_INTEGER): Integer; cdecl = nil;
    f_ASN1_STRING_print :                      function(B: PBIO; v: PASN1_STRING): integer; cdecl = nil;//AG;
    f_ASN1_item_free :                         procedure(Val: PASN1_VALUE; const It: PASN1_ITEM); cdecl = nil; //AG
    f_ASN1_STRING_to_UTF8 :                    function(POut: PPAnsiChar; PIn: PASN1_STRING) : Integer; cdecl = nil;//AG
    f_ASN1_INTEGER_set :                       function(a: PASN1_INTEGER; v: LongInt) : Integer; cdecl = nil;//AG
    f_ASN1_item_d2i :                          function(Val: PPASN1_VALUE; _In: PPAnsiChar; Len: Longword; const It: PASN1_ITEM): PASN1_VALUE; cdecl = nil;//AG;
    f_ASN1_STRING_free :                       procedure(a: PASN1_STRING); cdecl = nil;//AG;
    //ASN1_VALUE * ASN1_item_d2i(ASN1_VALUE **val, unsigned char **in, long len, const ASN1_ITEM *it);
    f_i2a_ASN1_OBJECT :                        function(B: PBIO; A: PASN1_OBJECT): Integer; cdecl = nil;//AG;
    f_X509_gmtime_adj :                        function(S: PASN1_TIME; Adj: LongInt): PASN1_TIME; cdecl = nil;//AG
    f_X509_set_pubkey :                        function(Cert: PX509; PKey: PEVP_PKEY): Integer; cdecl = nil;//AG
    f_X509_new :                               function: PX509; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_txt :             function(Name: PX509_NAME; Field: PAnsiChar; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_add_entry_by_NID :             function(Name: PX509_NAME; Nid: Integer; Type_: Integer; Buf: PAnsiChar; BufferSize: Integer; Loc: Integer; Set_: Integer): Integer; cdecl = nil;//AG
    f_X509_NAME_new :                          function: PX509_NAME; cdecl = nil;//AG
    f_X509_set_issuer_name :                   function(Cert: PX509; Name: PX509_NAME): Integer; cdecl = nil;//AG
    f_X509_sign :                              function(Cert: PX509; PKey: PEVP_PKEY; const Md: PEVP_MD): Integer; cdecl = nil;//AG
    f_X509_INFO_free :                         procedure(Xi: PX509_INFO); cdecl = nil;//AG;
    f_X509_CRL_dup :                           function(CRL: PX509_CRL): PX509_CRL; cdecl = nil;//AG;
    f_X509_PKEY_free :                         procedure(PKey: PX509_PKEY); cdecl = nil;//AG;
    f_i2d_X509 :                               function(Cert: PX509; pOut: PPAnsiChar): Integer; cdecl = nil;//AG
    f_i2d_PrivateKey :                         function(A: PEVP_PKEY; PP: PPAnsiChar): Integer; cdecl = nil;//AG
    f_d2i_PrivateKey :                         function(type_: Integer; var a: PEVP_PKEY; var pp : PAnsiChar; length: Integer): PEVP_PKEY; cdecl = nil;//AG
    f_PEM_read_bio_PrivateKey :                function(B: PBIO; X:PPEVP_PKEY; CB: TPem_password_cb; UData: PAnsiChar): PEVP_PKEY; cdecl = nil; //AG
    f_PEM_write_bio_PrivateKey :               function(B: PBIO; X: PEVP_PKEY; const Enc: PEVP_CIPHER; Kstr: PAnsiChar; Klen: Integer; CallBack: TPem_password_cb; U: Pointer): Integer; cdecl = nil;//AG
    f_i2d_ASN1_bytes :                         function(A : PASN1_STRING; var p: PAnsiChar; tag: Integer; xclass: Integer): Integer; cdecl = nil;//AG
    f_X509_get_pubkey :                        function(Cert: PX509): PEVP_PKEY; cdecl = nil; //AG;
    f_X509_PUBKEY_free :                       procedure(Key: PEVP_PKEY); cdecl = nil; //AG;

    f_d2i_PKCS8PrivateKey_bio:                 function(bp: PBIO; x: PPEVP_PKEY; cb: Tpem_password_cb; u: pointer): PEVP_PKEY; cdecl = nil;     { V8.11 }

    f_X509_check_purpose :                     function(Cert: PX509; ID: Integer; CA: Integer): Integer; cdecl = nil;//AG;
    f_X509_PURPOSE_get_id :                    function(XP: PX509_PURPOSE): Integer; cdecl = nil;//AG;
    f_X509_PURPOSE_get0 :                      function(Idx: Integer): PX509_PURPOSE; cdecl = nil;//AG;
    f_X509_PURPOSE_get0_name :                 function(XP: PX509_PURPOSE): PAnsiChar; cdecl = nil;//AG;
    f_X509_PURPOSE_get0_sname :                function(XP: PX509_PURPOSE): PAnsiChar; cdecl = nil;//AG;
    f_X509_PURPOSE_get_count :                 function: Integer; cdecl = nil;//AG;
    f_CONF_modules_unload :                    procedure(all: Integer); cdecl = nil;//AG;
    {
    f_OPENSSL_add_all_algorithms_noconf :      procedure; cdecl = nil;
    f_OPENSSL_add_all_algorithms_conf :        procedure; cdecl = nil;
    }
    f_OpenSSL_add_all_ciphers :                procedure; cdecl = nil;
    f_OpenSSL_add_all_digests :                procedure; cdecl = nil;

    f_PKCS7_new :                              function: PPKCS7; cdecl = nil;//AG;
    f_PKCS7_free :                             procedure(P7: PPKCS7); cdecl = nil;//AG;
    f_PKCS7_set_type :                         function(P7: PPKCS7; type_: Integer): Integer; cdecl = nil;//AG;
    f_PKCS7_content_new :                      function(P7: PPKCS7; nid: Integer): Integer; cdecl = nil;//AG;
    f_PKCS7_add_certificate :                  function (p7: PPKCS7; x509: PX509): Integer; cdecl = nil;//AG;

    f_PKCS12_parse :                           function(P12: PPKCS12; Pass: PAnsiChar; var Pkey: PEVP_PKEY; var Cert: PX509; var Ca: PSTACK_OF_X509): Integer; cdecl = nil;//AG
    f_PKCS12_verify_mac :                      function(p12: PPKCS12; const pass: PAnsiChar; passlen: Integer): Integer; cdecl = nil;//AG;
    f_PKCS12_free :                            procedure(P12: PPKCS12); cdecl = nil;//AG;
    f_PKCS12_create :                          function(pass: PAnsiChar; name: PAnsiChar; pkey: PEVP_PKEY; cert: PX509; ca: PSTACK_OF_X509; nid_key, nid_cert, iter, mac_iter, keytype: Integer):PPKCS12; cdecl = nil;//AG;

    f_DH_free :                                procedure(dh: PDH) cdecl = nil;  { V8.07 }
    f_EC_KEY_new_by_curve_name :               function (nid: integer): PEC_KEY; cdecl = nil;   { V8.07 }
    f_EC_KEY_free :                            procedure (key: PEC_KEY); cdecl = nil;           { V8.07 }


{$IFNDEF OPENSSL_NO_ENGINE}
    f_ENGINE_load_builtin_engines :            procedure; cdecl = nil; //AG;
    f_ENGINE_register_all_complete :           procedure; cdecl = nil; //AG;
    f_ENGINE_cleanup :                         procedure; cdecl = nil; //AG;
    f_ENGINE_by_id :                           function(const id: PAnsiChar): PENGINE; cdecl = nil; //AG;
    f_ENGINE_init :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_finish :                          function(e: PENGINE): Integer; cdecl = nil; //AG;
    f_ENGINE_set_default :                     function(e: PENGINE; flags: Cardinal): Integer; cdecl = nil; //AG;
    f_ENGINE_ctrl_cmd_string :                 function(e: PENGINE; const cmd_name: PAnsiChar; const arg: PAnsiChar; cmd_optional: Integer): Integer; cdecl = nil; //AG;
    f_ENGINE_free :                            function(e: PENGINE): Integer; cdecl = nil; //AG;
    //* The following functions handle keys that are stored in some secondary
    //* location, handled by the engine.  The storage may be on a card or
    //* whatever. */
    f_ENGINE_load_private_key :                function(e: PENGINE; key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    f_ENGINE_load_public_key :                 function(e: PENGINE; const key_id: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl = nil; //AG;
    { Since V0.98i there's also:
    int ENGINE_load_ssl_client_cert(ENGINE *e, SSL *s,
    STACK_OF(X509_NAME) *ca_dn, X509 **pcert, EVP_PKEY **ppkey,
    STACK_OF(X509) **pother,
    UI_METHOD *ui_method, void *callback_data);
    }
    f_ENGINE_load_ssl_client_cert :            function(e: PENGINE; SSL: PSSL; ca_dn: PSTACK_OF_X509_NAME; pcert: PPX509; ppkey: PPEVP_PKEY;
                                               pother: PSTACK_OF_X509; ui_method: PUI_METHOD; callback_data: Pointer): Integer; cdecl = nil;
    // ui.h //
    f_UI_new :                                 function: PUI; cdecl = nil; //AG;
    f_UI_new_method :                          function(const method: PUI_METHOD): PUI; cdecl = nil; //AG;
    f_UI_free :                                procedure(ui: PUI); cdecl = nil; //AG;
    f_UI_create_method :                       function(name: PAnsiChar): PUI_METHOD; cdecl = nil; //AG;
    f_UI_destroy_method :                      procedure(ui_method: PUI_METHOD); cdecl = nil; //AG;
    f_UI_set_ex_data :                         function(r: PUI; idx: Integer; arg: Pointer): Integer; cdecl = nil; //AG;
    f_UI_get_ex_data :                         function(r: PUI; idx: Integer): Pointer; cdecl = nil; //AG;
    f_UI_method_set_reader :                   function(method: PUI_METHOD; reader: TPinCallBack):Integer; cdecl = nil; //AG;
    f_UI_set_result :                          function(ui: PUI; uis: PUI_STRING; const result: PAnsiChar): Integer; cdecl = nil; //AG;
    f_UI_OpenSSL :                             function: PUI_METHOD; cdecl = nil; //AG;
 (*
    http://openssl.org/docs/crypto/engine.html
    Here we'll assume we want to load and register all ENGINE implementations
    bundled with OpenSSL, such that for any cryptographic algorithm required by
    OpenSSL - if there is an ENGINE that implements it and can be initialise, it
    should be used. The following code illustrates how this can work;

    /* Load all bundled ENGINEs into memory and make them visible */
    ENGINE_load_builtin_engines();
    /* Register all of them for every algorithm they collectively implement */
    ENGINE_register_all_complete();

    That's all that's required. Eg. the next time OpenSSL tries to set up an
    RSA key, any bundled ENGINEs that implement RSA_METHOD will be passed to
    ENGINE_init() and if any of those succeed, that ENGINE will be set as the
    default for RSA use from then on.
    *)
{$ENDIF}

    { Function name constants }
    FN_SSLeay                                 = 'SSLeay';
    FN_SSLeay_version                         = 'SSLeay_version';
    FN_ERR_get_error_line_data                = 'ERR_get_error_line_data';
    FN_ERR_peek_error                         = 'ERR_peek_error';
    FN_ERR_peek_last_error                    = 'ERR_peek_last_error';
    FN_ERR_get_error                          = 'ERR_get_error';
    FN_ERR_error_string                       = 'ERR_error_string';
    FN_ERR_error_string_n                     = 'ERR_error_string_n';
    FN_ERR_lib_error_string                   = 'ERR_lib_error_string';
    FN_ERR_func_error_string                  = 'ERR_func_error_string';
    FN_ERR_reason_error_string                = 'ERR_reason_error_string';
    FN_ERR_load_crypto_strings                = 'ERR_load_crypto_strings';
    FN_ERR_clear_error                        = 'ERR_clear_error';
    FN_ERR_remove_state                       = 'ERR_remove_state';
    FN_ERR_remove_thread_state                = 'ERR_remove_thread_state';
    FN_ERR_free_strings                       = 'ERR_free_strings';

    FN_RAND_seed                              = 'RAND_seed';

    FN_BIO_new                                = 'BIO_new';
    FN_BIO_new_socket                         = 'BIO_new_socket';
    FN_BIO_new_fd                             = 'BIO_new_fd';
    FN_BIO_new_file                           = 'BIO_new_file';
    FN_BIO_new_mem_buf                        = 'BIO_new_mem_buf';
    FN_BIO_new_bio_pair                       = 'BIO_new_bio_pair';

    FN_BIO_ctrl                               = 'BIO_ctrl';
    FN_BIO_ctrl_pending                       = 'BIO_ctrl_pending';
    FN_BIO_ctrl_get_write_guarantee           = 'BIO_ctrl_get_write_guarantee';
    FN_BIO_ctrl_get_read_request              = 'BIO_ctrl_get_read_request';

    FN_BIO_read                               = 'BIO_read';
    FN_BIO_nread                              = 'BIO_nread';
    FN_BIO_nread0                             = 'BIO_nread0';
    FN_BIO_nwrite                             = 'BIO_nwrite';
    FN_BIO_nwrite0                            = 'BIO_nwrite0';
    FN_BIO_write                              = 'BIO_write';
    FN_BIO_free                               = 'BIO_free';
    FN_BIO_gets                               = 'BIO_gets';
    FN_BIO_puts                               = 'BIO_puts';
    FN_BIO_push                               = 'BIO_push';
    FN_BIO_s_mem                              = 'BIO_s_mem';
    FN_BIO_get_retry_BIO                      = 'BIO_get_retry_BIO';
    FN_BIO_get_retry_reason                   = 'BIO_get_retry_reason';

    FN_BN_new                                 = 'BN_new';
    FN_BN_free                                = 'BN_free';
    FN_BN_set_word                            = 'BN_set_word';

    FN_d2i_X509_bio                           = 'd2i_X509_bio';
    FN_i2d_X509_bio                           = 'i2d_X509_bio';
    FN_d2i_PrivateKey_bio                     = 'd2i_PrivateKey_bio';
    FN_i2d_PrivateKey_bio                     = 'i2d_PrivateKey_bio';
    FN_d2i_X509                               = 'd2i_X509';
    FN_d2i_PKCS12_bio                         = 'd2i_PKCS12_bio';
    FN_i2d_PKCS12_bio                         = 'i2d_PKCS12_bio';
    FN_d2i_PKCS7_bio                          = 'd2i_PKCS7_bio';

    FN_CRYPTO_lock                            = 'CRYPTO_lock';
    FN_CRYPTO_add_lock                        = 'CRYPTO_add_lock';
    FN_CRYPTO_num_locks                       = 'CRYPTO_num_locks';
    FN_CRYPTO_set_locking_callback            = 'CRYPTO_set_locking_callback';
    FN_CRYPTO_set_id_callback                 = 'CRYPTO_set_id_callback';

    FN_CRYPTO_THREADID_set_callback           = 'CRYPTO_THREADID_set_callback';
    FN_CRYPTO_THREADID_set_numeric            = 'CRYPTO_THREADID_set_numeric';
    FN_CRYPTO_THREADID_set_pointer            = 'CRYPTO_THREADID_set_pointer';

    FN_CRYPTO_set_dynlock_create_callback     = 'CRYPTO_set_dynlock_create_callback';
    FN_CRYPTO_set_dynlock_lock_callback       = 'CRYPTO_set_dynlock_lock_callback';
    FN_CRYPTO_set_dynlock_destroy_callback    = 'CRYPTO_set_dynlock_destroy_callback';
{$IFDEF OPENSSL_USE_DELPHI_MM}
    FN_CRYPTO_set_mem_functions               = 'CRYPTO_set_mem_functions';
{$ENDIF}
    FN_CRYPTO_cleanup_all_ex_data             = 'CRYPTO_cleanup_all_ex_data';

    FN_X509_dup                               = 'X509_dup'; //AG
    FN_X509_check_ca                          = 'X509_check_ca'; //AG
    FN_X509_STORE_new                         = 'X509_STORE_new'; //AG
    FN_X509_STORE_free                        = 'X509_STORE_free'; //AG
    FN_X509_STORE_add_cert                    = 'X509_STORE_add_cert'; //AG
    FN_X509_STORE_add_crl                     = 'X509_STORE_add_crl'; //AG
    FN_X509_STORE_add_lookup                  = 'X509_STORE_add_lookup'; //AG
    FN_X509_STORE_set_flags                   = 'X509_STORE_set_flags'; //AG

    FN_X509_STORE_CTX_new                     = 'X509_STORE_CTX_new'; //AG
    FN_X509_STORE_CTX_free                    = 'X509_STORE_CTX_free'; //AG
    FN_X509_STORE_CTX_init                    = 'X509_STORE_CTX_init'; //AG
    FN_X509_STORE_CTX_cleanup                 = 'X509_STORE_CTX_cleanup'; //AG
    FN_X509_STORE_CTX_get_ex_data             = 'X509_STORE_CTX_get_ex_data';
    FN_X509_STORE_CTX_get_current_cert        = 'X509_STORE_CTX_get_current_cert';
    FN_X509_STORE_CTX_get_error               = 'X509_STORE_CTX_get_error';
    FN_X509_STORE_CTX_set_error               = 'X509_STORE_CTX_set_error';
    FN_X509_STORE_CTX_get_error_depth         = 'X509_STORE_CTX_get_error_depth';
    FN_X509_STORE_CTX_get_chain               = 'X509_STORE_CTX_get_chain'; //AG
    FN_X509_STORE_CTX_trusted_stack           = 'X509_STORE_CTX_trusted_stack'; //AG
    FN_X509_STORE_CTX_set_purpose             = 'X509_STORE_CTX_set_purpose'; //AG
    FN_X509_STORE_CTX_set_verify_cb           = 'X509_STORE_CTX_set_verify_cb'; //AG
    FN_X509_STORE_CTX_set_ex_data             = 'X509_STORE_CTX_set_ex_data'; //AG

    FN_X509_load_crl_file                     = 'X509_load_crl_file'; //AG

    FN_X509_LOOKUP_file                       = 'X509_LOOKUP_file'; //AG
    FN_X509_LOOKUP_hash_dir                   = 'X509_LOOKUP_hash_dir'; //AG
    FN_X509_LOOKUP_new                        = 'X509_LOOKUP_new'; //AG
    FN_X509_LOOKUP_free                       = 'X509_LOOKUP_free'; //AG
    FN_X509_LOOKUP_by_issuer_serial           = 'X509_LOOKUP_by_issuer_serial'; //AG
    FN_X509_LOOKUP_by_fingerprint             = 'X509_LOOKUP_by_fingerprint'; //AG
    FN_X509_LOOKUP_ctrl                       = 'X509_LOOKUP_ctrl'; //AG

    FN_X509_check_issued                      = 'X509_check_issued'; //AG
    FN_X509_verify_cert                       = 'X509_verify_cert'; //AG

    FN_X509_verify_cert_error_string          = 'X509_verify_cert_error_string';

    FN_X509_get_issuer_name                   = 'X509_get_issuer_name';
    FN_X509_get_subject_name                  = 'X509_get_subject_name';
    FN_X509_get_serialNumber                  = 'X509_get_serialNumber';
    FN_X509_NAME_oneline                      = 'X509_NAME_oneline';
    FN_X509_NAME_get_text_by_NID              = 'X509_NAME_get_text_by_NID';
    FN_X509_NAME_get_index_by_NID             = 'X509_NAME_get_index_by_NID'; //AG

    FN_X509_NAME_free                         = 'X509_NAME_free';
    FN_X509_NAME_cmp                          = 'X509_NAME_cmp';
    FN_X509_get_ext                           = 'X509_get_ext';
    FN_X509_get_ext_count                     = 'X509_get_ext_count';
    FN_X509_free                              = 'X509_free';
    FN_X509_CRL_free                          = 'X509_CRL_free';
    FN_X509V3_EXT_get                         = 'X509V3_EXT_get';
    FN_X509V3_EXT_print                       = 'X509V3_EXT_print'; //AG
    FN_X509V3_EXT_d2i                         = 'X509V3_EXT_d2i'; //AG
    FN_X509V3_conf_free                       = 'X509V3_conf_free'; //AG
    FN_X509_EXTENSION_get_object              = 'X509_EXTENSION_get_object';
    FN_X509_EXTENSION_get_data                = 'X509_EXTENSION_get_data'; //AG
    FN_X509_EXTENSION_get_critical            = 'X509_EXTENSION_get_critical'; //AG
    FN_X509_subject_name_hash                 = 'X509_subject_name_hash';
    FN_X509_print                             = 'X509_print';
    FN_X509_digest                            = 'X509_digest'; //AG
    FN_X509_check_private_key                 = 'X509_check_private_key'; //AG

    FN_EVP_sha1                               = 'EVP_sha1'; //AG
    FN_EVP_sha256                             = 'EVP_sha256';//AG
    FN_EVP_md5                                = 'EVP_md5'; //AG
    FN_EVP_PKEY_new                           = 'EVP_PKEY_new'; //AG
    FN_EVP_PKEY_free                          = 'EVP_PKEY_free'; //AG
    FN_EVP_PKEY_get0                          = 'EVP_PKEY_get0'; // AG
    FN_EVP_PKEY_assign                        = 'EVP_PKEY_assign'; //AG
    FN_EVP_PKEY_size                          = 'EVP_PKEY_size'; //AG
    FN_EVP_PKEY_bits                          = 'EVP_PKEY_bits'; //AG
    FN_EVP_get_cipherbyname                   = 'EVP_get_cipherbyname'; //AG
    FN_EVP_des_ede3_cbc                       = 'EVP_des_ede3_cbc'; //AG
    FN_EVP_cleanup                            = 'EVP_cleanup';

    FN_RSA_generate_key                       = 'RSA_generate_key'; //AG
    FN_RSA_print                              = 'RSA_print'; //AG
    FN_DSA_print                              = 'DSA_print'; //AG
    FN_EC_KEY_print                           = 'EC_KEY_print'; //AG
    FN_OBJ_nid2sn                             = 'OBJ_nid2sn';
    FN_OBJ_nid2ln                             = 'OBJ_nid2ln';
    FN_OBJ_obj2nid                            = 'OBJ_obj2nid';

    FN_sk_num                                 = 'sk_num';
    FN_sk_value                               = 'sk_value';
    FN_sk_new_null                            = 'sk_new_null'; //AG
    FN_sk_free                                = 'sk_free'; //AG
    FN_sk_pop_free                            = 'sk_pop_free'; //AG
    FN_sk_push                                = 'sk_push'; //AG
    FN_sk_delete                              = 'sk_delete'; //AG
    FN_sk_pop                                 = 'sk_pop'; //AG
    FN_sk_find                                = 'sk_find'; //AG
    FN_sk_insert                              = 'sk_insert'; //AG
    FN_sk_dup                                 = 'sk_dup'; //AG
    FN_sk_set                                 = 'sk_set'; //AG

    FN_PEM_write_bio_X509                     = 'PEM_write_bio_X509';
    FN_PEM_write_bio_X509_REQ                 = 'PEM_write_bio_X509_REQ';
    FN_PEM_write_bio_X509_CRL                 = 'PEM_write_bio_X509_CRL';
    FN_PEM_read_bio_X509_CRL                  = 'PEM_read_bio_X509_CRL';//AG
    FN_PEM_read_bio_X509                      = 'PEM_read_bio_X509';
    FN_PEM_read_bio_PKCS7                     = 'PEM_read_bio_PKCS7';
    FN_PEM_read_bio_DHparams                  = 'PEM_read_bio_DHparams'; { V8.07 }
    FN_PEM_write_bio_PKCS7                    = 'PEM_write_bio_PKCS7';
    FN_PEM_do_header                          = 'PEM_do_header';
    FN_PEM_X509_INFO_read_bio                 = 'PEM_X509_INFO_read_bio'; //AG

    FN_PEM_read_bio_RSA_PUBKEY                = 'PEM_read_bio_RSA_PUBKEY';
    FN_PEM_read_bio_RSAPrivateKey             = 'PEM_read_bio_RSAPrivateKey';
    FN_d2i_RSAPrivateKey                      = 'd2i_RSAPrivateKey';
    FN_i2d_RSAPublicKey                       = 'i2d_RSAPublicKey';
    FN_i2d_RSA_PUBKEY                         = 'i2d_RSA_PUBKEY';
    FN_PEM_write_bio_RSAPrivateKey            = 'PEM_write_bio_RSAPrivateKey'; { V8.12 }
    FN_PEM_write_bio_RSAPublicKey             = 'PEM_write_bio_RSAPublicKey'; { V8.12 }

    FN_CRYPTO_free                            = 'CRYPTO_free'; //AG
    FN_X509_NAME_ENTRY_get_object             = 'X509_NAME_ENTRY_get_object'; //AG
    FN_X509_NAME_get_entry                    = 'X509_NAME_get_entry'; //AG
    FN_X509_NAME_entry_count                  = 'X509_NAME_entry_count'; //AG
    FN_X509_NAME_ENTRY_get_data               = 'X509_NAME_ENTRY_get_data'; //AG
    FN_X509_set_version                       = 'X509_set_version'; //AG

    FN_ASN1_STRING_to_UTF8                    = 'ASN1_STRING_to_UTF8'; //AG
    FN_ASN1_INTEGER_set                       = 'ASN1_INTEGER_set'; //AG
    FN_ASN1_INTEGER_get                       = 'ASN1_INTEGER_get';
    FN_ASN1_STRING_print                      = 'ASN1_STRING_print'; //AG
    FN_ASN1_item_d2i                          = 'ASN1_item_d2i'; //AG
    FN_ASN1_item_free                         = 'ASN1_item_free'; //AG
    FN_ASN1_STRING_free                       = 'ASN1_STRING_free'; //AG

    FN_i2a_ASN1_OBJECT                        = 'i2a_ASN1_OBJECT'; //AG
    FN_X509_gmtime_adj                        = 'X509_gmtime_adj'; //AG
    FN_X509_set_pubkey                        = 'X509_set_pubkey'; //AG
    FN_X509_new                               = 'X509_new'; //AG
    FN_X509_NAME_add_entry_by_txt             = 'X509_NAME_add_entry_by_txt'; //AG
    FN_X509_NAME_add_entry_by_NID             = 'X509_NAME_add_entry_by_NID'; //AG
    FN_X509_NAME_new                          = 'X509_NAME_new'; //AG
    FN_X509_set_issuer_name                   = 'X509_set_issuer_name'; //AG
    FN_X509_sign                              = 'X509_sign'; //AG
    FN_X509_INFO_free                         = 'X509_INFO_free'; //AG
    FN_X509_CRL_dup                           = 'X509_CRL_dup'; //AG
    FN_X509_PKEY_free                         = 'X509_PKEY_free'; //AG
    FN_i2d_X509                               = 'i2d_X509'; //AG
    FN_i2d_PrivateKey                         = 'i2d_PrivateKey'; //AG
    FN_d2i_PrivateKey                         = 'd2i_PrivateKey'; //AG
    FN_PEM_write_bio_PrivateKey               = 'PEM_write_bio_PrivateKey'; //AG
    FN_PEM_read_bio_PrivateKey                = 'PEM_read_bio_PrivateKey'; //AG
    FN_i2d_ASN1_bytes                         = 'i2d_ASN1_bytes'; //AG
    FN_X509_get_pubkey                        = 'X509_get_pubkey';//AG
    FN_X509_PUBKEY_free                       = 'X509_PUBKEY_free'; //AG

    FN_d2i_PKCS8PrivateKey_bio                = 'd2i_PKCS8PrivateKey_bio';

    FN_X509_check_purpose                     = 'X509_check_purpose'; //AG
    FN_X509_PURPOSE_get_id                    = 'X509_PURPOSE_get_id'; //AG
    FN_X509_PURPOSE_get0                      = 'X509_PURPOSE_get0'; //AG
    FN_X509_PURPOSE_get0_name                 = 'X509_PURPOSE_get0_name'; //AG
    FN_X509_PURPOSE_get0_sname                = 'X509_PURPOSE_get0_sname'; //AG
    FN_X509_PURPOSE_get_count                 = 'X509_PURPOSE_get_count'; //AG
    FN_CONF_modules_unload                    = 'CONF_modules_unload'; //AG
    {
    FN_OPENSSL_add_all_algorithms_noconf      = 'OPENSSL_add_all_algorithms_noconf';
    FN_OPENSSL_add_all_algorithms_conf        = 'OPENSSL_add_all_algorithms_conf';
    }
    FN_OpenSSL_add_all_ciphers                = 'OpenSSL_add_all_ciphers';
    FN_OpenSSL_add_all_digests                = 'OpenSSL_add_all_digests';

    FN_PKCS7_new                              = 'PKCS7_new';
    FN_PKCS7_free                             = 'PKCS7_free';
    FN_PKCS7_set_type                         = 'PKCS7_set_type';
    FN_PKCS7_content_new                      = 'PKCS7_content_new';
    FN_PKCS7_add_certificate                  = 'PKCS7_add_certificate';

    FN_PKCS12_parse                           = 'PKCS12_parse';
    FN_PKCS12_verify_mac                      = 'PKCS12_verify_mac';
    FN_PKCS12_free                            = 'PKCS12_free';
    FN_PKCS12_create                          = 'PKCS12_create';

    FN_DH_free                                = 'DH_free'; { V8.07 }
    FN_EC_KEY_new_by_curve_name               = 'EC_KEY_new_by_curve_name';   { V8.07 }
    FN_EC_KEY_free                            = 'EC_KEY_free';   { V8.07 }

{$IFNDEF OPENSSL_NO_ENGINE}
    FN_ENGINE_load_builtin_engines            = 'ENGINE_load_builtin_engines'; //AG
    FN_ENGINE_register_all_complete           = 'ENGINE_register_all_complete'; //AG
    FN_ENGINE_cleanup                         = 'ENGINE_cleanup'; //AG
    FN_ENGINE_by_id                           = 'ENGINE_by_id'; //AG
    FN_ENGINE_init                            = 'ENGINE_init'; //AG
    FN_ENGINE_finish                          = 'ENGINE_finish'; //AG
    FN_ENGINE_set_default                     = 'ENGINE_set_default'; //AG
    FN_ENGINE_ctrl_cmd_string                 = 'ENGINE_ctrl_cmd_string'; //AG
    FN_ENGINE_free                            = 'ENGINE_free'; //AG
    FN_ENGINE_load_private_key                = 'ENGINE_load_private_key'; //AG
    FN_ENGINE_load_public_key                 = 'ENGINE_load_public_key'; //AG
    FN_ENGINE_load_ssl_client_cert            = 'ENGINE_load_ssl_client_cert';//AG
    FN_UI_new                                 = 'UI_new'; //AG
    FN_UI_new_method                          = 'UI_new_method'; //AG
    FN_UI_free                                = 'UI_free'; //AG
    FN_UI_create_method                       = 'UI_create_method'; //AG
    FN_UI_destroy_method                      = 'UI_destroy_method'; //AG
    FN_UI_set_ex_data                         = 'UI_set_ex_data'; //AG
    FN_UI_get_ex_data                         = 'UI_get_ex_data'; //AG
    FN_UI_method_set_reader                   = 'UI_method_set_reader'; //AG
    FN_UI_set_result                          = 'UI_set_result'; //AG
    FN_UI_OpenSSL                             = 'UI_OpenSSL'; //AG
{$ENDIF}

function Load : Boolean;
function WhichFailedToLoad : String;
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_get_flags(b: PBIO): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_retry(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_read(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_write(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_should_io_special(b: PBIO): Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function BIO_retry_type(b: PBIO): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslVersion : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslCompilerFlags : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslBuiltOn : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslPlatForm : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function OpenSslDir : String; {$IFDEF USE_INLINE} inline; {$ENDIF}
function IcsX509VerifyErrorToStr(ErrCode: Integer): String;
function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;
function f_Ics_X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME; {$IFDEF USE_INLINE} inline; {$ENDIF}// Macro
function f_Ics_X509_get_version(X509: PX509): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME; out UT: TDateTime): Boolean;
function Asn1ToString(PAsn1 : PASN1_STRING): String;
{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_UI_get_app_data(r: PUI): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}
function f_Ics_X509_LOOKUP_load_file(Ctx: PX509_LOOKUP; FileName: PAnsiChar; Type_: Longword): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_X509_LOOKUP_add_dir(Ctx: PX509_LOOKUP; DirName: PAnsiChar; Type_: Longword): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_Ics_X509_get_signature_algorithm(X509: PX509): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}

procedure Ics_Ssl_EVP_PKEY_IncRefCnt(K: PEVP_PKEY; Increment: Integer = 1); {$IFDEF USE_INLINE} inline; {$ENDIF}
function  Ics_Ssl_EVP_PKEY_GetKey(K: PEVP_PKEY): Pointer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  Ics_Ssl_EVP_PKEY_GetType(K: PEVP_PKEY): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}

{ 0.9.8n }
function f_SSL_get_secure_renegotiation_support(S: PSSL): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}

function Ics_Ssl_ERR_GET_LIB(E: DWORD): Integer;
function Ics_Ssl_ERR_GET_FUNC(E: DWORD): Integer;
function Ics_Ssl_ERR_GET_REASON(E: DWORD): Integer;

const
    GLIBEAY_DLL_Handle   : THandle = 0;
    GLIBEAY_DLL_Name     : String  = {$IFDEF MACOS} '/usr/lib/libcrypto.dylib'; {$ELSE} 'LIBEAY32.DLL'; {$ENDIF}
    GLIBEAY_DLL_FileName : String  = '*NOT LOADED*';

    { Version stuff added 07/12/05                                            }
    ICS_OPENSSL_VERSION_NUMBER  : Longword  = 0;
    ICS_SSL_NO_RENEGOTIATION    : Boolean = FALSE;

    //OSSL_VER_0906G = $0090607f; no longer supported
 {   OSSL_VER_0907G = $0090707f;
    OSSL_VER_0907I = $0090709f;
    OSSL_VER_0908  = $00908000;
    OSSL_VER_0908A = $0090801f;
    OSSL_VER_0908E = $0090805f;
    OSSL_VER_0908F = $0090806f;
    OSSL_VER_0908H = $0090808f;
    OSSL_VER_0908I = $0090809f;
    OSSL_VER_0908K = $009080bf;
    OSSL_VER_0908L = $009080cf;
    OSSL_VER_0908N = $009080ef;
    OSSL_VER_0908R = $0090812f;
    OSSL_VER_1000  = $10000000; // Untested, did not build with MinGW
    OSSL_VER_1000D = $1000004f; // Might be still buggy, had to incl. one workaround so far, see TSslContext.InitContext
    OSSL_VER_1000J = $100000af; // just briefly tested}

{ V8.07 only supporting versions with TLS 1.1 and 1.2 }
    OSSL_VER_1001  = $1000100F; // untested
    OSSL_VER_1001G = $1000107F; // just briefly tested  { V8.02 }
    OSSL_VER_1001H = $1000108F; // just briefly tested  { V8.03 }
    OSSL_VER_1001I = $1000109F; // just briefly tested  { V8.04 }
    OSSL_VER_1001J = $100010AF; // untested  { V8.05 }
    OSSL_VER_1001K = $100010BF; // just briefly tested   { V8.06 }
    OSSL_VER_1001L = $100010CF; // untested              { V8.07 }
    OSSL_VER_1002  = $1000200F; // just briefly tested   { V8.07 }
    OSSL_VER_1002A = $1000201F; // just briefly tested   { V8.08 }
    OSSL_VER_1002ZZ= $10002FFF; // not yet released  { V8.09 }
    OSSL_VER_1003  = $1000300F; // not yet released  { V8.09 }
    { Basically versions listed above are tested if not otherwise commented.  }
    { Versions between are assumed to work, however they are untested.        }
    { OpenSSL libraries for ICS are available for download here:              }
    { http://wiki.overbyte.be/wiki/index.php/ICS_Download                     }

    MIN_OSSL_VER   = OSSL_VER_1001;  { V8.07 }
    MAX_OSSL_VER   = OSSL_VER_1002ZZ; { V8.09 }

{$ENDIF} // USE_SSL
implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsMalloc(Size: size_t): Pointer; cdecl;
begin
    GetMem(Result, Size);
    FillChar(Result^, Size, 0);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsRealloc(P: Pointer; Size: size_t): Pointer; cdecl;
begin
    Result := P;
    ReallocMem(Result, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsFreeMem(P: Pointer); cdecl;
begin
    FreeMem(P);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Load : Boolean;
var
    ErrCode : Integer;
begin
    ICS_OPENSSL_VERSION_NUMBER := 0;

    if GLIBEAY_DLL_Handle <> 0 then begin
        Result := TRUE;
        Exit;                                 // Already loaded
    end;
    GLIBEAY_DLL_Handle := LoadLibrary(PChar(GLIBEAY_DLL_Name));
    if GLIBEAY_DLL_Handle {$IFDEF POSIX} = 0 {$ELSE} < HINSTANCE_ERROR {$ENDIF} then begin
        ErrCode            := GLIBEAY_DLL_Handle;
        GLIBEAY_DLL_Handle := 0;
        raise EIcsLIBEAYException.Create('Unable to load ' +
                                         GLIBEAY_DLL_Name +
                                         '. Error #' + IntToStr(ErrCode) +
                                         #13#10 + SysErrorMessage(GetLastError));
    end;
    SetLength(GLIBEAY_DLL_FileName, 256);
    SetLength(GLIBEAY_DLL_FileName, GetModuleFileName(GLIBEAY_DLL_Handle,
                 PChar(GLIBEAY_DLL_FileName), Length(GLIBEAY_DLL_FileName)));

    //This function is available in all versions so we can safely call it
    f_SSLeay := GetProcAddress(GLIBEAY_DLL_Handle, FN_SSLeay);
    if @f_SSLeay = nil then begin
        Result := False;
        Exit;
    end;
    ICS_OPENSSL_VERSION_NUMBER := f_SSLeay;
 {   ICS_SSL_NO_RENEGOTIATION   :=
            (ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_0908L) and
            (ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_0908N);   }
{$IFNDEF OPENSSL_NO_ENGINE}
 {   if ICS_OPENSSL_VERSION_NUMBER < OSSL_VER_0908I then begin
        FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
        OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0;
        raise EIcsLibeayException.Create('Experimental engine code requires ' +
                                         'at least OpenSSL v0.9.8i');
    end;    }
{$ENDIF}
    { Version Check }
{$IFNDEF NO_OSSL_VERSION_CHECK}
    if (ICS_OPENSSL_VERSION_NUMBER < MIN_OSSL_VER) or
       (ICS_OPENSSL_VERSION_NUMBER > MAX_OSSL_VER) then begin
        FreeLibrary(OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle);
        OverbyteIcsLIBEAY.GLIBEAY_DLL_Handle := 0;
        raise EIcsLibeayException.Create(
                  'Unsupported OpenSSL version (0x' +
                  IntToHex(ICS_OPENSSL_VERSION_NUMBER, 8) + ') !'#13#10 +
                  'Supported versions are 0x' +
                  IntToHex(MIN_OSSL_VER, 8) +
                  ' - 0x' + IntToHex(MAX_OSSL_VER, 8) + #13#10 +
                  'FileName: ' + GLIBEAY_DLL_FileName);
    end;
{$ENDIF}
    { Let's set some values of constants having changed in v0.9.8 }
//       SSL_CTRL_EXTRA_CHAIN_CERT    := 14;                     // Ssl.h   V8.07 now set as literal
//       SSL_CTRL_GET_SESSION_REUSED  :=  8;                     // Ssl.h   V8.07 now set as literal
        MBSTRING_UNIV                := MBSTRING_FLAG or 4;     // Asn1.h
        MBSTRING_UTF8                := MBSTRING_FLAG;          // Asn1.h

    f_SSLeay_version                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_SSLeay_version);
    f_ERR_get_error_line_data                := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_get_error_line_data);
    f_ERR_peek_error                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_peek_error);
    f_ERR_peek_last_error                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_peek_last_error);
    f_ERR_get_error                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_get_error);
    f_ERR_error_string                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_error_string);
    f_ERR_error_string_n                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_error_string_n);
    f_ERR_lib_error_string                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_lib_error_string);
    f_ERR_func_error_string                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_func_error_string);
    f_ERR_reason_error_string                := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_reason_error_string);
    f_ERR_load_crypto_strings                := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_load_crypto_strings);
    f_ERR_clear_error                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_clear_error);
    f_ERR_remove_state                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_remove_state);
    f_ERR_remove_thread_state                := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_remove_thread_state);
    f_ERR_free_strings                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ERR_free_strings);
    f_RAND_seed                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_RAND_seed);

    f_BIO_new                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new);
    f_BIO_new_socket                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new_socket);
    f_BIO_new_fd                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new_fd);
    f_BIO_new_file                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new_file);
    f_BIO_new_mem_buf                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new_mem_buf);
    f_BIO_new_bio_pair                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_new_bio_pair);

    f_BIO_ctrl                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_ctrl);
    f_BIO_ctrl_pending                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_ctrl_pending);
    f_BIO_ctrl_get_write_guarantee           := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_ctrl_get_write_guarantee);
    f_BIO_ctrl_get_read_request              := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_ctrl_get_read_request);

    f_BIO_read                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_read);
    f_BIO_nread                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_nread);
    f_BIO_nread0                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_nread0);
    f_BIO_nwrite                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_nwrite);
    f_BIO_nwrite0                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_nwrite0);
    f_BIO_write                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_write);
    f_BIO_free                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_free);
    f_BIO_gets                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_gets);
    f_BIO_puts                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_puts);
    f_BIO_push                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_push);
    f_BIO_s_mem                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_s_mem);
    f_BIO_get_retry_BIO                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_get_retry_BIO);
    f_BIO_get_retry_reason                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_BIO_get_retry_reason);

    f_BN_new                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_BN_new);
    f_BN_free                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_BN_free);
    f_BN_set_word                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_BN_set_word);

    f_d2i_X509_bio                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_X509_bio);
    f_i2d_X509_bio                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_X509_bio);
    f_d2i_PrivateKey_bio                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_PrivateKey_bio);
    f_i2d_PrivateKey_bio                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_PrivateKey_bio);
    f_d2i_X509                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_X509);
    f_d2i_PKCS12_bio                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_PKCS12_bio);
    f_i2d_PKCS12_bio                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_PKCS12_bio);
    f_d2i_PKCS7_bio                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_PKCS7_bio);

    f_CRYPTO_lock                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_lock);
    f_CRYPTO_add_lock                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_add_lock);
    f_CRYPTO_num_locks                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_num_locks);
    f_CRYPTO_set_locking_callback            := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_locking_callback);
    f_CRYPTO_set_id_callback                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_id_callback);

    f_CRYPTO_THREADID_set_callback           := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_THREADID_set_callback);
    f_CRYPTO_THREADID_set_numeric            := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_THREADID_set_numeric);
    f_CRYPTO_THREADID_set_pointer            := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_THREADID_set_pointer);

    f_CRYPTO_set_dynlock_create_callback     := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_dynlock_create_callback);
    f_CRYPTO_set_dynlock_lock_callback       := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_dynlock_lock_callback);
    f_CRYPTO_set_dynlock_destroy_callback    := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_dynlock_destroy_callback);
{$IFDEF OPENSSL_USE_DELPHI_MM}
    f_CRYPTO_set_mem_functions               := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_set_mem_functions);
{$ENDIF}
    f_CRYPTO_cleanup_all_ex_data             := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_cleanup_all_ex_data);

    f_X509_dup                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_dup); //AG
    f_X509_check_ca                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_check_ca); //AG
    f_X509_STORE_new                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_new); //AG
    f_X509_STORE_free                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_free); //AG
    f_X509_STORE_add_cert                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_add_cert); //AG
    f_X509_STORE_add_crl                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_add_crl); //AG
    f_X509_STORE_add_lookup                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_add_lookup); //AG
    f_X509_STORE_set_flags                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_set_flags); //AG

    f_X509_STORE_CTX_new                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_new); //AG
    f_X509_STORE_CTX_free                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_free); //AG
    f_X509_STORE_CTX_init                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_init); //AG
    f_X509_STORE_CTX_cleanup                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_cleanup); //AG
    f_X509_STORE_CTX_get_ex_data             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_get_ex_data);
    f_X509_STORE_CTX_get_current_cert        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_get_current_cert);
    f_X509_STORE_CTX_get_error               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_get_error);
    f_X509_STORE_CTX_set_error               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_set_error);
    f_X509_STORE_CTX_get_error_depth         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_get_error_depth);
    f_X509_STORE_CTX_get_chain               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_get_chain); //AG
    f_X509_STORE_CTX_trusted_stack           := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_trusted_stack); //AG
    f_X509_STORE_CTX_set_purpose             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_set_purpose); //AG
    f_X509_STORE_CTX_set_verify_cb           := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_set_verify_cb); //AG
    f_X509_STORE_CTX_set_ex_data             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_STORE_CTX_set_ex_data); //AG

    f_X509_load_crl_file                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_load_crl_file); //AG

    f_X509_LOOKUP_file                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_file); //AG
    f_X509_LOOKUP_hash_dir                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_hash_dir); //AG
    f_X509_LOOKUP_new                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_new); //AG
    f_X509_LOOKUP_free                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_free); //AG
    f_X509_LOOKUP_by_issuer_serial           := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_by_issuer_serial); //AG
    f_X509_LOOKUP_by_fingerprint             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_by_fingerprint); //AG
    f_X509_LOOKUP_ctrl                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_LOOKUP_ctrl); //AG

    f_X509_check_issued                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_check_issued); //AG
    f_X509_verify_cert                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_verify_cert); //AG
    f_X509_verify_cert_error_string          := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_verify_cert_error_string);

    f_X509_get_issuer_name                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_issuer_name);
    f_X509_get_subject_name                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_subject_name);
    f_X509_get_serialNumber                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_serialNumber);
    f_X509_NAME_oneline                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_oneline);
    f_X509_NAME_get_text_by_NID              := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_get_text_by_NID);
    f_X509_NAME_get_index_by_NID             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_get_index_by_NID); //AG
    f_X509_NAME_free                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_free);
    f_X509_NAME_cmp                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_cmp);
    f_X509_get_ext                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_ext);
    f_X509_get_ext_count                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_ext_count);
    f_X509_free                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_free);
    f_X509_CRL_free                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_CRL_free);
    f_X509V3_EXT_get                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509V3_EXT_get);
    f_X509V3_EXT_print                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509V3_EXT_print); //AG
    f_X509V3_EXT_d2i                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509V3_EXT_d2i); //AG
    f_X509V3_conf_free                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509V3_conf_free); //AG
    f_X509_EXTENSION_get_object              := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_EXTENSION_get_object);
    f_X509_EXTENSION_get_data                := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_EXTENSION_get_data); //AG
    f_X509_EXTENSION_get_critical            := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_EXTENSION_get_critical); //AG
    f_X509_subject_name_hash                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_subject_name_hash);
    f_X509_print                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_print);
    f_X509_digest                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_digest); //AG
    f_X509_check_private_key                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_check_private_key); //AG

    f_EVP_sha1                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_sha1); //AG
    f_EVP_sha256                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_sha256); //AG
    f_EVP_md5                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_md5); //AG
    f_EVP_PKEY_new                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_new); //AG
    f_EVP_PKEY_free                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_free); //AG
    { Next is v1.0.0+ ** check for nil ** }
    f_EVP_PKEY_get0                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_get0); //AG
    f_EVP_PKEY_assign                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_assign); //AG
    f_EVP_PKEY_size                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_size); //AG
    f_EVP_PKEY_bits                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_PKEY_bits); //AG
    f_EVP_get_cipherbyname                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_get_cipherbyname); //AG
    f_EVP_des_ede3_cbc                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_des_ede3_cbc); //AG
    f_EVP_cleanup                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_EVP_cleanup);

    f_RSA_generate_key                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_RSA_generate_key); //AG
    f_RSA_print                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_RSA_print); //AG
    f_DSA_print                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_DSA_print); //AG
    f_EC_KEY_print                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_EC_KEY_print); //AG
    f_OBJ_nid2sn                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_OBJ_nid2sn);
    f_OBJ_nid2ln                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_OBJ_nid2ln);
    f_OBJ_obj2nid                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_OBJ_obj2nid);

    f_sk_num                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_num);
    f_sk_value                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_value);
    f_sk_new_null                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_new_null); //AG
    f_sk_free                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_free); //AG
    f_sk_pop_free                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_pop_free); //AG
    f_sk_push                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_push); //AG
    f_sk_delete                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_delete); //AG
    f_sk_pop                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_pop); //AG
    f_sk_find                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_find); //AG
    f_sk_insert                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_insert); //AG
    f_sk_dup                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_dup); //AG
    f_sk_set                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_sk_set); //AG

    f_PEM_write_bio_X509                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_X509);
    f_PEM_write_bio_X509_REQ                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_X509_REQ);
    f_PEM_write_bio_X509_CRL                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_X509_CRL);
    f_PEM_read_bio_X509_CRL                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_X509_CRL);//AG
    f_PEM_read_bio_X509                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_X509);
    f_PEM_read_bio_PKCS7                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_PKCS7);
    f_PEM_read_bio_DHparams                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_DHparams); { V8.07 }
    f_PEM_write_bio_PKCS7                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_PKCS7);
    f_PEM_do_header                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_do_header);
    f_PEM_X509_INFO_read_bio                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_X509_INFO_read_bio); //AG
    f_PEM_write_bio_PrivateKey               := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_PrivateKey); //AG
    f_PEM_read_bio_PrivateKey                := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_PrivateKey); //AG

    f_PEM_read_bio_RSA_PUBKEY                := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_RSA_PUBKEY);
    f_PEM_read_bio_RSAPrivateKey             := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_read_bio_RSAPrivateKey);
    f_d2i_RSAPrivateKey                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_RSAPrivateKey);
    f_i2d_RSAPublicKey                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_RSAPublicKey);
    f_i2d_RSA_PUBKEY                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_RSA_PUBKEY);
    f_PEM_write_bio_RSAPrivateKey            := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_RSAPrivateKey); { V8.12 }
    f_PEM_write_bio_RSAPublicKey             := GetProcAddress(GLIBEAY_DLL_Handle, FN_PEM_write_bio_RSAPublicKey); { V8.12 }

    f_CRYPTO_free                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_CRYPTO_free); //AG
    f_X509_NAME_ENTRY_get_object             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_ENTRY_get_object); //AG
    f_X509_NAME_get_entry                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_get_entry); //AG
    f_X509_NAME_entry_count                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_entry_count); //AG
    f_X509_NAME_ENTRY_get_data               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_ENTRY_get_data); //AG
    f_X509_set_version                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_set_version); //AG
    f_ASN1_STRING_to_UTF8                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_STRING_to_UTF8); //AG
    f_ASN1_INTEGER_set                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_INTEGER_set); //AG
    f_ASN1_INTEGER_get                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_INTEGER_get);
    f_ASN1_STRING_print                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_STRING_print); //AG
    f_ASN1_item_d2i                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_item_d2i); //AG
    f_ASN1_item_free                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_item_free); //AG
    f_ASN1_STRING_free                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_ASN1_STRING_free); //AG
    f_i2a_ASN1_OBJECT                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2a_ASN1_OBJECT); //AG
    f_X509_gmtime_adj                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_gmtime_adj); //AG
    f_X509_set_pubkey                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_set_pubkey); //AG
    f_X509_new                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_new); //AG
    f_X509_NAME_add_entry_by_txt             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_add_entry_by_txt); //AG
    f_X509_NAME_add_entry_by_NID             := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_add_entry_by_NID); //AG
    f_X509_NAME_new                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_NAME_new); //AG
    f_X509_set_issuer_name                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_set_issuer_name); //AG
    f_X509_sign                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_sign); //AG
    f_X509_INFO_free                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_INFO_free); //AG
    f_X509_CRL_dup                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_CRL_dup); //AG
    f_X509_PKEY_free                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PKEY_free); //AG
    f_i2d_X509                               := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_X509); //AG
    f_i2d_PrivateKey                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_PrivateKey); //AG
    f_d2i_PrivateKey                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_PrivateKey); //AG

    f_i2d_ASN1_bytes                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_i2d_ASN1_bytes); //AG
    f_X509_get_pubkey                        := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_get_pubkey);//AG
    f_X509_PUBKEY_free                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PUBKEY_free); //AG

    f_d2i_PKCS8PrivateKey_bio                := GetProcAddress(GLIBEAY_DLL_Handle, FN_d2i_PKCS8PrivateKey_bio);

    f_X509_check_purpose                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_check_purpose); //AG
    f_X509_PURPOSE_get_id                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PURPOSE_get_id); //AG
    f_X509_PURPOSE_get0                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PURPOSE_get0); //AG
    f_X509_PURPOSE_get0_name                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PURPOSE_get0_name); //AG
    f_X509_PURPOSE_get0_sname                := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PURPOSE_get0_sname); //AG
    f_X509_PURPOSE_get_count                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_X509_PURPOSE_get_count); //AG
    f_CONF_modules_unload                    := GetProcAddress(GLIBEAY_DLL_Handle, FN_CONF_modules_unload); //AG
    {
    f_OPENSSL_add_all_algorithms_noconf      := GetProcAddress(GLIBEAY_DLL_Handle, FN_OPENSSL_add_all_algorithms_noconf);
    f_OPENSSL_add_all_algorithms_conf        := GetProcAddress(GLIBEAY_DLL_Handle, FN_OPENSSL_add_all_algorithms_conf);
    }
    f_OpenSSL_add_all_ciphers                := GetProcAddress(GLIBEAY_DLL_Handle, FN_OpenSSL_add_all_ciphers);
    f_OpenSSL_add_all_digests                := GetProcAddress(GLIBEAY_DLL_Handle, FN_OpenSSL_add_all_digests);

    f_PKCS7_new                              := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS7_new);
    f_PKCS7_free                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS7_free);
    f_PKCS7_set_type                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS7_set_type);
    f_PKCS7_content_new                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS7_content_new);
    f_PKCS7_add_certificate                  := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS7_add_certificate);

    f_PKCS12_parse                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS12_parse);
    f_PKCS12_verify_mac                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS12_verify_mac);
    f_PKCS12_free                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS12_free);
    f_PKCS12_create                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_PKCS12_create);

    f_DH_free                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_DH_free); { V8.07 }
    f_EC_KEY_new_by_curve_name               := GetProcAddress(GLIBEAY_DLL_Handle, FN_EC_KEY_new_by_curve_name);  { V8.07 }
    f_EC_KEY_free                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_EC_KEY_free);  { V8.07 }

{$IFNDEF OPENSSL_NO_ENGINE}
    f_ENGINE_load_builtin_engines            := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_load_builtin_engines); //AG
    f_ENGINE_register_all_complete           := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_register_all_complete); //AG
    f_ENGINE_cleanup                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_cleanup); //AG
    f_ENGINE_by_id                           := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_by_id); //AG
    f_ENGINE_init                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_init); //AG
    f_ENGINE_finish                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_finish); //AG
    f_ENGINE_set_default                     := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_set_default); //AG
    f_ENGINE_ctrl_cmd_string                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_ctrl_cmd_string); //AG
    f_ENGINE_free                            := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_free); //AG
    f_ENGINE_load_private_key                := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_load_private_key); //AG
    f_ENGINE_load_public_key                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_load_public_key); //AG
    f_ENGINE_load_ssl_client_cert            := GetProcAddress(GLIBEAY_DLL_Handle, FN_ENGINE_load_ssl_client_cert); //AG
    f_UI_new                                 := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_new); //AG
    f_UI_new_method                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_new_method); //AG
    f_UI_free                                := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_free); //AG
    f_UI_create_method                       := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_create_method); //AG
    f_UI_destroy_method                      := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_destroy_method); //AG
    f_UI_set_ex_data                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_set_ex_data); //AG
    f_UI_get_ex_data                         := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_get_ex_data); //AG
    f_UI_method_set_reader                   := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_method_set_reader); //AG
    f_UI_set_result                          := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_set_result); //AG
    f_UI_OpenSSL                             := GetProcAddress(GLIBEAY_DLL_Handle, FN_UI_OpenSSL); //AG
{$ENDIF}
    // Check if any failed
    Result := not ((@f_SSLeay                                 = nil) or
                   (@f_SSLeay_version                         = nil) or
                   (@f_ERR_get_error_line_data                = nil) or
                   (@f_ERR_peek_error                         = nil) or
                   (@f_ERR_peek_last_error                    = nil) or
                   (@f_ERR_get_error                          = nil) or
                   (@f_ERR_error_string                       = nil) or
                   (@f_ERR_error_string_n                     = nil) or
                   (@f_ERR_clear_error                        = nil) or
                   (@f_ERR_remove_state                       = nil) or
                   //(@f_ERR_remove_thread_state                = nil) or v1.0.0+ check for nil
                   (@f_ERR_free_strings                       = nil) or
                   (@f_RAND_seed                              = nil) or

                   (@f_BIO_new                                = nil) or
                   (@f_BIO_new_socket                         = nil) or
                   (@f_BIO_new_fd                             = nil) or
                   (@f_BIO_new_file                           = nil) or
                   (@f_BIO_new_mem_buf                        = nil) or
                   (@f_BIO_new_bio_pair                       = nil) or

                   (@f_BIO_ctrl                               = nil) or
                   (@f_BIO_ctrl_pending                       = nil) or
                   (@f_BIO_ctrl_get_read_request              = nil) or // B.S.
                   (@f_BIO_ctrl_get_write_guarantee           = nil) or

                   (@f_BIO_s_mem                              = nil) or
                   (@f_BIO_get_retry_BIO                      = nil) or
                   (@f_BIO_get_retry_reason                   = nil) or
                   (@f_BIO_free                               = nil) or
                   (@f_BIO_read                               = nil) or
                   (@f_BIO_nread                              = nil) or
                   (@f_BIO_nread0                             = nil) or
                   (@f_BIO_gets                               = nil) or
                   (@f_BIO_puts                               = nil) or
                   (@f_BIO_push                               = nil) or
                   (@f_BIO_write                              = nil) or
                   (@f_BIO_nwrite                             = nil) or
                   (@f_BIO_nwrite0                            = nil) or

                   (@f_d2i_X509_bio                           = nil) or
                   (@f_i2d_X509_bio                           = nil) or
                   (@f_d2i_PrivateKey_bio                     = nil) or
                   (@f_i2d_PrivateKey_bio                     = nil) or
                   (@f_d2i_X509                               = nil) or
                   (@f_d2i_PKCS12_bio                         = nil) or
                   (@f_i2d_PKCS12_bio                         = nil) or
                   (@f_d2i_PKCS7_bio                          = nil) or

                   (@f_CRYPTO_lock                            = nil) or
                   (@f_CRYPTO_add_lock                        = nil) or
                   (@f_CRYPTO_num_locks                       = nil) or
                   (@f_CRYPTO_set_locking_callback            = nil) or
                   (@f_CRYPTO_set_id_callback                 = nil) or
                   //(@f_CRYPTO_THREADID_set_callback           = nil) or  // check for nil at runtime
                   //(@f_CRYPTO_THREADID_set_numeric            = nil) or  // check for nil at runtime
                   //(@f_CRYPTO_THREADID_set_pointer            = nil) or  // check for nil at runtime
                   (@f_CRYPTO_set_dynlock_create_callback     = nil) or
                   (@f_CRYPTO_set_dynlock_lock_callback       = nil) or
                   (@f_CRYPTO_set_dynlock_destroy_callback    = nil) or
               {$IFDEF OPENSSL_USE_DELPHI_MM}
                   (@f_CRYPTO_set_mem_functions               = nil) or
               {$ENDIF}
                   (@f_CRYPTO_cleanup_all_ex_data             = nil) or

                   (@f_X509_dup                               = nil) or
                   (@f_X509_check_ca                          = nil) or
                   (@f_X509_STORE_new                         = nil) or
                   (@f_X509_STORE_free                        = nil) or
                   (@f_X509_STORE_add_cert                    = nil) or
                   (@f_X509_STORE_add_crl                     = nil) or
                   (@f_X509_STORE_add_lookup                  = nil) or
                   (@f_X509_STORE_set_flags                   = nil) or

                   (@f_X509_STORE_CTX_new                     = nil) or
                   (@f_X509_STORE_CTX_free                    = nil) or
                   (@f_X509_STORE_CTX_init                    = nil) or
                   (@f_X509_STORE_CTX_cleanup                 = nil) or
                   (@f_X509_STORE_CTX_get_ex_data             = nil) or
                   (@f_X509_STORE_CTX_get_current_cert        = nil) or
                   (@f_X509_STORE_CTX_get_error               = nil) or
                   (@f_X509_STORE_CTX_set_error               = nil) or
                   (@f_X509_STORE_CTX_get_error_depth         = nil) or
                   (@f_X509_STORE_CTX_get_chain               = nil) or
                   (@f_X509_STORE_CTX_trusted_stack           = nil) or
                   (@f_X509_STORE_CTX_set_purpose             = nil) or
                   (@f_X509_STORE_CTX_set_verify_cb           = nil) or
                   (@f_X509_STORE_CTX_set_ex_data             = nil) or

                   (@f_X509_load_crl_file                     = nil) or

                   (@f_X509_LOOKUP_file                       = nil) or
                   (@f_X509_LOOKUP_hash_dir                   = nil) or
                   (@f_X509_LOOKUP_new                        = nil) or
                   (@f_X509_LOOKUP_free                       = nil) or
                   (@f_X509_LOOKUP_by_issuer_serial           = nil) or
                   (@f_X509_LOOKUP_by_fingerprint             = nil) or //AG
                   (@f_X509_LOOKUP_ctrl                       = nil) or

                   (@f_X509_check_issued                      = nil) or
                   (@f_X509_verify_cert                       = nil) or
                   (@f_X509_verify_cert_error_string          = nil) or

                   (@f_X509_get_issuer_name                   = nil) or
                   (@f_X509_get_subject_name                  = nil) or
                   (@f_X509_get_serialNumber                  = nil) or
                   (@f_X509_NAME_oneline                      = nil) or
                   (@f_X509_NAME_get_text_by_NID              = nil) or
                   (@f_X509_NAME_get_index_by_NID             = nil) or //AG
                   (@f_X509_NAME_cmp                          = nil) or //AG
                   (@f_X509_NAME_free                         = nil) or
                   (@f_X509_get_ext                           = nil) or
                   (@f_X509_get_ext_count                     = nil) or
                   (@f_X509_free                              = nil) or
                   (@f_X509_CRL_free                          = nil) or
                   (@f_X509V3_EXT_get                         = nil) or
                   (@f_X509V3_EXT_print                       = nil) or
                   (@f_X509V3_EXT_d2i                         = nil) or
                   (@f_X509V3_conf_free                       = nil) or
                   (@f_X509_EXTENSION_get_object              = nil) or
                   (@f_X509_EXTENSION_get_data                = nil) or
                   (@f_X509_EXTENSION_get_critical            = nil) or
                   (@f_X509_subject_name_hash                 = nil) or
                   (@f_X509_print                             = nil) or
                   (@f_X509_digest                            = nil) or //AG
                   (@f_X509_check_private_key                 = nil) or //AG

                   (@f_EVP_sha1                               = nil) or //AG
                   (@f_EVP_sha256                             = nil) or //AG
                   (@f_EVP_md5                                = nil) or //AG
                   (@f_EVP_PKEY_free                          = nil) or //AG
                   { Next is v1.0.0+ ** check for nil ** }
                   //(@f_EVP_PKEY_get0                          = nil) or //AG
                   (@f_EVP_PKEY_new                           = nil) or //AG
                   (@f_EVP_PKEY_assign                        = nil) or //AG
                   (@f_EVP_PKEY_size                          = nil) or //AG
                   (@f_EVP_PKEY_bits                          = nil) or //AG
                   (@f_EVP_get_cipherbyname                   = nil) or //AG
                   (@f_EVP_des_ede3_cbc                       = nil) or //AG
                   (@f_EVP_cleanup                            = nil) or

                   (@f_RSA_generate_key                       = nil) or //AG
                   (@f_RSA_print                              = nil) or //AG
                   (@f_DSA_print                              = nil) or //AG
                   (@f_EC_KEY_print                           = nil) or //AG
                   (@f_OBJ_nid2sn                             = nil) or
                   (@f_OBJ_nid2ln                             = nil) or
                   (@f_OBJ_obj2nid                            = nil) or

                   (@f_sk_num                                 = nil) or
                   (@f_sk_value                               = nil) or
                   (@f_sk_new_null                            = nil) or
                   (@f_sk_free                                = nil) or
                   (@f_sk_pop_free                            = nil) or
                   (@f_sk_push                                = nil) or
                   (@f_sk_delete                              = nil) or
                   (@f_sk_pop                                 = nil) or
                   (@f_sk_find                                = nil) or
                   (@f_sk_insert                              = nil) or
                   (@f_sk_dup                                 = nil) or
                   (@f_sk_set                                 = nil) or

                   (@f_PEM_write_bio_X509                     = nil) or
                   (@f_PEM_write_bio_X509_REQ                 = nil) or
                   (@f_PEM_write_bio_X509_CRL                 = nil) or
                   (@f_PEM_read_bio_X509_CRL                  = nil) or
                   (@f_PEM_read_bio_X509                      = nil) or
                   (@f_PEM_read_bio_PKCS7                     = nil) or
                   (@f_PEM_read_bio_DHparams                  = nil) or
                   (@f_PEM_write_bio_PKCS7                    = nil) or
                   (@f_PEM_do_header                          = nil) or
                   (@f_PEM_X509_INFO_read_bio                 = nil) or
                   (@f_PEM_read_bio_PrivateKey                = nil) or
                   (@f_PEM_write_bio_PrivateKey               = nil) or

                   (@f_CRYPTO_free                            = nil) or
                   (@f_X509_NAME_get_entry                    = nil) or
                   (@f_X509_NAME_ENTRY_get_object             = nil) or
                   (@f_X509_NAME_entry_count                  = nil) or
                   (@f_X509_NAME_ENTRY_get_data               = nil) or
                   (@f_X509_set_version                       = nil) or
                   (@f_ASN1_STRING_to_UTF8                    = nil) or
                   (@f_ASN1_INTEGER_set                       = nil) or
                   (@f_ASN1_INTEGER_get                       = nil) or
                   (@f_ASN1_STRING_print                      = nil) or
                   (@f_ASN1_item_free                         = nil) or
                   (@f_ASN1_item_d2i                          = nil) or
                   (@f_ASN1_STRING_free                       = nil) or //AG
                   (@f_i2a_ASN1_OBJECT                        = nil) or
                   (@f_X509_gmtime_adj                        = nil) or
                   (@f_X509_set_pubkey                        = nil) or
                   (@f_X509_new                               = nil) or
                   (@f_X509_NAME_add_entry_by_txt             = nil) or
                   (@f_X509_NAME_add_entry_by_NID             = nil) or
                   (@f_X509_NAME_new                          = nil) or
                   (@f_X509_set_issuer_name                   = nil) or
                   (@f_X509_sign                              = nil) or
                   (@f_X509_INFO_free                         = nil) or
                   (@f_X509_CRL_dup                           = nil) or
                   (@f_i2d_X509                               = nil) or
                   (@f_i2d_PrivateKey                         = nil) or
                   (@f_d2i_PrivateKey                         = nil) or
                   (@f_i2d_ASN1_bytes                         = nil) or
                   (@f_X509_get_pubkey                        = nil) or

                   (@f_X509_PUBKEY_free                       = nil) or

                   (@f_X509_check_purpose                     = nil) or
                   (@f_X509_PURPOSE_get_id                    = nil) or
                   (@f_X509_PURPOSE_get0                      = nil) or
                   (@f_X509_PURPOSE_get0_name                 = nil) or
                   (@f_X509_PURPOSE_get0_sname                = nil) or
                   (@f_X509_PURPOSE_get_count                 = nil) or
                   (@f_CONF_modules_unload                    = nil) or

                   (@f_X509_PUBKEY_free                       = nil) or
                   (@f_CONF_modules_unload                    = nil) or
                   {
                   (@f_OPENSSL_add_all_algorithms_noconf      = nil) or
                   (@f_OPENSSL_add_all_algorithms_conf        = nil) or
                   }
                   (@f_OpenSSL_add_all_ciphers                = nil) or
                   (@f_OpenSSL_add_all_digests                = nil) or

                   (@f_PKCS7_new                              = nil) or
                   (@f_PKCS7_free                             = nil) or
                   (@f_PKCS7_set_type                         = nil) or
                   (@f_PKCS7_content_new                      = nil) or
                   (@f_PKCS7_add_certificate                  = nil) or
                   (@f_PKCS12_parse                           = nil) or
                   (@f_PKCS12_verify_mac                      = nil) or
                   (@f_PKCS12_free                            = nil) or
                   (@f_PKCS12_create                          = nil) or

                   (@f_DH_free                                = nil) or
                   (@f_EC_KEY_new_by_curve_name               = Nil) or
                   (@f_EC_KEY_free                            = Nil)

                {$IFNDEF OPENSSL_NO_ENGINE}
                                                                     or
                   (@f_ENGINE_load_builtin_engines            = nil) or
                   (@f_ENGINE_register_all_complete           = nil) or
                   (@f_ENGINE_cleanup                         = nil) or
                   (@f_ENGINE_by_id                           = nil) or
                   (@f_ENGINE_init                            = nil) or
                   (@f_ENGINE_finish                          = nil) or
                   (@f_ENGINE_set_default                     = nil) or
                   (@f_ENGINE_ctrl_cmd_string                 = nil) or
                   (@f_ENGINE_free                            = nil) or
                   (@f_ENGINE_load_private_key                = nil) or
                   (@f_ENGINE_load_public_key                 = nil) or
                   (@f_ENGINE_load_ssl_client_cert            = nil) or
                   (@f_UI_new                                 = nil) or
                   (@f_UI_new_method                          = nil) or
                   (@f_UI_free                                = nil) or
                   (@f_UI_create_method                       = nil) or
                   (@f_UI_destroy_method                      = nil) or
                   (@f_UI_set_ex_data                         = nil) or
                   (@f_UI_get_ex_data                         = nil) or
                   (@f_UI_method_set_reader                   = nil) or
                   (@f_UI_set_result                          = nil) or
                   (@f_UI_OpenSSL                             = nil)
                {$ENDIF}
                   );
{$IFDEF OPENSSL_USE_DELPHI_MM}
    if Result then
        Assert(f_CRYPTO_set_mem_functions(@IcsMalloc, @IcsRealloc, @IcsFreeMem) <> 0);
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WhichFailedToLoad : String;
const
    SP = #32;
begin
    Result := '';
    if @f_SSLeay                                 = nil then Result := Result + SP + FN_SSLeay;
    if @f_SSLeay_version                         = nil then Result := Result + SP + FN_SSLeay_version;
    if @f_ERR_get_error_line_data                = nil then Result := Result + SP + FN_ERR_get_error_line_data;
    if @f_ERR_peek_error                         = nil then Result := Result + SP + FN_ERR_peek_error;
    if @f_ERR_peek_last_error                    = nil then Result := Result + SP + FN_ERR_peek_last_error;
    if @f_ERR_get_error                          = nil then Result := Result + SP + FN_ERR_get_error;
    if @f_ERR_error_string                       = nil then Result := Result + SP + FN_ERR_error_string;
    if @f_ERR_error_string_n                     = nil then Result := Result + SP + FN_ERR_error_string_n;
    if @f_ERR_clear_error                        = nil then Result := Result + SP + FN_ERR_clear_error;
    if @f_ERR_remove_state                       = nil then Result := Result + SP + FN_ERR_remove_state;
    //if @f_ERR_remove_thread_state                = nil then Result := Result + SP + FN_ERR_remove_thread_state; v1.0.0+ check for nil
    if @f_ERR_free_strings                       = nil then Result := Result + SP + FN_ERR_free_strings;

    if @f_RAND_seed                              = nil then Result := Result + SP + FN_RAND_seed;

    if @f_BIO_new                                = nil then Result := Result + SP + FN_BIO_new;
    if @f_BIO_new_socket                         = nil then Result := Result + SP + FN_BIO_new_socket;
    if @f_BIO_new_fd                             = nil then Result := Result + SP + FN_BIO_new_fd;
    if @f_BIO_new_file                           = nil then Result := Result + SP + FN_BIO_new_file;
    if @f_BIO_new_mem_buf                        = nil then Result := Result + SP + FN_BIO_new_mem_buf;
    if @f_BIO_new_bio_pair                       = nil then Result := Result + SP + FN_BIO_new_bio_pair;

    if @f_BIO_ctrl                               = nil then Result := Result + SP + FN_BIO_ctrl;
    if @f_BIO_ctrl_get_read_request              = nil then Result := Result + SP + FN_BIO_ctrl_get_read_request; // B.S.
    if @f_BIO_ctrl_pending                       = nil then Result := Result + SP + FN_BIO_ctrl_pending;
    if @f_BIO_ctrl_get_write_guarantee           = nil then Result := Result + SP + FN_BIO_ctrl_get_write_guarantee;

    if @f_BIO_s_mem                              = nil then Result := Result + SP + FN_BIO_s_mem;
    if @f_BIO_get_retry_BIO                      = nil then Result := Result + SP + FN_BIO_get_retry_BIO;
    if @f_BIO_get_retry_reason                   = nil then Result := Result + SP + FN_BIO_get_retry_reason;
    if @f_BIO_free                               = nil then Result := Result + SP + FN_BIO_free;
    if @f_BIO_read                               = nil then Result := Result + SP + FN_BIO_read;
    if @f_BIO_nread                              = nil then Result := Result + SP + FN_BIO_nread;
    if @f_BIO_nread0                             = nil then Result := Result + SP + FN_BIO_nread0;
    if @f_BIO_gets                               = nil then Result := Result + SP + FN_BIO_gets;
    if @f_BIO_puts                               = nil then Result := Result + SP + FN_BIO_puts;
    if @f_BIO_push                               = nil then Result := Result + SP + FN_BIO_push;
    if @f_BIO_write                              = nil then Result := Result + SP + FN_BIO_write;
    if @f_BIO_nwrite                             = nil then Result := Result + SP + FN_BIO_nwrite;
    if @f_BIO_nwrite0                            = nil then Result := Result + SP + FN_BIO_nwrite0;

    if @f_d2i_X509_bio                           = nil then Result := Result + SP + FN_d2i_X509_bio;
    if @f_i2d_X509_bio                           = nil then Result := Result + SP + FN_i2d_X509_bio;
    if @f_d2i_PrivateKey_bio                     = nil then Result := Result + SP + FN_d2i_PrivateKey_bio;
    if @f_i2d_PrivateKey_bio                     = nil then Result := Result + SP + FN_i2d_PrivateKey_bio;
    if @f_d2i_X509                               = nil then Result := Result + SP + FN_d2i_X509;
    if @f_d2i_PKCS12_bio                         = nil then Result := Result + SP + FN_d2i_PKCS12_bio;
    if @f_i2d_PKCS12_bio                         = nil then Result := Result + SP + FN_i2d_PKCS12_bio;
    if @f_d2i_PKCS7_bio                          = nil then Result := Result + SP + FN_d2i_PKCS7_bio;

    if @f_CRYPTO_lock                            = nil then Result := Result + SP + FN_CRYPTO_lock;
    if @f_CRYPTO_add_lock                        = nil then Result := Result + SP + FN_CRYPTO_add_lock;
    if @f_CRYPTO_num_locks                       = nil then Result := Result + SP + FN_CRYPTO_num_locks;
    if @f_CRYPTO_set_locking_callback            = nil then Result := Result + SP + FN_CRYPTO_set_locking_callback;
    if @f_CRYPTO_set_id_callback                 = nil then Result := Result + SP + FN_CRYPTO_set_id_callback;
    //if @f_CRYPTO_THREADID_set_callback           = nil then Result := Result + SP + FN_CRYPTO_THREADID_set_callback; // check for nil at runtime
    //if @f_CRYPTO_THREADID_set_numeric            = nil then Result := Result + SP + FN_CRYPTO_THREADID_set_numeric;  // check for nil at runtime
    //if @f_CRYPTO_THREADID_set_pointer            = nil then Result := Result + SP + FN_CRYPTO_THREADID_set_pointer;  // check for nil at runtime
    if @f_CRYPTO_set_dynlock_create_callback     = nil then Result := Result + SP + FN_CRYPTO_set_dynlock_create_callback;
    if @f_CRYPTO_set_dynlock_lock_callback       = nil then Result := Result + SP + FN_CRYPTO_set_dynlock_lock_callback;
    if @f_CRYPTO_set_dynlock_destroy_callback    = nil then Result := Result + SP + FN_CRYPTO_set_dynlock_destroy_callback;
{$IFDEF OPENSSL_USE_DELPHI_MM}
    if @f_CRYPTO_set_mem_functions               = nil then Result := Result + SP + FN_CRYPTO_set_mem_functions;
{$ENDIF}
    if @f_CRYPTO_cleanup_all_ex_data             = nil then Result := Result + SP + FN_CRYPTO_cleanup_all_ex_data;

    if @f_X509_dup                               = nil then Result := Result + SP + FN_X509_dup;//AG
    if @f_X509_check_ca                          = nil then Result := Result + SP + FN_X509_check_ca;//AG
    if @f_X509_STORE_new                         = nil then Result := Result + SP + FN_X509_STORE_new;//AG
    if @f_X509_STORE_free                        = nil then Result := Result + SP + FN_X509_STORE_free;//AG
    if @f_X509_STORE_add_cert                    = nil then Result := Result + SP + FN_X509_STORE_add_cert;//AG
    if @f_X509_STORE_add_crl                     = nil then Result := Result + SP + FN_X509_STORE_add_crl;//AG
    if @f_X509_STORE_add_lookup                  = nil then Result := Result + SP + FN_X509_STORE_add_lookup;//AG
    if @f_X509_STORE_set_flags                   = nil then Result := Result + SP + FN_X509_STORE_set_flags;//AG

    if @f_X509_STORE_CTX_new                     = nil then Result := Result + SP + FN_X509_STORE_CTX_new;//AG
    if @f_X509_STORE_CTX_free                    = nil then Result := Result + SP + FN_X509_STORE_CTX_free;//AG
    if @f_X509_STORE_CTX_init                    = nil then Result := Result + SP + FN_X509_STORE_CTX_init;//AG
    if @f_X509_STORE_CTX_cleanup                 = nil then Result := Result + SP + FN_X509_STORE_CTX_cleanup;//AG
    if @f_X509_STORE_CTX_get_ex_data             = nil then Result := Result + SP + FN_X509_STORE_CTX_get_ex_data;
    if @f_X509_STORE_CTX_get_current_cert        = nil then Result := Result + SP + FN_X509_STORE_CTX_get_current_cert;
    if @f_X509_STORE_CTX_get_error               = nil then Result := Result + SP + FN_X509_STORE_CTX_get_error;
    if @f_X509_STORE_CTX_set_error               = nil then Result := Result + SP + FN_X509_STORE_CTX_set_error;
    if @f_X509_STORE_CTX_get_error_depth         = nil then Result := Result + SP + FN_X509_STORE_CTX_get_error_depth;
    if @f_X509_STORE_CTX_get_chain               = nil then Result := Result + SP + FN_X509_STORE_CTX_get_chain;//AG
    if @f_X509_STORE_CTX_trusted_stack           = nil then Result := Result + SP + FN_X509_STORE_CTX_trusted_stack;//AG
    if @f_X509_STORE_CTX_set_purpose             = nil then Result := Result + SP + FN_X509_STORE_CTX_set_purpose;//AG
    if @f_X509_STORE_CTX_set_verify_cb           = nil then Result := Result + SP + FN_X509_STORE_CTX_set_verify_cb;//AG
    if @f_X509_STORE_CTX_set_ex_data             = nil then Result := Result + SP + FN_X509_STORE_CTX_set_ex_data;//AG

    if @f_X509_load_crl_file                     = nil then Result := Result + SP + FN_X509_load_crl_file;//AG

    if @f_X509_LOOKUP_file                       = nil then Result := Result + SP + FN_X509_LOOKUP_file;//AG
    if @f_X509_LOOKUP_hash_dir                   = nil then Result := Result + SP + FN_X509_LOOKUP_hash_dir;//AG
    if @f_X509_LOOKUP_new                        = nil then Result := Result + SP + FN_X509_LOOKUP_new;//AG
    if @f_X509_LOOKUP_free                       = nil then Result := Result + SP + FN_X509_LOOKUP_free;//AG
    if @f_X509_LOOKUP_by_issuer_serial           = nil then Result := Result + SP + FN_X509_LOOKUP_by_issuer_serial;//AG
    if @f_X509_LOOKUP_by_fingerprint             = nil then Result := Result + SP + FN_X509_LOOKUP_by_fingerprint;//AG
    if @f_X509_LOOKUP_ctrl                       = nil then Result := Result + SP + FN_X509_LOOKUP_ctrl;//AG

    if @f_X509_check_issued                      = nil then Result := Result + SP + FN_X509_check_issued;//AG
    if @f_X509_verify_cert                       = nil then Result := Result + SP + FN_X509_verify_cert;//AG
    if @f_X509_verify_cert_error_string          = nil then Result := Result + SP + FN_X509_verify_cert_error_string;

    if @f_X509_get_issuer_name                   = nil then Result := Result + SP + FN_X509_get_issuer_name;
    if @f_X509_get_subject_name                  = nil then Result := Result + SP + FN_X509_get_subject_name;
    if @f_X509_get_serialNumber                  = nil then Result := Result + SP + FN_X509_get_serialNumber;
    if @f_X509_NAME_oneline                      = nil then Result := Result + SP + FN_X509_NAME_oneline;
    if @f_X509_NAME_get_text_by_NID              = nil then Result := Result + SP + FN_X509_NAME_get_text_by_NID;
    if @f_X509_NAME_get_index_by_NID             = nil then Result := Result + SP + FN_X509_NAME_get_index_by_NID;//AG
    if @f_X509_NAME_free                         = nil then Result := Result + SP + FN_X509_NAME_free;
    if @f_X509_NAME_cmp                          = nil then Result := Result + SP + FN_X509_NAME_cmp;
    if @f_X509_get_ext                           = nil then Result := Result + SP + FN_X509_get_ext;
    if @f_X509_get_ext_count                     = nil then Result := Result + SP + FN_X509_get_ext_count;
    if @f_X509_CRL_free                          = nil then Result := Result + SP + FN_X509_CRL_free;//AG
    if @f_X509_free                              = nil then Result := Result + SP + FN_X509_free;//AG
    if @f_X509V3_EXT_get                         = nil then Result := Result + SP + FN_X509V3_EXT_get;
    if @f_X509V3_EXT_print                       = nil then Result := Result + SP + FN_X509V3_EXT_print;//AG
    if @f_X509V3_EXT_d2i                         = nil then Result := Result + SP + FN_X509V3_EXT_d2i;//AG
    if @f_X509V3_conf_free                       = nil then Result := Result + SP + FN_X509V3_conf_free;//AG
    if @f_X509_EXTENSION_get_object              = nil then Result := Result + SP + FN_X509_EXTENSION_get_object;
    if @f_X509_EXTENSION_get_data                = nil then Result := Result + SP + FN_X509_EXTENSION_get_data;//AG
    if @f_X509_EXTENSION_get_critical            = nil then Result := Result + SP + FN_X509_EXTENSION_get_critical;//AG
    if @f_X509_subject_name_hash                 = nil then Result := Result + SP + FN_X509_subject_name_hash;
    if @f_X509_print                             = nil then Result := Result + SP + FN_X509_print;
    if @f_X509_digest                            = nil then Result := Result + SP + FN_X509_digest; //AG
    if @f_X509_check_private_key                 = nil then Result := Result + SP + FN_X509_check_private_key; //AG

    if @f_EVP_sha1                               = nil then Result := Result + SP + FN_EVP_sha1; //AG
    if @f_EVP_sha256                             = nil then Result := Result + SP + FN_EVP_sha256; //AG
    if @f_EVP_md5                                = nil then Result := Result + SP + FN_EVP_md5; //AG
    if @f_EVP_PKEY_free                          = nil then Result := Result + SP + FN_EVP_PKEY_free; //AG
    { Next is v1.0.0+ ** check for nil ** }
    //if @f_EVP_PKEY_get0                          = nil then Result := Result + SP + FN_EVP_PKEY_get0; //AG
    if @f_EVP_PKEY_new                           = nil then Result := Result + SP + FN_EVP_PKEY_new; //AG
    if @f_EVP_PKEY_assign                        = nil then Result := Result + SP + FN_EVP_PKEY_assign; //AG
    if @f_EVP_PKEY_size                          = nil then Result := Result + SP + FN_EVP_PKEY_size; //AG
    if @f_EVP_PKEY_bits                          = nil then Result := Result + SP + FN_EVP_PKEY_bits; //AG
    if @f_EVP_get_cipherbyname                   = nil then Result := Result + SP + FN_EVP_get_cipherbyname; //AG
    if @f_EVP_des_ede3_cbc                       = nil then Result := Result + SP + FN_EVP_des_ede3_cbc; //AG
    if @f_EVP_cleanup                            = nil then Result := Result + SP + FN_EVP_cleanup;

    if @f_RSA_generate_key                       = nil then Result := Result + SP + FN_RSA_generate_key; //AG
    if @f_RSA_print                              = nil then Result := Result + SP + FN_RSA_print; //AG
    if @f_DSA_print                              = nil then Result := Result + SP + FN_DSA_print; //AG
    if @f_EC_KEY_print                           = nil then Result := Result + SP + FN_EC_KEY_print; //AG
    if @f_OBJ_nid2sn                             = nil then Result := Result + SP + FN_OBJ_nid2sn;
    if @f_OBJ_nid2ln                             = nil then Result := Result + SP + FN_OBJ_nid2ln;
    if @f_OBJ_obj2nid                            = nil then Result := Result + SP + FN_OBJ_obj2nid;

    if @f_sk_num                                 = nil then Result := Result + SP + FN_sk_num;
    if @f_sk_value                               = nil then Result := Result + SP + FN_sk_value;
    if @f_sk_new_null                            = nil then Result := Result + SP + FN_sk_new_null;//AG
    if @f_sk_free                                = nil then Result := Result + SP + FN_sk_free;//AG
    if @f_sk_pop_free                            = nil then Result := Result + SP + FN_sk_pop_free;//AG
    if @f_sk_push                                = nil then Result := Result + SP + FN_sk_push;//AG
    if @f_sk_delete                              = nil then Result := Result + SP + FN_sk_delete;//AG
    if @f_sk_pop                                 = nil then Result := Result + SP + FN_sk_pop;//AG
    if @f_sk_find                                = nil then Result := Result + SP + FN_sk_find;//AG
    if @f_sk_insert                              = nil then Result := Result + SP + FN_sk_insert;//AG
    if @f_sk_dup                                 = nil then Result := Result + SP + FN_sk_dup;//AG
    if @f_sk_set                                 = nil then Result := Result + SP + FN_sk_set;//AG

    if @f_PEM_write_bio_X509_REQ                 = nil then Result := Result + SP + FN_PEM_write_bio_X509_REQ;
    if @f_PEM_write_bio_X509_CRL                 = nil then Result := Result + SP + FN_PEM_write_bio_X509_CRL;
    if @f_PEM_read_bio_X509_CRL                  = nil then Result := Result + SP + FN_PEM_read_bio_X509_CRL; //AG
    if @f_PEM_read_bio_X509                      = nil then Result := Result + SP + FN_PEM_read_bio_X509;
    if @f_PEM_read_bio_PKCS7                     = nil then Result := Result + SP + FN_PEM_read_bio_PKCS7;
    if @f_PEM_read_bio_DHparams                  = nil then Result := Result + SP + FN_PEM_read_bio_DHparams;
    if @f_PEM_write_bio_PKCS7                    = nil then Result := Result + SP + FN_PEM_write_bio_PKCS7;
    if @f_PEM_do_header                          = nil then Result := Result + SP + FN_PEM_do_header;
    if @f_PEM_X509_INFO_read_bio                 = nil then Result := Result + SP + FN_PEM_X509_INFO_read_bio; //AG
    if @f_PEM_read_bio_PrivateKey                = nil then Result := Result + SP + FN_PEM_read_bio_PrivateKey;//AG
    if @f_PEM_write_bio_PrivateKey               = nil then Result := Result + SP + FN_PEM_write_bio_PrivateKey;//AG

    if @f_CRYPTO_free                            = nil then Result := Result + SP + FN_CRYPTO_free;//AG
    if @f_X509_NAME_ENTRY_get_object             = nil then Result := Result + SP + FN_X509_NAME_ENTRY_get_object;//AG
    if @f_X509_NAME_get_entry                    = nil then Result := Result + SP + FN_X509_NAME_get_entry;//AG
    if @f_X509_NAME_entry_count                  = nil then Result := Result + SP + FN_X509_NAME_entry_count;//AG
    if @f_X509_NAME_ENTRY_get_data               = nil then Result := Result + SP + FN_X509_NAME_ENTRY_get_data;//AG
    if @f_X509_set_version                       = nil then Result := Result + SP + FN_X509_set_version;//AG
    if @f_ASN1_STRING_to_UTF8                    = nil then Result := Result + SP + FN_ASN1_STRING_to_UTF8;//AG
    if @f_ASN1_INTEGER_set                       = nil then Result := Result + SP + FN_ASN1_INTEGER_set;//AG
    if @f_ASN1_INTEGER_get                       = nil then Result := Result + SP + FN_ASN1_INTEGER_get;
    if @f_ASN1_STRING_print                      = nil then Result := Result + SP + FN_ASN1_STRING_print;//AG
    if @f_ASN1_item_d2i                          = nil then Result := Result + SP + FN_ASN1_item_d2i;//AG
    if @f_ASN1_item_free                         = nil then Result := Result + SP + FN_ASN1_item_free;//AG
    if @f_ASN1_STRING_free                       = nil then Result := Result + SP + FN_ASN1_STRING_free;//AG

    if @f_i2a_ASN1_OBJECT                        = nil then Result := Result + SP + FN_i2a_ASN1_OBJECT;//AG
    if @f_X509_gmtime_adj                        = nil then Result := Result + SP + FN_X509_gmtime_adj;//AG
    if @f_X509_set_pubkey                        = nil then Result := Result + SP + FN_X509_set_pubkey;//AG
    if @f_X509_new                               = nil then Result := Result + SP + FN_X509_new;//AG
    if @f_X509_NAME_add_entry_by_txt             = nil then Result := Result + SP + FN_X509_NAME_add_entry_by_txt;//AG
    if @f_X509_NAME_add_entry_by_NID             = nil then Result := Result + SP + FN_X509_NAME_add_entry_by_NID;//AG
    if @f_X509_NAME_new                          = nil then Result := Result + SP + FN_X509_NAME_new;//AG
    if @f_X509_set_issuer_name                   = nil then Result := Result + SP + FN_X509_set_issuer_name;//AG
    if @f_X509_sign                              = nil then Result := Result + SP + FN_X509_sign;//AG
    if @f_X509_INFO_free                         = nil then Result := Result + SP + FN_X509_INFO_free;//AG
    if @f_X509_CRL_dup                           = nil then Result := Result + SP + FN_X509_CRL_dup;//AG
    if @f_X509_PKEY_free                         = nil then Result := Result + SP + FN_X509_PKEY_free;//AG
    if @f_i2d_X509                               = nil then Result := Result + SP + FN_i2d_X509;//AG
    if @f_i2d_PrivateKey                         = nil then Result := Result + SP + FN_i2d_PrivateKey;//AG
    if @f_d2i_PrivateKey                         = nil then Result := Result + SP + FN_d2i_PrivateKey;//AG

    if @f_i2d_ASN1_bytes                         = nil then Result := Result + SP + FN_i2d_ASN1_bytes;//AG
    if @f_X509_get_pubkey                        = nil then Result := Result + SP + FN_X509_get_pubkey;//AG
    if @f_X509_PUBKEY_free                       = nil then Result := Result + SP + FN_X509_PUBKEY_free;//AG
    if @f_X509_check_purpose                     = nil then Result := Result + SP + FN_X509_check_purpose;//AG
    if @f_X509_PURPOSE_get_id                    = nil then Result := Result + SP + FN_X509_PURPOSE_get_id;//AG
    if @f_X509_PURPOSE_get0                      = nil then Result := Result + SP + FN_X509_PURPOSE_get0;//AG
    if @f_X509_PURPOSE_get0_name                 = nil then Result := Result + SP + FN_X509_PURPOSE_get0_name;//AG
    if @f_X509_PURPOSE_get0_sname                = nil then Result := Result + SP + FN_X509_PURPOSE_get0_sname;//AG
    if @f_X509_PURPOSE_get_count                 = nil then Result := Result + SP + FN_X509_PURPOSE_get_count;//AG
    if @f_CONF_modules_unload                    = nil then Result := Result + SP + FN_CONF_modules_unload;//AG
    {
    if @f_OPENSSL_add_all_algorithms_noconf      = nil then Result := Result + SP + FN_OPENSSL_add_all_algorithms_noconf;
    if @f_OPENSSL_add_all_algorithms_conf        = nil then Result := Result + SP + FN_OPENSSL_add_all_algorithms_conf;
    }
    if @f_OpenSSL_add_all_ciphers                = nil then Result := Result + SP + FN_OpenSSL_add_all_ciphers;
    if @f_OpenSSL_add_all_digests                = nil then Result := Result + SP + FN_OpenSSL_add_all_digests;

    if @f_PKCS7_new                              = nil then Result := Result + SP + FN_PKCS7_new;
    if @f_PKCS7_free                             = nil then Result := Result + SP + FN_PKCS7_free;
    if @f_PKCS7_set_type                         = nil then Result := Result + SP + FN_PKCS7_set_type;
    if @f_PKCS7_content_new                      = nil then Result := Result + SP + FN_PKCS7_content_new;
    if @f_PKCS7_add_certificate                  = nil then Result := Result + SP + FN_PKCS7_add_certificate;

    if @f_PKCS12_parse                           = nil then Result := Result + SP + FN_PKCS12_parse;
    if @f_PKCS12_verify_mac                      = nil then Result := Result + SP + FN_PKCS12_verify_mac;
    if @f_PKCS12_free                            = nil then Result := Result + SP + FN_PKCS12_free;
    if @f_PKCS12_create                          = nil then Result := Result + SP + FN_PKCS12_create;

    if @f_DH_free                                = nil then Result := Result + SP + FN_DH_free;
    if @f_EC_KEY_new_by_curve_name               = Nil then Result := Result + SP + FN_EC_KEY_new_by_curve_name;
    if @f_EC_KEY_free                            = Nil then Result := Result + SP + FN_EC_KEY_free;

{$IFNDEF OPENSSL_NO_ENGINE}
    if @f_ENGINE_load_builtin_engines            = nil then Result := Result + SP + FN_ENGINE_load_builtin_engines;//AG
    if @f_ENGINE_register_all_complete           = nil then Result := Result + SP + FN_ENGINE_register_all_complete;//AG
    if @f_ENGINE_cleanup                         = nil then Result := Result + SP + FN_ENGINE_cleanup;//AG
    if @f_ENGINE_by_id                           = nil then Result := Result + SP + FN_ENGINE_by_id;//AG
    if @f_ENGINE_init                            = nil then Result := Result + SP + FN_ENGINE_init;//AG
    if @f_ENGINE_finish                          = nil then Result := Result + SP + FN_ENGINE_finish;//AG
    if @f_ENGINE_set_default                     = nil then Result := Result + SP + FN_ENGINE_set_default;//AG
    if @f_ENGINE_ctrl_cmd_string                 = nil then Result := Result + SP + FN_ENGINE_ctrl_cmd_string;//AG
    if @f_ENGINE_free                            = nil then Result := Result + SP + FN_ENGINE_free;//AG
    if @f_ENGINE_load_private_key                = nil then Result := Result + SP + FN_ENGINE_load_private_key;//AG
    if @f_ENGINE_load_public_key                 = nil then Result := Result + SP + FN_ENGINE_load_public_key;//AG
    if @f_ENGINE_load_ssl_client_cert            = nil then Result := Result + SP + FN_ENGINE_load_ssl_client_cert;//AG
    if @f_UI_new                                 = nil then Result := Result + SP + FN_UI_new;//AG
    if @f_UI_new_method                          = nil then Result := Result + SP + FN_UI_new_method;//AG
    if @f_UI_free                                = nil then Result := Result + SP + FN_UI_free;//AG
    if @f_UI_create_method                       = nil then Result := Result + SP + FN_UI_create_method;//AG
    if @f_UI_destroy_method                      = nil then Result := Result + SP + FN_UI_destroy_method;//AG
    if @f_UI_set_ex_data                         = nil then Result := Result + SP + FN_UI_set_ex_data;//AG
    if @f_UI_get_ex_data                         = nil then Result := Result + SP + FN_UI_get_ex_data;//AG
    if @f_UI_method_set_reader                   = nil then Result := Result + SP + FN_UI_method_set_reader;//AG
    if @f_UI_set_result                          = nil then Result := Result + SP + FN_UI_set_result;//AG
    if @f_UI_OpenSSL                             = nil then Result := Result + SP + FN_UI_OpenSSL;//AG
{$ENDIF}
    if Length(Result) > 0 then
       Delete(Result, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsX509VerifyErrorToStr(ErrCode: Integer): String;
begin
{$IFNDEF OPENSSL_USE_RESOURCE_STRINGS}
    Result := String(AnsiString(f_X509_verify_cert_error_string(ErrCode)));
{$ELSE}
    case ErrCode of
        X509_V_OK :
            Result := sX509_V_OK;
        X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
            Result := sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT;
        X509_V_ERR_UNABLE_TO_GET_CRL:
            Result := sX509_V_ERR_UNABLE_TO_GET_CRL;
        X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE;
        X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE;
        X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
            Result := sX509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY;
        X509_V_ERR_CERT_SIGNATURE_FAILURE:
            Result := sX509_V_ERR_CERT_SIGNATURE_FAILURE;
        X509_V_ERR_CRL_SIGNATURE_FAILURE:
            Result := sX509_V_ERR_CRL_SIGNATURE_FAILURE;
        X509_V_ERR_CERT_NOT_YET_VALID:
            Result := sX509_V_ERR_CERT_NOT_YET_VALID;
        X509_V_ERR_CRL_NOT_YET_VALID:
            Result := sX509_V_ERR_CRL_NOT_YET_VALID;
        X509_V_ERR_CERT_HAS_EXPIRED:
            Result := sX509_V_ERR_CERT_HAS_EXPIRED;
        X509_V_ERR_CRL_HAS_EXPIRED:
            Result := sX509_V_ERR_CRL_HAS_EXPIRED;
        X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD;
        X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD;
        X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD;
        X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD:
            Result := sX509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD;
        X509_V_ERR_OUT_OF_MEM:
            Result := sX509_V_ERR_OUT_OF_MEM;
        X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
            Result := sX509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT;
        X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
            Result := sX509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN;
        X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
            Result := sX509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY;
        X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
            Result := sX509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE;
        X509_V_ERR_CERT_CHAIN_TOO_LONG:
            Result := sX509_V_ERR_CERT_CHAIN_TOO_LONG;
        X509_V_ERR_CERT_REVOKED:
            Result := sX509_V_ERR_CERT_REVOKED;
        X509_V_ERR_INVALID_CA:
            Result := sX509_V_ERR_INVALID_CA;
        X509_V_ERR_INVALID_NON_CA:
            Result := sX509_V_ERR_INVALID_NON_CA;
        X509_V_ERR_PATH_LENGTH_EXCEEDED:
            Result := sX509_V_ERR_PATH_LENGTH_EXCEEDED;
        X509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED:
            Result := sX509_V_ERR_PROXY_PATH_LENGTH_EXCEEDED;
        X509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED:
            Result := sX509_V_ERR_PROXY_CERTIFICATES_NOT_ALLOWED;
        X509_V_ERR_INVALID_PURPOSE:
            Result := sX509_V_ERR_INVALID_PURPOSE;
        X509_V_ERR_CERT_UNTRUSTED:
            Result := sX509_V_ERR_CERT_UNTRUSTED;
        X509_V_ERR_CERT_REJECTED:
            Result := sX509_V_ERR_CERT_REJECTED;
        X509_V_ERR_APPLICATION_VERIFICATION:
            Result := sX509_V_ERR_APPLICATION_VERIFICATION;
        X509_V_ERR_SUBJECT_ISSUER_MISMATCH:
            Result := sX509_V_ERR_SUBJECT_ISSUER_MISMATCH;
        X509_V_ERR_AKID_SKID_MISMATCH:
            Result := sX509_V_ERR_AKID_SKID_MISMATCH;
        X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH:
            Result := sX509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH;
        X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
            Result := sX509_V_ERR_KEYUSAGE_NO_CERTSIGN;
        X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER:
            Result := sX509_V_ERR_UNABLE_TO_GET_CRL_ISSUER;
        X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION:
            Result := sX509_V_ERR_UNHANDLED_CRITICAL_EXTENSION;
        X509_V_ERR_KEYUSAGE_NO_CRL_SIGN:
            Result := sX509_V_ERR_KEYUSAGE_NO_CRL_SIGN;
        X509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE:
            Result := sX509_V_ERR_KEYUSAGE_NO_DIGITAL_SIGNATURE;
        X509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION:
            Result := sX509_V_ERR_UNHANDLED_CRITICAL_CRL_EXTENSION;
        X509_V_ERR_INVALID_EXTENSION:
            Result := sX509_V_ERR_INVALID_EXTENSION;
        X509_V_ERR_INVALID_POLICY_EXTENSION:
            Result := sX509_V_ERR_INVALID_POLICY_EXTENSION;
        X509_V_ERR_NO_EXPLICIT_POLICY:
            Result := sX509_V_ERR_NO_EXPLICIT_POLICY;
        X509_V_ERR_UNNESTED_RESOURCE:
            Result := sX509_V_ERR_UNNESTED_RESOURCE;
    else
        Result := sX509_V_ERR_NUMBER + IntToStr(ErrCode);
    end;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_CRL_get_issuer(crl: PX509_CRL): PX509_NAME;
begin
    Result := crl^.crl^.issuer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_version(X509: PX509): Integer;
begin
    Result := f_ASN1_INTEGER_get(X509^.cert_info^.version);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_signature_algorithm(X509: PX509): Integer;
begin
    Result := f_OBJ_obj2nid(X509^.sig_alg^.algorithm);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_ENGINE}
function f_Ics_UI_set_app_data(r: PUI; arg: Pointer): Integer;
begin
    Result := f_UI_set_ex_data(r, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_UI_get_app_data(r: PUI): Pointer;
begin
    Result := f_UI_get_ex_data(r, 0);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
     X509_L_FILE_LOAD   = 1;
     X509_L_ADD_DIR     = 2;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Openssl macro }
function f_Ics_X509_LOOKUP_load_file(Ctx: PX509_LOOKUP; FileName: PAnsiChar;
    Type_: Longword): Integer;
begin
    Result := f_X509_LOOKUP_ctrl(Ctx, X509_L_FILE_LOAD, FileName, Type_, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Openssl macro }
function f_Ics_X509_LOOKUP_add_dir(Ctx: PX509_LOOKUP; DirName: PAnsiChar;
    Type_: Longword): Integer;
begin
    Result := f_X509_LOOKUP_ctrl(Ctx, X509_L_ADD_DIR, DirName, Type_, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_REASON(ErrCode : Cardinal) : Cardinal;
begin
    Result := (ErrCode and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_LIB(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 24) and $FF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_GET_FUNC(ErrCode : Cardinal) : Cardinal;
begin
    Result := ((ErrCode shr 12) and $FFF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ERR_FATAL_ERROR(ErrCode : Cardinal) : Boolean;
begin
    Result := ((ErrCode and ERR_R_FATAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_notBefore(X: PX509): PASN1_TIME;        {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        PCInfo := Pointer(PINT_PTR(X)^);
        Result := PCInfo^.Validity^.notBefore;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_Ics_X509_get_notAfter(X: PX509): PASN1_TIME;         {AG 03/03/06}
var
    PCInfo : PX509_CINF;
begin
    if Assigned(X) then begin
        PCInfo := Pointer(PINT_PTR(X)^);
        Result := PCInfo^.Validity^.notAfter;
    end
    else
        Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToUTDateTime(Asn1Time: PASN1_TIME;               {AG 03/03/06}
    out UT: TDateTime): Boolean;

    function IncHour(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 24) + IncBy) / 24;
    end;

    function IncMin(const DT: TDateTime; const IncBy: Integer): TDateTime;
    begin
        Result := ((DT * 1440) + IncBy) / 1440;
    end;

var
    Y, M, D, H, N, S : Word;
    I : Integer;
    YC : Word;  { Current century }
    P  : PAnsiChar;
    Offset : Integer;
    Str    : AnsiString;
    IntH, IntM : Integer;
    Sign : Boolean;
begin
    Result  := FALSE;
    UT      := MinDateTime;
    if not Assigned(Asn1Time) then
        Exit;
    try
        I := Asn1Time^.length;
        if I < 10 then Exit;
        P   := Asn1Time.data;
        Y := 0; M := 0; D := 0; {H := 0; N := 0;} S := 0;

        if Asn1Time^.Type_ = V_ASN1_UTCTIME then begin
            {if I < 10 then
                Exit;}
            for I := 0 to 10 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            DecodeDate(Now, Y, M, D);
            YC := (Trunc(Y / 100) * 100);
            Y := atoi(P[0] + P[1]);
            if Y < 50 then   { fix century }
                Y := Y + YC
            else
                Y := Y + YC - 100;
            M := atoi(P[2] + P[3]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[4] + P[5]);
            H := atoi(P[6] + P[7]);
            N := atoi(P[8] + P[9]);
            { Do we have seconds? }
            if (P[10] in ['0'..'9']) and
               (P[11] in ['0'..'9']) then
            S := atoi(P[10] + P[11]);
        end else
        if Asn1Time^.Type_ = V_ASN1_GENERALIZEDTIME then begin
            if I < 12 then Exit;
            for I := 0 to 12 - 1 do
                if not (P[I] in ['0'..'9']) then
                    Exit;
            Y := atoi(P[0] + P[1] + P[2] + P[3]);
            M := atoi(P[4] + P[5]);
            if (M > 12) or (M < 1) then
                Exit;
            D := atoi(P[6] + P[7]);
            H := atoi(P[8] + P[9]);
            N := atoi(P[10] + P[11]);
            { Do we have seconds? }
            if (P[12] in ['0'..'9']) and
               (P[13] in ['0'..'9']) then
            S := atoi(P[12] + P[13]);
        end else
            Exit;
        UT := EncodeDate(Y, M, D) + EncodeTime(H, N, S, 0);

        { Timezone Offset                                          }
        { '980101000000Z' sample V_ASN1_UTCTIME GMT                }
        { '990630000000+1000' sample timezone + 10 hours           }
        { '20000322085551Z' sample V_ASN1_GENERALIZEDTIME GMT      }
        I := Asn1Time^.length;
        if P[I - 1] <> 'Z' then  // Z = GMT = offset = 0
           { Offset := 0         // offset 0
        else} begin              // get the offset
            SetLength(Str, I);
            Dec(I);
            while I >= 0 do begin
                if P[I] in ['0'..'9'] then
                    Dec(I)
                else begin
                    if P[I] in ['-', '+'] then
                    begin
                        if P[I] = '-' then
                            Sign := TRUE
                        else
                            Sign := FALSE;
                        StrECopy(PAnsiChar(Str), PAnsiChar(@P[I + 1]));
                        SetLength(Str, StrLen(PAnsiChar(Str)));
                        Offset := atoi(Str);
                        if Sign then
                            Offset := -Offset;
                        if (Offset <> 0) and (Offset >= -1200) and
                           (Offset <= 1300) then begin
                            IntH := (Offset div 100);
                            IntM := (Offset mod 100);
                            if IntH <> 0 then
                                UT := IncHour(UT, IntH);
                            if IntM <> 0 then
                                UT := IncMin(UT,  IntM);
                        end;
                    end;
                    Break;
                end;
            end;
        end;
        Result := True;
    except
        // do nothing
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_get_flags(b: PBIO): Integer;
begin
    // This is a hack : BIO structure has not been defined. But I know
    // flags member is the 6th field in the structure (index is 5)
    // This could change when OpenSSL is updated. Check "struct bio_st".
    Result := PInteger(PAnsiChar(b) + 3 * SizeOf(Pointer) + 2 * SizeOf(Integer))^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_retry(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_SHOULD_RETRY) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ASN1_ITEM_ptr(iptr: PASN1_ITEM_EXP): PASN1_ITEM;
begin
    Result := iptr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_read(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_READ) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_write(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_WRITE) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_should_io_special(b: PBIO): Boolean;
begin
    Result := ((BIO_get_flags(b) and BIO_FLAGS_IO_SPECIAL) <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BIO_retry_type(b: PBIO): Integer;
begin
    Result := (BIO_get_flags(b) and BIO_FLAGS_RWS);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslVersion : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_VERSION)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslCompilerFlags : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_CFLAGS)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslBuiltOn : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_BUILT_ON)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslPlatForm : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_PLATFORM)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OpenSslDir : String;
begin
    Result := String(StrPas(f_SSLeay_version(SSLEAY_DIR)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BMPStrToWideStr(Str : PAnsiChar; Len : Integer): UnicodeString;
var
    I : Integer;
begin
    SetLength(Result, Len shr 1);
    for I := 0 to (Len shr 1) - 1 do
        Result[I + 1] := WideChar(Byte(Str[I * 2 + 1]) or Byte(Str[I * 2]) shl 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function EncodeOctetStr(Str : PAnsiChar; Len : Integer) : String;
var
    I : Integer;
    Item : String;
begin
    if (Len = 0) or (Str = nil) then Exit;
    SetLength(Result, Len * 3);
    I := 0;
    while I <= Len - 1 do begin
        Item := IntToHex(Ord(Str[I]), 2) + ':';
        Move(Item[1], Result[I * 3 + 1], 3 * SizeOf(Char));
        Inc(I);
    end;
    SetLength(Result, Length(Result) - 1);
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Asn1ToString(PAsn1 : PASN1_STRING): String;
{$IFDEF UNICODE}
var
    Len : Integer;
{$ENDIF}
begin
    if (PAsn1 = nil) or (PAsn1^.data = nil) or (PAsn1^.length <= 0) then
        Exit;
    case PAsn1^.type_ of

      V_ASN1_OCTET_STRING :
          //Result := EncodeOctetStr(PAsn1^.data, PAsn1^.length);
          Result := IcsBufferToHex(PAsn1^.data, PAsn1^.length, ':');
{$IFNDEF UNICODE}
      V_ASN1_UTF8STRING :
      begin  { Slow, but rarely used }
          SetLength(Result, PAsn1^.length);
          Move(PAnsiChar(PAsn1^.data)^, PAnsiChar(Result)^, PAsn1^.length);
          Result := Utf8ToStringA(Result); { convert to Ansi }
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order and convert to Ansi }
          Result := UnicodeToAnsi(BMPStrToWideStr(PAsn1^.data, PAsn1^.length));

      else  { dump }
          SetLength(Result, PAsn1^.length);
          Move(Pointer(PAsn1^.data)^, Pointer(Result)^, PAsn1^.length);
{$ELSE}
      V_ASN1_UTF8STRING :
      begin
          Len := IcsMbToWc(CP_UTF8, 0, PAsn1^.data,  PAsn1^.length, nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
              IcsMbToWc(CP_UTF8, 0, PAsn1^.data, PAsn1^.length, Pointer(Result), Len);
      end;

      V_ASN1_BMPSTRING :
          { Reverse byte order }
          Result := BMPStrToWideStr(PAsn1^.data, PAsn1^.length);

      else  { dump }
          Len := IcsMbToWc(CP_ACP, 0, PAsn1^.data, PAsn1^.length, nil, 0);
          SetLength(Result, Len);
          if Len > 0 then
              IcsMbToWc(CP_ACP, 0, PAsn1^.data, PAsn1^.length, Pointer(Result), Len);
{$ENDIF}
      end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_get_secure_renegotiation_support(S: PSSL): Longint;
begin
//    if ICS_OPENSSL_VERSION_NUMBER >= OSSL_VER_0908N then
        Result := f_SSL_ctrl(S, SSL_CTRL_GET_RI_SUPPORT, 0, nil)
//    else
//        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Ics_Ssl_EVP_PKEY_IncRefCnt(K: PEVP_PKEY; Increment: Integer = 1);
begin
    { This is thread-safe only with a TSslStaticLock or TSslDynamicLock.    }
    { From the OpenSSL sources I know that lock-ID CRYPTO_LOCK_EVP_PKEY is  }
    { used by OpenSSL to protect EVP_PKEY_st.references field.              }
    f_Crypto_lock(CRYPTO_LOCK, CRYPTO_LOCK_EVP_PKEY,
                  PAnsiChar('Ics_Ssl_EVP_PKEY_IncRefCnt'), 0);
    try
        { This is a hack and might change with new OSSL version, search for }
        { "struct EVP_PKEY_st" field "references".                          }
        Inc(PInteger(PAnsiChar(K) + 2 * SizeOf(Longint))^, Increment);
    finally
        f_Crypto_lock(CRYPTO_UNLOCK, CRYPTO_LOCK_EVP_PKEY,
                      PAnsiChar('Ics_Ssl_EVP_PKEY_IncRefCnt'), 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_Ssl_EVP_PKEY_GetKey(K: PEVP_PKEY): Pointer;
begin
    if @f_EVP_PKEY_get0 <> nil then // v1.0.0+
        Result := f_EVP_PKEY_get0(K)
    else
        { * This is a hack * }
    {$IFDEF CPUX64} // Alignment of OSSL records is 8 bytes!
        Result := Pointer(PSize_t(PAnsiChar(K) + 4 * SizeOf(Longint))^);
    {$ELSE}
        Result := Pointer(PSize_t(PAnsiChar(K) + 3 * SizeOf(Longint))^);
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Ics_Ssl_EVP_PKEY_GetType(K: PEVP_PKEY): Integer;
begin
    { This is a hack and might change with new OSSL version, search }
    { for "struct EVP_PKEY_st"                                      }
    Result := PInteger(K)^;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get library from OpenSsl error code - ERR_LIB_xxx literals }
function Ics_Ssl_ERR_GET_LIB(E: DWORD): Integer;
begin
    result := (E shr 24) and $0ff;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get function from OpenSsl error code - SSL_F_xxx literals }
function Ics_Ssl_ERR_GET_FUNC(E: DWORD): Integer;
begin
    result := (E shr 12) and $0fff;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.05 get reason from OpenSsl error code - SSL_R_xxx literals }
function Ics_Ssl_ERR_GET_REASON(E: DWORD): Integer;
begin
    Result := E and $0fff;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} //USE_SSL
end.


