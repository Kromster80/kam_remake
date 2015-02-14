{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Delphi encapsulation for SSLEAY32.DLL (OpenSSL)
              This is only the subset needed by ICS.
Creation:     Jan 12, 2003
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2011 by François PIETTE
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
Dec 11, 2004 Fixed Load to correctly check for LoadLibrary failure and to
             return correct error number.
Nov 08, 2005 F. Piette Worked around Delphi 6 bug related to type redefinition.
             Search for NoTypeEnforce in the type declarations.
Nov 08, 2005 Arno Garrels - Checked non-dummy structures against different
             OpenSSL versions see comments.
             Changed declaration of TX509_EXTENSION according OpenSSL v.0.9.7g.
Nov 19, 2005 F. Piette fixed internal error for Delphi 5. Same fix as Delphi 6.
             Introduced symbol NoTypeEnforce for that purpose.
Dec 07, 2005 A. Garrels support of OSSL v0.9.8a added.
Jan 27, 2006 A. Garrels made BDS2006 (BCB & Pascal) compilers happy.
Mar 03, 2007 A. Garrels: Small changes to support OpenSSL 0.9.8e.
             Read comments in OverbyteIcsSslDefs.inc.
Jun 30, 2008 A.Garrels made some changes to prepare code for Unicode.
             Added a few constants and dummy records.
Aug 02, 2008 Still one PChar caught in one of the records.
Dec 20, 2009 A.Garrels added plenty of stuff. Some is not yet used some is, like
             Server Name Indication (SNI).
May 08, 2010 A. Garrels added two declarations required to support
             Open SSL 0.9.8n.
Apr 23, 2011 A. Garrels added C-macro f_SSL_clear_options.
Apr 24, 2011 Arno - Record TEVP_PKEY_st changed in 1.0.0 and had to
             be declared as dummy. See helper functions Ics_Ssl_EVP_PKEY_xxx
             in OverbyteIcsLibeay.pas.
May 03, 2011 Arno added some function declarations.
May 31, 2011 Arno removed the packed modifier from non-dummy records.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{$I Include\OverbyteIcsDefs.inc}
{$I Include\OverbyteIcsSslDefs.inc}
{$A8}

unit OverbyteIcsSSLEAY;

{$IFDEF VER140}
    // Delphi 6 is bugged
    // [Erreur fatale] IcsSSLEAY.pas: Erreur interne : URW533
    {$DEFINE NoTypeEnforce}
{$ENDIF}
{$IFDEF VER130}
    // Delphi 5 is bugged
    // [Erreur fatale] IcsSSLEAY.pas: Erreur interne : URW533
    {$DEFINE NoTypeEnforce}
{$ENDIF}

interface

{$IFDEF USE_SSL}

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
  {$ENDIF}
  {$IFDEF POSIX}
    Posix.Errno,
  {$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    OverbyteIcsUtils;

const
    IcsSSLEAYVersion   = 800;
    CopyRight : String = ' IcsSSLEAY (c) 2003-2012 F. Piette V8.00 ';

    EVP_MAX_IV_LENGTH                 = 16;       { 03/02/07 AG }
    EVP_MAX_BLOCK_LENGTH              = 32;       { 11/08/07 AG }
    EVP_MAX_KEY_LENGTH                = 32;       { 11/08/07 AG }

type
    EIcsSsleayException = class(Exception);
    PPChar   = ^PChar;
    PPAnsiChar = ^PAnsiChar;
    //PInteger = ^Integer;

    // All datatypes defined below using the Dummy array can't be used
    // directly. They all must be used thru pointers only. If you really need
    // to use those structure, you must replace Dummy member with the actual
    // members defined in the OpenSSL header !
    TCRYPTO_THREADID_st = packed record
        Dummy : array [0..0] of Byte;
	      //ptr : Pointer;
	      //val : LongWord;
    end;
    PCRYPTO_THREADID = ^TCRYPTO_THREADID_st;

{$IFNDEF OPENSSL_NO_ENGINE}
    TEngine_st = record
        Dummy : array [0..0] of Byte;
    end;
    PENGINE = ^TEngine_st;
{$ENDIF}

    TSSL_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL            = ^TSSL_st;

    TSSL_SESSION_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_SESSION    = ^TSSL_SESSION_st;
    PPSSL_SESSION   = ^PSSL_SESSION;

    TBIO_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PBIO            = ^TBIO_st;
    PPBIO           = ^PBIO;

    TBIO_METHOD_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PBIO_METHOD     = ^TBIO_METHOD_st;

    TSSL_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_CTX        = ^TSSL_CTX_st;

    TSSL_METHOD_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PSSL_METHOD     = ^TSSL_METHOD_st;

    TX509_STORE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_STORE = ^TX509_STORE_st;

    TX509_STORE_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_STORE_CTX = ^TX509_STORE_CTX_st;

    TX509_NAME_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_NAME = ^TX509_NAME_st;

    TSTACK_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PSTACK = ^TSTACK_st;

    TASN1_TYPE_st = packed record                   //AG
        Dummy : array [0..0] of Byte;
    end;
    PASN1_TYPE = ^TASN1_TYPE_st;

    { Stack - dummies i.e. STACK_OF(X509) }

    PSTACK_OF_X509_EXTENSION    = PStack;     //AG
    PSTACK_OF_X509_ALGOR        = PStack;     //AG

    PSTACK_OF_X509              = PSTACK;     //AG
    PSTACK_OF_X509_CRL          = PSTACK;     //AG

    PPSTACK_OF_X509             = ^PSTACK_OF_X509; //AG

    PSTACK_OF_PKCS7_RECIP_INFO  = PStack;     //AG
    PSTACK_OF_X509_ATTRIBUTE    = PStack;     //AG
    PSTACK_OF_PKCS7_SIGNER_INFO = PStack;     //AG
    PSTACK_OF_509_LOOKUP        = PStack;     //AG
    PSTACK_OF_X509_OBJECT       = PStack;     //AG

    PSTACK_OF_X509_NAME = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;
    PSTACK_OF_X509_INFO = {$IFNDEF NoTypeEnforce}type{$ENDIF} PStack;


    TX509_lookup_method_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_LOOKUP_METHOD = ^TX509_lookup_method_st;

    TX509_lookup_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_LOOKUP = ^TX509_lookup_st;

    TX509_OBJECT_st = packed record         //AG
        Dummy : array [0..0] of Byte;
    end;
    PX509_OBJECT = ^TX509_OBJECT_st;

    TX509_NAME_ENTRY_st = packed record      //AG
        Dummy : array [0..0] of Byte;
    end;
    PX509_NAME_ENTRY = ^TX509_NAME_ENTRY_st;

    TEVP_MD_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PEVP_MD = ^TEVP_MD_st;

    TRSA_st = packed record
        Dummy : array [0..0] of Byte;      //AG
    end;
    PRSA = ^TRSA_st;

    TDSA_st = packed record                //AG
        Dummy : array [0..0] of Byte;
    end;
    PDSA = ^TDSA_st;

    TDH_st = packed record                 //AG
        Dummy : array [0..0] of Byte;
    end;
    PDH = ^TDH_st;

    TEC_KEY_st = packed record                 //AG
        Dummy : array [0..0] of Byte;
    end;
    PEC_KEY = ^TEC_KEY_st;

    { We may no longer define it since changed in 1.0.0+               }
    { See helper functions Ics_Ssl_EVP_PKEYxxx in OverbyteIcsLibeay32  }
    TEVP_PKEY_st = packed record
        Dummy : array [0..0] of Byte;
    (*
        type_       : Longint;
        save_type   : Longint;
        references  : Longint;
    {OSSL_100 two fields added}
        ameth       : Pointer; //PEVP_PKEY_ASN1_METHOD;
        engine      : Pointer; //PENGINE;
    {/OSSL_100}
        case Integer of
        0 : (ptr  : PAnsiChar);
        1 : (rsa  : PRSA); // RSA
        2 : (dsa  : PDSA); // DSA
        3 : (dh   : PDH);  // DH
        4 : (ec   : PEC_KEY); //* ECC */
        { more not needed ...
        int save_parameters;
        STACK_OF(X509_ATTRIBUTE) *attributes; /* [ 0 ] */ }
    *)
    end;
    PEVP_PKEY = ^TEVP_PKEY_st;
    PPEVP_PKEY = ^PEVP_PKEY;

    TEVP_CIPHER_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PEVP_CIPHER = ^TEVP_CIPHER_st;

    TASN1_OBJECT_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_OBJECT = ^TASN1_OBJECT_st;

    TX509_ALGOR_st = record
        algorithm : PASN1_OBJECT;
        parameter : PASN1_TYPE;
    end;
    PX509_ALGOR = ^TX509_ALGOR_st;

    TX509_PURPOSE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_PURPOSE = ^TX509_PURPOSE_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TASN1_STRING_st = record
        length : Integer;
        type_  : Integer;
        data   : PAnsiChar;
        //* The value of the following field depends on the type being
        //* held.  It is mostly being used for BIT_STRING so if the
        //* input data has a non-zero 'unused bits' value, it will be
        //* handled correctly */
        flags  : Longword;
    end;
    PASN1_STRING       = ^TASN1_STRING_st;
    TASN1_OCTET_STRING = TASN1_STRING_st;
    PASN1_OCTET_STRING = ^TASN1_OCTET_STRING;
    TASN1_BIT_STRING   = TASN1_STRING_st;
    PASN1_BIT_STRING   = ^TASN1_BIT_STRING;

    TASN1_TIME = {$IFNDEF NoTypeEnforce}type{$ENDIF} TASN1_STRING_st;
    PASN1_TIME = ^TASN1_TIME;

    TASN1_INTEGER = {$IFNDEF NoTypeEnforce}type{$ENDIF} TASN1_STRING_st;
    PASN1_INTEGER = ^TASN1_INTEGER;

    TASN1_VALUE_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_VALUE  = ^TASN1_VALUE_st;
    PPASN1_VALUE = ^PASN1_VALUE;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TEVP_CIPHER_INFO_st = record      { 03/02/07 AG }
        cipher : PEVP_CIPHER;
        iv     : array [0..EVP_MAX_IV_LENGTH - 1] of AnsiChar;
    end;
    EVP_CIPHER_INFO  = TEVP_CIPHER_INFO_st;
    PEVP_CIPHER_INFO = ^EVP_CIPHER_INFO;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TPrivate_key_st = record            //AG
        //Dummy : array [0..0] of Byte;
        version     : Integer;
        // The PKCS#8 data types
        enc_algor   : PX509_ALGOR;
        enc_pkey    : PASN1_OCTET_STRING; // encrypted pub key
        // When decrypted, the following will not be NULL
        dec_pkey    : PEVP_PKEY;
        // used to encrypt and decrypt
        key_length  : Integer ;
        key_data    : PAnsiChar;
        key_free    : Integer; // true if we should auto free key_data
        // expanded version of 'enc_algor'
        cipher      : PEVP_CIPHER_INFO;
        references  : Integer ;
    end;
    PX509_PKEY = ^TPrivate_key_st;

    TX509_REQ_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PX509_REQ = ^TX509_REQ_st;

    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TX509_CRL_INFO_st = record
        version     : PASN1_INTEGER;
        sig_alg     : PX509_ALGOR;
        issuer      : PX509_NAME;
        lastUpdate  : PASN1_TIME;
        nextUpdate  : PASN1_TIME;
        {
        STACK_OF(X509_REVOKED) *revoked;
        STACK_OF(X509_EXTENSION) /* [0] */ *extensions;
        ASN1_ENCODING enc; }
    end;
    PX509_CRL_INFO = ^TX509_CRL_INFO_st;

    TX509_CRL_st = record
        //* actual signature *//
        crl       : PX509_CRL_INFO;
        sig_alg   : PX509_ALGOR;
        signature : PASN1_BIT_STRING;
        references: Integer;
        {more..}
    end;
    PX509_CRL = ^TX509_CRL_st;
    PPX509_CRL = ^PX509_CRL;


    PX509  = ^TX509_st;
    PPX509 = ^PX509;
    
    // 0.9.7g, 0.9.8a 0.9.8e, 1.0.0d
    TX509_INFO_st = record
        x509        : PX509;
        crl         : PX509_CRL;
        x_pkey      : PX509_PKEY;
        enc_cipher  : EVP_CIPHER_INFO;
        enc_len     : Integer;
        enc_data    : PAnsiChar;
        references  : Integer;
    end;
    PX509_INFO = ^TX509_INFO_st;

    (* // 0.9.6g                  {11/07/05 AG}
    TX509_EXTENSION = packed record
        object_       : PASN1_OBJECT;
        critical      : SmallInt;
        netscape_hack : SmallInt;
        value         : PASN1_OCTET_STRING;
        method        : PX509V3_EXT_METHOD;
        ext_val       : Pointer;	        // extension value
    end;
    PX509_EXTENSION = ^TX509_EXTENSION;
    *)

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_VAL_st = record                    {AG 02/06/06}
        notBefore : PASN1_TIME;
        notAfter  : PASN1_TIME;
    end;
    PX509_VAL = ^TX509_VAL_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_PUBKEY_st = record                 //AG
        algor       : PX509_ALGOR;
        public_key  : PASN1_BIT_STRING;
        pkey        : PEVP_PKEY;
    end;
    PX509_PUBKEY = ^TX509_PUBKEY_st;

    { Certinfo }  // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d         {AG 02/06/06}
    TX509_CINF_st = record
        version         : PASN1_INTEGER;            // [ 0 ] default of v1
        serialNumber    : PASN1_INTEGER;
        signature       : PX509_ALGOR;
        issuer          : PX509_NAME;
        validity        : PX509_VAL;
        subject         : PX509_NAME;
        key             : PX509_PUBKEY;
        {issuerUID       : PASN1_BIT_STRING;         // [ 1 ] optional in v2
        subjectUID      : PASN1_BIT_STRING;         // [ 2 ] optional in v2
        extensions      : PSTACK_OF_X509_EXTENSION; // [ 3 ] optional in v3}
    end;
    PX509_CINF = ^TX509_CINF_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d             {11/07/05 AG}
    ASN1_BOOLEAN = {$IFNDEF NoTypeEnforce}type{$ENDIF} Longint;
    TX509_EXTENSION_st = record
        object_       : PASN1_OBJECT;
        critical      : ASN1_BOOLEAN;
        value         : PASN1_OCTET_STRING;
    end;
    PX509_EXTENSION = ^TX509_EXTENSION_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509_st = record
        cert_info   : PX509_CINF;
        sig_alg     : PX509_ALGOR;
        signature   : PASN1_BIT_STRING;
        valid       : Integer ;
        references  : Integer;
        name        : PAnsiChar;
        {more ...}
    end;

    TPKCS7_ISSUER_AND_SERIAL_st = packed record     //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ISSUER_AND_SERIAL = ^TPKCS7_ISSUER_AND_SERIAL_st;

    TPKCS7_ENC_CONTENT_st = packed record           //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ENC_CONTENT = ^TPKCS7_ENC_CONTENT_st;

    TPKCS7_DIGEST_st = packed record                //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_DIGEST = ^TPKCS7_DIGEST_st;

    TPKCS7_ENCRYPT_st = packed record               //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS7_ENCRYPT = ^TPKCS7_ENCRYPT_st;

    { Caution! Structures may change in future versions 0.96g-0.98 beta OK} {AG}
    { However needed for S/MIME PKCS#7 parsing }
    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_SIGNER_INFO_st = record
        version             : PASN1_INTEGER;                // version 1
        issuer_and_serial   : PPKCS7_ISSUER_AND_SERIAL;
        digest_alg          : PX509_ALGOR;
        auth_attr           : PSTACK_OF_X509_ATTRIBUTE;     // [ 0 ]
        digest_enc_alg      : PX509_ALGOR;
        enc_digest          : PASN1_OCTET_STRING;
        unauth_attr         : PSTACK_OF_X509_ATTRIBUTE;     // [ 1 ]
        // The private key to sign with //
        pkey                : PEVP_PKEY;
    end;
    //PKCS7_SIGNER_INFO = ^TPKCS7_SIGNER_INFO_st; // **Name conflict with wincrypt.h**
    PKCS7_SIGNER_INFO_OSSL = ^TPKCS7_SIGNER_INFO_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_ENVELOPED_st = record
        version       : PASN1_INTEGER;
        recipientinfo : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data      : PPKCS7_ENC_CONTENT;
    end;
    PPKCS7_ENVELOPE = ^TPKCS7_ENVELOPED_st;

    PPKCS7  = ^TPKCS7_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_SIGNED_st = record
        version     : PASN1_INTEGER;
        md_algs     : PSTACK_OF_X509_ALGOR;
        cert        : PSTACK_OF_X509;
        crl         : PSTACK_OF_X509_CRL;
        signer_info : PSTACK_OF_PKCS7_SIGNER_INFO;
        contents    : PPKCS7;
    end;
    PPKCS7_SIGNED = ^TPKCS7_SIGNED_st;

    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    PKCS7_signedandenveloped = record
        version         : PASN1_INTEGER;
        md_algs         : PSTACK_OF_X509_ALGOR;
        cert            : PSTACK_OF_X509;
        crl             : PSTACK_OF_X509_CRL;
        signer_info     : PSTACK_OF_PKCS7_SIGNER_INFO;
        enc_data        : PPKCS7_ENC_CONTENT;
        recipientinfo   : PSTACK_OF_PKCS7_RECIP_INFO;
    end;
    PPKCS7_SIGN_ENVELOPE = ^PKCS7_signedandenveloped;
    
    // 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TPKCS7_st = record                         //AG
      { The following is non NULL if it contains ASN1 encoding of this structure }
        asn1        : PAnsiChar;
        length      : Integer;
        state       : Integer;
        detached    : Integer;
        type_       : PASN1_OBJECT;
        case Integer of
        0: (ptr                  : PAnsiChar);
        // NID_pkcs7_data
        1: (data                 : PASN1_OCTET_STRING);
        // NID_pkcs7_signed
        2: (sign                 : PPKCS7_SIGNED);
        // NID_pkcs7_enveloped
        3: (enveloped            : PPKCS7_ENVELOPE);
        // NID_pkcs7_signedAndEnveloped
        4: (signed_and_enveloped : PPKCS7_SIGN_ENVELOPE);
        // NID_pkcs7_digest
        5: (digest               : PPKCS7_DIGEST);
        // NID_pkcs7_encrypted
        6: (encrypted            : PPKCS7_ENCRYPT);
        // Anything else
        7: (other                : PASN1_TYPE);
    end;
    PPPKCS7 = ^PPKCS7;
    { Danger ends } {AG}

    TPKCS12_st = packed record                          //AG
        Dummy : array [0..0] of Byte;
    end;
    PPKCS12 = ^TPKCS12_st;
    PPPKCS12 = ^PPKCS12;


    TV3_EXT_CTX_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PV3_EXT_CTX = ^TV3_EXT_CTX_st;

    // 0.9.7g, 0.9.8a, 0.9.8e
    TCONF_VALUE = record
        Section : PAnsiChar;
        Name    : PAnsiChar;
        Value   : PAnsiChar;
    end;
    PCONF_VALUE = ^TCONF_VALUE;

     //used in old PostConnectionCheck()  {11/07/05 AG}
    { TCONF_VALUE_STACK_st = packed record
        Dummy : array [0..0] of Byte;
    end;
    PCONF_VALUE_STACK = ^TCONF_VALUE_STACK_st; }

    TASN1_ITEM_st  = packed record
        Dummy : array [0..0] of Byte;
    end;
    PASN1_ITEM  = ^TASN1_ITEM_st;

    PASN1_ITEM_EXP     = function: PASN1_ITEM; cdecl;
    PX509V3_EXT_NEW    = function: Pointer; cdecl;
    PX509V3_EXT_FREE   = procedure(Arg: Pointer); cdecl;
    PX509V3_EXT_D2I    = function(Arg1: Pointer; Arg2: PPAnsiChar; Arg3: LongInt): Pointer; cdecl;
    PX509V3_EXT_I2D    = function(Arg1: Pointer; Arg2: PPAnsiChar): Integer; cdecl;
    PX509V3_EXT_I2S    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer): PAnsiChar; cdecl;
    PX509V3_EXT_S2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; cdecl;
    PX509V3_EXT_I2V    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; ExtList: PSTACK): PSTACK; cdecl;
    PX509V3_EXT_V2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; Values: PSTACK): Pointer; cdecl;
    PX509V3_EXT_I2R    = function(X509V3_EXT_METHOD: Pointer; Ext: Pointer; Output: PBIO; Indent: Integer): Integer; cdecl;
    PX509V3_EXT_R2I    = function(X509V3_EXT_METHOD: Pointer; Ctx: PV3_EXT_CTX; S: PAnsiChar): Pointer; cdecl;

    // V3 extension structure 0.9.7g, 0.9.8a, 0.9.8e, 1.0.0d
    TX509V3_EXT_METHOD = record // struct v3_ext_method
        ext_nid   : Integer;
        ext_flags : Integer;

        // If this is set the following four fields are ignored - since v0.9.0.7
        it        : PASN1_ITEM_EXP;                              //AG

        // Old style ASN1 calls
        ext_new   : PX509V3_EXT_NEW;
        ext_free  : PX509V3_EXT_FREE;
        d2i       : PX509V3_EXT_D2I;
        i2d       : PX509V3_EXT_I2D;
        // The following pair is used for string extensions
        i2s       : PX509V3_EXT_I2S;
        s2i       : PX509V3_EXT_S2I;
        // The following pair is used for multi-valued extensions
        i2v       : PX509V3_EXT_I2V;
        v2i       : PX509V3_EXT_V2I;
        // The following are used for raw extensions
        i2r       : PX509V3_EXT_I2R;
        r2i       : PX509V3_EXT_R2I;
        // Any extension specific data
        usr_data  : Pointer;
    end;
    PX509V3_EXT_METHOD = ^TX509V3_EXT_METHOD;
    
type
    TPem_password_cb = function(Buf      : PAnsiChar;
                                Num      : Integer;
                                RWFlag   : Integer;
                                UserData : Pointer) : Integer; cdecl;

    TRSA_genkey_cb = procedure(N1, N2 : Integer;     //AG
                               cb_arg : Pointer); cdecl;

    TSetVerify_cb = function(Ok : Integer; StoreCtx : PX509_STORE_CTX) : Integer; cdecl;
    TSetInfo_cb   = procedure(const ssl: PSSL; CbType: Integer; Val: Integer); cdecl;

    TNew_session_cb = function(const Ssl : PSSL; Sess : PSSL_SESSION): Integer; cdecl;
    PNew_session_cb = ^TNew_session_cb;
    TRemove_session_cb = procedure(const Ctx : PSSL_CTX; Sess : PSSL_SESSION); cdecl;
    PRemove_session_cb = ^TRemove_session_cb;
    TGet_session_cb = function(const Ssl : PSSL; SessID : Pointer; IdLen : Integer; Ref : PInteger) : PSSL_SESSION; cdecl;
    PGet_session_cb = ^TGet_session_cb;
    TClient_cert_cb = function (Ssl : PSSL; X509 : PPX509; PKEY : PPEVP_PKEY): Integer; cdecl;
    PClient_cert_cb = ^TClient_cert_cb;

{$IFNDEF OPENSSL_NO_TLSEXT}
    TCallback_ctrl_fp = procedure (p : Pointer); cdecl;
    TSsl_servername_cb = function (s: PSSL; var ad: Integer; arg: Pointer): Integer; cdecl;
{$ENDIF}

const
    SSL2_VERSION                                = $0002;
    SSL2_VERSION_MAJOR                          = $00;
    SSL2_VERSION_MINOR                          = $02;

    SSL3_VERSION                                = $0300;
    SSL3_VERSION_MAJOR                          = $03;
    SSL3_VERSION_MINOR                          = $00;
    TLS1_VERSION                                = $0301;
    TLS1_VERSION_MAJOR                          = $03;
    TLS1_VERSION_MINOR                          = $01;

    BIO_NOCLOSE                                 = 0;
    BIO_CLOSE                                   = 1;
    SSL_ERROR_NONE                              = 0;
    SSL_ERROR_SSL                               = 1;
    SSL_ERROR_WANT_READ                         = 2;
    SSL_ERROR_WANT_WRITE                        = 3;
    SSL_ERROR_WANT_X509_LOOKUP                  = 4;
    SSL_ERROR_SYSCALL                           = 5;
    SSL_ERROR_ZERO_RETURN                       = 6;
    SSL_ERROR_WANT_CONNECT                      = 7;
    SSL_ERROR_WANT_ACCEPT                       = 8;
    
    X509_FILETYPE_PEM                           = 1;
    X509_FILETYPE_ASN1                          = 2;
    X509_FILETYPE_DEFAULT                       = 3;

    X509_EXT_PACK_UNKNOWN                       = 1;
    X509_EXT_PACK_STRING                        = 2;
    SSL_FILETYPE_ASN1                           = X509_FILETYPE_ASN1;
    SSL_FILETYPE_PEM                            = X509_FILETYPE_PEM;
    SSL_VERIFY_NONE                             = 0;
    SSL_VERIFY_PEER                             = 1;
    SSL_VERIFY_FAIL_IF_NO_PEER_CERT             = 2;
    SSL_VERIFY_CLIENT_ONCE                      = 4;

    { Removed 12/07/05 - due to changes in v0.9.8a
    SSL_CTRL_NEED_TMP_RSA                       = 1;
    SSL_CTRL_SET_TMP_RSA                        = 2;
    SSL_CTRL_SET_TMP_DH                         = 3;
    SSL_CTRL_SET_TMP_RSA_CB                     = 4;
    SSL_CTRL_SET_TMP_DH_CB                      = 5;
    SSL_CTRL_GET_SESSION_REUSED                 = 6;
    SSL_CTRL_GET_CLIENT_CERT_REQUEST            = 7;
    SSL_CTRL_GET_NUM_RENEGOTIATIONS             = 8;
    SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS           = 9;
    SSL_CTRL_GET_TOTAL_RENEGOTIATIONS           = 10;
    SSL_CTRL_GET_FLAGS                          = 11;
    SSL_CTRL_EXTRA_CHAIN_CERT                   = 12;}

    { These constants will be set dynamically in IcsLibeay.Load() } //12/07/05 added
    SSL_CTRL_EXTRA_CHAIN_CERT    : Integer = 12; // v.0.9.7 - Ssl.h SSL_CTRL_EXTRA_CHAIN_CERT;
    SSL_CTRL_GET_SESSION_REUSED  : Integer = 6; // v.0.9.7  - Ssl.h SSL_CTRL_GET_SESSION_REUSED

    // stats
    SSL_CTRL_SESS_NUMBER                        = 20;
    SSL_CTRL_SESS_CONNECT                       = 21;
    SSL_CTRL_SESS_CONNECT_GOOD                  = 22;
    SSL_CTRL_SESS_CONNECT_RENEGOTIATE           = 23;
    SSL_CTRL_SESS_ACCEPT                        = 24;
    SSL_CTRL_SESS_ACCEPT_GOOD                   = 25;
    SSL_CTRL_SESS_ACCEPT_RENEGOTIATE            = 26;
    SSL_CTRL_SESS_HIT                           = 27;
    SSL_CTRL_SESS_CB_HIT                        = 28;
    SSL_CTRL_SESS_MISSES                        = 29;
    SSL_CTRL_SESS_TIMEOUTS                      = 30;
    SSL_CTRL_SESS_CACHE_FULL                    = 31;
    SSL_CTRL_OPTIONS                            = 32;
    SSL_CTRL_MODE                               = 33;
    SSL_CTRL_GET_READ_AHEAD                     = 40;
    SSL_CTRL_SET_READ_AHEAD                     = 41;
    SSL_CTRL_SET_SESS_CACHE_SIZE                = 42;
    SSL_CTRL_GET_SESS_CACHE_SIZE                = 43;
    SSL_CTRL_SET_SESS_CACHE_MODE                = 44;
    SSL_CTRL_GET_SESS_CACHE_MODE                = 45;

    SSL_CTRL_GET_RI_SUPPORT                     = 76; { 0.9.8n }
    SSL_CTRL_CLEAR_OPTIONS                      = 77; { 0.9.8n }

    SSL_OP_MICROSOFT_SESS_ID_BUG                = $00000001;
    SSL_OP_NETSCAPE_CHALLENGE_BUG               = $00000002;
    SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG     = $00000008;
    SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG          = $00000010;
    SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER           = $00000020;
    SSL_OP_MSIE_SSLV2_RSA_PADDING               = $00000040;
    SSL_OP_SSLEAY_080_CLIENT_DH_BUG             = $00000080;
    SSL_OP_TLS_D5_BUG                           = $00000100;
    SSL_OP_TLS_BLOCK_PADDING_BUG                = $00000200;

    // Disable SSL 3.0/TLS 1.0 CBC vulnerability workaround that was added
    // in OpenSSL 0.9.6d.  Usually (depending on the application protocol)
    // the workaround is not needed.  Unfortunately some broken SSL/TLS
    //implementations cannot handle it at all, which is why we include
    //it in SSL_OP_ALL.

    SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS          = $00000800;
    //SSL_OP_ALL: various bug workarounds that should be rather harmless.
    //This used to be 0x000FFFFFL before 0.9.7.
    // 0.9.8h, 0.9.8n, 0.9.8e, 0.9.7g $00000FFF
    SSL_OP_ALL                                  = $00000FFF;
    //SSL_OP_ALL                                  = $80000FFF; 1.0.0d

    //* DTLS options */ since 0.9.8
    SSL_OP_NO_QUERY_MTU                         = $00001000;
    //Turn on Cookie Exchange (on relevant for servers)
    SSL_OP_COOKIE_EXCHANGE                      = $00002000;

    // Don't use RFC4507 ticket extension
    SSL_OP_NO_TICKET                            = $00004000;

    // As server, disallow session resumption on renegotiation
    SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION  = $00010000;
    // Don't use compression even if supported
    SSL_OP_NO_COMPRESSION                          = $00020000; // 1.0.0x
    // Permit unsafe legacy renegotiation { 0.9.8n }
    // which can be set with SSL_CTX_set_options(). This is really
    // not recommended unless you know what you are doing.
    SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION    = $00040000;

    SSL_OP_SINGLE_DH_USE                        = $00100000;
    SSL_OP_EPHEMERAL_RSA                        = $00200000;
    // Set on servers to choose the cipher according to the server's
    // preferences */
    SSL_OP_CIPHER_SERVER_PREFERENCE             = $00400000;
    // If set, a server will allow a client to issue a SSLv3.0 version number
    // as latest version supported in the premaster secret, even when TLSv1.0
    // (version 3.1) was announced in the client hello. Normally this is
    // forbidden to prevent version rollback attacks.
    ////SSL_OP_TLS_ROLLBACK_BUG                 = $00000400;
    SSL_OP_TLS_ROLLBACK_BUG                     = $00800000;

    SSL_OP_NO_SSLv2                             = $01000000;
    SSL_OP_NO_SSLv3                             = $02000000;
    SSL_OP_NO_TLSv1                             = $04000000;
    SSL_OP_PKCS1_CHECK_1                        = $08000000;
    SSL_OP_PKCS1_CHECK_2                        = $10000000;
    SSL_OP_NETSCAPE_CA_DN_BUG                   = $20000000;
    //SSL_OP_NON_EXPORT_FIRST                     = $40000000;
    SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG      = $40000000;
    // Make server add server-hello extension from early version of
    // cryptopro draft, when GOST ciphersuite is negotiated.
    // Required for interoperability with CryptoPro CSP 3.x
    SSL_OP_CRYPTOPRO_TLSEXT_BUG                 = $80000000; // 1.0.0x


    SSL_MODE_ENABLE_PARTIAL_WRITE               = $00000001;

    SSL_SESS_CACHE_OFF                          = $0000;
    SSL_SESS_CACHE_CLIENT                       = $0001;
    SSL_SESS_CACHE_SERVER                       = $0002;
    SSL_SESS_CACHE_BOTH                         = (SSL_SESS_CACHE_CLIENT or SSL_SESS_CACHE_SERVER);
    SSL_SESS_CACHE_NO_AUTO_CLEAR                = $0080;
    //* enough comments already ... see SSL_CTX_set_session_cache_mode(3) */
    SSL_SESS_CACHE_NO_INTERNAL_LOOKUP           = $0100;
    SSL_SESS_CACHE_NO_INTERNAL_STORE            = $0200;
    SSL_SESS_CACHE_NO_INTERNAL                  = (SSL_SESS_CACHE_NO_INTERNAL_LOOKUP or SSL_SESS_CACHE_NO_INTERNAL_STORE);

    SSL_SESSION_CACHE_MAX_SIZE_DEFAULT          = (1024 * 20);

    SSL_ST_CONNECT                              = $1000;
    SSL_ST_ACCEPT                               = $2000;
    SSL_ST_MASK                                 = $0FFF;
    SSL_ST_INIT                                 = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
    SSL_ST_BEFORE                               = $4000;
    SSL_ST_OK                                   = $03;
    SSL_ST_RENEGOTIATE                          = ($04 or SSL_ST_INIT);

    SSL_CB_LOOP                                 = 1;
    SSL_CB_EXIT                                 = 2;
    SSL_CB_READ                                 = 4;
    SSL_CB_WRITE                                = 8;
    SSL_CB_ALERT                                = $4000; // used in callback
    SSL_CB_READ_ALERT                           = (SSL_CB_ALERT or SSL_CB_READ);
    SSL_CB_WRITE_ALERT                          = (SSL_CB_ALERT or SSL_CB_WRITE);
    SSL_CB_ACCEPT_LOOP                          = (SSL_ST_ACCEPT or SSL_CB_LOOP);
    SSL_CB_ACCEPT_EXIT                          = (SSL_ST_ACCEPT or SSL_CB_EXIT);
    SSL_CB_CONNECT_LOOP                         = (SSL_ST_CONNECT or SSL_CB_LOOP);
    SSL_CB_CONNECT_EXIT                         = (SSL_ST_CONNECT or SSL_CB_EXIT);
    SSL_CB_HANDSHAKE_START                      = $10;
    SSL_CB_HANDSHAKE_DONE                       = $20;

    SSL_NOTHING                                 = 1;
    SSL_WRITING                                 = 2;
    SSL_READING                                 = 3;
    SSL_X509_LOOKUP                             = 4;

    // Used in SSL_set_shutdown()/SSL_get_shutdown()
    SSL_SENT_SHUTDOWN                           = 1;
    SSL_RECEIVED_SHUTDOWN                       = 2;

    X509_TRUST_COMPAT                           = 1; //AG
    X509_TRUST_SSL_CLIENT                       = 2; //AG
    X509_TRUST_SSL_SERVER                       = 3; //AG
    X509_TRUST_EMAIL                            = 4; //AG
    X509_TRUST_OBJECT_SIGN                      = 5; //AG
    X509_TRUST_OCSP_SIGN                        = 6; //AG
    X509_TRUST_OCSP_REQUEST                     = 7; //AG

    SSL_MAX_SSL_SESSION_ID_LENGTH               = 32; //AG
    SSL_MAX_SID_CTX_LENGTH                      = 32; //AG

{$IFNDEF OPENSSL_NO_TLSEXT}
    {* ExtensionType values from RFC 3546 *}
    TLSEXT_TYPE_server_name                     = 0;
    TLSEXT_TYPE_max_fragment_length             = 1;
    TLSEXT_TYPE_client_certificate_url          = 2;
    TLSEXT_TYPE_trusted_ca_keys                 = 3;
    TLSEXT_TYPE_truncated_hmac                  = 4;
    TLSEXT_TYPE_status_request                  = 5;
    TLSEXT_TYPE_elliptic_curves                 = 10;
    TLSEXT_TYPE_ec_point_formats                = 11;
    TLSEXT_TYPE_session_ticket                  = 35;

    TLSEXT_MAXLEN_host_name                     = 255;
    TLSEXT_NAMETYPE_host_name                   = 0;

    SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           = 53;
    SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          = 54;
    SSL_CTRL_SET_TLSEXT_HOSTNAME                = 55;
    SSL_CTRL_SET_TLSEXT_DEBUG_CB                = 56;
    SSL_CTRL_SET_TLSEXT_DEBUG_ARG               = 57;

    SSL_TLSEXT_ERR_OK                           = 0;
    SSL_TLSEXT_ERR_ALERT_WARNING                = 1;
    SSL_TLSEXT_ERR_ALERT_FATAL                  = 2;
    SSL_TLSEXT_ERR_NOACK                        = 3;
{$ENDIF}
    
const
    f_SSL_do_handshake :                       function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_renegotiate :                        function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_renegotiate_pending :                function(S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_library_init :                       function: Integer; cdecl = nil;
    f_SSL_load_error_strings :                 procedure; cdecl = nil;
    f_SSLv2_method :                           function: PSSL_METHOD; cdecl = nil;
    f_SSLv2_client_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSLv2_server_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_method :                           function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_client_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSLv3_server_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSLv23_method :                          function: PSSL_METHOD; cdecl = nil;
    f_SSLv23_client_method :                   function: PSSL_METHOD; cdecl = nil;
    f_SSLv23_server_method :                   function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_method :                           function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_client_method :                    function: PSSL_METHOD; cdecl = nil;
    f_TLSv1_server_method :                    function: PSSL_METHOD; cdecl = nil;
    f_SSL_CTX_new :                            function(Meth: PSSL_METHOD): PSSL_CTX; cdecl = nil;
    f_SSL_new :                                function(Ctx: PSSL_CTX): PSSL; cdecl = nil;
    f_SSL_set_bio :                            procedure(S: PSSL; RBio: PBIO; WBio: PBIO); cdecl = nil;
    f_SSL_set_session :                        function(S: PSSL; Session: PSSL_SESSION): Integer; cdecl = nil;
    f_SSL_get_session :                        function(S: PSSL): PSSL_SESSION; cdecl = nil;
    f_SSL_get_rbio :                           function(S: PSSL): PBIO; cdecl = nil;
    f_SSL_get_wbio :                           function(S: PSSL): PBIO; cdecl = nil;
    f_SSL_get1_session :                       function(S: PSSL): PSSL_SESSION; cdecl = nil;
    f_SSL_session_free :                       procedure(Session: PSSL_SESSION); cdecl = nil;
    f_d2i_SSL_SESSION :                        function(Session: PPSSL_SESSION; const pp: PPAnsiChar; Length: Longword): PSSL_SESSION; cdecl = nil;
    f_i2d_SSL_SESSION :                        function(InSession: PSSL_SESSION; pp: PPAnsiChar): Integer; cdecl = nil;
    f_SSL_SESSION_get_time :                   function(const Sess: PSSL_SESSION): Longword; cdecl = nil;
    f_SSL_SESSION_set_time :                   function(Sess: PSSL_SESSION; T: Longword): Longword; cdecl = nil;
    f_SSL_SESSION_get_timeout :                function(const Sess: PSSL_SESSION): Longword; cdecl = nil;
    f_SSL_SESSION_set_timeout :                function(Sess: PSSL_SESSION; T: Longword): Longword; cdecl = nil;
    f_SSL_CTX_set_session_id_context :         function(Ctx: PSSL_CTX; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    f_SSL_CTX_set_timeout :                    function(Ctx: PSSL_CTX; Timeout: Longword): Longword; cdecl = nil;
    f_SSL_CTX_get_cert_store :                 function(const Ctx: PSSL_CTX): PX509_STORE; cdecl = nil; //AG
    f_SSL_set_session_id_context :             function(S: PSSL; const Sid_ctx: PAnsiChar; sid_ctx_len: Integer): Integer; cdecl = nil;
    f_SSL_accept :                             function(S: PSSL): Integer; cdecl = nil;
    f_SSL_connect :                            function(S: PSSL): Integer; cdecl = nil;
    f_SSL_ctrl :                               function(S: PSSL; Cmd: Integer; LArg: LongInt; PArg: Pointer): LongInt; cdecl = nil;
    f_SSL_read :                               function(S: PSSL; Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    f_SSL_want :                               function(S: PSSL): Integer; cdecl = nil;
    f_SSL_write :                              function(S: PSSL; const Buf: Pointer; Num: Integer): Integer; cdecl = nil;
    f_SSL_get_error :                          function(S: PSSL; ErrCode: Integer): Integer; cdecl = nil;
    f_SSL_get_shutdown :                       function(S: PSSL): Integer; cdecl = nil;
    f_SSL_shutdown :                           function(S: PSSL): Integer; cdecl = nil;
    f_SSL_clear :                              procedure(S: PSSL); cdecl = nil;
    f_SSL_free :                               procedure(S: PSSL); cdecl = nil;
    f_SSL_set_ex_data :                        function(S: PSSL; Idx: Integer; Arg: Pointer): Integer; cdecl = nil;
    f_SSL_get_ex_data :                        function(S: PSSL; Idx: Integer): Pointer; cdecl = nil;
    f_SSL_get_peer_certificate :               function(S: PSSL): PX509; cdecl = nil;
    f_SSL_get_peer_cert_chain :                function(const S: PSSL): PSTACK_OF_X509; cdecl = nil;
    f_SSL_get_verify_depth :                   function(const S: PSSL): Integer; cdecl = nil;
    f_SSL_get_verify_result :                  function(S: PSSL): LongInt; cdecl = nil;
    f_SSL_set_verify_result :                  procedure(S: PSSL; VResult: LongInt); cdecl = nil;
    f_SSL_set_info_callback :                  procedure(S: PSSL; cb : TSetInfo_cb); cdecl = nil;
    f_SSL_set_connect_state :                  procedure(S: PSSL); cdecl = nil;
    f_SSL_set_accept_state :                   procedure(S: PSSL); cdecl = nil; //AG
    f_SSL_set_shutdown :                       procedure(S: PSSL; Mode: Integer); cdecl = nil;
    f_SSL_get_version :                        function(S: PSSL): PAnsiChar; cdecl = nil;
    f_SSL_version :                            function(const S: PSSL): Integer; cdecl = nil; //AG
    f_SSL_get_current_cipher :                 function(S: PSSL): Pointer; cdecl = nil;
    f_SSL_state :                              function(S: PSSL): Integer; cdecl = nil;
    f_SSL_state_string_long :                  function(S: PSSL): PAnsiChar; cdecl = nil;
    f_SSL_alert_type_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    f_SSL_alert_desc_string_long :             function(value: Integer): PAnsiChar; cdecl = nil;
    f_SSL_CIPHER_get_bits :                    function(Cipher, Alg_Bits: Pointer): Integer; cdecl = nil;
    f_SSL_CIPHER_get_name :                    function(Cipher: Pointer): PAnsiChar; cdecl = nil;
    f_SSL_CIPHER_description :                 function(Cipher: Pointer; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl = nil;
    f_SSL_CTX_free :                           procedure(C: PSSL_CTX); cdecl = nil;
    f_SSL_CTX_set_info_callback:               procedure(ctx: PSSL_CTX; cb : TSetInfo_cb); cdecl = nil;
    f_SSL_CTX_use_certificate_chain_file :     function(C: PSSL_CTX; const FileName: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_use_certificate_file :           function(C: PSSL_CTX; const FileName: PAnsiChar; type_: Integer): Integer; cdecl = nil; //AG
    f_SSL_CTX_set_default_passwd_cb :          procedure(C: PSSL_CTX; CallBack: TPem_password_cb); cdecl = nil;
    f_SSL_CTX_set_default_passwd_cb_userdata : procedure(C: PSSL_CTX; UData: Pointer); cdecl = nil;
    f_SSL_CTX_use_PrivateKey_file :            function(C: PSSL_CTX; const FileName: PAnsiChar; CertType: Integer): Integer; cdecl = nil;
    f_SSL_CTX_use_PrivateKey :                 function(C: PSSL_CTX; pkey: PEVP_PKEY): Integer; cdecl = nil;
    f_SSL_CTX_load_verify_locations :          function(C: PSSL_CTX; const FileName: PAnsiChar; const SearchPath: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_set_default_verify_paths :       function(C: PSSL_CTX): Integer; cdecl = nil;
    f_SSL_CTX_set_verify :                     procedure(C: PSSL_CTX; Mode: Integer; CallBack : TSetVerify_cb); cdecl = nil;
    f_SSL_set_verify :                         procedure(S: PSSL; Mode: Integer; CallBack : TSetVerify_cb); cdecl = nil;
    f_SSL_CTX_get_verify_mode :                function(const C: PSSL_CTX): Integer; cdecl = nil; //AG
    f_SSL_CTX_get_verify_depth :               function(const ctx: PSSL_CTX): Integer; cdecl = nil; //AG
    f_SSL_CTX_set_verify_depth :               procedure(C: PSSL_CTX; Depth: Integer); cdecl = nil;
    f_SSL_CTX_ctrl :                           function(C: PSSL_CTX; Cmd: Integer; LArg: LongInt; PArg: PAnsiChar): LongInt; cdecl = nil;
    f_SSL_CTX_set_ex_data :                    function(C: PSSL_CTX; Idx: Integer; Arg: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_get_ex_data :                    function(const C: PSSL_CTX; Idx: Integer): PAnsiChar; cdecl = nil;
    f_SSL_CTX_set_cipher_list :                function(C: PSSL_CTX; CipherString: PAnsiChar): Integer; cdecl = nil;
    f_SSL_CTX_set_trust :                      function(C: PSSL_CTX; Trust: Integer): Integer; cdecl = nil; //AG
{$IFNDEF BEFORE_OSSL_098E}
    f_SSL_CTX_set_client_cert_cb:              procedure(CTX: PSSL_CTX; CB: TClient_cert_cb); cdecl = nil; //AG
    f_SSL_CTX_get_client_cert_cb:              function(CTX: PSSL_CTX): TClient_cert_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_set_remove_cb:              procedure(Ctx: PSSL_CTX; CB: TRemove_session_cb); cdecl = nil; //AG
    f_SSL_CTX_sess_get_remove_cb:              function(CTX: PSSL_CTX): TRemove_session_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_set_get_cb:                 procedure(Ctx: PSSL_CTX; CB: TGet_session_cb); cdecl = nil; //AG
    f_SSL_CTX_sess_get_get_cb:                 function(CTX: PSSL_CTX): TGet_session_cb; cdecl = nil; //AG
    f_SSL_CTX_sess_set_new_cb:                 procedure(Ctx: PSSL_CTX; CB: TNew_session_cb); cdecl = nil; //AG
    f_SSL_CTX_sess_get_new_cb:                 function (CTX: PSSL_CTX): TNew_session_cb; cdecl = nil; //AG
    f_SSL_SESSION_get_id:                      function (const Ses: PSSL_SESSION; var Len: LongInt): PAnsiChar; cdecl = nil; //AG
{$ENDIF}
    { The next four functions are only useful for TLS/SSL servers. }
    f_SSL_CTX_add_client_CA :                  function(C: PSSL_CTX; CaCert: PX509): Integer; cdecl = nil; //AG
    f_SSL_add_client_CA :                      function(ssl: PSSL; CaCert: PX509): Integer; cdecl = nil; //AG
    f_SSL_CTX_set_client_CA_list :             procedure(C: PSSL_CTX; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG
    f_SSL_set_client_CA_list :                 procedure(s: PSSL; List: PSTACK_OF_X509_NAME); cdecl = nil; //AG

{$IFNDEF OPENSSL_NO_ENGINE}
    f_SSL_CTX_set_client_cert_engine :         function(Ctx: PSSL_CTX; e: PENGINE): Integer; cdecl = nil; //AG
{$ENDIF}
    f_SSL_load_client_CA_file :                function(const FileName: PAnsiChar): PSTACK_OF_X509_NAME; cdecl = nil; //AG

    f_SSL_get_ex_data_X509_STORE_CTX_idx:      function: Integer; cdecl = nil;
    f_BIO_f_ssl :                              function : PBIO_METHOD; cdecl = nil;
    f_SSL_set_fd:                              function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_set_rfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_set_wfd:                             function(S: PSSL; fd: Integer): Integer; cdecl = nil; // B.S.
    f_SSL_get_fd:                              function(S: PSSL): Integer; cdecl = nil; // B.S.
    f_SSL_get_rfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.
    f_SSL_get_wfd:                             function(S: PSSL): Integer; cdecl = nil; // B.S.

    f_SSL_get_SSL_CTX:                         function(const S: PSSL): PSSL_CTX; cdecl = nil;
    f_SSL_get_client_CA_list :                 function(const S: PSSL): PSTACK_OF_X509_NAME; cdecl = nil;
{$IFNDEF OPENSSL_NO_TLSEXT}
    f_SSL_set_SSL_CTX:                         function(S: PSSL; ctx: PSSL_CTX): PSSL_CTX; cdecl = nil;
    f_SSL_get_servername:                      function(const S: PSSL; const type_: Integer): PAnsiChar; cdecl = nil;
    f_SSL_get_servername_type:                 function(const S: PSSL): Integer; cdecl = nil;
    f_SSL_CTX_callback_ctrl:                   function(ctx: PSSL_CTX; cb_id: Integer; fp: TCallback_ctrl_fp): Longint; cdecl = nil;
    f_SSL_callback_ctrl:                       function(s: PSSL; cb_id: Integer; fp: TCallback_ctrl_fp): Longint; cdecl = nil;
{$ENDIF}

function Load : Boolean;
function WhichFailedToLoad : String;
{$IFDEF MSWINDOWS}
function GetFileVerInfo(
    const AppName         : String;
    out   FileVersion     : String;
    out   FileDescription : String): Boolean;
{$ENDIF}
function  f_SSL_CTX_set_options(C: PSSL_CTX; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_get_options(C: PSSL_CTX): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_get_options(S: PSSL): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_set_options(S: PSSL; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_clear_options(S: PSSL; Op: LongInt): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_read(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_write(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_nothing(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_want_x509_lookup(S: PSSL) : Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_session_reused(SSL: PSSL): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  f_SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Longword; {$IFDEF USE_INLINE} inline; {$ENDIF}

{$IFDEF BEFORE_OSSL_098E}
//procedure f_SSL_session_get_id(Ses: PSSL_SESSION; var SessID: Pointer; var IdLen: Integer);
function  f_SSL_SESSION_get_id(const Ses: PSSL_SESSION; var IdLen: LongInt): PAnsiChar;
procedure f_SSL_CTX_sess_set_new_cb(Ctx: PSSL_CTX; CB: TNew_session_cb);
procedure f_SSL_CTX_sess_set_get_cb(Ctx: PSSL_CTX; CB: TGet_session_cb);
procedure f_SSL_CTX_sess_set_remove_cb(Ctx: PSSL_CTX; CB: TRemove_session_cb);
procedure f_SSL_CTX_set_client_cert_cb(Ctx: PSSL_CTX; CB: TClient_cert_cb);
function  f_SSL_CTX_get_client_cert_cb(CTX: PSSL_CTX): Pointer;
{$ENDIF}

{$IFNDEF OPENSSL_NO_TLSEXT}
function f_SSL_set_tlsext_host_name(const S: PSSL; const name: String): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX; cb: TCallback_ctrl_fp): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): LongInt; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_set_tlsext_debug_callback(S: PSSL; cb: TCallback_ctrl_fp): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}
function f_SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Longint; {$IFDEF USE_INLINE} inline; {$ENDIF}
{$ENDIF}

const
    GSSLEAY_DLL_Handle          : THandle = 0;
    GSSLEAY_DLL_Name            : String  = {$IFDEF MACOS} '/usr/lib/libssl.dylib'; {$ELSE} 'SSLEAY32.DLL'; {$ENDIF}
    GSSLEAY_DLL_FileName        : String  = '*NOT_LOADED*';
    GSSLEAY_DLL_FileVersion     : String = '';
    GSSLEAY_DLL_FileDescription : String = '';

{$ENDIF} // USE_SSL

implementation

{$IFDEF USE_SSL}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function GetFileVerInfo(
    const AppName         : String;
    out   FileVersion     : String;
    out   FileDescription : String): Boolean;
const
    DEFAULT_LANG_ID       = $0409;
    DEFAULT_CHAR_SET_ID   = $04E4;
type
    TTranslationPair = packed record
        Lang, CharSet: WORD;
    end;
    PTranslationIDList = ^TTranslationIDList;
    TTranslationIDList = array[0..MAXINT div SizeOf(TTranslationPair) - 1]
                             of TTranslationPair;
var
    Buffer, PStr    : PChar;
    BufSize         : DWORD;
    StrSize, IDsLen : DWORD;
    Status          : Boolean;
    LangCharSet     : String;
    IDs             : PTranslationIDList;
begin
    Result          := FALSE;
    FileVersion     := '';
    FileDescription := '';
    BufSize         := GetFileVersionInfoSize(PChar(AppName), StrSize);
    if BufSize = 0 then
        Exit;
    GetMem(Buffer, BufSize);
    try
        // get all version info into Buffer
        Status := GetFileVersionInfo(PChar(AppName), 0, BufSize, Buffer);
        if not Status then
            Exit;
        // set language Id
        LangCharSet := '040904E4';
        if VerQueryValue(Buffer, PChar('\VarFileInfo\Translation'),
                         Pointer(IDs), IDsLen) then begin
            if IDs^[0].Lang = 0 then
                IDs^[0].Lang := DEFAULT_LANG_ID;
            if IDs^[0].CharSet = 0 then
                IDs^[0].CharSet := DEFAULT_CHAR_SET_ID;
            LangCharSet := Format('%.4x%.4x',
                                  [IDs^[0].Lang, IDs^[0].CharSet]);
        end;

        // now read real information
        Status := VerQueryValue(Buffer, PChar('\StringFileInfo\' +
                                LangCharSet + '\FileVersion'),
                                Pointer(PStr), StrSize);
        if Status then begin
            FileVersion := StrPas(PStr);
            Result      := TRUE;
        end;
        Status := VerQueryValue(Buffer, PChar('\StringFileInfo\' +
                                LangCharSet + '\FileDescription'),
                                Pointer(PStr), StrSize);
        if Status then
            FileDescription := StrPas(PStr);
    finally
        FreeMem(Buffer);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}

function Load : Boolean;
var
    ErrCode : Integer;
begin
    if GSSLEAY_DLL_Handle <> 0 then begin
        Result := TRUE;
        Exit;                                 // Already loaded
    end;
  {$IFDEF MSWINDOWS}
    GetFileVerInfo(GSSLEAY_DLL_Name,
                   GSSLEAY_DLL_FileVersion,
                   GSSLEAY_DLL_FileDescription);
  {$ENDIF}
    GSSLEAY_DLL_Handle := LoadLibrary(PChar(GSSLEAY_DLL_Name));
    if GSSLEAY_DLL_Handle = 0 then begin
        ErrCode            := GetLastError;
        GSSLEAY_DLL_Handle := 0;
        if ErrCode = {$IFDEF POSIX} ENOENT {$ELSE} ERROR_MOD_NOT_FOUND {$ENDIF} then
            raise EIcsSsleayException.Create('File not found: ' +
                                             GSSLEAY_DLL_Name)
        else
            raise EIcsSsleayException.Create('Unable to load ' +
                                             GSSLEAY_DLL_Name  +
                                             '. Win32 error #' +
                                             IntToStr(ErrCode));
    end;
    SetLength(GSSLEAY_DLL_FileName, 256);
    SetLength(GSSLEAY_DLL_FileName, GetModuleFileName(GSSLEAY_DLL_Handle,
                 PChar(GSSLEAY_DLL_FileName), Length(GSSLEAY_DLL_FileName)));
    f_SSL_do_handshake                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_do_handshake');
    f_SSL_renegotiate                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_renegotiate');
    f_SSL_renegotiate_pending                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_renegotiate_pending');
    f_SSL_library_init                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_library_init');
    f_SSL_load_error_strings                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_load_error_strings');
    f_SSLv2_method                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv2_method');
    f_SSLv2_client_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv2_client_method');
    f_SSLv2_server_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv2_server_method');
    f_SSLv3_method                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv3_method');
    f_SSLv3_client_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv3_client_method');
    f_SSLv3_server_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv3_server_method');
    f_SSLv23_method                          := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv23_method');
    f_SSLv23_client_method                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv23_client_method');
    f_SSLv23_server_method                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSLv23_server_method');
    f_TLSv1_method                           := GetProcAddress(GSSLEAY_DLL_Handle, 'TLSv1_method');
    f_TLSv1_client_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'TLSv1_client_method');
    f_TLSv1_server_method                    := GetProcAddress(GSSLEAY_DLL_Handle, 'TLSv1_server_method');
    f_SSL_CTX_new                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_new');
    f_SSL_new                                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_new');
    f_SSL_set_bio                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_bio');
    f_SSL_set_session                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_session');
    f_SSL_get_session                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_session');
    f_SSL_get_rbio                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_rbio');
    f_SSL_get_wbio                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_wbio');
    f_SSL_get1_session                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get1_session');
    f_SSL_SESSION_free                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_free');
    f_SSL_SESSION_set_timeout                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_set_timeout');
    f_SSL_SESSION_get_timeout                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_get_timeout');
    f_SSL_SESSION_set_time                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_set_time');
    f_SSL_SESSION_get_time                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_get_time');
    f_d2i_SSL_SESSION                        := GetProcAddress(GSSLEAY_DLL_Handle, 'd2i_SSL_SESSION');
    f_i2d_SSL_SESSION                        := GetProcAddress(GSSLEAY_DLL_Handle, 'i2d_SSL_SESSION');
    f_SSL_CTX_set_session_id_context         := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_session_id_context');
    f_SSL_set_session_id_context             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_session_id_context');
    f_SSL_accept                             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_accept');
    f_SSL_connect                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_connect');
    f_SSL_ctrl                               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_ctrl');
    f_SSL_read                               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_read');
    f_SSL_want                               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_want');
    f_SSL_write                              := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_write');
    f_SSL_get_error                          := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_error');
    f_SSL_get_shutdown                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_shutdown');
    f_SSL_shutdown                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_shutdown');
    f_SSL_clear                              := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_clear');
    f_SSL_free                               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_free');
    f_SSL_set_ex_data                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_ex_data');
    f_SSL_get_ex_data                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_ex_data');
    f_SSL_get_peer_certificate               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_peer_certificate');
    f_SSL_get_peer_cert_chain                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_peer_cert_chain');
    f_SSL_get_verify_depth                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_verify_depth');
    f_SSL_get_verify_result                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_verify_result');
    f_SSL_set_verify_result                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_verify_result');
    f_SSL_set_info_callback                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_info_callback');
    f_SSL_set_connect_state                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_connect_state');
    f_SSL_set_shutdown                       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_shutdown');
    f_SSL_set_accept_state                   := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_accept_state'); //AG
    f_SSL_get_version                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_version');
    f_SSL_version                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_version'); //AG
    f_SSL_get_current_cipher                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_current_cipher');
    f_SSL_state                              := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_state');
    f_SSL_state_string_long                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_state_string_long');
    f_SSL_alert_type_string_long             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_alert_type_string_long');
    f_SSL_alert_desc_string_long             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_alert_desc_string_long');
    f_SSL_CIPHER_get_bits                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CIPHER_get_bits');
    f_SSL_CIPHER_get_name                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CIPHER_get_name');
    f_SSL_CIPHER_description                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CIPHER_description');
    f_SSL_CTX_free                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_free');
    f_SSL_CTX_set_info_callback              := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_info_callback');
    f_SSL_CTX_set_timeout                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_timeout');
    f_SSL_CTX_use_certificate_chain_file     := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_use_certificate_chain_file');
    f_SSL_CTX_use_certificate_file           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_use_certificate_file');
    f_SSL_CTX_set_default_passwd_cb          := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_default_passwd_cb');
    f_SSL_CTX_set_default_passwd_cb_userdata := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_default_passwd_cb_userdata');
    f_SSL_CTX_use_PrivateKey_file            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_use_PrivateKey_file');
    f_SSL_CTX_use_PrivateKey                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_use_PrivateKey');
    f_SSL_CTX_load_verify_locations          := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_load_verify_locations');
    f_SSL_CTX_set_default_verify_paths       := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_default_verify_paths');
    f_SSL_CTX_set_verify                     := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_verify');
    f_SSL_set_verify                         := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_verify');
    f_SSL_CTX_get_verify_mode                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_get_verify_mode');
    f_SSL_CTX_get_verify_depth               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_get_verify_depth');
    f_SSL_CTX_set_verify_depth               := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_verify_depth');
    f_SSL_CTX_ctrl                           := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_ctrl');
    f_SSL_CTX_set_cipher_list                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_cipher_list');
    f_SSL_CTX_set_ex_data                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_ex_data');
    f_SSL_CTX_get_ex_data                    := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_get_ex_data');
    f_SSL_CTX_get_cert_store                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_get_cert_store');
    f_SSL_CTX_set_trust                      := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_trust');
{$IFNDEF BEFORE_OSSL_098E}
    f_SSL_CTX_set_client_cert_cb             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_client_cert_cb'); //AG
    f_SSL_CTX_get_client_cert_cb             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_get_client_cert_cb'); //AG
    f_SSL_CTX_sess_set_remove_cb             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_set_remove_cb'); //AG
    f_SSL_CTX_sess_get_remove_cb             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_get_remove_cb'); //AG
    f_SSL_CTX_sess_set_get_cb                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_set_get_cb'); //AG
    f_SSL_CTX_sess_get_get_cb                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_get_get_cb'); //AG
    f_SSL_CTX_sess_set_new_cb                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_set_new_cb'); //AG
    f_SSL_CTX_sess_get_new_cb                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_sess_get_new_cb'); //AG
    f_SSL_SESSION_get_id                     := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_SESSION_get_id'); //AG
{$ENDIF}
    f_SSL_CTX_add_client_CA                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_add_client_CA'); //AG
    f_SSL_add_client_CA                      := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_add_client_CA'); //AG
    f_SSL_CTX_set_client_CA_list             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_client_CA_list'); //AG
    f_SSL_set_client_CA_list                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_client_CA_list'); //AG
{$IFNDEF OPENSSL_NO_ENGINE}
    f_SSL_CTX_set_client_cert_engine         := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_set_client_cert_engine'); //AG
{$ENDIF}    
    f_SSL_load_client_CA_file                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_load_client_CA_file'); //AG

    f_SSL_get_ex_data_X509_STORE_CTX_idx     := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_ex_data_X509_STORE_CTX_idx');
    f_BIO_f_ssl                              := GetProcAddress(GSSLEAY_DLL_Handle, 'BIO_f_ssl');
    f_SSL_set_fd                             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_fd'); // B.S.
    f_SSL_set_rfd                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_rfd'); // B.S.
    f_SSL_set_wfd                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_wfd'); // B.S.
    f_SSL_get_fd                             := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_fd'); // B.S.
    f_SSL_get_rfd                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_rfd'); // B.S.
    f_SSL_get_wfd                            := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_wfd'); // B.S.

    f_SSL_get_SSL_CTX                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_SSL_CTX'); //AG
    f_SSL_get_client_CA_list                 := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_client_CA_list'); //AG
{$IFNDEF OPENSSL_NO_TLSEXT}
    f_SSL_set_SSL_CTX                        := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_set_SSL_CTX'); //AG
    f_SSL_get_servername                     := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_servername'); //AG
    f_SSL_get_servername_type                := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_get_servername_type'); //AG
    f_SSL_CTX_callback_ctrl                  := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_CTX_callback_ctrl'); //AG
    f_SSL_callback_ctrl                      := GetProcAddress(GSSLEAY_DLL_Handle, 'SSL_callback_ctrl'); //AG
{$ENDIF}

    // Check if any failed
    Result := not ((@f_SSL_do_handshake                       = nil) or
                   (@f_SSL_renegotiate                        = nil) or
                   (@f_SSL_renegotiate_pending                = nil) or
                   (@f_SSL_library_init                       = nil) or
                   (@f_SSL_load_error_strings                 = nil) or
                   (@f_SSLv2_method                           = nil) or
                   (@f_SSLv2_client_method                    = nil) or
                   (@f_SSLv2_server_method                    = nil) or
                   (@f_SSLv3_method                           = nil) or
                   (@f_SSLv3_client_method                    = nil) or
                   (@f_SSLv3_server_method                    = nil) or
                   (@f_SSLv23_method                          = nil) or
                   (@f_SSLv23_client_method                   = nil) or
                   (@f_SSLv23_server_method                   = nil) or
                   (@f_TLSv1_method                           = nil) or
                   (@f_TLSv1_client_method                    = nil) or
                   (@f_TLSv1_server_method                    = nil) or
                   (@f_SSL_CTX_new                            = nil) or
                   (@f_SSL_new                                = nil) or
                   (@f_SSL_set_bio                            = nil) or
                   (@f_SSL_set_session                        = nil) or
                   (@f_SSL_get_session                        = nil) or
                   (@f_SSL_get_rbio                           = nil) or
                   (@f_SSL_get_wbio                           = nil) or
                   (@f_SSL_get1_session                       = nil) or
                   (@f_SSL_SESSION_free                       = nil) or
                   (@f_SSL_SESSION_set_timeout                = nil) or
                   (@f_SSL_SESSION_get_timeout                = nil) or
                   (@f_SSL_SESSION_set_time                   = nil) or
                   (@f_SSL_SESSION_get_time                   = nil) or
                   (@f_d2i_SSL_SESSION                        = nil) or
                   (@f_i2d_SSL_SESSION                        = nil) or
                   (@f_SSL_CTX_set_session_id_context         = nil) or
                   (@f_SSL_set_session_id_context             = nil) or
                   (@f_SSL_accept                             = nil) or
                   (@f_SSL_connect                            = nil) or
                   (@f_SSL_ctrl                               = nil) or
                   (@f_SSL_read                               = nil) or
                   (@f_SSL_want                               = nil) or
                   (@f_SSL_write                              = nil) or
                   (@f_SSL_get_error                          = nil) or
                   (@f_SSL_get_shutdown                       = nil) or
                   (@f_SSL_shutdown                           = nil) or
                   (@f_SSL_clear                              = nil) or
                   (@f_SSL_free                               = nil) or
                   (@f_SSL_set_ex_data                        = nil) or
                   (@f_SSL_get_ex_data                        = nil) or
                   (@f_SSL_get_peer_certificate               = nil) or
                   (@f_SSL_get_peer_cert_chain                = nil) or
                   (@f_SSL_get_verify_depth                   = nil) or
                   (@f_SSL_get_verify_result                  = nil) or
                   (@f_SSL_set_verify_result                  = nil) or
                   (@f_SSL_set_info_callback                  = nil) or
                   (@f_SSL_set_connect_state                  = nil) or
                   (@f_SSL_set_shutdown                       = nil) or
                   (@f_SSL_set_accept_state                   = nil) or //AG
                   (@f_SSL_get_version                        = nil) or
                   (@f_SSL_version                            = nil) or
                   (@f_SSL_get_current_cipher                 = nil) or
                   (@f_SSL_state                              = nil) or
                   (@f_SSL_state_string_long                  = nil) or
                   (@f_SSL_alert_type_string_long             = nil) or
                   (@f_SSL_alert_desc_string_long             = nil) or
                   (@f_SSL_CIPHER_get_bits                    = nil) or
                   (@f_SSL_CIPHER_get_name                    = nil) or
                   (@f_SSL_CIPHER_description                 = nil) or
                   (@f_SSL_CTX_free                           = nil) or
                   (@f_SSL_CTX_set_info_callback              = nil) or
                   (@f_SSL_CTX_set_timeout                    = nil) or
                   (@f_SSL_CTX_use_certificate_chain_file     = nil) or
                   (@f_SSL_CTX_use_certificate_file           = nil) or
                   (@f_SSL_CTX_set_default_passwd_cb          = nil) or
                   (@f_SSL_CTX_set_default_passwd_cb_userdata = nil) or
                   (@f_SSL_CTX_use_PrivateKey_file            = nil) or
                   (@f_SSL_CTX_load_verify_locations          = nil) or
                   (@f_SSL_CTX_set_default_verify_paths       = nil) or
                   (@f_SSL_CTX_set_verify                     = nil) or
                   (@f_SSL_set_verify                         = nil) or
                   (@f_SSL_CTX_get_verify_mode                = nil) or
                   (@f_SSL_CTX_ctrl                           = nil) or
                   (@f_SSL_CTX_set_cipher_list                = nil) or
                   (@f_SSL_CTX_get_verify_depth               = nil) or
                   (@f_SSL_CTX_set_verify_depth               = nil) or
                   (@f_SSL_CTX_get_ex_data                    = nil) or
                   (@f_SSL_CTX_set_ex_data                    = nil) or
                   (@f_SSL_CTX_get_cert_store                 = nil) or
                   (@f_SSL_CTX_set_trust                      = nil) or
              {$IFNDEF BEFORE_OSSL_098E}
                   (@f_SSL_CTX_set_client_cert_cb             = nil) or
                   (@f_SSL_CTX_get_client_cert_cb             = nil) or
                   (@f_SSL_CTX_sess_set_remove_cb             = nil) or
                   (@f_SSL_CTX_sess_get_remove_cb             = nil) or
                   (@f_SSL_CTX_sess_set_get_cb                = nil) or
                   (@f_SSL_CTX_sess_get_get_cb                = nil) or
                   (@f_SSL_CTX_sess_set_new_cb                = nil) or
                   (@f_SSL_CTX_sess_get_new_cb                = nil) or
                   (@f_SSL_SESSION_get_id                     = nil) or
              {$ENDIF}
                   (@f_SSL_CTX_add_client_CA                  = nil) or
                   (@f_SSL_add_client_CA                      = nil) or
                   (@f_SSL_CTX_set_client_CA_list             = nil) or
                   (@f_SSL_set_client_CA_list                 = nil) or
              {$IFNDEF OPENSSL_NO_ENGINE}
                   (@f_SSL_CTX_set_client_cert_engine         = nil) or
              {$ENDIF}     
                   (@f_SSL_load_client_CA_file                = nil) or

                   (@f_SSL_get_ex_data_X509_STORE_CTX_idx     = nil) or
                   (@f_BIO_f_ssl                              = nil) or
                   (@f_SSL_set_fd                             = nil) or
                   (@f_SSL_set_rfd                            = nil) or
                   (@f_SSL_set_wfd                            = nil) or
                   (@f_SSL_get_fd                             = nil) or
                   (@f_SSL_get_rfd                            = nil) or
                   (@f_SSL_get_wfd                            = nil) or
                   (@f_SSL_CTX_use_PrivateKey                 = nil) or
                   (@f_SSL_get_SSL_CTX                        = nil) or

                   (@f_SSL_get_client_CA_list                 = nil)
              {$IFNDEF OPENSSL_NO_TLSEXT}
                                                                     or

                   (@f_SSL_set_SSL_CTX                        = nil) or
                   (@f_SSL_get_servername                     = nil) or
                   (@f_SSL_get_servername_type                = nil) or
                   (@f_SSL_CTX_callback_ctrl                  = nil) or
                   (@f_SSL_callback_ctrl                      = nil)
              {$ENDIF}
                   );
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WhichFailedToLoad : String;
begin
    Result := '';
    if @f_SSL_do_handshake                       = nil then Result := Result + ' SSL_do_handshake';
    if @f_SSL_renegotiate                        = nil then Result := Result + ' SSL_renegotiate';
    if @f_SSL_renegotiate_pending                = nil then Result := Result + ' SSL_renegotiate_pending';
    if @f_SSL_library_init                       = nil then Result := Result + ' SSL_library_init';
    if @f_SSL_load_error_strings                 = nil then Result := Result + ' SSL_load_error_strings';
    if @f_SSLv2_method                           = nil then Result := Result + ' SSLv2_method';
    if @f_SSLv2_client_method                    = nil then Result := Result + ' SSLv2_client_method';
    if @f_SSLv2_server_method                    = nil then Result := Result + ' SSLv2_server_method';
    if @f_SSLv3_method                           = nil then Result := Result + ' SSLv3_method';
    if @f_SSLv3_client_method                    = nil then Result := Result + ' SSLv3_client_method';
    if @f_SSLv3_server_method                    = nil then Result := Result + ' SSLv3_server_method';
    if @f_SSLv23_method                          = nil then Result := Result + ' SSLv23_method';
    if @f_SSLv23_client_method                   = nil then Result := Result + ' SSLv23_client_method';
    if @f_SSLv23_server_method                   = nil then Result := Result + ' SSLv23_server_method';
    if @f_TLSv1_method                           = nil then Result := Result + ' TLSv1_method';
    if @f_TLSv1_client_method                    = nil then Result := Result + ' TLSv1_client_method';
    if @f_TLSv1_server_method                    = nil then Result := Result + ' TLSv1_server_method';
    if @f_SSL_CTX_new                            = nil then Result := Result + ' SSL_CTX_new';
    if @f_SSL_new                                = nil then Result := Result + ' SSL_new';
    if @f_SSL_set_bio                            = nil then Result := Result + ' SSL_set_bio';
    if @f_SSL_set_session                        = nil then Result := Result + ' SSL_set_session';
    if @f_SSL_get_session                        = nil then Result := Result + ' SSL_get_session';
    if @f_SSL_get_rbio                           = nil then Result := Result + ' SSL_get_rbio';
    if @f_SSL_get_wbio                           = nil then Result := Result + ' SSL_get_wbio';
    if @f_SSL_get1_session                       = nil then Result := Result + ' SSL_get1_session';
    if @f_SSL_SESSION_free                       = nil then Result := Result + ' SSL_SESSION_free';
    if @f_SSL_SESSION_set_timeout                = nil then Result := Result + ' SSL_SESSION_set_timeout';
    if @f_SSL_SESSION_get_timeout                = nil then Result := Result + ' SSL_SESSION_get_timeout';
    if @f_SSL_SESSION_set_time                   = nil then Result := Result + ' SSL_SESSION_set_time';
    if @f_SSL_SESSION_get_time                   = nil then Result := Result + ' SSL_SESSION_get_time';
    if @f_d2i_SSL_SESSION                        = nil then Result := Result + ' d2i_SSL_SESSION';
    if @f_i2d_SSL_SESSION                        = nil then Result := Result + ' i2d_SSL_SESSION';
    if @f_SSL_CTX_set_session_id_context         = nil then Result := Result + ' SSL_CTX_set_session_id_context';
    if @f_SSL_set_session_id_context             = nil then Result := Result + ' SSL_set_session_id_context';
    if @f_SSL_accept                             = nil then Result := Result + ' SSL_accept';
    if @f_SSL_connect                            = nil then Result := Result + ' SSL_connect';
    if @f_SSL_ctrl                               = nil then Result := Result + ' SSL_ctrl';
    if @f_SSL_read                               = nil then Result := Result + ' SSL_read';
    if @f_SSL_want                               = nil then Result := Result + ' SSL_want';
    if @f_SSL_write                              = nil then Result := Result + ' SSL_write';
    if @f_SSL_get_error                          = nil then Result := Result + ' SSL_get_error';
    if @f_SSL_get_shutdown                       = nil then Result := Result + ' SSL_get_shutdown';
    if @f_SSL_shutdown                           = nil then Result := Result + ' SSL_shutdown';
    if @f_SSL_clear                              = nil then Result := Result + ' SSL_clear';
    if @f_SSL_free                               = nil then Result := Result + ' SSL_free';
    if @f_SSL_set_ex_data                        = nil then Result := Result + ' SSL_set_ex_data';
    if @f_SSL_get_ex_data                        = nil then Result := Result + ' SSL_get_ex_data';
    if @f_SSL_get_peer_certificate               = nil then Result := Result + ' SSL_get_peer_certificate';
    if @f_SSL_get_peer_cert_chain                = nil then Result := Result + ' SSL_get_peer_cert_chain';
    if @f_SSL_get_verify_depth                   = nil then Result := Result + ' SSL_get_verify_depth';
    if @f_SSL_get_verify_result                  = nil then Result := Result + ' SSL_get_verify_result';
    if @f_SSL_set_verify_result                  = nil then Result := Result + ' SSL_set_verify_result';
    if @f_SSL_set_info_callback                  = nil then Result := Result + ' SSL_set_info_callback';
    if @f_SSL_set_connect_state                  = nil then Result := Result + ' SSL_set_connect_state';
    if @f_SSL_set_shutdown                       = nil then Result := Result + ' SSL_set_shutdown';
    if @f_SSL_set_accept_state                   = nil then Result := Result + ' SSL_set_accept_state';//AG
    if @f_SSL_set_bio                            = nil then Result := Result + ' SSL_set_bio';
    if @f_SSL_get_version                        = nil then Result := Result + ' SSL_get_version';
    if @f_SSL_version                            = nil then Result := Result + ' SSL_version';
    if @f_SSL_get_current_cipher                 = nil then Result := Result + ' SSL_get_current_cipher';
    if @f_SSL_state                              = nil then Result := Result + ' SSL_state';
    if @f_SSL_state_string_long                  = nil then Result := Result + ' SSL_state_string_long';
    if @f_SSL_alert_type_string_long             = nil then Result := Result + ' SSL_alert_type_string_long';
    if @f_SSL_alert_desc_string_long             = nil then Result := Result + ' SSL_alert_desc_string_long';
    if @f_SSL_CIPHER_get_bits                    = nil then Result := Result + ' SSL_CIPHER_get_bits';
    if @f_SSL_CIPHER_get_name                    = nil then Result := Result + ' SSL_CIPHER_get_name';
    if @f_SSL_CIPHER_description                 = nil then Result := Result + ' SSL_CIPHER_description';
    if @f_SSL_CTX_free                           = nil then Result := Result + ' SSL_CTX_free';
    if @f_SSL_CTX_set_info_callback              = nil then Result := Result + ' SSL_CTX_set_info_callback';
    if @f_SSL_CTX_set_timeout                    = nil then Result := Result + ' SSL_CTX_set_timeout';
    if @f_SSL_CTX_use_certificate_chain_file     = nil then Result := Result + ' SSL_CTX_use_certificate_chain_file';
    if @f_SSL_CTX_use_certificate_file           = nil then Result := Result + ' SSL_CTX_use_certificate_file';
    if @f_SSL_CTX_use_PrivateKey                 = nil then Result := Result + ' SSL_CTX_use_certificate';
    if @f_SSL_CTX_set_default_passwd_cb          = nil then Result := Result + ' SSL_CTX_set_default_passwd_cb';
    if @f_SSL_CTX_set_default_passwd_cb_userdata = nil then Result := Result + ' SSL_CTX_set_default_passwd_cb_userdata';
    if @f_SSL_CTX_use_PrivateKey_file            = nil then Result := Result + ' SSL_CTX_use_PrivateKey_file';
    if @f_SSL_CTX_load_verify_locations          = nil then Result := Result + ' SSL_CTX_load_verify_locations';
    if @f_SSL_CTX_set_default_verify_paths       = nil then Result := Result + ' SSL_CTX_set_default_verify_paths';
    if @f_SSL_CTX_set_verify                     = nil then Result := Result + ' SSL_CTX_set_verify';
    if @f_SSL_set_verify                         = nil then Result := Result + ' SSL_set_verify';
    if @f_SSL_CTX_get_verify_mode                = nil then Result := Result + ' SSL_CTX_get_verify_mode';
    if @f_SSL_CTX_ctrl                           = nil then Result := Result + ' SSL_CTX_ctrl';
    if @f_SSL_CTX_set_cipher_list                = nil then Result := Result + ' SSL_CTX_set_cipher_list';
    if @f_SSL_CTX_get_verify_depth               = nil then Result := Result + ' SSL_CTX_get_verify_depth';
    if @f_SSL_CTX_set_verify_depth               = nil then Result := Result + ' SSL_CTX_set_verify_depth';
    if @f_SSL_CTX_get_ex_data                    = nil then Result := Result + ' SSL_CTX_get_ex_data';
    if @f_SSL_CTX_set_ex_data                    = nil then Result := Result + ' SSL_CTX_set_ex_data';
    if @f_SSL_CTX_get_cert_store                 = nil then Result := Result + ' SSL_CTX_get_cert_store';
    if @f_SSL_CTX_set_trust                      = nil then Result := Result + ' SSL_CTX_set_trust';
{$IFNDEF BEFORE_OSSL_098E}
    if @f_SSL_CTX_set_client_cert_cb             = nil then Result := Result + ' SSL_CTX_set_client_cert_cb';
    if @f_SSL_CTX_get_client_cert_cb             = nil then Result := Result + ' SSL_CTX_get_client_cert_cb';
    if @f_SSL_CTX_sess_set_remove_cb             = nil then Result := Result + ' SSL_CTX_sess_set_remove_cb';
    if @f_SSL_CTX_sess_get_remove_cb             = nil then Result := Result + ' SSL_CTX_sess_get_remove_cb';
    if @f_SSL_CTX_sess_set_get_cb                = nil then Result := Result + ' SSL_CTX_sess_set_get_cb';
    if @f_SSL_CTX_sess_get_get_cb                = nil then Result := Result + ' SSL_CTX_sess_get_get_cb';
    if @f_SSL_CTX_sess_set_new_cb                = nil then Result := Result + ' SSL_CTX_sess_set_new_cb';
    if @f_SSL_CTX_sess_get_new_cb                = nil then Result := Result + ' SSL_CTX_sess_get_new_cb';
    if @f_SSL_SESSION_get_id                     = nil then Result := Result + ' SSL_SESSION_get_id';
{$ENDIF}
    if @f_SSL_CTX_add_client_CA                  = nil then Result := Result + ' SSL_CTX_add_client_CA';
    if @f_SSL_add_client_CA                      = nil then Result := Result + ' SSL_add_client_CA';
    if @f_SSL_CTX_set_client_CA_list             = nil then Result := Result + ' SSL_CTX_set_client_CA_list';
    if @f_SSL_set_client_CA_list                 = nil then Result := Result + ' SSL_set_client_CA_list';
{$IFNDEF OPENSSL_NO_ENGINE}
    if @f_SSL_CTX_set_client_cert_engine         = nil then Result := Result + ' SSL_CTX_set_client_cert_engine';
{$ENDIF}
    if @f_SSL_load_client_CA_file                = nil then Result := Result + ' SSL_load_client_CA_file';

    if @f_SSL_get_ex_data_X509_STORE_CTX_idx     = nil then Result := Result + ' SSL_get_ex_data_X509_STORE_CTX_idx';
    if @f_BIO_f_ssl                              = nil then Result := Result + ' BIO_f_ssl';
    if @f_SSL_set_fd                             = nil then Result := Result + ' SSL_set_fd';
    if @f_SSL_set_rfd                            = nil then Result := Result + ' SSL_set_rfd';
    if @f_SSL_set_wfd                            = nil then Result := Result + ' SSL_set_wfd';
    if @f_SSL_get_fd                             = nil then Result := Result + ' SSL_get_fd';
    if @f_SSL_get_rfd                            = nil then Result := Result + ' SSL_get_rfd';
    if @f_SSL_get_wfd                            = nil then Result := Result + ' SSL_get_wfd';

    if @f_SSL_get_SSL_CTX                        = nil then Result := Result + ' SSL_get_SSL_CTX';
    if @f_SSL_get_client_CA_list                 = nil then Result := Result + ' SSL_get_client_CA_list';
{$IFNDEF OPENSSL_NO_TLSEXT}
    if @f_SSL_set_SSL_CTX                        = nil then Result := Result + ' SSL_set_SSL_CTX';
    if @f_SSL_get_servername                     = nil then Result := Result + ' SSL_get_servername';
    if @f_SSL_get_servername_type                = nil then Result := Result + ' SSL_get_servername_type';
    if @f_SSL_CTX_callback_ctrl                  = nil then Result := Result + ' SSL_CTX_callback_ctrl';
    if @f_SSL_callback_ctrl                      = nil then Result := Result + ' SSL_callback_ctrl';
{$ENDIF}

    if Length(Result) > 0 then
       Delete(Result, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_options(C: PSSL_CTX; Op: LongInt): LongInt;
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_get_options(C: PSSL_CTX): LongInt;
begin
    Result := f_SSL_CTX_ctrl(C, SSL_CTRL_OPTIONS, 0, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_options(S: PSSL; Op: LongInt): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_clear_options(S: PSSL; Op: LongInt): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_CLEAR_OPTIONS, Op, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_get_options(S: PSSL): LongInt;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_OPTIONS, 0, nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_read(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_READING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_write(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_WRITING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_nothing(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_NOTHING);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_want_x509_lookup(S: PSSL) : Boolean;
begin
    Result := (f_SSL_want(S) = SSL_X509_LOOKUP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_session_cache_mode(Ctx: PSSL_CTX; Mode: Integer): Integer;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_MODE, Mode, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_session_reused(SSL: PSSL): Integer;
begin
    Result := f_SSL_ctrl(SSL, SSL_CTRL_GET_SESSION_REUSED, 0, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_sess_set_cache_size(Ctx: PSSL_CTX; CacheSize: Integer): Integer;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_SET_SESS_CACHE_SIZE, CacheSize, nil)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_add_extra_chain_cert(Ctx: PSSL_CTX; Cert: PX509): Longword;
begin
    Result := f_SSL_CTX_ctrl(Ctx, SSL_CTRL_EXTRA_CHAIN_CERT, 0, PAnsiChar(Cert))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF BEFORE_OSSL_098E}
function f_SSL_SESSION_get_id(const Ses: PSSL_SESSION; var IdLen: LongInt): PAnsiChar; { 03/02/07 AG }
begin
    // This is a hack : SSL_SESSION structure has not been defined.
    // There's also no function in openssl to get the session_id
    // from the SSL_SESSION_st.
    IdLen  := PInteger(PAnsiChar(Ses) + 68)^;
    Result := PAnsiChar(Ses) + 72;
end;

{ procedure f_SSL_session_get_id(Ses: PSSL_SESSION; var SessID: Pointer; var IdLen: Integer);
begin
    // This is a hack : SSL_SESSION structure has not been defined.
    // There's also no function in openssl to get the session_id
    // from the SSL_SESSION_st.
    IdLen  := PInteger(PAnsiChar(Ses) + 68)^;
    SessID := Pointer(PAnsiChar(Ses) + 72);
end; }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure f_SSL_CTX_sess_set_new_cb(Ctx: PSSL_CTX; CB: TNew_session_cb);
begin
    // This is a hack : Ctx structure has not been defined. But we know
    // CB member is the 11th 32 bit field in the structure (index is 10)
    // This could change when OpenSSL is updated. Check "struct ssl_ctx_st".
    PNew_session_cb(PAnsiChar(Ctx) + 10 * 4)^ := @CB;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure f_SSL_CTX_sess_set_remove_cb(Ctx: PSSL_CTX; CB: TRemove_session_cb);
begin
    // This is a hack : Ctx structure has not been defined. But we know
    // CB member is the 12th 32 bit field in the structure (index is 11)
    // This could change when OpenSSL is updated. Check "struct ssl_ctx_st".
    PRemove_session_cb(PAnsiChar(Ctx) + 11 * 4)^ := @CB;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure f_SSL_CTX_sess_set_get_cb(Ctx: PSSL_CTX; CB: TGet_session_cb);
begin
    // This is a hack : Ctx structure has not been defined. But we know
    // CB member is the 13th 32 bit field in the structure (index is 12)
    // This could change when OpenSSL is updated. Check "struct ssl_ctx_st".
    PGet_session_cb(PAnsiChar(Ctx) + 12 * 4)^ := @CB;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure f_SSL_CTX_set_client_cert_cb(CTX: PSSL_CTX; CB: TClient_cert_cb);
begin
    // This is a hack : Ctx structure has not been defined. But we know
    // CB member is the 30th 32 bit field in the structure (index is 29)
    // This could change when OpenSSL is updated. Check "struct ssl_ctx_st".
    PClient_cert_cb(PAnsiChar(CTX) + 29 * 4)^ := @CB;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_get_client_cert_cb(CTX: PSSL_CTX): Pointer;
begin
    // This is a hack : Ctx structure has not been defined. But we know
    // CB member is the 30th 32 bit field in the structure (index is 29)
    // This could change when OpenSSL is updated. Check "struct ssl_ctx_st".
    Result := PAnsiChar(CTX) + 29 * 4;
    if PAnsiChar(Result)^ = #0 then
        Result := nil
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF OPENSSL_NO_TLSEXT}
function f_SSL_set_tlsext_host_name(const S: PSSL; const name: String): Longint;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_HOSTNAME,
                      TLSEXT_NAMETYPE_host_name, Pointer(StringToUtf8(name)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_tlsext_servername_callback(ctx: PSSL_CTX;
  cb: TCallback_ctrl_fp): Longint;
begin
    Result := f_SSL_CTX_callback_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cb);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_CTX_set_tlsext_servername_arg(ctx: PSSL_CTX; arg: Pointer): Longint;
begin
    Result := f_SSL_CTX_ctrl(ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_tlsext_debug_callback(S: PSSL; cb: TCallback_ctrl_fp): Longint;
begin
    Result := f_SSL_callback_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_CB, cb);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function f_SSL_set_tlsext_debug_arg(S: PSSL; arg: Pointer): Longint;
begin
    Result := f_SSL_ctrl(S, SSL_CTRL_SET_TLSEXT_DEBUG_ARG, 0, arg);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}//USE_SSL
end.








