{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 or NNTP component.
              MIME is described in RFC-1521. Headers are described if RFC-822.
Creation:     March 08, 1998
Version:      8.04
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2016 by François PIETTE
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

QUICK REFERENCE:
--------------
TMimeDecode take a file or a stream as input and produce several event when
the message is parsed. each event can be used to display or to save to a file
the message parts.

Two methods can be called to decode either a file or a stream:
procedure DecodeFile(FileName : String);
procedure DecodeStream(aStream : TStream);

During the decode process, the component trigger several events. You have to
use those events to save data to a file or to display somehow on the
user interface.

Events are organized by groups of three for message header, part header and
part data:
Message header events: OnHeaderBegin      OnHeaderLine      OnHeaderEnd
Part header events:    OnPartHeaderBegin  OnPartHeaderLine  OnPartHeaderEnd
Part data events:      OnPartDataBegin    OnPartDataLine    OnPartDataEnd

The 'Begin' event is triggered once just before the first item will occur.
The 'Line' event is triggered for each item of the given type.
The 'End' event is triggered once after the last item.

For a multi-part message, we have this sequence:
a) The message header
OnHeaderBegin, then many OnHeaderLine, one for each line in the header. Lines
can be continuated in the message. The event here is triggered with continuated
lines concatenated (so it can be quite large !). After the last header line
has been processed, the OnHeaderEnd is triggered once.
b) The non-significant message part which can be empty. This is part 0. We
get OnPartBegin once, then OnPartLine for each line and finally OnPartEnd once.
c) The first significant part header with his three events, just like the
message header: OnPartHeaderBegin, OnPartHeaderLine and OnPartHeaderEnd.
d) The first significant part data with his three events: OnPartBegin once,
OnPartLine for each line and OnPartEnd once at the end of the part.
It's possible to have an empty part. This gives the OnPartBegin and OnPartEnd
events and NO OnPartLine event.
e) We can have many other parts. The sequence is always the same. We restart
at point (b) here above for each part (header, then data). Note that there is
often en empty part at the end of a message.

TMimeDecode decode encoded parts using 'base64' and 'quoted-printable' methods.
For those parts, the OnPartLine event will gives DECODED data. Other methods
are passed not decoded. You can use the property ContentTransferEncoding to
know which encoding method is used and add your own decoding mechanism.

For each OnHeaderLine, OnPartHeaderLine and OnPartLine, you can find the
actual data at the address pointed by the property CurrentData (a PAnsiChar).
The reason for a PAnsiChar is that the data can be quite large. The data pointed
is a null terminated AnsiString. You can get the length using StrLen, or convert
to a AnsiString with StrPas. It is more efficient to process the data using a
pointer. Using AnsiStrings tends to copy the data several times.
The OnPartLine event passes a PAnsiChar and a length to the handler. This actully
point to the internal buffer and overwrite the original data (base64 and
quote-printable method produce decoded data smaller tha encoded one).

>From the message header, the component extract the following values:
>From         The message author. Not necessary the real author...
             Looks like "Francois Piette" <francois.piette@overbyte.be>
Dest         The message destination (To field, but To is a reserved word)
             Looks like "Francois Piette" <francois.piette@overbyte.be>
Subject      The message subject. Free text.
Date         The message date.
             Look like: Mon, 16 Feb 1998 12:45:11 -0800
ContentType  'multipart/mixed' or empty.
For details about those header fields and others, read RFC-822

For each part, we have the following properties updated (the header is parsed
on the fly):
PartNumber                Starting from 0 for the non-significant part
PartLine                  Starting 1 for the first line of each part or header
PartContentType           Such as 'text/plain' or 'application/x-zip-compressed'
PartCharset               This is a complement for the PartContentType.
ApplicationType           When PartContentType is 'application/something', we
                          get the 'something' extracted
PartName                  This is the value for 'name=something' in the
                          Content-Type header line.
PartEncoding              Encoding method (Content-Transfer-Encoding).
                          Can be used to decode unsupported
                          methods (supported methods are 'base64' and
                          'quoted-printable'. '7bit' and '8bit' does'nt
                          generally require processing.
PartDisposition           Can be 'inline' or 'attachement' and is generally
                          followed by a 'filename=something'
PartFileName              The specified filename in Content-Disposition header
                          line. Be aware that the file name is not necessary
                          suitable for windows ! Use it with caution...
For details about those header fields and others, read RFC-1521.

To write part data to files, you can either implement your own writing in
the OnPartLine event handler, or use the DestStream property. If assigned,
this property will be used to write the data. If not assigned, it will be
ignore.

To select a file name for each part, you can use the PartFileName property or
the 'PartName' property or a comnination of both. But be aware that those value
can be either missing or even invalid as a filename because the message was
generated with another opertaing system which has different filename
conventions.

Updates:
Apr 13, 1998  V1.01 Corrected a bug in ProcessLineBase64 which decoded one
              byte too much. Thanks to Rune Fredriksen <runefr@mail.link.no>.
Apr 15, 1998  V1.02 Corrected bug in ProcessHeaderLine which retreived only
              the first word for each item.
              Added the ReturnPath property.
Apr 24, 1998  V1.03 Removed the modification made in version 1.01 !
Apr 26, 1998  V1.04 Corrected a bug in ReallocMem with Delphi 1
Aug 27, 1998  V1.05 Corrected a bug in decoding which incorrectly merge
              the first message line with the header when the line begon
              by a space. Thanks to Mitch Cant <mitchcant@hotmail.com> for
              finding the bug and correction.
Sep 13, 1998  V1.06 Correctly handled unterminated messages.
              Correctly handled parts without header.
Dec 26, 1998  V1.07 Added features coded by Eric Fortier <efortier@videotron.ca>
              (Embedded mime parts, UUDecode).
Dec 30, 1998  V1.08 Check for header end when a header line begin with a
              space or tab character. (Normally a header end with a blank
              line, we also accept invalid header line).
Feb 01, 1999  V1.09 Corrected a bug ProcessLineUUDecode where 'end' was not
              checked. Thanks to Eric Fortier.
Feb 16, 1999  V1.10 Added UUEncoded embedded parts. Thanks to Eric Fortier.
              Corrected a line termination problem in ProcessLineBase64.
Jul 21, 1999  V1.11 Added support for encoded message without multipart.
              Added Encoding property with the encoding value.
              Thanks to Marcelo S Massuda <massuda@4web.com.br> for pinting this
              lack of feature.
Aug 20, 1999  V1.12 Added compile time options. Revised for BCB4.
Nov 25, 1999  V1.13 Changed continuation line character for quoted printable
              encoding. By Ken Petersen <KPT@edbgruppen.dk>.
              Created GetTokenEx function to take care of comments in header
              lines. This affect ProcessPartHeaderLine and ProcessHeaderLine.
              Thanks to Boris Daljevic <biber@eunet.yu> for his code.
              Added CharSet property related to main part charset (see also
              existing PartCharset property). Thanks to Boris Daljevic
              <biber@eunet.yu> for his code.
Jun 20, 2000  V1.14 Poessler Thomas <Thomas.Poessler@uta.at> corrected a bug in
              ProcessLineQuotedPrintable.
Jul 02, 2000  V1.15 Added OnMessageEnd event
Jul 15, 2000  V1.16 Added code from Wolfgang Baron <Wolfgang.Baron@gwtel.de>
              to support content-description header line.
              Changed GetToken and GetTokenEx so that a space before a delimiter
              will not break token parsing. Outlook generate such invalid
              formatting thanks for Arno van Rossum <a.van.rossum@mmp-obec.nl>
              for finding this bug.
              Revised code to handle inline UUEncoded messages.
Jul 21, 2000  V1.17 Use GetValue instead of GetToken to solve problem with
              boundaries of embbeded parts.
              With help of Jan Bartak <bart@seznam.cz>.
              As suggested by Sebastien Gariepy <beeper@globetrotter.net>, I
              added PartContentID.
Oct 29, 2000  V1.18 Checked for missing content-type before calling
              UUProcessLine. Without the check, a part with a line beginning
              with 'begin 666' will be wrongly decoded.
Feb 17, 2001  V1.19 Top of the messages with a field multipart was incorrectly
              processed.Property FCharset was not initialized in procedure
              MessageBegin. Thanks to Bayanov <bayanov@alt.ru>
Jul 26, 2001  V1.20 Cleared FEncoding in MessageBegin. Thanks to Joel
              lauvinerie <joel.lauvinerie@wanadoo.fr> who found this bug.
              Poessler Thomas <Thomas.Poessler@uta.at> added new properties:
              HeaderName, FileName, HeaderLines, Disposition, EndOfMime,
              IsMultipart.
Jul 29, 2001  V1.21 Moved ProcessLineBase64 to public section. Made CurrentData
              property read/write. This permit to use Base64 decoding from
              outside of the component.
              Corrected a glitche with Delphi 1
May 04, 2002  V1.23 Added "Len" argument to OnInlineDecodeLine event.
              Corrected UUDec so that nul is handled as space. Thanks to
              arnaud.mesnews@free.fr who provided a test case.
              Made UUOutDec a little bit faster.
May 10, 2002  V1.24 Accept 'begin 644' as well as 'begin 666' for UUEncoding
              start. arnaud.mesnews@free.fr found that OE does that.
Nov 01, 2002  V1.25 Changed PChar arguments to Pointer to work around Delphi 7
              bug with PAnsiChar<->PChar.
              This will require small changes in your application code: change
              PChar args to Pointer and add a PChar cast when using the arg.
              Changed Base64 decoding so that is doesn't crash even if input
              data is malformed (corrupted message).
              Changed UUEncoded detection procedure. Thanks to Arnaud
              <arnaud.mesnews@free.fr> for providing his code.
Apr 22, 2003  V1.26 Corrected ProcessLineQuotedPrintable which overflowed input
              data when an empty line was given. Thanks to Dmitry Andreev for
              finding a test case.
              V1.27 Christophe Thiaux <tophet@free.fr> added PartFormat and
              format properties.
Jul 20, 2003  V1.28 <arnaud.mesnews@free.fr> added yEnc decoding and fixed
              uudecode when "begin" has to be lower case.
Aug 06, 2003  V1.29 Dmitry Andreev <advadvadv@mailgate.ru> and Arnaud
              <arnaud.mesnews@free.fr> corrected a bug with continuation
              lines in ProcessLineQuotedPrintable.
Aug 10, 2003  V1.30 Reformatted the source line to make Arnaud and Dmitry
              changes looking like my own code. Translated all comments to
              english. Englicized identificators.
Jan 03, 2004  V1.31 Replaced private section by protected.
              Moved procedure ProcessLineQuotedPrintable to public section.
May 31, 2004  V1.32 John Bridgwater <jbridgwater@goodyear.com> fixed GetTokenEx
              to allow a space around delimiter.
Jul 24, 2004  V1.33 arnaud.mesnews@free.fr added TriggerInlineDecodeBegin,
              TriggerInlineDecodeLine and TriggerInlineDecodeEnd and called
              them where needed. He also added InlineDecodeLine and
              LengthHeader properties
Nov 3, 2006   V6.00 New version 6.00 started
Nov 13, 2007  V6.01 Fixed TMimeDecode.ProcessPartLine to avoid adding a CRLF
              at the end of attached text file.
Nov 14, 2007  V6.02 Added Cc decoding
Mar 10, 2008  V6.03 Francois Piette made some changes to prepare code
                    for Unicode.
Aug 02, 2008  V6.04 A. Garrels made some changes to prepare code
              for Unicode.
Oct 03, 2008  V6.10 A. Garrels moved IsCharInSysCharSet, IsSpaceChar and
              IsCrLfChar to OverbyteIcsUtils.pas (and removed Char suffix).
Oct 11, 2008  V7.11 Angus added TMimeDecodeEx component which extends TMimeDecode by
              decoding a MIME file or stream into a PartInfos array with TotParts
              and HeaderLines StringList without the application needing to use any events
              Also added functions to decode email MIME header lines with RFC2047 encoded words
              DecodeHeaderLine returns 8-bit raw text and MimeCharSet
              DecodeHeaderLineWide returns Unicode text
              See OverbyteIcsMimeDemo1 for TMimeDecodeEx and DecodeHeaderLine examples
Oct 12, 2008  V7.12 Angus fixed DecodeHeaderLine for Q starting with = word
Oct 15, 2008  V7.13 Arno added TMimeDecodeW and replaced GetHeaderValue()
              which was buggy by UnfoldHdrValue. (See comments below).
              Also changed Angus' TMimeDecodeEx. Removed many implicit
              string casts.
Oct 16, 2008  V7.14 Arno - Formated my previous changes in ICS style. Minor
              change and correction of the comment in DecodeMimeValue().
Oct 18, 2008  V7.15 Angus added DecodeMimeInlineValueEx, removed DecodeHeaderLine/Ex
              internal changes and fixes to TMimeDecodeEx
Oct 23, 2008  V7.16 Arno - PrepareNextPart did not clear FPartCharset.
              Property ApplicationType was not implemented.
              Fixed a bug in DecodeMimeInlineValue and DecodeMimeInlineValueEx,
              Improved UnfoldHdrValue. In InternalDecodeStream replace both Tab
              and Space after CRLF by #1 . Made two implicit string casts
              explicit casts. Added properties PartCodePage and IsTextpart
              to TMimeDecode.
Oct 24, 2008  V7.17 Arno - TMimeDecode: Added property CodePage.
              Initialization of FIsTextPart changed to TRUE.
              Added property DefaultCodePage, its value is assigned to both
              properties CodePage and PartCodePage whenever no code page can
              be retrieved from a header or if a valid code page cannot be
              looked up from a Charset value. I'm pretty sure that
              DefaultCodePage should be set to CP_US_ASCII (20127), however
              the component initializes this value with the default system
              code page.
              TMimeDecodeW: Initializes part properties of part #0 with values
              from message header if IsMultipart equals FALSE.
              TMimeDecodeEX: MimeDecodePartEnd adjusted to use FCodePage and
              FPartCodePage.
Oct 9, 2009   V7.18 Bjørnar added PContentId and PSubject to PartInfos array
Nov 17, 2009  V7.19 Arno added UTF-16 and UTF-32 support in TMimeDecodeW and
              TMimeDecodeEx. Made property PartCodePage writable.
              TMimeDecodeEx.PSubject is MIME inline decoded now (I wonder why
              PSubject was added to the parts at all).
Nov 19, 2009  V7.20 Angus added PIsTextpart to PartInfos and removed PSubject
              which is the same for all parts
Feb 20, 2011  V7.21 Angus, prevent range error for malformed blank lines
Mar 11, 2011  V7.22 Angus, prevent range error for blank header in UnfoldHdrValue
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Apr 25, 2013 V8.01 Arno minor XE4 changes.
Jul 14, 2013 V8.02 Arno - Some default values changed in TMimeDecode.MessageBegin.
                   Set IsTextPart to False if FPartContentType contains "application/".
Nov 11, 2015 V8.03 Angus fixed bug that ignored body if boundary specified but
                     never found, also meant base64 decoding was ignored
Feb 20, 2016 V8.04 Angus content-disposition now reads non-delimited filename


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMimeDec;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long AnsiStrings                }
{$J+}           { Allow typed constant to be modified }
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
{$IFDEF COMPILER12_UP}
    {$WARN IMPLICIT_STRING_CAST       ON}
    {$WARN IMPLICIT_STRING_CAST_LOSS  ON}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Ics.Posix.WinTypes,
{$ENDIF}
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
{$IFDEF COMPILER12_UP}
    {$IFDEF RTL_NAMESPACES}System.AnsiStrings{$ELSE}AnsiStrings{$ENDIF}, { for PosEx }
{$ELSE}
    {$IFDEF RTL_NAMESPACES}System.StrUtils{$ELSE}StrUtils{$ENDIF},  { for PosEx }
{$ENDIF}
    OverbyteIcsTypes,
    OverbyteIcsUtils,
    OverbyteIcsMimeUtils,
    OverbyteIcsCharsetUtils;

const
    MimeDecodeVersion  = 804;
    CopyRight : String = ' TMimeDecode (c) 1998-2016 Francois Piette V8.04';

type
    TMimeDecodePartLine = procedure (Sender  : TObject;
                                     Data    : Pointer;
                                     DataLen : Integer) of object;

    TInlineDecodeBegin = procedure (Sender: TObject; Filename: AnsiString) of object;
    TInlineDecodeLine  = procedure (Sender: TObject; Line: Pointer; Len : Integer) of object;
    TInlineDecodeEnd   = procedure (Sender: TObject; Filename: AnsiString) of object;

    TMimeDecode = class(TComponent)
    protected
        FFrom                     : AnsiString;
        FDest                     : AnsiString;
        FCc                       : AnsiString;
        FSubject                  : AnsiString;
        FDate                     : AnsiString;
        FReturnPath               : AnsiString;
        FEncoding                 : AnsiString;
        FCharSet                  : AnsiString;
        FCodePage                 : LongWord;
        FContentType              : AnsiString;
        FMimeVersion              : AnsiString;
        FHeaderName               : AnsiString;
        FDisposition              : AnsiString;
        FFileName                 : AnsiString;
        FFormat                   : AnsiString;
        FHeaderLines              : TStrings;
        FIsMultipart              : Boolean;
        FIsTextpart               : Boolean;
        FEndOfMime                : Boolean;
        FPartContentType          : AnsiString;
        FPartEncoding             : AnsiString;
        FPartNumber               : Integer;
        FPartHeaderBeginSignaled  : Boolean;
        FPartName                 : AnsiString;
        FPartDisposition          : AnsiString;
        FPartContentID            : AnsiString;
        FPartFileName             : AnsiString;
        FPartFormat               : AnsiString;
        FPartCharset              : AnsiString;
        FPartCodePage             : LongWord;
        FApplicationType          : AnsiString;
        FPartOpened               : Boolean;
        FHeaderFlag               : Boolean;
        FLineNum                  : Integer;
        FBuffer                   : PAnsiChar;
        FBufferSize               : Integer;
        FCurrentData              : PAnsiChar;
        FBoundary                 : AnsiString;
        FBoundaryFound            : Boolean;    { V8.03 } 
        FUUProcessFlag            : Boolean;
        FProcessFlagYBegin        : Boolean;   { AS: YEnc handling }
        FSizeFileY                : Integer;   { AS: YEnc handling }
        FSizeBlocY                : Integer;   { AS: YEnc handling }
        FSizeLeftY                : Integer;   { AS: YEnc handling }
        FNext                     : procedure of object;
        FDestStream               : TStream;
        cUUFilename               : AnsiString;         { ##ERIC }
        FEmbeddedBoundary         : TStringList;        { ##ERIC }
        cIsEmbedded               : Boolean;            { ##ERIC }
        FOnHeaderBegin            : TNotifyEvent;
        FOnHeaderLine             : TNotifyEvent;
        FOnHeaderEnd              : TNotifyEvent;
        FOnPartHeaderBegin        : TNotifyEvent;
        FOnPartHeaderLine         : TNotifyEvent;
        FOnPartHeaderEnd          : TNotifyEvent;
        FOnPartBegin              : TNotifyEvent;
        FOnPartLine               : TMimeDecodePartLine;
        FOnPartEnd                : TNotifyEvent;
        FOnMessageEnd             : TNotifyEvent;
        FOnInlineDecodeBegin      : TInlineDecodeBegin;
        FOnInlineDecodeLine       : TInlineDecodeLine;
        FOnInlineDecodeEnd        : TInlineDecodeEnd;
        { Used to force InLine decoding even if there was no OnInlineDecodeLine
          event. See UUProcessLine }
        FInlineDecodeLine         : Boolean;
        FLengthHeader             : Integer;
        FPartFirstLine            : Boolean;
        FDefaultCodePage          : LongWord;
        procedure SetDefaultCodePage(const Value: LongWord);
        procedure TriggerHeaderBegin; virtual;
        procedure TriggerHeaderLine; virtual;
        procedure TriggerHeaderEnd; virtual;
        procedure TriggerPartHeaderBegin; virtual;
        procedure TriggerPartHeaderLine; virtual;
        procedure TriggerPartHeaderEnd; virtual;
        procedure TriggerPartBegin; virtual;
        procedure TriggerPartLine(Data : Pointer; DataLen : Integer); virtual;
        procedure TriggerPartEnd; virtual;
        procedure TriggerMessageEnd; virtual;
        procedure TriggerInlineDecodeBegin(const Filename: AnsiString); virtual;
        procedure TriggerInlineDecodeLine(Line: Pointer; Len : Integer); virtual;
        procedure TriggerInlineDecodeEnd(const Filename: AnsiString); virtual;
        procedure ProcessLineUUDecode;
        function  UUProcessLine(FCurrentData: PAnsiChar): boolean;
        procedure ProcessHeaderLine;
        procedure ProcessPartHeaderLine;
        procedure ProcessPartLine;
        procedure ProcessWaitBoundary;
        procedure ProcessMessageLine;
        procedure PreparePart;
        procedure PrepareNextPart;
        procedure ProcessDecodedLine(Line : Pointer; Len : Integer);
        procedure InternalDecodeStream(aStream : TStream);
        procedure MessageBegin;
        procedure MessageEnd;
        procedure ParseYBegin(const Ch : AnsiString);
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;

        procedure DecodeFile(const FileName : String);
        procedure DecodeStream(aStream : TStream);
        procedure ProcessLineBase64;
        procedure ProcessLineQuotedPrintable;
        property From             : AnsiString       read  FFrom;
        property Dest             : AnsiString       read  FDest;
        property Cc               : AnsiString       read  FCc;
        property Subject          : AnsiString       read  FSubject;
        property Date             : AnsiString       read  FDate;
        property ReturnPath       : AnsiString       read  FReturnPath;
        property ContentType      : AnsiString       read  FContentType;
        property Encoding         : AnsiString       read  FEncoding;
        property Charset          : AnsiString       read  FCharset;
        property CodePage         : LongWord         read  FCodePage;
        property MimeVersion      : AnsiString       read  FMimeVersion;
        property HeaderName       : AnsiString       read  FHeaderName;
        property Disposition      : AnsiString       read  FDisposition;
        property FileName         : AnsiString       read  FFileName;
        property Format           : AnsiString       read  FFormat;
        property HeaderLines      : TStrings         read  FHeaderLines;
        property IsMultipart      : Boolean          read  FIsMultipart;
        property IsTextpart       : Boolean          read  FIsTextpart;
        property EndOfMime        : Boolean          read  FEndOfMime;
        property PartContentType  : AnsiString       read  FPartContentType;
        property PartEncoding     : AnsiString       read  FPartEncoding;
        property PartName         : AnsiString       read  FPartName;
        property PartDisposition  : AnsiString       read  FPartDisposition;
        property PartContentID    : AnsiString       read  FPartContentID;
        property PartFileName     : AnsiString       read  FPartFileName;
        property PartFormat       : AnsiString       read  FPartFormat;
        property PartCharset      : AnsiString       read  FPartCharset;
        property PartCodePage     : LongWord         read  FPartCodePage
                                                     write FPartCodePage;
        property ApplicationType  : AnsiString       read  FApplicationType;
        property PartNumber       : Integer          read  FPartNumber;
        property CurrentData      : PAnsiChar        read  FCurrentData
                                                     write FCurrentData;
        property DestStream       : TStream          read  FDestStream
                                                     write FDestStream;
        property InlineDecodeLine : boolean          read  FInlineDecodeLine
                                                     write FInlineDecodeLine
                                                     default FALSE;
        property LengthHeader     : Integer          read  FLengthHeader;
        property DefaultCodePage  : LongWord         read  FDefaultCodePage
                                                     write SetDefaultCodePage; 
    published
        property OnHeaderBegin : TNotifyEvent        read  FOnHeaderBegin
                                                     write FOnHeaderBegin;
        property OnHeaderLine : TNotifyEvent         read  FOnHeaderLine
                                                     write FOnHeaderLine;
        property OnHeaderEnd : TNotifyEvent          read  FOnHeaderEnd
                                                     write FOnHeaderEnd;
        property OnPartHeaderBegin : TNotifyEvent    read  FOnPartHeaderBegin
                                                     write FOnPartHeaderBegin;
        property OnPartHeaderLine : TNotifyEvent     read  FOnPartHeaderLine
                                                     write FOnPartHeaderLine;
        property OnPartHeaderEnd : TNotifyEvent      read  FOnPartHeaderEnd
                                                     write FOnPartHeaderEnd;
        property OnPartBegin : TNotifyEvent          read  FOnPartBegin
                                                     write FOnPartBegin;
        property OnPartLine : TMimeDecodePartLine    read  FOnPartLine
                                                     write FOnPartLine;
        property OnPartEnd : TNotifyEvent            read  FOnPartEnd
                                                     write FOnPartEnd;
        property OnMessageEnd : TNotifyEvent         read  FOnMessageEnd
                                                     write FOnMessageEnd;
        property OnInlineDecodeBegin : TInlineDecodeBegin
                                                     read  FOnInlineDecodeBegin
                                                     write FOnInlineDecodeBegin;
        property OnInlineDecodeLine  : TInlineDecodeLine
                                                     read  FOnInlineDecodeLine
                                                     write FOnInlineDecodeLine;
        property OnInlineDecodeEnd   : TInlineDecodeEnd
                                                     read  FOnInlineDecodeEnd
                                                     write FOnInlineDecodeEnd;
    end;

    TMimeDecodeW = class(TMimeDecode)
      private
          function GetCcW: UnicodeString;
          function GetDestW: UnicodeString;
          function GetFileNameW: UnicodeString;
          function GetFromW: UnicodeString;
          function GetPartFileNameW: UnicodeString;
          function GetPartNameW: UnicodeString;
          function GetSubjectW: UnicodeString;
      protected
          procedure TriggerPartBegin; override;
      public
          property FromW            : UnicodeString    read  GetFromW;
          property DestW            : UnicodeString    read  GetDestW;
          property CcW              : UnicodeString    read  GetCcW;
          property SubjectW         : UnicodeString    read  GetSubjectW;
          property FileNameW        : UnicodeString    read  GetFileNameW;
          property PartNameW        : UnicodeString    read  GetPartNameW;
          property PartFileNameW    : UnicodeString    read  GetPartFileNameW;
      end;

 { V7.11 MIME Part Information record }
    TPartInfo = record
        PContentType: AnsiString ;
        PCharset: AnsiString ;
        PApplType: AnsiString ;
        PName: UnicodeString ;
        PEncoding: AnsiString ;
        PDisposition: AnsiString ;
        PContentId: AnsiString ;  {V7.18 Bjørnar}
        PFileName: UnicodeString ;
//      PSubject: UnicodeString ; {V7.18 Bjørnar, gone V7.20}
        PartStream: TMemoryStream ;
        PSize: integer ;
        PCodePage: LongWord ;
        PIsTextpart: Boolean ;   { V7.20 Angus }
    end ;

{ V7.11 Decode file or stream into MIME Part Information records }
   TMimeDecodeEx = class(TComponent)
      private
        { Private declarations }
        procedure MimeDecodeHeaderLine(Sender: TObject);
        procedure MimeDecodePartBegin(Sender: TObject);
        procedure MimeDecodePartEnd(Sender: TObject);
      protected
        { Protected declarations }
        FDecodeW: TMimeDecodeW;
        FMaxParts: integer ;                   // maximum MIME parts decoded
        FTotParts: integer ;                   // number of parts in current body
        FDecParts: integer ;                   // number of parts decoded (no more than fMaxParts)
        FTotHeaders: integer ;                 // number of decoded header lines in WideHeaders
        FHeaderCharset: AnsiString ;           // first character set found decoding headers
        FSkipBlankParts: boolean ;             // ignore parts without ContentType or zero size
        function GetPartInfo (Index: Integer): TPartInfo ;
      public
        { Public declarations }
        FHeaderLines: TStrings;                // raw header lines, may be inline encoded
        WideHeaders: array of UnicodeString;   // Unicode version of each header line, read this to get header lines
        PartInfos: array of TPartInfo ;        // body part info records, one per part, read this to get content
        constructor Create (Aowner: TComponent) ; override ;
        destructor  Destroy ; override ;
        procedure Initialise ;
        procedure Reset ;
        procedure Finalise ;
        procedure DecodeFileEx (const FileName : String);
        procedure DecodeStreamEx (aStream : TStream);
      published
        { Published declarations }
        property DecodeW: TMimeDecodeW              read FDecodeW ;
        property HeaderLines: TStrings              read FHeaderLines;
        property MaxParts: integer                  read FMaxParts write FMaxParts ;
        property TotParts: integer                  read FTotParts ;
        property DecParts: integer                  read FDecParts ;
        property TotHeaders: integer                read FTotHeaders ;
        property HeaderCharset: AnsiString          read FHeaderCharset ;
        property SkipBlankParts: boolean            read FSkipBlankParts write FSkipBlankParts ;
      end;


function GetToken(Src : PAnsiChar; var Dst : AnsiString; var Delim : AnsiChar) : PAnsiChar;
{ This function did not unfold header values correctly, it also removed valid }
{ spaces. Since its name was also missleading it's been replaced by function  }
{ UnfoldHdrValue().                                                       AG  }
// function GetHeaderValue(X : PAnsiChar) : AnsiString;
function UnfoldHdrValue(const Value: PAnsiChar) : AnsiString; overload;
function UnfoldHdrValue(const Value: AnsiString) : AnsiString; {$IFDEF USE_INLINE} inline; {$ENDIF} overload;
function DecodeMimeInlineValue(const Value: AnsiString): UnicodeString;
function DecodeMimeInlineValueEx(const Value : AnsiString; var CharSet: AnsiString): UnicodeString;
function IsCrLf1Char(Ch : AnsiChar) : Boolean;
function IsCrLf1OrSpaceChar(Ch : AnsiChar) : Boolean;

implementation

type
  TLookup = array [0..127] of Byte;

const
  Base64In: TLookup = (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
     56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
     13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
     33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
     46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
  );


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HexConv(Ch : AnsiChar) : Integer;
begin
    if IsCharInSysCharSet(Ch, ['0'..'9']) then
        Result := Ord(Ch) - Ord('0')
    else
        Result := (Ord(Ch) and 15) + 9;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMimeDecode.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FHeaderLines      := TStringList.Create;
    FIsMultipart      := FALSE;
    FEndOfMime        := FALSE;
    FInlineDecodeLine := false;
    FDefaultCodePage  := IcsSystemCodePage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMimeDecode.Destroy;
begin
    if Assigned(FHeaderLines) then begin
        FHeaderLines.Destroy;
        FHeaderLines := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderBegin;
begin
    if Assigned(FOnHeaderBegin) then
        FOnHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderLine;
begin
    if Assigned(FOnHeaderLine) then
        FOnHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderEnd;
begin
    if Assigned(FOnHeaderEnd) then
        FOnHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderBegin;
begin
    if Assigned(FOnPartHeaderBegin) then
        FOnPartHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderLine;
begin
    if Assigned(FOnPartHeaderLine) then
        FOnPartHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderEnd;
begin
    if Assigned(FOnPartHeaderEnd) then
        FOnPartHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartBegin;
begin
    if Assigned(FOnPartBegin) then
        FOnPartBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartLine(Data : Pointer; DataLen : Integer);
begin
    if Assigned(FOnPartLine) then
        FOnPartLine(Self, Data, DataLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartEnd;
begin
    if Assigned(FOnPartEnd) then
        FOnPartEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerMessageEnd;
begin
    if Assigned(FOnMessageEnd) then
        FOnMessageEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeBegin(const Filename: AnsiString);
begin
    if Assigned(FOnInlineDecodeBegin) then
        FOnInlineDecodeBegin(self, Filename);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeLine(Line: Pointer; Len : Integer);
begin
    if Assigned(FOnInlineDecodeLine) then
        FOnInlineDecodeLine(self, Line, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeEnd(const Filename: AnsiString);
begin
    if Assigned(FOnInlineDecodeEnd) then
        FOnInlineDecodeEnd(self, Filename);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessDecodedLine(Line : Pointer; Len : Integer);
begin
    if Len > 0 then begin
        if (FPartContentType = '')  { Not sure it is always OK !              }
                                    { As such we can't have a MIME part which }
                                    { is uu-encoded.                          }
           and uuprocessline(line) then
                Exit;
    end;
    TriggerPartLine(Line, Len);

    { Write decoded characters to the destination stream }
    if Assigned(FDestStream) and (Len > 0) then
        FDestStream.WriteBuffer(Line^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This works if charset="iso-8859-1" !                                        }
procedure TMimeDecode.ProcessLineQuotedPrintable;
var
    SourceIndex         : Integer;
    DecodedIndex        : Integer;
    Ch                  : AnsiChar;
    Code                : Integer;
    DecodedBuf          : AnsiString;
const
    EmptyLine : array [0..2] of AnsiChar = (#13, #10, #0);
begin
    if FCurrentData = nil then
        Exit;

    { Allocate a buffer for decode line. At most the length of encoded data }
    { plus 2 bytes for CRLF                                                 }
    SetLength(DecodedBuf, IcsStrLen(FCurrentData) + 2);
    SourceIndex  := 0; { It's a PAnsiChar so index start at 0   }
    DecodedIndex := 1; { It's a AnsiString, so index start at 1 }
    while TRUE do begin
        Ch := FCurrentData[SourceIndex];
        if Ch = #0 then begin
            { End of line, add CRLF and let's go }
            DecodedBuf[DecodedIndex] := #13;
            Inc(DecodedIndex);
            DecodedBuf[DecodedIndex] := #10;
            SetLength(DecodedBuf, DecodedIndex);
            ProcessDecodedLine(Pointer(DecodedBuf), DecodedIndex);  // V7.21 Angus
            break;
        end;
        if Ch = '=' then begin
            { Encoded character. Next two chars should be hex code }
            Inc(SourceIndex);
            Ch := FCurrentData[SourceIndex];
            if Ch = #0 then begin
{*** Changed 20030806 ***}
                { process without #13#10 adding }
                SetLength(DecodedBuf, DecodedIndex-1);
                ProcessDecodedLine(Pointer(DecodedBuf), DecodedIndex-1); // V7.21 Angus, prevent range error for malformed blank lines
                break;
{***         ***}
            end;
            Code := HexConv(Ch);
            Inc(SourceIndex);
            Ch := FCurrentData[SourceIndex];
            if Ch = #0 then begin
                { Should not occur: code truncated, ignore }
                continue;
            end;
            Code := (Code shl 4) + HexConv(Ch);
            DecodedBuf[DecodedIndex] := AnsiChar(Code);
        end
        else
            DecodedBuf[DecodedIndex] := FCurrentData[SourceIndex];
        Inc(SourceIndex);
        Inc(DecodedIndex);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineBase64;
var
    SourceIndex         : Integer;
    DataIn0             : Byte;
    DataIn1             : Byte;
    DataIn2             : Byte;
    DataIn3             : Byte;
    DecodedIndex        : Integer;
    Len                 : Integer;
begin
    SourceIndex  := 0;
    DecodedIndex := 0;
    Len          := IcsStrLen(FCurrentData);

    { Remove spaces at the end of line }
    while (Len > 0) and IsSpace(FCurrentData[Len - 1]) do
        Dec(Len);

    { Skip white spaces at the start of line }
    while (SourceIndex < Len) and IsSpace(FCurrentData[SourceIndex]) do
        Inc(SourceIndex);

    { Decode until end of line. Replace coded chars by decoded ones       }
    { Protect agains malformed messages. Normally we have a length which  }
    { is multiple of four. But this may be corrupted !                    }
    while SourceIndex < Len do begin
        { "And $7F" will clear 8th bit and avoid range error. If found in }
        { a message, it is probably a corrupted message !                 }
        DataIn0 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
        Inc(SourceIndex);
        if SourceIndex >= Len then begin
            DataIn1 := $40;
            DataIn2 := $40;
            DataIn3 := $40;
        end
        else begin
            DataIn1 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
            Inc(SourceIndex);
            if SourceIndex >= Len then begin
                DataIn2 := $40;
                DataIn3 := $40;
            end
            else begin
                DataIn2 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
                Inc(SourceIndex);
                if SourceIndex >= Len then
                    DataIn3 := $40
                else begin
                    DataIn3 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
                    Inc(SourceIndex);
                end;
            end;
        end;

        FCurrentData[DecodedIndex] := AnsiChar((DataIn0 and $3F) shl 2 + (DataIn1 and $30) shr 4);
        if DataIn2 <> $40 then begin
            FCurrentData[DecodedIndex + 1] := AnsiChar((DataIn1 and $0F) shl 4 + (DataIn2 and $3C) shr 2);
            if DataIn3 <> $40 then begin
                FCurrentData[DecodedIndex + 2] := AnsiChar((DataIn2 and $03) shl 6 + (DataIn3 and $3F));
                Inc(DecodedIndex, 3);
            end
            else
                Inc(DecodedIndex, 2);
        end
        else
            Inc(DecodedIndex, 1);
    end;

    { Nul terminate decoded line }
    FCurrentData[DecodedIndex] := #0; { 16/02/99 }
    ProcessDecodedLine(FCurrentData, DecodedIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UUDec(Sym : AnsiChar): Byte;
begin
    if Sym = #0 then
        Result := 0
    else
        Result := (Ord(Sym) - Ord(' ')) and $3F;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UUOutDec(buf: PAnsiChar; n: Integer; var out1 : AnsiString);
begin
    case n of
    0:   ;
    1:   out1 := out1 + AnsiChar((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4));
    2:   out1 := out1 + AnsiChar((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4)) +
                        AnsiChar((UUDec(buf[1]) SHL 4) + (UUDec(buf[2]) SHR 2));
    else out1 := out1 + AnsiChar((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4)) +
                        AnsiChar((UUDec(buf[1]) SHL 4) + (UUDec(buf[2]) SHR 2)) +
                        AnsiChar((UUDec(buf[2]) SHL 6) + (UUDec(buf[3])));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
procedure UUOutDec(buf: PAnsiChar; n: Integer; var out1 : AnsiString);
var
    c1, c2, c3: AnsiChar;
begin
    c1 := Chr((word(UUDec(buf[0])) SHL 2) or (word(UUDec(buf[1])) SHR 4));
    c2 := Chr((word(UUDec(buf[1])) SHL 4) or (word(UUDec(buf[2])) SHR 2));
    c3 := Chr((word(UUDec(buf[2])) SHL 6) or (word(UUDec(buf[3]))));
    if n >= 1 then
        out1 := out1 + c1;
    if n >= 2 then
        out1 := out1 + c2;
    if n >= 3 then
        out1 := out1 + c3;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ AG:                                                                         }
{ This function did not unfold header values correctly, it also removed valid }
{ spaces. Since its name was also missleading it's been replaced by function  }
{ UnfoldHdrValue().                                                           }
(*
{ AS: Get a value from a header line. Support multiline header.               }
{ When a second line is present, make sure only ONE space is taken.           }
{ InternalDecodeStream has replaced CR, LF and TAB by #1 character.           }
function GetHeaderValue(X : PAnsiChar) : AnsiString;
var
    I, J : Integer;
    Len  : Integer;
    EncStart, EncEnd: Integer;
begin
    Result := SysUtils.StrPas(X);
    Len    := Length(Result);
    I      := Len;
    while I >= 1 do begin
        if IsCrLf1Char(Result[I]) then begin
            { Make sure we preserve a single space }
            J := I;
            while (I >= 1) and IsCrLf1OrSpaceChar(Result[I - 1]) do
                Dec(I);
            while (J < Len) and
                  IsCrLf1OrSpaceChar(Result[J + 1]) do
                Inc(J);
            Delete(Result, I, J - I);
            Result[I] := ' ';
        end;
        Dec(I);
    end;
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ AG:                                                                         }
{ InternalDecodeStream has replaced CR, LF and TAB by #1 dummy character.     }
{ If CRLF+[Space or Tab] or #1#1#1 is found, preserve a space only if we are  }
{ not inside a MIME inline block and the continuation line does not begin     }
{ with a MIME inline encoded block or another continuation line. This is how  }
{ Thunderbird handles MIME inline decoding, OE behaves a little bit           }
{ different but IMO a little bit buggy.                                       }
{                                                                             }
{ Examples:                                                                   }
{   '=?ISO-8859-1?Q?a?='#1#1#1'=?ISO-8859-1?Q?b?='                            }
{   Result: =?ISO-8859-1?Q?a?==?ISO-8859-1?Q?b?=                              }
{                                                                             }
{   '=?ISO-8859-1?Q?a?='#13#10#9' =?ISO-8859-1?Q?b?='                         }
{   Result: =?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=                            }
{                                                                             }
{   '=?ISO-8859-1?Q?a?='#1#1#1'<foo@bar.com>'                                 }
{   Result: =?ISO-8859-1?Q?a?= <foo@bar.com>                                  }
{                                                                             }
{   #10#10'=?ISO-8859-1?Q?a?= c'#1#1#1'=?ISO-8859-1?Q?b?='                    }
{   Result: =?ISO-8859-1?Q?a?= c =?ISO-8859-1?Q?b?=                           }
{                                                                             }
{   'a'#1#1#1'b'                                                                }
{   Result: a b                                                               }
{                                                                             }
{  ToDo: Skip continuation lines inside address blocks <>.                    }

function UnfoldHdrValue(const Value: PAnsiChar) : AnsiString;
var
    NextInlineEndOffset, PrevInlineEndOffset : Integer;
    L, I, J, R: Integer;
    ShouldPreserveSpace : Boolean;

    { Checks roughly whether a valid MIME inline block begins at current }
    { offset. If true returns offset of the last "=", otherwise zero.    }
    function GetNextInlineEnd : Integer;
    var
        X, Part : Integer;
    begin
        Result := 0;
        if (I + 9 > L) or (not ((Value[I] = '=') and (Value[I + 1] = '?'))) then
            Exit;
        X := I + 2;
        Part := 1;
        while X <= L do
        begin
            if (Part = 1) and (Value[X] = '?') and (X + 2 <= L) and
                (Value[X + 2] = '?') then
            begin
                Inc(Part);
                Inc(X, 2);
            end
            else if (Value[X] = '?') and (X + 1 <= L) and
                   (Value[X + 1] = '=') then
            begin
                Result := X + 1;
                Exit;
            end;
            Inc(X);
        end;
    end;

begin
    L := IcsStrLen(Value); // V7.22 prevent range error
    SetLength(Result, L);
    Dec (L);
    I := 0;
    J := I;
    R := 0;
    NextInlineEndOffset := 0;
    PrevInlineEndOffset := 0;
    ShouldPreserveSpace := FALSE;
    while I <= L do
    begin
        if NextInlineEndOffset = 0 then
        begin
            { if continuation line follows }
            if (Value[I] in [#13, #1]) and (I + 2 <= L) and
               (Value[I + 1] in [#10, #1]) and
               (Value[I + 2] in [#9, #1, ' ']) then
            begin
                if I > J then
                begin
                    Move(Value[J], Result[R + 1], I - J);
                    Inc(R, I - J);
                end;
                Inc(I, 3);
                J := I;
                if PrevInlineEndOffset > 0 then
                    Inc(PrevInlineEndOffset, 3);
                if not ShouldPreserveSpace then
                    ShouldPreserveSpace := TRUE;
                NextInlineEndOffset := GetNextInlineEnd;
            end
            else if (Value[I] in [#1, #10, #13]) then
            begin
                if I > J then
                begin
                    Move(Value[J], Result[R + 1], I - J);
                    Inc(R, I - J);
                end;
                Inc(I);
                J := I;
                if PrevInlineEndOffset > 0 then
                    Inc(PrevInlineEndOffset);
                NextInlineEndOffset := GetNextInlineEnd;
            end
            else begin
                if ShouldPreserveSpace then
                begin
                    ShouldPreserveSpace := FALSE;
                    Inc(R);
                    Result[R] := ' ';
                end;
                NextInlineEndOffset := GetNextInlineEnd;
                Inc(I);
            end;
        end
        else begin  // NextInlineEndOffset > 0
            if ShouldPreserveSpace then
            begin
                ShouldPreserveSpace := FALSE;
                { Skip if this block follows a previous block immediately }
                if PrevInlineEndOffset <> I - 1 then
                begin
                    Inc(R);
                    Result[R] := ' ';
                end;
            end;
            while I <= NextInlineEndOffset do
            begin
                if (Value[I] in [#1, #10, #13]) then
                begin
                    if I > J then
                    begin
                        Move(Value[J], Result[R + 1], I - J);
                        Inc(R, I - J);
                    end;
                    Inc(I);
                    J := I;
                end
                else
                    Inc(I);
            end;
            if I > J then
            begin
                Move(Value[J], Result[R + 1], I - J);
                Inc(R, I - J);
            end;
            J := I;
            PrevInlineEndOffset := NextInlineEndOffset;
            NextInlineEndOffset := GetNextInlineEnd;
        end;
    end;
    if L >= J then
    begin
        Move(Value[J], Result[R + 1], L - J + 1);
        Inc(R, L - J + 1);
    end;
    if R <> L + 1 then
        SetLength(Result, R);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnfoldHdrValue(const Value: AnsiString) : AnsiString;
begin
    Result := UnfoldHdrValue(PAnsiChar(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineUUDecode; { ##ERIC }
var
    count, Size : Integer;
    s           : AnsiString;
    out1        : AnsiString;
    bp          : PAnsiChar;
    pos1        : Integer;
begin
    if FCurrentData^ = #0 then
        exit;
    s := IcsStrPas(FCurrentData);

    if IcsLowerCase(copy(s, 1, 6)) = 'begin ' then begin
        out1:=IcsLowerCase(s);
        if (Pos(AnsiString('--'), out1) > 0) and (Pos(AnsiString('cut here'), out1) > 0) then
            Exit;
        pos1 := Pos(AnsiString(' '), s);
        s    := Copy(s, pos1 + 1, 255);
        pos1 := Pos(AnsiString(' '), s);
        s    := Copy(s, pos1 + 1, 255);
        cUUFilename := s;
        exit;
    end
    else if IcsLowerCase(Copy(s, 1, 3)) = 'end' then begin
        out1 := IcsLowerCase(s);
        if (Pos(AnsiString('--'), out1) > 0) and (Pos(AnsiString('cut here'), out1) > 0) then
            Exit;
        cUUFilename := '';
        exit;
    end;

    { if no filename defined yet, exit }
    if cUUFilename = '' then
        exit;

    { decode the line }
    count := UUDec(s[1]);
    Size  := Count;
    if count > 0 then begin
        bp := @s[2];
        repeat
            UUOutDec(bp, count, out1);
            count := count - 3;
            bp    := bp + 4;
        until count <= 0;
    end;

    { we're done. copy and leave }
    Move(Out1[1], FCurrentData[0], Size);
    ProcessDecodedLine(FCurrentData, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UUSectionBegin(
    const Line     : AnsiString;
    var   FileName : AnsiString) : Boolean;
var
    I : Integer;
begin
    { A UUEncoded section begins by a line having the syntax:               }
    {   "begin nnn filename" with xxx being a number (unix file permission) }
    { We accept xxx with at least 2 digits. Filename is optional.           }
    Result   := FALSE;
    FileName := '';
    { AS: "begin" _must_ be in lower case !                                 }
    if Copy(Line, 1, 6) = 'begin ' then begin
        I := 7;
        while I <= Length(Line) do begin
            if Line[I] = ' ' then begin
                Result := (I > 8);
                if Result then
                    FileName := Copy(Line, I + 1, Length(Line));
                break
            end;
            if not IsCharInSysCharSet(Line[I], ['0'..'9']) then
                break;
            Inc(I)
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ AS: YEnc support routine                                                  }
procedure TMimeDecode.ParseYBegin(const Ch : AnsiString);
var
    I, J : Integer;
begin
    { AS: Line format is "=ybegin line=128 size=XXXX name=YYYY";            }
    FSizeFileY := 0;
    FSizeBlocY := 0;
    I          := 9;
    while I < Length(Ch) do begin
        if Copy(Ch, I, 5) = 'line=' then begin
            I := I + 5;
            while IsCharInSysCharSet(Ch[I], ['0'..'9']) do begin
                FSizeBlocY := 10 * FSizeBlocY + (Ord(Ch[I]) - Ord('0'));
                Inc(I);
            end;
        end
        else if Copy(Ch, I, 5) = 'size=' then begin
            I := I + 5;
            while IsCharInSysCharSet(Ch[I], ['0'..'9']) do begin
                FSizeFileY := 10 * FSizeFileY + (Ord(Ch[I]) - Ord('0'));
                Inc(I);
            end;
        end
        else if Copy(Ch, I, 5) = 'name=' then begin
            I := I + 5;
            J := I;
            repeat
                while (J <= Length(Ch)) and (Ch[J] <> ' ') do
                    Inc(J);
                if (J >= Length(Ch)) or (Ch[J + 1] = '=') then
                    break
                else
                    Inc(J);
            until FALSE;
            cUUFilename := Copy(Ch, I, J - I);
            I           := J;
        end;
        Inc(I);
    end;
    FSizeLeftY := FSizeFileY;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecode.UUProcessLine(FCurrentData: PAnsiChar): Boolean;
var
    S           : AnsiString;
    out1        : AnsiString;
    count       : Integer;
    bp          : PAnsiChar;
    chName      : AnsiString;
    I, C        : Integer;
begin
    Result := TRUE;
    S := IcsStrPas(FCurrentData); { AS }
    if IcsTrim(S) = '' then begin
        Result := FALSE;
        Exit;
    end;

    if (not FUUProcessFlag) and UUSectionBegin(S, chName) then begin { AS }
        if chName <> '' then                                         { AS }
            cUUFilename := chName;                                   { AS }
        out1 := IcsLowerCase(S);
        if (Pos(AnsiString('--'), out1) > 0) and (Pos(AnsiString('cut here'), out1) > 0) then
            Exit;
        FUUProcessFlag := TRUE;
        FProcessFlagYBegin := false;
        TriggerInlineDecodeBegin(cUUFilename);
        Exit;
    end;
    { AS: Handle YEnc }
    if (not FUUProcessFlag) and (Copy(S, 1, 8) = '=ybegin ') then begin
        { Line format : "=ybegin line=128 size=XXXX name=YYYY"; }
        ParseYBegin(S);
        FUUProcessFlag     := TRUE;
        FProcessFlagYBegin := TRUE;
        TriggerInlineDecodeBegin(cUUFilename);
        Exit;
    end;

    if not FUUProcessFlag then begin
        Result := FALSE;
        Exit;
    end;

    if IcsCompareText(Copy(S, 1, 3), AnsiString('end')) = 0 then begin
        out1 := IcsLowerCase(S);
        if (Pos(AnsiString('--'), out1) > 0) and (Pos(AnsiString('cut here'), out1) > 0) then
            Exit;
        FUUProcessFlag := FALSE;
        { I also use the filename here in case the client prefer to save   }
        { data to a stream and save to a file when the decoding is complete }
        TriggerInlineDecodeEnd(cUUFileName);
        cUUFilename := '';
        Exit;
    end;

    { AS: Handle YEnc }
    if IcsCompareText(Copy(S, 1, 6), AnsiString('=yend ')) = 0 then begin
        FUUProcessFlag := FALSE;
        FProcessFlagYBegin := false;
        { I also use the filename here in case the client prefer to save   }
        { data to a stream and save to a file when the decoding is complete }
        TriggerInlineDecodeEnd(cUUFilename);
        cUUFilename := '';
        Exit;
    end;

    if IcsCompareText(Copy(S, 1, 7), AnsiString('=ypart ')) = 0 then begin
        { The message is in several parts. Something to do ? }
        Exit;
    end;

    if FInlineDecodeLine or Assigned(FOnInlineDecodeLine) then begin
        { decode the line }
        { AS: Handle YEnc }
        if not FProcessFlagYBegin then begin
            Count := UUDec(S[1]);
            out1  := ''; { AS: 25/11/2002 }

           {AS : new method to ignore wrongly coded lines }
           I := Length(S) - 1;
           if (Count > 0) and (Length(S) > 1) then begin
               bp := @S[2];
               repeat
                   UUOutDec(bp, Count, out1);
                   if Count >= 3 then begin
                       Count := Count - 3;
                       I     := I - 4;
                   end
                   else begin
                       if I >= 4 then
                           I := I - 4
                       else if I > 0 then
                           I := 0;
                       Count := 0;
                   end;
                   bp    := bp + 4;
               until Count <= 0;
               if I <> 0 then
                   out1 := '';
           end;
{ Old code
           if (Count > 0) and (Length(S) > 1) then begin
               bp := @S[2];
               repeat
                   UUOutDec(bp, Count, out1);
                   Count := Count - 3;
                   bp    := bp + 4;
               until Count <= 0;
           end;
}
        end
        else begin { AS: Handle YEnc }
            out1 := '';
            I    := 0;
            bp   := FCurrentData;
            while (I < FSizeBlocY) and (bp[I] <> #0) do begin
                if bp[I] = '=' then begin
                    C := Byte(bp[I + 1]) - 64 - 42;
                    Inc(I);
                end
                else
                    C := byte(bp[I]) - 42;
                if C < 0 then
                    C := C + 256;
                out1 := out1 + AnsiChar(C);
                Inc(I);
            end;
        end;
        TriggerInlineDecodeLine(PAnsiChar(Out1), Length(Out1));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PAnsiChar) : PAnsiChar;
begin
    Result := PValue;
    { AS: Add #1 which is used to handle header lines }
    while IsCrLf1OrSpaceChar(Result^) do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetValue(Src : PAnsiChar; var Dst : AnsiString; var Delim : AnsiChar) : PAnsiChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Delim  := Result^;
    if Delim = '"' then begin
        Inc(Result);
        while TRUE do begin
            Delim  := Result^;
            if Delim = #0 then
                break;
            if Delim = '"' then begin
                Inc(Result);
                Delim := Result^;
                break;
            end;
            Dst := Dst + Delim;
            Inc(Result);
        end;
    end
    else begin
        while TRUE do begin
            Delim  := Result^;
            if IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9, #0]) then
                break;
            Dst := Dst + IcsLowerCase(Result^);
            Inc(Result);
        end;
    end;
    if IsSpace(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then
            Inc(Result);
    end
    else if Delim <> #0 then
        Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(Src : PAnsiChar; var Dst : AnsiString; var Delim : AnsiChar) : PAnsiChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    while TRUE do begin
        Delim := Result^;
        if IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9, #0]) then
                break;
        Dst := Dst + IcsLowerCase(Result^);
        Inc(Result);
    end;
    if IsSpace(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then begin
            {AS: Take delimiter after space }
            Delim := Result^;
            Inc(Result);
        end;
    end
    else if Delim <> #0 then
        Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Same as GetToken, but take be aware of comments                           }
function GetTokenEx(Src : PAnsiChar; var Dst : AnsiString; var Delim : AnsiChar) : PAnsiChar;
var
    Comment: Integer;
begin
    Result  := StpBlk(Src);
    Dst     := '';
    Comment := 0;
    while TRUE do begin
        Delim := Result^;
        if Delim = #0 then
            break;
        if Delim = '(' then begin
            Inc(comment); { Comments can be nested }
            Inc(Result);
            Continue;
        end
        else if Delim = ')' then begin
            Dec(Comment);
            Inc(Result);
            Continue;
        end
        else if (Comment = 0) and
                IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9]) then
            break;
        Dst := Dst + IcsLowerCase(Result^);
        Inc(Result);
    end;
    if IsSpace(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then begin
            Delim := Result^;
            Inc(Result);
        end;
    end
    else if Delim <> #0 then
    Inc(Result);
    Result := StpBlk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetQuoted(Src : PAnsiChar; var Dst : AnsiString) : PAnsiChar;
var
    Quote : AnsiChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Quote  := Result^;
    if Quote <> #34 then begin  { ##ERIC }
        Dst := IcsStrPas(Src);     { ##ERIC }
        Exit;                   { ##ERIC }
    end;                        { ##ERIC }

    Inc(Result);
    while (Result^ <> #0) and (Result^ <> Quote) do begin
        Dst := Dst + Result^;
        Inc(Result);
    end;
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PreparePart;
begin
    FPartOpened    := FALSE;
    TriggerPartEnd;
    PrepareNextPart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessWaitBoundary;       { ##ERIC }
var
    T : Integer;
    S : AnsiString;
begin
    S := IcsLowerCase(IcsStrPas(FCurrentData));
    if S = FBoundary then begin
        FBoundaryFound := True;  { V8.03 }
        PreparePart;
        Exit;
    end
    else begin
        { are we in the embedded boundaries ? }
        for T := 0 to FEmbeddedBoundary.Count - 1 do begin
            if AnsiString(FEmbeddedBoundary[T]) = S then begin
                cIsEmbedded := true;
                PreparePart;
                Exit;
            end;
        end;
       { if not in primary boundary or embedded boundaries, then process it.}
       ProcessDecodedLine(FCurrentData, IcsStrLen(FCurrentData));
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.SetDefaultCodePage(const Value: LongWord);
begin
    if not IcsIsValidAnsiCodePage(Value) then
        raise Exception.Create('Code page "' + IntToStr(Value) + '"' +
            'is not a valid ANSI code page or currently not installed');
    FDefaultCodePage := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessMessageLine;
begin
    Inc(FLineNum);
    if FLineNum = 1 then begin
        FPartFirstLine := TRUE;
        TriggerPartBegin;
    end;
    if FEncoding = 'base64' then
        ProcessLineBase64
    else if FEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FEncoding = 'x-uuencode' then
        ProcessLineUUDecode                       { ##ERIC }
    else begin {tap}
        ProcessDecodedLine(FCurrentData, IcsStrLen(FCurrentData));
        ProcessDecodedLine(PAnsiChar(#13#10), 2); {tap: add \r\n to other encodings}
    end; {tap}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PrepareNextPart;
begin
    FApplicationType         := '';
    FIsTextpart              := TRUE;
    FPartCharset             := '';
    FPartCodePage            := FDefaultCodePage;
    FPartEncoding            := '';
    FPartContentType         := '';
    FPartDisposition         := '';
    FPartContentID           := '';
    FPartName                := '';
    FPartFileName            := '';
    FPartFormat              := '';
    FHeaderFlag              := TRUE;  { We begin by a header }
    FLineNum                 := 0;
    FUUProcessFlag           := FALSE;
    FProcessFlagYBegin       := FALSE;  { AS: Handle YEnc }
    FPartHeaderBeginSignaled := FALSE;
    FNext                    := ProcessPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartLine; { ##ERIC }
var
    Len : Integer;
    t   : Integer;
    s   : AnsiString;            { ##ERIC }
begin
    { Check if end of part (boundary line found) }
    if (FCurrentData <> nil) and (FCurrentData^ <> #0) then begin
        s := IcsLowerCase(IcsStrPas(FCurrentData));
        if (s = FBoundary) then begin
            FBoundaryFound := True;  { V8.03 }
            PreparePart;
            exit;
        end
        else if (s = (FBoundary + '--')) then begin
            FBoundaryFound := True;  { V8.03 }
            FEndOfMime := TRUE;
            PreparePart;
            exit;
        end
        else begin
            for t := 0 to FEmbeddedBoundary.Count - 1 do begin
                if (s = AnsiString(FEmbeddedBoundary[t])) or
                   (s = (AnsiString(FEmbeddedBoundary[t]) + '--')) then begin
                    { we now have to wait for the next part }
                    PreparePart;
                    exit;
                end
            end;
        end;
    end;

    if not FPartOpened then begin
        FPartOpened    := TRUE;
        FPartFirstLine := TRUE;
        TriggerPartBegin;
    end;

    if FPartEncoding = 'base64' then
        ProcessLineBase64
    else if FPartEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FPartEncoding = 'x-uuencode' then   { ##ERIC }
        ProcessLineUUDecode                     { ##ERIC }
    else begin
        if FCurrentData = nil then
            Len := 0
        else
            Len := IcsStrLen(FCurrentData);
        if FPartFirstLine then               { FP Nov 13, 2007 }
            FPartFirstLine := FALSE
        else
            ProcessDecodedLine(PAnsiChar(#13#10), 2);

        ProcessDecodedLine(FCurrentData, Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartHeaderLine;
var
    p       : PAnsiChar;
    Delim   : AnsiChar;
    Token   : AnsiString;
    KeyWord : AnsiString;
    Value   : AnsiString;
{   Value1  : AnsiString; }
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        { End of part header }
        if not FPartHeaderBeginSignaled then begin
            Inc(FPartNumber);
            TriggerPartHeaderBegin;
        end;
        TriggerPartHeaderEnd;
        FHeaderFlag        := FALSE;  { Remember we are no more in a header }
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        FNext              := ProcessPartLine;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then begin
        Inc(FPartNumber);
        FPartHeaderBeginSignaled := TRUE;
        TriggerPartHeaderBegin;
{       FEmbeddedBoundary.clear; }
    end;

    { A header line can't begin with a space nor tab char. If we got that }
    { then we consider the header as begin finished and process line      }
    if FHeaderFlag and IsSpace(FCurrentData[0]) then begin
        TriggerPartHeaderEnd;
        FHeaderFlag        := FALSE;
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        FNext              := ProcessPartLine;
        ProcessPartLine;
        Exit;
    end;

    p := GetToken(FCurrentData, KeyWord, Delim);
    if KeyWord = 'content-type' then begin
        p := GetTokenEx(p, FPartContentType, Delim);
        if Pos(AnsiString('application/'), FPartContentType) = 1 then
        begin
            FApplicationType := Copy(FPartContentType, 13, MaxInt);
            FIsTextpart := False; { V8.02 }
        end
        else
            FIsTextpart := Pos(AnsiString('text'), FPartContentType) = 1;
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetValue(p, Value, Delim);
                if Token = 'name' then
                    FPartName := UnfoldHdrValue(Value)
                else if Token = 'charset' then begin
                    FPartCharset := Value;
                    if not MimeCharsetToCodePageEx(CsuString(FPartCharset),
                                                   FPartCodePage) then
                        FPartCodePage := FDefaultCodePage;
                end
                else if Token = 'format' then
                    FPartFormat := Value
                else if Token = 'boundary' then begin
                    { we have an embedded boundary }
                    FEmbeddedBoundary.Add('--' + IcsLowerCase(String(Value)));
{                   Value := Value + #0;  }{ NUL terminate AnsiString for Delphi 1 }
{                   GetQuoted(@Value[1], Value1);}                    { ##ERIC }
{                   FEmbeddedBoundary.Add('--' + LowerCase(Value1));} { ##ERIC }
                end;                                                  { ##ERIC }
            end;
        end;
    end
    else if KeyWord = 'content-transfer-encoding' then begin
        GetTokenEx(p, FPartEncoding, Delim);
    end
    else if KeyWord = 'content-id' then begin
        FPartContentID := IcsStrPas(p);
        if (Length(FPartContentID) >= 2) and
           (FPartContentID[1] = '<') and
           (FPartContentID[Length(FPartContentID)] = '>') then
               FPartContentID := Copy(FPartContentID, 2, Length(FPartContentID) - 2);
    end
    else if KeyWord = 'content-disposition' then begin
        p := GetTokenEx(p, FPartDisposition, Delim);
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                if p^ = #34 then     { V8.04 support non-delimited file names }
                  p := GetQuoted(p, Value)
                else
                  p := GetToken(p, Value, Delim);  { V8.04 }
                if Token = 'filename' then
                    FPartFileName := UnfoldHdrValue(Value);
            end;
        end;
    end
    else if (KeyWord = 'content-description') and (FPartFileName = '') then begin
        Delim:= ';';
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetQuoted(p, Value);
                if Token = 'filename' then
                    FPartFileName :=  UnfoldHdrValue(Value);
            end;
        end;
    end;

    TriggerPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessHeaderLine;
var
    p     : PAnsiChar;
    pVal  : PAnsiChar;
    Delim : AnsiChar;
    Token : AnsiString;
    Value : AnsiString;
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        FHeaderFlag        := FALSE;  { We are no more in a header }
        TriggerHeaderEnd;
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        if (FBoundary = '') or (not FBoundaryFound) then  { V8.03 }
            FNext := ProcessMessageLine
        else begin
            FPartFirstLine := TRUE;
            TriggerPartBegin;
            FNext          := ProcessWaitBoundary;
        end;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then
        TriggerHeaderBegin;

    p    := GetToken(FCurrentData, Token, Delim);
    pVal := StpBlk(p);
    if Delim = ':' then begin
        p := GetTokenEx(p, Value, Delim);
        if Token = 'from' then
            FFrom := UnfoldHdrValue(pVal)
        else if Token = 'to' then
            FDest := UnfoldHdrValue(pVal)
        else if Token = 'cc' then
            FCc := UnfoldHdrValue(pVal)
        else if Token = 'subject' then
            FSubject := UnfoldHdrValue(pVal)
        else if Token = 'return-path' then begin
            FReturnPath := UnfoldHdrValue(pVal);
            if (Length(FReturnPath) >= 2) and
               (FReturnPath[1] = '<') and
               (FReturnPath[Length(FReturnPath)] = '>') then
                FReturnPath := Copy(FReturnPath, 2, Length(FReturnPath) - 2);
        end
        else if Token = 'date' then
            FDate := UnfoldHdrValue(pVal)
        else if Token = 'mime-version' then
            FMimeVersion := UnfoldHdrValue(pVal)
        else if Token = 'content-type' then begin
            FContentType := Value;
            while Delim = ';' do begin
                p := GetToken(p, Token, Delim);
                if Delim = '=' then begin
                    p := GetValue(p, Value, Delim);
                    if Token = 'name' then
                        FHeaderName := Value
                    else if Token = 'charset' then begin
                        FCharset := Value;
                        if not MimeCharsetToCodePage(CsuString(FCharset),
                                                 FCodePage) then
                            FCodePage := FDefaultCodePage;
                    end
                    else if Token = 'format' then
                        FFormat := Value
                    else if Token = 'boundary' then begin
                        FBoundary := '--' + IcsLowerCase(Value);
                        FIsMultipart := TRUE;
                        if (Pos (AnsiString('multipart'), IcsLowerCase (FContentType)) = 1) then
                                                                   FBoundaryFound := True; { V8.03 }
                    end;             { ##ERIC }
                end;
            end;
        end
        else if Token = 'content-transfer-encoding' then
            FEncoding := Value
        else if Token = 'content-disposition' then begin
            FDisposition := Value;
            while Delim = ';' do begin
                p := GetToken(p, Token, Delim);
                if Delim = '=' then begin
                    p := GetValue(p, Value, Delim);
{                   p := GetQuoted(p, Value);}
                    if Token = 'filename' then
                        FFileName := UnfoldHdrValue(Value);
                end
            end
        end
    end;
    FLengthHeader := FLengthHeader + Integer(IcsStrLen(FCurrentData)) + 2;
    FHeaderLines.Add(String(UnfoldHdrValue(FCurrentData)));
    TriggerHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageEnd;
begin
    if (FBoundary = '') or FPartOpened or (NOT FBoundaryFound) then { V8.03 don't ignore body without a boundary }
        TriggerPartEnd;
    TriggerMessageEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageBegin;
begin
    FIsTextPart              := TRUE; 
    FApplicationType         := '';
    FBoundary                := '';
    FBoundaryFound           := false;             { V8.03  }
    FCharset                 := 'us-ascii';        { V8.02 }
    FCodePage                := FDefaultCodePage;
    FContentType             := 'text/plain';      { V8.02 }
    FCurrentData             := nil;
    FDate                    := '';
    FDest                    := '';
    FDisposition             := '';
    FEncoding                := '7bit';            { V8.02 }
    FEndOfMime               := FALSE;
    FFileName                := '';
    FFormat                  := '';
    FFrom                    := '';
    FCc                      := '';
    FHeaderFlag              := TRUE;
    FHeaderName              := '';
    FIsMultiPart             := FALSE;
    FLineNum                 := 0;
    FMimeVersion             := '';
    FNext                    := ProcessHeaderLine;
    FPartContentType         := '';
    FPartCharset             := '';
    FPartContentID           := '';
    FPartDisposition         := '';
    FPartEncoding            := '';
    FPartFileName            := '';
    FPartFormat              := '';
    FPartHeaderBeginSignaled := FALSE;
    FPartName                := '';
    FPartNumber              := 0;
    FPartOpened              := FALSE;
    FReturnPath              := '';
    FSubject                 := '';
    FUUProcessFlag           := FALSE;
    FProcessFlagYBegin       := FALSE;
    FHeaderLines.Clear;
    FEmbeddedBoundary.Clear;
    FLengthHeader := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeFile(const FileName : String);
var
    aStream  : TStream;
begin
    aStream  := TFileStream.Create(FileName, fmOpenRead);
    try
        DecodeStream(aStream);
    finally
        aStream.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeStream(aStream : TStream);
begin
    FBufferSize := 2048;      { Start with a reasonable FBuffer }
    GetMem(FBuffer, FBufferSize);
    try
        cUUFilename       := '';                    { ##ERIC }
        FEmbeddedBoundary := TStringList.Create;    { ##ERIC }
        try
            InternalDecodeStream(aStream);
        finally
            FEmbeddedBoundary.Free;                 { ##ERIC }
        end;
    finally
        FreeMem(FBuffer, FBufferSize);
        FBuffer     := nil;
        FBufferSize := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This routine use an intelligent buffer management, trying to move data    }
{ the less possible times. The buffer is enlarged as necessary to contains  }
{ the largest line we encounter.                                            }
procedure TMimeDecode.InternalDecodeStream(aStream : TStream);
var
    RdCnt   : LongInt;
    nUsed   : Integer;
    nStart  : Integer;
    nLast   : Integer;
    nSearch : Integer;
    I, J    : Integer;
begin
    nUsed  := 0;
    nStart := 0;
    MessageBegin;
    while TRUE do begin
        nSearch := nStart + nUsed;
        RdCnt   := aStream.Read(FBuffer[nSearch],
                                FBufferSize - nUsed - nStart -
                                2);  { for next char and #0 }
        if RdCnt <= 0 then begin
            break;
        end;

        nUsed  := nUsed + RdCnt;
        nLast  := nStart + nUsed;

        { Nul terminate the FBuffer }
        FBuffer[nLast] := #0;

        { Search for terminating line feed }
        while TRUE do begin
            I := nSearch;
            while (I < nLast) and (FBuffer[I] <> #10) do
                Inc(I);
            if I >= nLast then begin
                { We did'nt find any LF in the FBuffer, need to read more ! }
                if nStart > (3 * (FBufferSize div 4)) then begin
                    { Reuse start of FBuffer because 3/4 buffer is unused   }
                    Move(FBuffer[nStart], FBuffer[0], nUsed + 1);
                    nStart := 0;
                end
                else begin
                    { Makes FBuffer larger }
                    ReallocMem(FBuffer, FBufferSize + 32);
                    FBufferSize := FBufferSize + 32;
                end;
                break;
            end;

            { We found a line feed, process FBuffer up to this point }
            { Remove any preceding CR                               }
            if (I > nStart) and (FBuffer[I - 1] = #13) then
                J := I - 1
            else
                J := I;

            { We found a LF, if we are processing a header, we must     }
            { have the next character to see if the line is continuated }
            if FHeaderFlag then begin
                if I >= (nLast - 1) then begin
                    { We don't have the next character in our FBuffer, }
                    { we need to read more data                        }
                    { Read a single byte at the end of the FBuffer     }
                    { We have room because we preserved it previously  }
                    RdCnt := aStream.Read(FBuffer[I + 1], 1);
                    if RdCnt > 0 then begin
                        { We have read the next char }
                        Inc(nLast);
                        Inc(nUsed);
                        FBuffer[I + 2] := #0;
                    end;
                end;

                if I < nLast then begin
                    if (not IsCrLf(FBuffer[nStart])) and  { 27/08/98 }
                       IsSpace(FBuffer[I + 1]) then begin
                        { We have a continuation line, replace CRLF +  }
                        { [TAB or Space] by #1 which will be handled   }
                        { in UnfoldHdrValue. Comment adjusted AG V7.16 }
                        FBuffer[I] := #1;
                        FBuffer[J] := #1;
                        //if FBuffer[I + 1] = #9 then       { AG V7.16 }
                            FBuffer[I + 1] := #1;
                        nSearch   := I;
                        { and search new line end }
                        continue;
                    end;
                end;
            end;

            FBuffer[J]   := #0;
            FCurrentData := FBuffer + nStart;

            FNext;
            FBuffer[J] := #10;         {tap: ERROR ? #13}
            nStart     := I + 1;
            nUsed      := nLast - nStart;
            nSearch    := nStart;
        end;
    end;
    { Process the last line }
    if nUsed > 0 then begin
        FCurrentData := FBuffer + nStart;
        FNext;
    end;

    { vxx see if reached end of file before end of part }
    if FPartOpened then TriggerPartEnd;

    MessageEnd;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V7.11 Extended MIME decoding that save all parts into arrays }
constructor TMimeDecodeEx.Create(Aowner:TComponent);
begin
    inherited Create(AOwner);
    FDecodeW := TMimeDecodeW.Create (Self);
    FHeaderLines := TStringList.Create;
    FMaxParts := 10 ;
    FSkipBlankParts := false ;
    FDecodeW.OnHeaderLine := MimeDecodeHeaderLine ;
    FDecodeW.OnPartBegin := MimeDecodePartBegin ;
    FDecodeW.OnPartEnd := MimeDecodePartEnd ;
    PartInfos := Nil ;
    WideHeaders := Nil ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMimeDecodeEx.Destroy;
begin
    Reset;
    FreeAndNil (FHeaderLines);
    FreeAndNil (FDecodeW);
    inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ reset all variables and arrays }
procedure TMimeDecodeEx.Reset ;
var
    I: integer ;
begin
    FTotParts := 0 ;
    FDecParts := 0 ;
    FTotHeaders := 0 ;
    FHeaderCharset := '' ;
    FHeaderLines.Clear ;
    if Length (PartInfos) > 0 then
    begin
        for I := 0 to Pred (Length (PartInfos)) do
            FreeAndNil (PartInfos[I].PartStream) ;
    end ;
    SetLength (PartInfos, 0) ;
    SetLength (WideHeaders, 0) ;
    FDecodeW.DestStream := Nil ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Initialise arrays and streams ready to decode MIME }
procedure TMimeDecodeEx.Initialise ;
var
    I: integer ;
begin
    Reset ;
    SetLength (PartInfos, 4) ;  // initial size only
    for I := 0 to Length (PartInfos) - 1 do
                PartInfos [I].PartStream := TMemoryStream.Create ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeEx.MimeDecodeHeaderLine(Sender: TObject);
begin
    FHeaderLines.Add (string(FDecodeW.CurrentData));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ found a new MIME part, extend array if necessary, set PartStream ready for content }
procedure TMimeDecodeEx.MimeDecodePartBegin(Sender: TObject);
var
    I, oldparts: integer ;
begin
    oldparts := Length (PartInfos) ;
    if FDecParts >= oldparts then
    begin
        if FDecParts >= FMaxParts then  // ignore more parts
        begin
            FDecodeW.DestStream := Nil ;
            exit ;
        end ;
        SetLength (PartInfos, FDecParts * 2) ;
        for I := oldparts to Length (PartInfos) - 1 do
            PartInfos [I].PartStream := TMemoryStream.Create ;
    end;
    PartInfos [FDecParts].PartStream.Clear ;
    FDecodeW.DestStream := PartInfos [FDecParts].PartStream ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ fisished reading MIME part, reset PartStream to start and keep stuff about part }
procedure TMimeDecodeEx.MimeDecodePartEnd(Sender: TObject);
begin
    if (FDecParts >= FMaxParts) or
                (FDecParts >= Length (PartInfos)) then  // ignore extra parts
    begin
        inc (FTotParts) ;
        exit ;
    end ;
    with PartInfos [FDecParts] do
    begin
        PartStream.Seek (soFromBeginning, 0) ;
        PSize := PartStream.Size ;
        if FDecodeW.PartNumber = 0 then  // main body
        begin
            if FSkipBlankParts and (Pos (AnsiString('multipart'), IcsLowerCase (FDecodeW.ContentType)) = 1) then exit ;
            PContentType := FDecodeW.ContentType ;
            PCharset     := FDecodeW.Charset ;
            PCodePage    := FDecodeW.CodePage;
            PApplType    := FDecodeW.ApplicationType ;
            PName        := DecodeMimeInlineValue (FDecodeW.HeaderName) ;
            PEncoding    := FDecodeW.Encoding ;
            PDisposition := FDecodeW.Disposition ;
            PFileName    := DecodeMimeInlineValue (FDecodeW.FileName) ;
            PContentId   := FDecodeW.FPartContentID ; {V7.18 Bjørnar}
            PIsTextpart  := FDecodeW.FIsTextpart ;    {V7.20 Angus }
        end
        else
        begin           // real part
            PContentType := FDecodeW.PartContentType ;
            PCharset     := FDecodeW.PartCharset ;
            PCodePage    := FDecodeW.PartCodePage;
            PApplType    := FDecodeW.ApplicationType ;
            PName        := DecodeMimeInlineValue (FDecodeW.PartName) ;
            PEncoding    := FDecodeW.PartEncoding ;
            PDisposition := FDecodeW.PartDisposition ;
            PFileName    := DecodeMimeInlineValue (FDecodeW.PartFileName) ;
            PContentId   := FDecodeW.FPartContentID ; {V7.18 Bjørnar}
            PIsTextpart  := FDecodeW.FIsTextpart ;    {V7.20 Angus }
        end ;
        if FSkipBlankParts then
        begin
            if PContentType = '' then exit ;
            if PSize = 0 then exit ;
        end;
        inc (FDecParts) ;
        inc (FTotParts)
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ decode headers to Unicode }
procedure TMimeDecodeEx.Finalise ;
var
    I: integer ;
    CharSet: AnsiString;
begin
    FTotHeaders := FHeaderLines.Count ;
    SetLength (WideHeaders, FTotHeaders) ;
    for I := 0 to FTotHeaders - 1 do
    begin
         WideHeaders [I] := DecodeMimeInlineValueEx
                            (UnfoldHdrValue (AnsiString(FHeaderLines [I])), CharSet) ;
        if FHeaderCharset = '' then FHeaderCharset := CharSet ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V7.11 Decode MIME file into PartInfos and PartStreams arrays with TotParts }
procedure TMimeDecodeEx.DecodeFileEx (const FileName: String);
begin
    Initialise ;
    FDecodeW.DecodeFile (FileName) ;
    Finalise ;
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V7.11 Decode MIME stream into PartInfos and PartStreams arrays with TotParts }
procedure TMimeDecodeEx.DecodeStreamEx (aStream: TStream);
begin
    Initialise ;
    FDecodeW.DecodeStream (aStream) ;
    Finalise ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeEx.GetPartInfo (Index: Integer): TPartInfo ;
begin
    if Index >= Length (PartInfos) then exit;
    result := PartInfos [Index] ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCrLf1Char(Ch : AnsiChar) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13) or
              (Ch = #1);                     // #1 is used to handle line break
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCrLf1OrSpaceChar(Ch : AnsiChar) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13) or
              (Ch = #1) or                  // #1 is used to handle line break
              (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ TMimeDecodeW }

function DecodeQuotedPrintable(const S: AnsiString) : AnsiString;
var
    I, J, L : Integer;
begin
    I      := 1;
    J      := 0;
    L      := Length(S);
    SetLength(Result, L);
    while I <= L do begin
        while (I <= L) and (S[I] <> '=') do begin
            Inc(J);
            if S[I] = '_' then
                Result[J] := ' '
            else
                Result[J] := S[I];
            Inc(I);
        end;
        if I >= L then
            Break;
        Inc(I);
        Inc(J);
        Result[J] := AnsiChar(htoi2(PAnsiChar(@S[I])));
        Inc(I, 2);
    end;
    SetLength(Result, J);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Parameter "Value" MUST NOT contain a folded header line. Pass the result  }
{ of UnfoldHdrValue() if the string still contains CRLF+[TAB or Space] or   }
{ dummy control characters (#1).                                            }
function DecodeMimeInlineValue(const Value : AnsiString): UnicodeString;
var
    BeginEnc, BeginType, L, I, J: Integer;
    EncType : AnsiChar;
    CharSet, S: AnsiString;
    CP : LongWord;
begin
    L := Length(Value);
    EncType   := #0;
    BeginEnc  := 0;
    BeginType := 0;
    I         := 1;
    J         := I;
    Result    := '';
    CharSet   := '';
    CP        := IcsSystemCodePage;

    while I <= L do begin
        if (BeginEnc = 0) then begin
            if (Value[I] = '=') and (I + 1 <= L) and
               (Value[I + 1] = '?') then begin
                BeginEnc := I;
                if I > J then begin
                    S := Copy(Value, J, I - J);
                    //Result := Result + AnsiToUnicode(S, CP);
                    Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
                    J := I + 1;
                end;
                Inc(I);
            end;
        end
        else if BeginType = 0 then begin
            if (Value[I] = '?') and (I + 2 <= L) and
               (Value[I + 2] = '?') then begin
                BeginType := I;
                CharSet := Copy(Value, BeginEnc + 2, BeginType - (BeginEnc + 2));
                //CP := MimeCharsetToCodePageDef(CsuString(CharSet));
                CP := MimeCharsetToCodePageExDef(CsuString(CharSet));
                EncType := Value[I + 1];
                //CharLowerA(@EncType);
                IcsCharLowerA(EncType);
                Inc(I, 2);
            end;
        end
        else begin
            if (Value[I] = '?') and (I + 1 <= L) and
               (Value[I + 1] = '=') then begin
                case EncType of
                'q' :
                    begin
                        S := Copy(Value, BeginType + 3, I - (BeginType + 3));
                        S := DecodeQuotedPrintable(S);
                    end;
                'b' :
                    begin
                        S := Copy(Value, BeginType + 3, I - (BeginType + 3));
                        S := Base64Decode(S);
                    end;
                else
                    S := Copy(Value, BeginEnc, I - BeginEnc);
                end;
                //Result := Result + AnsiToUnicode(S, CP);
                Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
                EncType   := #0;
                BeginEnc  := 0;
                BeginType := 0;
                CP        := IcsSystemCodePage;
                Inc(I);
                J := I + 1;
            end;
        end;
        Inc(I);
    end;
    if (L >= J) then begin  { AG 7.16 }
        S := Copy(Value, J, MaxInt);
        //Result := Result + AnsiToUnicode(S, CP);
        Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Parameter "Value" MUST NOT contain a folded header line. Pass the result  }
{ of UnfoldHdrValue() if the string still contains CRLF+[TAB or Space] or   }
{ dummy control characters (#1).                                            }
function DecodeMimeInlineValueEx(const Value : AnsiString; var CharSet: AnsiString): UnicodeString;
var
    BeginEnc, BeginType, L, I, J: Integer;
    EncType : AnsiChar;
    S: AnsiString;
    CP : LongWord;
begin
    L := Length(Value);
    EncType   := #0;
    BeginEnc  := 0;
    BeginType := 0;
    I         := 1;
    J         := I;
    Result    := '';
    CharSet   := '';
    CP        := IcsSystemCodePage;

    while I <= L do begin
        if (BeginEnc = 0) then begin
            if (Value[I] = '=') and (I + 1 <= L) and
               (Value[I + 1] = '?') then begin
                BeginEnc := I;
                if I > J then begin
                    S := Copy(Value, J, I - J);
                    //Result := Result + AnsiToUnicode(S, CP);
                    Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
                    J := I + 1;
                end;
                Inc(I);
            end;
        end
        else if BeginType = 0 then begin
            if (Value[I] = '?') and (I + 2 <= L) and
               (Value[I + 2] = '?') then begin
                BeginType := I;
                CharSet := Copy(Value, BeginEnc + 2, BeginType - (BeginEnc + 2));
                //CP := MimeCharsetToCodePageDef(CsuString(CharSet));
                CP := MimeCharsetToCodePageExDef(CsuString(CharSet));
                EncType := Value[I + 1];
                //CharLowerA(@EncType);
                IcsCharLowerA(EncType);
                Inc(I, 2);
            end;
        end
        else begin
            if (Value[I] = '?') and (I + 1 <= L) and
               (Value[I + 1] = '=') then begin
                case EncType of
                'q' :
                    begin
                        S := Copy(Value, BeginType + 3, I - (BeginType + 3));
                        S := DecodeQuotedPrintable(S);
                    end;
                'b' :
                    begin
                        S := Copy(Value, BeginType + 3, I - (BeginType + 3));
                        S := Base64Decode(S);
                    end;
                else
                    S := Copy(Value, BeginEnc, I - BeginEnc);
                end;
                //Result := Result + AnsiToUnicode(S, CP);
                Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
                EncType   := #0;
                BeginEnc  := 0;
                BeginType := 0;
                CP        := IcsSystemCodePage;
                Inc(I);
                J := I + 1;
            end;
        end;
        Inc(I);
    end;
    if (L >= J) then begin  { AG 7.16 }
        S := Copy(Value, J, MaxInt);
        //Result := Result + AnsiToUnicode(S, CP);
        Result := Result + IcsBufferToUnicode(Pointer(S)^, Length(S), CP);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetCcW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FCc);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetDestW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FDest);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetFileNameW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetFromW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FFrom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetPartFileNameW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FPartFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetPartNameW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FPartName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecodeW.GetSubjectW: UnicodeString;
begin
    Result := DecodeMimeInlineValue(FSubject);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeW.TriggerPartBegin;
begin
    if FPartNumber = 0 then
    begin
        if not FIsMultipart then
        begin
            { Init part #0 with message header values }
            FPartCharset     := FCharset;
            FPartCodePage    := FCodePage;
            FPartContentType := FContentType;
            FPartEncoding    := FEncoding;
            FPartFormat      := FFormat;
            FPartDisposition := FDisposition;
            FPartName        := FHeaderName;
            FPartFileName    := FFileName;
            FIsTextPart := (Length(FContentType) = 0) or
                       (Pos(AnsiString('text'), FContentType) = 1);
        end;
    end;
    inherited;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.


