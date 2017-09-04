{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  One Time Password support functions, see RFC2289/1938 (aka S/KEY)
Creation:     12 November 2007
Updated:      22 May 2012
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2012 by François PIETTE
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
16 Nov 2007 - 1.00 baseline Angus
12 May 2008 - 1.01 Uses OverbyteIcsUtils.pas for atoi
06 Aug 2008 - 1.02 Changed two strings to AnsiStrings so it works under Delphi 2009
5 Nov 2008  - 1.03 added OtpGetMethod, OtpKeyNames public
15 Apr 2011 - 1.04 Arno prepared for 64-bit, use functions from OverbyteIcsUtils.
14 Aug 2011 - 1.05 Arno fixed a bug that showed up since Delphi XE only and made
              two small optimizations.
Feb 29, 2012  1.06 Arno - Use IcsRandomInt
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


Background:

There is a test and demo application OverbyteIcsOneTimePassword for this unit.

See RFC2289, RFC1938 (old), the MD4 version was called S/KEY RFC1760

RFC2444 describes System Authentication and Secure Logon (SASL) using OTP-EXT
RFC2243, which is not yet supported here

One Time Passwords (also called S/Key) are a means of protecting passwords
from being logged or sniffed while on route to a server.  This is done by
the server generated information that is sent to the client, combined with
the clear password to create a 64-bit one-way hash digest called a One Time
Password, that is then returned to the server instead of the clear password.
The server then calculates the same OTP from it's known clear password
which should match the one created by the client assuming both clear
passwords are the same.  The OTP is usually sent by the client in a format
called 'six words' where combinations of six words taken from a 2,048 word 
dictionary encode the 64-bit password.  This is historic because words are
easier to type than a long hex string in a manual client. 

Note that One Time Password simply protect the password being sent, not any
subsequent data, but do prevent the clear password being stolen for later
logon to the server.  One Time Passwords are typically supported by FTP
and mail servers, but may be used with any telnet type protocol.

This unit contains two main functions used in servers to generate the
challenge and test the one time password returned, and one function
for the client to process the challenge and create the one time password.

Note the server needs to save the hash method (MD5, MD4 or SHA1), sequence
number and seed word with the account details of each logon, and update the
sequence number after each logon so the same sequence and seed are never
repeated.

RFC2289 states the clear password should be between 10 and 63 characters
long and OtpIsValidPassword may be used to check this, but the minimum
length is NOT enforced in these functions.


OtpCreateChallenge - server software
The first function used in server software to create a password challenge once
an account logon name from a client has been checked.  The challenge comprises
the hash method, sequence number and seed and is returned to the client.  For
a new account set the sequence to -1 to initialise it and create a new random
seed, but the sequence and seed must then be saved with the account details
for checking when the one time password calculated from them is returned by
the client.

Example Challenges:
otp-md5 99 test
otp-md4 99 test
otp-sha1 99 test

OtpProcessChallenge - client software
The main function used in client software to generate a one time password from
the challenge sent by the server in response to an account logon name, using
the clear account password.  The OTP is usually sent in 'six words' format,
but there is an option here to send a hex OTP instead.

Example One Time Passwords:
BAIL TUFT BITS GANG CHEF THY
50FE1962C4965880

OtpTestPassword - server software
The second function used in server software to test the one time password
returned by the client.  The server calculates a OTP from the account clear
password and the same information sent in the challenge (which was stored
with the account details) and then compares it with the one sent by the
client (in six word and hex formats) to check for a valid login.  If valid,
the sequence number is decremented and should be saved with the account
details so the same OTP is not generated during the next login.

}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsOneTimePw;

{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

interface

uses
    {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
    {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    OverbyteIcsMD5, OverbyteIcsMD4, OverbyteIcsSha1, OverbyteIcsFtpSrvT,
    OverbyteIcsUtils;

const
    OneTimePwVersion = 800;
    CopyRight : String = ' OneTimePw (c) 1997-2012 F. Piette V8.00 ';
    OtpKeyNames: array [0..3] of string =
                ('none', 'otp-md5', 'otp-md4', 'otp-sha1') ;

type
  { 64-bit result of OTP hash }
    TOtp64bit = array [0..7] of byte;

  { type conversion record }
    TInt64Rec = packed record
    case Integer of
      0: (Lo, Hi: Cardinal);
      1: (Cardinals: array [0..1] of Cardinal);
      2: (Words: array [0..3] of Word);
      3: (Bytes: array [0..7] of Byte);
      4: (Quad: Int64);
    end;

  { OTP hash methods }
    TOtpMethod = (OtpKeyNone, OtpKeyMd5, OtpKeyMd4, OtpKeySha1);

{ Q-}
{ R-}

function OtpIsValidPassword (const OtpPassword: string): boolean;
function OtpIsValidSeed (const OptSeed: string): boolean;
function OtpLowNoSpace (const AString: string): string;
function OtpParseChallenge (const OtpChallenge: string; var OtpMethod: TOtpMethod;
                            var OtpSequence: integer; var OtpSeed: string): boolean;
function OtpIsValidChallenge (const OtpChallenge: string): Boolean;

function OtpProcessChallenge (const OtpChallenge, OtpPassword: string;
                                 HexResp: boolean ; var OtpRespKey: string): boolean;
function OtpCreateChallenge (OtpMethod: TOtpMethod; var OtpSequence: integer;
                                                    var OtpSeed: string): string;
function OtpTestPassword (const OtpRespKey, OtpPassword: string;
    OtpMethod: TOtpMethod; var OtpSequence: integer; const OtpSeed: string): boolean;
function OtpGetMethod (const S: string): TOtpMethod;

implementation

const
    InitialSequence = 999;

{ six words list taken from RCF2289 document - each word represents 11-bits of a 64-bit number }

  SixWordsList: array [0..2047] of string =
('A',    'ABE',   'ACE',   'ACT',   'AD',    'ADA',   'ADD',
'AGO',   'AID',   'AIM',   'AIR',   'ALL',   'ALP',   'AM',    'AMY',
'AN',    'ANA',   'AND',   'ANN',   'ANT',   'ANY',   'APE',   'APS',
'APT',   'ARC',   'ARE',   'ARK',   'ARM',   'ART',   'AS',    'ASH',
'ASK',   'AT',    'ATE',   'AUG',   'AUK',   'AVE',   'AWE',   'AWK',
'AWL',   'AWN',   'AX',    'AYE',   'BAD',   'BAG',   'BAH',   'BAM',
'BAN',   'BAR',   'BAT',   'BAY',   'BE',    'BED',   'BEE',   'BEG',
'BEN',   'BET',   'BEY',   'BIB',   'BID',   'BIG',   'BIN',   'BIT',
'BOB',   'BOG',   'BON',   'BOO',   'BOP',   'BOW',   'BOY',   'BUB',
'BUD',   'BUG',   'BUM',   'BUN',   'BUS',   'BUT',   'BUY',   'BY',
'BYE',   'CAB',   'CAL',   'CAM',   'CAN',   'CAP',   'CAR',   'CAT',
'CAW',   'COD',   'COG',   'COL',   'CON',   'COO',   'COP',   'COT',
'COW',   'COY',   'CRY',   'CUB',   'CUE',   'CUP',   'CUR',   'CUT',
'DAB',   'DAD',   'DAM',   'DAN',   'DAR',   'DAY',   'DEE',   'DEL',
'DEN',   'DES',   'DEW',   'DID',   'DIE',   'DIG',   'DIN',   'DIP',
'DO',    'DOE',   'DOG',   'DON',   'DOT',   'DOW',   'DRY',   'DUB',
'DUD',   'DUE',   'DUG',   'DUN',   'EAR',   'EAT',   'ED',    'EEL',
'EGG',   'EGO',   'ELI',   'ELK',   'ELM',   'ELY',   'EM',    'END',
'EST',   'ETC',   'EVA',   'EVE',   'EWE',   'EYE',   'FAD',   'FAN',
'FAR',   'FAT',   'FAY',   'FED',   'FEE',   'FEW',   'FIB',   'FIG',
'FIN',   'FIR',   'FIT',   'FLO',   'FLY',   'FOE',   'FOG',   'FOR',
'FRY',   'FUM',   'FUN',   'FUR',   'GAB',   'GAD',   'GAG',   'GAL',
'GAM',   'GAP',   'GAS',   'GAY',   'GEE',   'GEL',   'GEM',   'GET',
'GIG',   'GIL',   'GIN',   'GO',    'GOT',   'GUM',   'GUN',   'GUS',
'GUT',   'GUY',   'GYM',   'GYP',   'HA',    'HAD',   'HAL',   'HAM',
'HAN',   'HAP',   'HAS',   'HAT',   'HAW',   'HAY',   'HE',    'HEM',
'HEN',   'HER',   'HEW',   'HEY',   'HI',    'HID',   'HIM',   'HIP',
'HIS',   'HIT',   'HO',    'HOB',   'HOC',   'HOE',   'HOG',   'HOP',
'HOT',   'HOW',   'HUB',   'HUE',   'HUG',   'HUH',   'HUM',   'HUT',
'I',     'ICY',   'IDA',   'IF',    'IKE',   'ILL',   'INK',   'INN',
'IO',    'ION',   'IQ',    'IRA',   'IRE',   'IRK',   'IS',    'IT',
'ITS',   'IVY',   'JAB',   'JAG',   'JAM',   'JAN',   'JAR',   'JAW',
'JAY',   'JET',   'JIG',   'JIM',   'JO',    'JOB',   'JOE',   'JOG',
'JOT',   'JOY',   'JUG',   'JUT',   'KAY',   'KEG',   'KEN',   'KEY',
'KID',   'KIM',   'KIN',   'KIT',   'LA',    'LAB',   'LAC',   'LAD',
'LAG',   'LAM',   'LAP',   'LAW',   'LAY',   'LEA',   'LED',   'LEE',
'LEG',   'LEN',   'LEO',   'LET',   'LEW',   'LID',   'LIE',   'LIN',
'LIP',   'LIT',   'LO',    'LOB',   'LOG',   'LOP',   'LOS',   'LOT',
'LOU',   'LOW',   'LOY',   'LUG',   'LYE',   'MA',    'MAC',   'MAD',
'MAE',   'MAN',   'MAO',   'MAP',   'MAT',   'MAW',   'MAY',   'ME',
'MEG',   'MEL',   'MEN',   'MET',   'MEW',   'MID',   'MIN',   'MIT',
'MOB',   'MOD',   'MOE',   'MOO',   'MOP',   'MOS',   'MOT',   'MOW',
'MUD',   'MUG',   'MUM',   'MY',    'NAB',   'NAG',   'NAN',   'NAP',
'NAT',   'NAY',   'NE',    'NED',   'NEE',   'NET',   'NEW',   'NIB',
'NIL',   'NIP',   'NIT',   'NO',    'NOB',   'NOD',   'NON',   'NOR',
'NOT',   'NOV',   'NOW',   'NU',    'NUN',   'NUT',   'O',     'OAF',
'OAK',   'OAR',   'OAT',   'ODD',   'ODE',   'OF',    'OFF',   'OFT',
'OH',    'OIL',   'OK',    'OLD',   'ON',    'ONE',   'OR',    'ORB',
'ORE',   'ORR',   'OS',    'OTT',   'OUR',   'OUT',   'OVA',   'OW',
'OWE',   'OWL',   'OWN',   'OX',    'PA',    'PAD',   'PAL',   'PAM',
'PAN',   'PAP',   'PAR',   'PAT',   'PAW',   'PAY',   'PEA',   'PEG',
'PEN',   'PEP',   'PER',   'PET',   'PEW',   'PHI',   'PI',    'PIE',
'PIN',   'PIT',   'PLY',   'PO',    'POD',   'POE',   'POP',   'POT',
'POW',   'PRO',   'PRY',   'PUB',   'PUG',   'PUN',   'PUP',   'PUT',
'QUO',   'RAG',   'RAM',   'RAN',   'RAP',   'RAT',   'RAW',   'RAY',
'REB',   'RED',   'REP',   'RET',   'RIB',   'RID',   'RIG',   'RIM',
'RIO',   'RIP',   'ROB',   'ROD',   'ROE',   'RON',   'ROT',   'ROW',
'ROY',   'RUB',   'RUE',   'RUG',   'RUM',   'RUN',   'RYE',   'SAC',
'SAD',   'SAG',   'SAL',   'SAM',   'SAN',   'SAP',   'SAT',   'SAW',
'SAY',   'SEA',   'SEC',   'SEE',   'SEN',   'SET',   'SEW',   'SHE',
'SHY',   'SIN',   'SIP',   'SIR',   'SIS',   'SIT',   'SKI',   'SKY',
'SLY',   'SO',    'SOB',   'SOD',   'SON',   'SOP',   'SOW',   'SOY',
'SPA',   'SPY',   'SUB',   'SUD',   'SUE',   'SUM',   'SUN',   'SUP',
'TAB',   'TAD',   'TAG',   'TAN',   'TAP',   'TAR',   'TEA',   'TED',
'TEE',   'TEN',   'THE',   'THY',   'TIC',   'TIE',   'TIM',   'TIN',
'TIP',   'TO',    'TOE',   'TOG',   'TOM',   'TON',   'TOO',   'TOP',
'TOW',   'TOY',   'TRY',   'TUB',   'TUG',   'TUM',   'TUN',   'TWO',
'UN',    'UP',    'US',    'USE',   'VAN',   'VAT',   'VET',   'VIE',
'WAD',   'WAG',   'WAR',   'WAS',   'WAY',   'WE',    'WEB',   'WED',
'WEE',   'WET',   'WHO',   'WHY',   'WIN',   'WIT',   'WOK',   'WON',
'WOO',   'WOW',   'WRY',   'WU',    'YAM',   'YAP',   'YAW',   'YE',
'YEA',   'YES',   'YET',   'YOU',   'ABED',  'ABEL',  'ABET',  'ABLE',
'ABUT',  'ACHE',  'ACID',  'ACME',  'ACRE',  'ACTA',  'ACTS',  'ADAM',
'ADDS',  'ADEN',  'AFAR',  'AFRO',  'AGEE',  'AHEM',  'AHOY',  'AIDA',
'AIDE',  'AIDS',  'AIRY',  'AJAR',  'AKIN',  'ALAN',  'ALEC',  'ALGA',
'ALIA',  'ALLY',  'ALMA',  'ALOE',  'ALSO',  'ALTO',  'ALUM',  'ALVA',
'AMEN',  'AMES',  'AMID',  'AMMO',  'AMOK',  'AMOS',  'AMRA',  'ANDY',
'ANEW',  'ANNA',  'ANNE',  'ANTE',  'ANTI',  'AQUA',  'ARAB',  'ARCH',
'AREA',  'ARGO',  'ARID',  'ARMY',  'ARTS',  'ARTY',  'ASIA',  'ASKS',
'ATOM',  'AUNT',  'AURA',  'AUTO',  'AVER',  'AVID',  'AVIS',  'AVON',
'AVOW',  'AWAY',  'AWRY',  'BABE',  'BABY',  'BACH',  'BACK',  'BADE',
'BAIL',  'BAIT',  'BAKE',  'BALD',  'BALE',  'BALI',  'BALK',  'BALL',
'BALM',  'BAND',  'BANE',  'BANG',  'BANK',  'BARB',  'BARD',  'BARE',
'BARK',  'BARN',  'BARR',  'BASE',  'BASH',  'BASK',  'BASS',  'BATE',
'BATH',  'BAWD',  'BAWL',  'BEAD',  'BEAK',  'BEAM',  'BEAN',  'BEAR',
'BEAT',  'BEAU',  'BECK',  'BEEF',  'BEEN',  'BEER',  'BEET',  'BELA',
'BELL',  'BELT',  'BEND',  'BENT',  'BERG',  'BERN',  'BERT',  'BESS',
'BEST',  'BETA',  'BETH',  'BHOY',  'BIAS',  'BIDE',  'BIEN',  'BILE',
'BILK',  'BILL',  'BIND',  'BING',  'BIRD',  'BITE',  'BITS',  'BLAB',
'BLAT',  'BLED',  'BLEW',  'BLOB',  'BLOC',  'BLOT',  'BLOW',  'BLUE',
'BLUM',  'BLUR',  'BOAR',  'BOAT',  'BOCA',  'BOCK',  'BODE',  'BODY',
'BOGY',  'BOHR',  'BOIL',  'BOLD',  'BOLO',  'BOLT',  'BOMB',  'BONA',
'BOND',  'BONE',  'BONG',  'BONN',  'BONY',  'BOOK',  'BOOM',  'BOON',
'BOOT',  'BORE',  'BORG',  'BORN',  'BOSE',  'BOSS',  'BOTH',  'BOUT',
'BOWL',  'BOYD',  'BRAD',  'BRAE',  'BRAG',  'BRAN',  'BRAY',  'BRED',
'BREW',  'BRIG',  'BRIM',  'BROW',  'BUCK',  'BUDD',  'BUFF',  'BULB',
'BULK',  'BULL',  'BUNK',  'BUNT',  'BUOY',  'BURG',  'BURL',  'BURN',
'BURR',  'BURT',  'BURY',  'BUSH',  'BUSS',  'BUST',  'BUSY',  'BYTE',
'CADY',  'CAFE',  'CAGE',  'CAIN',  'CAKE',  'CALF',  'CALL',  'CALM',
'CAME',  'CANE',  'CANT',  'CARD',  'CARE',  'CARL',  'CARR',  'CART',
'CASE',  'CASH',  'CASK',  'CAST',  'CAVE',  'CEIL',  'CELL',  'CENT',
'CERN',  'CHAD',  'CHAR',  'CHAT',  'CHAW',  'CHEF',  'CHEN',  'CHEW',
'CHIC',  'CHIN',  'CHOU',  'CHOW',  'CHUB',  'CHUG',  'CHUM',  'CITE',
'CITY',  'CLAD',  'CLAM',  'CLAN',  'CLAW',  'CLAY',  'CLOD',  'CLOG',
'CLOT',  'CLUB',  'CLUE',  'COAL',  'COAT',  'COCA',  'COCK',  'COCO',
'CODA',  'CODE',  'CODY',  'COED',  'COIL',  'COIN',  'COKE',  'COLA',
'COLD',  'COLT',  'COMA',  'COMB',  'COME',  'COOK',  'COOL',  'COON',
'COOT',  'CORD',  'CORE',  'CORK',  'CORN',  'COST',  'COVE',  'COWL',
'CRAB',  'CRAG',  'CRAM',  'CRAY',  'CREW',  'CRIB',  'CROW',  'CRUD',
'CUBA',  'CUBE',  'CUFF',  'CULL',  'CULT',  'CUNY',  'CURB',  'CURD',
'CURE',  'CURL',  'CURT',  'CUTS',  'DADE',  'DALE',  'DAME',  'DANA',
'DANE',  'DANG',  'DANK',  'DARE',  'DARK',  'DARN',  'DART',  'DASH',
'DATA',  'DATE',  'DAVE',  'DAVY',  'DAWN',  'DAYS',  'DEAD',  'DEAF',
'DEAL',  'DEAN',  'DEAR',  'DEBT',  'DECK',  'DEED',  'DEEM',  'DEER',
'DEFT',  'DEFY',  'DELL',  'DENT',  'DENY',  'DESK',  'DIAL',  'DICE',
'DIED',  'DIET',  'DIME',  'DINE',  'DING',  'DINT',  'DIRE',  'DIRT',
'DISC',  'DISH',  'DISK',  'DIVE',  'DOCK',  'DOES',  'DOLE',  'DOLL',
'DOLT',  'DOME',  'DONE',  'DOOM',  'DOOR',  'DORA',  'DOSE',  'DOTE',
'DOUG',  'DOUR',  'DOVE',  'DOWN',  'DRAB',  'DRAG',  'DRAM',  'DRAW',
'DREW',  'DRUB',  'DRUG',  'DRUM',  'DUAL',  'DUCK',  'DUCT',  'DUEL',
'DUET',  'DUKE',  'DULL',  'DUMB',  'DUNE',  'DUNK',  'DUSK',  'DUST',
'DUTY',  'EACH',  'EARL',  'EARN',  'EASE',  'EAST',  'EASY',  'EBEN',
'ECHO',  'EDDY',  'EDEN',  'EDGE',  'EDGY',  'EDIT',  'EDNA',  'EGAN',
'ELAN',  'ELBA',  'ELLA',  'ELSE',  'EMIL',  'EMIT',  'EMMA',  'ENDS',
'ERIC',  'EROS',  'EVEN',  'EVER',  'EVIL',  'EYED',  'FACE',  'FACT',
'FADE',  'FAIL',  'FAIN',  'FAIR',  'FAKE',  'FALL',  'FAME',  'FANG',
'FARM',  'FAST',  'FATE',  'FAWN',  'FEAR',  'FEAT',  'FEED',  'FEEL',
'FEET',  'FELL',  'FELT',  'FEND',  'FERN',  'FEST',  'FEUD',  'FIEF',
'FIGS',  'FILE',  'FILL',  'FILM',  'FIND',  'FINE',  'FINK',  'FIRE',
'FIRM',  'FISH',  'FISK',  'FIST',  'FITS',  'FIVE',  'FLAG',  'FLAK',
'FLAM',  'FLAT',  'FLAW',  'FLEA',  'FLED',  'FLEW',  'FLIT',  'FLOC',
'FLOG',  'FLOW',  'FLUB',  'FLUE',  'FOAL',  'FOAM',  'FOGY',  'FOIL',
'FOLD',  'FOLK',  'FOND',  'FONT',  'FOOD',  'FOOL',  'FOOT',  'FORD',
'FORE',  'FORK',  'FORM',  'FORT',  'FOSS',  'FOUL',  'FOUR',  'FOWL',
'FRAU',  'FRAY',  'FRED',  'FREE',  'FRET',  'FREY',  'FROG',  'FROM',
'FUEL',  'FULL',  'FUME',  'FUND',  'FUNK',  'FURY',  'FUSE',  'FUSS',
'GAFF',  'GAGE',  'GAIL',  'GAIN',  'GAIT',  'GALA',  'GALE',  'GALL',
'GALT',  'GAME',  'GANG',  'GARB',  'GARY',  'GASH',  'GATE',  'GAUL',
'GAUR',  'GAVE',  'GAWK',  'GEAR',  'GELD',  'GENE',  'GENT',  'GERM',
'GETS',  'GIBE',  'GIFT',  'GILD',  'GILL',  'GILT',  'GINA',  'GIRD',
'GIRL',  'GIST',  'GIVE',  'GLAD',  'GLEE',  'GLEN',  'GLIB',  'GLOB',
'GLOM',  'GLOW',  'GLUE',  'GLUM',  'GLUT',  'GOAD',  'GOAL',  'GOAT',
'GOER',  'GOES',  'GOLD',  'GOLF',  'GONE',  'GONG',  'GOOD',  'GOOF',
'GORE',  'GORY',  'GOSH',  'GOUT',  'GOWN',  'GRAB',  'GRAD',  'GRAY',
'GREG',  'GREW',  'GREY',  'GRID',  'GRIM',  'GRIN',  'GRIT',  'GROW',
'GRUB',  'GULF',  'GULL',  'GUNK',  'GURU',  'GUSH',  'GUST',  'GWEN',
'GWYN',  'HAAG',  'HAAS',  'HACK',  'HAIL',  'HAIR',  'HALE',  'HALF',
'HALL',  'HALO',  'HALT',  'HAND',  'HANG',  'HANK',  'HANS',  'HARD',
'HARK',  'HARM',  'HART',  'HASH',  'HAST',  'HATE',  'HATH',  'HAUL',
'HAVE',  'HAWK',  'HAYS',  'HEAD',  'HEAL',  'HEAR',  'HEAT',  'HEBE',
'HECK',  'HEED',  'HEEL',  'HEFT',  'HELD',  'HELL',  'HELM',  'HERB',
'HERD',  'HERE',  'HERO',  'HERS',  'HESS',  'HEWN',  'HICK',  'HIDE',
'HIGH',  'HIKE',  'HILL',  'HILT',  'HIND',  'HINT',  'HIRE',  'HISS',
'HIVE',  'HOBO',  'HOCK',  'HOFF',  'HOLD',  'HOLE',  'HOLM',  'HOLT',
'HOME',  'HONE',  'HONK',  'HOOD',  'HOOF',  'HOOK',  'HOOT',  'HORN',
'HOSE',  'HOST',  'HOUR',  'HOVE',  'HOWE',  'HOWL',  'HOYT',  'HUCK',
'HUED',  'HUFF',  'HUGE',  'HUGH',  'HUGO',  'HULK',  'HULL',  'HUNK',
'HUNT',  'HURD',  'HURL',  'HURT',  'HUSH',  'HYDE',  'HYMN',  'IBIS',
'ICON',  'IDEA',  'IDLE',  'IFFY',  'INCA',  'INCH',  'INTO',  'IONS',
'IOTA',  'IOWA',  'IRIS',  'IRMA',  'IRON',  'ISLE',  'ITCH',  'ITEM',
'IVAN',  'JACK',  'JADE',  'JAIL',  'JAKE',  'JANE',  'JAVA',  'JEAN',
'JEFF',  'JERK',  'JESS',  'JEST',  'JIBE',  'JILL',  'JILT',  'JIVE',
'JOAN',  'JOBS',  'JOCK',  'JOEL',  'JOEY',  'JOHN',  'JOIN',  'JOKE',
'JOLT',  'JOVE',  'JUDD',  'JUDE',  'JUDO',  'JUDY',  'JUJU',  'JUKE',
'JULY',  'JUNE',  'JUNK',  'JUNO',  'JURY',  'JUST',  'JUTE',  'KAHN',
'KALE',  'KANE',  'KANT',  'KARL',  'KATE',  'KEEL',  'KEEN',  'KENO',
'KENT',  'KERN',  'KERR',  'KEYS',  'KICK',  'KILL',  'KIND',  'KING',
'KIRK',  'KISS',  'KITE',  'KLAN',  'KNEE',  'KNEW',  'KNIT',  'KNOB',
'KNOT',  'KNOW',  'KOCH',  'KONG',  'KUDO',  'KURD',  'KURT',  'KYLE',
'LACE',  'LACK',  'LACY',  'LADY',  'LAID',  'LAIN',  'LAIR',  'LAKE',
'LAMB',  'LAME',  'LAND',  'LANE',  'LANG',  'LARD',  'LARK',  'LASS',
'LAST',  'LATE',  'LAUD',  'LAVA',  'LAWN',  'LAWS',  'LAYS',  'LEAD',
'LEAF',  'LEAK',  'LEAN',  'LEAR',  'LEEK',  'LEER',  'LEFT',  'LEND',
'LENS',  'LENT',  'LEON',  'LESK',  'LESS',  'LEST',  'LETS',  'LIAR',
'LICE',  'LICK',  'LIED',  'LIEN',  'LIES',  'LIEU',  'LIFE',  'LIFT',
'LIKE',  'LILA',  'LILT',  'LILY',  'LIMA',  'LIMB',  'LIME',  'LIND',
'LINE',  'LINK',  'LINT',  'LION',  'LISA',  'LIST',  'LIVE',  'LOAD',
'LOAF',  'LOAM',  'LOAN',  'LOCK',  'LOFT',  'LOGE',  'LOIS',  'LOLA',
'LONE',  'LONG',  'LOOK',  'LOON',  'LOOT',  'LORD',  'LORE',  'LOSE',
'LOSS',  'LOST',  'LOUD',  'LOVE',  'LOWE',  'LUCK',  'LUCY',  'LUGE',
'LUKE',  'LULU',  'LUND',  'LUNG',  'LURA',  'LURE',  'LURK',  'LUSH',
'LUST',  'LYLE',  'LYNN',  'LYON',  'LYRA',  'MACE',  'MADE',  'MAGI',
'MAID',  'MAIL',  'MAIN',  'MAKE',  'MALE',  'MALI',  'MALL',  'MALT',
'MANA',  'MANN',  'MANY',  'MARC',  'MARE',  'MARK',  'MARS',  'MART',
'MARY',  'MASH',  'MASK',  'MASS',  'MAST',  'MATE',  'MATH',  'MAUL',
'MAYO',  'MEAD',  'MEAL',  'MEAN',  'MEAT',  'MEEK',  'MEET',  'MELD',
'MELT',  'MEMO',  'MEND',  'MENU',  'MERT',  'MESH',  'MESS',  'MICE',
'MIKE',  'MILD',  'MILE',  'MILK',  'MILL',  'MILT',  'MIMI',  'MIND',
'MINE',  'MINI',  'MINK',  'MINT',  'MIRE',  'MISS',  'MIST',  'MITE',
'MITT',  'MOAN',  'MOAT',  'MOCK',  'MODE',  'MOLD',  'MOLE',  'MOLL',
'MOLT',  'MONA',  'MONK',  'MONT',  'MOOD',  'MOON',  'MOOR',  'MOOT',
'MORE',  'MORN',  'MORT',  'MOSS',  'MOST',  'MOTH',  'MOVE',  'MUCH',
'MUCK',  'MUDD',  'MUFF',  'MULE',  'MULL',  'MURK',  'MUSH',  'MUST',
'MUTE',  'MUTT',  'MYRA',  'MYTH',  'NAGY',  'NAIL',  'NAIR',  'NAME',
'NARY',  'NASH',  'NAVE',  'NAVY',  'NEAL',  'NEAR',  'NEAT',  'NECK',
'NEED',  'NEIL',  'NELL',  'NEON',  'NERO',  'NESS',  'NEST',  'NEWS',
'NEWT',  'NIBS',  'NICE',  'NICK',  'NILE',  'NINA',  'NINE',  'NOAH',
'NODE',  'NOEL',  'NOLL',  'NONE',  'NOOK',  'NOON',  'NORM',  'NOSE',
'NOTE',  'NOUN',  'NOVA',  'NUDE',  'NULL',  'NUMB',  'OATH',  'OBEY',
'OBOE',  'ODIN',  'OHIO',  'OILY',  'OINT',  'OKAY',  'OLAF',  'OLDY',
'OLGA',  'OLIN',  'OMAN',  'OMEN',  'OMIT',  'ONCE',  'ONES',  'ONLY',
'ONTO',  'ONUS',  'ORAL',  'ORGY',  'OSLO',  'OTIS',  'OTTO',  'OUCH',
'OUST',  'OUTS',  'OVAL',  'OVEN',  'OVER',  'OWLY',  'OWNS',  'QUAD',
'QUIT',  'QUOD',  'RACE',  'RACK',  'RACY',  'RAFT',  'RAGE',  'RAID',
'RAIL',  'RAIN',  'RAKE',  'RANK',  'RANT',  'RARE',  'RASH',  'RATE',
'RAVE',  'RAYS',  'READ',  'REAL',  'REAM',  'REAR',  'RECK',  'REED',
'REEF',  'REEK',  'REEL',  'REID',  'REIN',  'RENA',  'REND',  'RENT',
'REST',  'RICE',  'RICH',  'RICK',  'RIDE',  'RIFT',  'RILL',  'RIME',
'RING',  'RINK',  'RISE',  'RISK',  'RITE',  'ROAD',  'ROAM',  'ROAR',
'ROBE',  'ROCK',  'RODE',  'ROIL',  'ROLL',  'ROME',  'ROOD',  'ROOF',
'ROOK',  'ROOM',  'ROOT',  'ROSA',  'ROSE',  'ROSS',  'ROSY',  'ROTH',
'ROUT',  'ROVE',  'ROWE',  'ROWS',  'RUBE',  'RUBY',  'RUDE',  'RUDY',
'RUIN',  'RULE',  'RUNG',  'RUNS',  'RUNT',  'RUSE',  'RUSH',  'RUSK',
'RUSS',  'RUST',  'RUTH',  'SACK',  'SAFE',  'SAGE',  'SAID',  'SAIL',
'SALE',  'SALK',  'SALT',  'SAME',  'SAND',  'SANE',  'SANG',  'SANK',
'SARA',  'SAUL',  'SAVE',  'SAYS',  'SCAN',  'SCAR',  'SCAT',  'SCOT',
'SEAL',  'SEAM',  'SEAR',  'SEAT',  'SEED',  'SEEK',  'SEEM',  'SEEN',
'SEES',  'SELF',  'SELL',  'SEND',  'SENT',  'SETS',  'SEWN',  'SHAG',
'SHAM',  'SHAW',  'SHAY',  'SHED',  'SHIM',  'SHIN',  'SHOD',  'SHOE',
'SHOT',  'SHOW',  'SHUN',  'SHUT',  'SICK',  'SIDE',  'SIFT',  'SIGH',
'SIGN',  'SILK',  'SILL',  'SILO',  'SILT',  'SINE',  'SING',  'SINK',
'SIRE',  'SITE',  'SITS',  'SITU',  'SKAT',  'SKEW',  'SKID',  'SKIM',
'SKIN',  'SKIT',  'SLAB',  'SLAM',  'SLAT',  'SLAY',  'SLED',  'SLEW',
'SLID',  'SLIM',  'SLIT',  'SLOB',  'SLOG',  'SLOT',  'SLOW',  'SLUG',
'SLUM',  'SLUR',  'SMOG',  'SMUG',  'SNAG',  'SNOB',  'SNOW',  'SNUB',
'SNUG',  'SOAK',  'SOAR',  'SOCK',  'SODA',  'SOFA',  'SOFT',  'SOIL',
'SOLD',  'SOME',  'SONG',  'SOON',  'SOOT',  'SORE',  'SORT',  'SOUL',
'SOUR',  'SOWN',  'STAB',  'STAG',  'STAN',  'STAR',  'STAY',  'STEM',
'STEW',  'STIR',  'STOW',  'STUB',  'STUN',  'SUCH',  'SUDS',  'SUIT',
'SULK',  'SUMS',  'SUNG',  'SUNK',  'SURE',  'SURF',  'SWAB',  'SWAG',
'SWAM',  'SWAN',  'SWAT',  'SWAY',  'SWIM',  'SWUM',  'TACK',  'TACT',
'TAIL',  'TAKE',  'TALE',  'TALK',  'TALL',  'TANK',  'TASK',  'TATE',
'TAUT',  'TEAL',  'TEAM',  'TEAR',  'TECH',  'TEEM',  'TEEN',  'TEET',
'TELL',  'TEND',  'TENT',  'TERM',  'TERN',  'TESS',  'TEST',  'THAN',
'THAT',  'THEE',  'THEM',  'THEN',  'THEY',  'THIN',  'THIS',  'THUD',
'THUG',  'TICK',  'TIDE',  'TIDY',  'TIED',  'TIER',  'TILE',  'TILL',
'TILT',  'TIME',  'TINA',  'TINE',  'TINT',  'TINY',  'TIRE',  'TOAD',
'TOGO',  'TOIL',  'TOLD',  'TOLL',  'TONE',  'TONG',  'TONY',  'TOOK',
'TOOL',  'TOOT',  'TORE',  'TORN',  'TOTE',  'TOUR',  'TOUT',  'TOWN',
'TRAG',  'TRAM',  'TRAY',  'TREE',  'TREK',  'TRIG',  'TRIM',  'TRIO',
'TROD',  'TROT',  'TROY',  'TRUE',  'TUBA',  'TUBE',  'TUCK',  'TUFT',
'TUNA',  'TUNE',  'TUNG',  'TURF',  'TURN',  'TUSK',  'TWIG',  'TWIN',
'TWIT',  'ULAN',  'UNIT',  'URGE',  'USED',  'USER',  'USES',  'UTAH',
'VAIL',  'VAIN',  'VALE',  'VARY',  'VASE',  'VAST',  'VEAL',  'VEDA',
'VEIL',  'VEIN',  'VEND',  'VENT',  'VERB',  'VERY',  'VETO',  'VICE',
'VIEW',  'VINE',  'VISE',  'VOID',  'VOLT',  'VOTE',  'WACK',  'WADE',
'WAGE',  'WAIL',  'WAIT',  'WAKE',  'WALE',  'WALK',  'WALL',  'WALT',
'WAND',  'WANE',  'WANG',  'WANT',  'WARD',  'WARM',  'WARN',  'WART',
'WASH',  'WAST',  'WATS',  'WATT',  'WAVE',  'WAVY',  'WAYS',  'WEAK',
'WEAL',  'WEAN',  'WEAR',  'WEED',  'WEEK',  'WEIR',  'WELD',  'WELL',
'WELT',  'WENT',  'WERE',  'WERT',  'WEST',  'WHAM',  'WHAT',  'WHEE',
'WHEN',  'WHET',  'WHOA',  'WHOM',  'WICK',  'WIFE',  'WILD',  'WILL',
'WIND',  'WINE',  'WING',  'WINK',  'WINO',  'WIRE',  'WISE',  'WISH',
'WITH',  'WOLF',  'WONT',  'WOOD',  'WOOL',  'WORD',  'WORE',  'WORK',
'WORM',  'WORN',  'WOVE',  'WRIT',  'WYNN',  'YALE',  'YANG',  'YANK',
'YARD',  'YARN',  'YAWL',  'YAWN',  'YEAH',  'YEAR',  'YELL',  'YOGA',
'YOKE' ) ;

const
  CharMap = 'abcdefghijklmnopqrstuvwxyz1234567890';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpGetRandomSeed: string ;
var
    I: Integer ;
    Maplen, Seedlen: integer ;
begin
    //result := '' ;
    Seedlen := IcsRandomInt (12) + 4 ;  { seed length 4 to 16 }
    Maplen := Length (CharMap) - 1 ;
    SetLength(Result, SeedLen);
    for I := 1 to Seedlen do
        //result := result + CharMap [IcsRandom32 (Maplen) + 1] ;
        Result[I] := CharMap [IcsRandomInt (Maplen) + 1];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpIsValidPassword (const OtpPassword: string): boolean;
begin
    result := (Length(OtpPassword) > 9) and (Length(OtpPassword) <= 63);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpIsValidSeed (const OptSeed: string): boolean;
var
    I: integer;
begin
    result := (OptSeed <> '') and (Length (OptSeed) <= 16);
    if not Result then
        exit;
    for I := 1 to Length (OptSeed) do begin
        if Pos (OptSeed [I], CharMap) <= 0 then begin
            result := false;
            break;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpLowNoSpace (const AString: string): string;
var
    I, J: integer;
begin
    {result := '';
    for I := 1 to Length(AString) do begin
        if (AString[I] <> ' ') and (AString[I] <> #9) then
            result := result + LowerCase (AString [I]);
    end;}
    SetLength(Result, Length(AString));
    J := 0;
    for I := 1 to Length(AString) do begin
        if (AString[I] <> ' ') and (AString[I] <> #9) then
        begin
            Inc(J);
            Result[J] := AString[I];
        end;
    end;
    SetLength(Result, J);
    Result := LowerCase(Result);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function RevEndian64(X: Int64): Int64;
begin
    result :=          (X and $00000000000000FF) shl 56;
    result := result + (X and $000000000000FF00) shl 40;
    result := result + (X and $0000000000FF0000) shl 24;
    result := result + (X and $00000000FF000000) shl 8;
    result := result + (X and $000000FF00000000) shr 8;
    result := result + (X and $0000FF0000000000) shr 24;
    result := result + (X and $00FF000000000000) shr 40;
    result := result + (X and $FF00000000000000) shr 56;
end;
*)
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function KeyToHex(OtpKey: TOtp64bit): string; {$IFDEF USE_INLINE} inline; {$ENDIF}
{var
    I: integer ;}
begin
    {result := '';
    for I := 0 to 7 do
        result := result + IntToHex(OtpKey [I], 2);}
    Result := IcsBufferToHex(OtpKey[0], SizeOf(OtpKey));
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function KeyToSixWordFormat (OtpKey: TOtp64bit): string;
var
    I, parity: integer;
    Key64: int64;

    function GetBits (Start: integer; Count: integer): word;
    begin
        result := (Key64 shl Start) shr (64 - Count);
    end;

begin
 { convert 8 bytes to int64 }
    //Key64 := RevEndian64 (TInt64Rec (OtpKey).Quad) ;
    //IcsSwap64Buf(@TInt64Rec(OtpKey).Quad, @Key64, 1);
    Key64 := IcsSwap64(TInt64Rec(OtpKey).Quad); // New and faster
 { get 11-bits five times and get five words from dictionary }
    for I := 0 to 4 do
        Result := Result + SixWordsList [GetBits (I * 11, 11)] + ' ' ;
    parity := 0;
  { sixth word includes two parity bits }
    for I := 0 to 31 do inc (parity, GetBits (I * 2, 2));
    parity := parity and 3;
    result := result + SixWordsList [GetBits (55, 11) + parity];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetMD4Half (Buffer: Pointer; BufSize: Integer): TOtp64bit;
var
    I: integer;
    MD4Context: TMD4Context;
    MD4Digest: TMD4Digest;
begin
  { get normal 128-bit MD4 hash }
    MD4Init (MD4Context);
    MD4Update (MD4Context, Buffer^, BufSize);
    MD4Final (MD4Context, MD4Digest);

  { fold 128-bits of MD4 hash into 64-bits }
    for I := 0 to 7 do
        Result [I] := MD4Digest [I] XOR MD4Digest [I + 8];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetMD5Half (Buffer: Pointer; BufSize: Integer): TOtp64bit;
var
    I: integer;
    MD5Context: TMD5Context;
    MD5Digest: TMD5Digest;
begin
  { get normal 128-bit MD5 hash }
    MD5Init (MD5Context);
    MD5UpdateBuffer (MD5Context, Buffer, BufSize);
    MD5Final (MD5Digest, MD5Context);

  { fold 128-bits of MD5 hash into 64-bits }
    for I := 0 to 7 do result [I] := MD5Digest [I] XOR MD5Digest [I + 8];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
procedure SwapIntBuf(Source, Dest: Pointer; Count: Integer);
asm
       TEST   ECX,ECX
       JLE    @Exit
       PUSH   EBX
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EBX,[EAX + ECX * 4]
       BSWAP  EBX
       MOV    [EDX + ECX * 4],EBX
       DEC    ECX
       JNZ    @@1
       POP    EBX
@Exit:
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetSha1Half (Buffer: Pointer; BufSize: Integer): TOtp64bit;
var
    I: integer;
    Digest: AnsiString;   { V1.02 }
begin
    Digest := SHA1ofBuf (Buffer, BufSize);
    if Length (Digest) <> 20 then exit;  { sanity check }

  { fold 160-bits of Sha1 hash into 64-bits }
    for I := 0 to 7 do
        Result [I] := Ord (Digest [I + 1]) XOR Ord (Digest [I + 9]);
    for I := 0 to 3 do
        Result [I] := Result [I] XOR Ord (Digest [I + 17]);

  { change endian order }
    //SwapIntBuf(@Result[0], @Result[0], 2);
    IcsSwap32Buf(@Result[0], @Result[0], 2);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GenerateKey64 (OtpMethod: TOtpMethod; const OptSeed: string;
                const OtpPassword: string; const OtpSequence: Integer): TOtp64bit;
var
    I: integer;
    HashText: AnsiString;     { V1.02 }
begin
  { hash seed and password (max 63) into 64-bits }
    HashText := AnsiString (LowerCase (OptSeed) + OtpPassword);   { V1.02 }
    case OtpMethod of
        OtpKeyMd5: result := GetMD5Half (@HashText [1], Length (HashText));
        OtpKeyMd4: result := GetMD4Half (@HashText [1], Length (HashText));
        OtpKeySha1: result := GetSha1Half (@HashText [1], Length (HashText));
    end;

  { rehash the 64-bits repeatedly, according to sequence number in challenge }
    if OtpSequence <= 0 then exit ;
    for I := 1 to OtpSequence do begin
        case OtpMethod of
            OtpKeyMd5: result := GetMD5Half (@result, 8);
            OtpKeyMd4: result := GetMD4Half (@result, 8);
            OtpKeySha1: result := GetSha1Half (@result, 8);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpGetMethod (const S: string): TOtpMethod;
var
    Method: TOtpMethod;
begin
  { find hash method from string }
    for Method := Low (TOtpMethod) to High (TOtpMethod) do
    begin
        if Pos (OtpKeyNames [Ord (Method)], S) = 1 then
        begin
            result := Method;
            exit;
        end ;
    end;
    result := OtpKeyNone;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpParseChallenge (const OtpChallenge: string; var OtpMethod: TOtpMethod;
                            var OtpSequence: integer; var OtpSeed: string): boolean;
var
    J: integer;
    Method: TOtpMethod;
    S: string ;
begin
   { challenge example: Response to otp-md5 999 sill116 required for skey. }
    result := false;
    OtpMethod := OtpKeyNone;
    OtpSequence := 0;
    OtpSeed := '';

  { find hash method from challenge }
    for Method := Low (TOtpMethod) to High (TOtpMethod) do
    begin
        J := Pos (OtpKeyNames [Ord (Method)], OtpChallenge);
        if J >= 1 then
        begin
            OtpMethod := Method;
            S := Trim (Copy (OtpChallenge, J + Length (OtpKeyNames [Ord (OtpMethod)]), 999));
            break;
        end ;
    end;
    if OtpMethod = OtpKeyNone then exit;

  { find Sequence - 0 to 999 }
    J := Pos (' ', S);
    if J <= 1 then exit;
    OtpSequence := atoi (Copy (S, 1, J));

  { find Seed }
    S := Trim (Copy (S, J, 999)) + ' ' ;
    J := Pos (' ', S);
    OtpSeed := Lowercase (Trim (Copy (S, 1, J)));
    result := OtpIsValidSeed (OtpSeed);
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpIsValidChallenge (const OtpChallenge: string): boolean;
var
    OtpMethod: TOtpMethod;
    OtpSequence: integer;
    OtpSeed: string;
begin
    result := OtpParseChallenge (OtpChallenge, OtpMethod, OtpSequence, OtpSeed) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpProcessChallenge (const OtpChallenge, OtpPassword: string;
                                 HexResp: boolean ; var OtpRespKey: string): boolean;
var
    OtpMethod: TOtpMethod;
    OtpSequence: integer;
    OtpSeed: string;
    Key64: TOtp64bit;
begin
    OtpRespKey := '';

  { check for valid challenge, extract parameters }
    result := OtpParseChallenge (OtpChallenge, OtpMethod, OtpSequence, OtpSeed);
    if not Result then
        exit;

  { generate 64-bit response key, convert to 'six words' or hex one time password }
    Key64 := GenerateKey64 (OtpMethod, OtpSeed, OtpPassword, OtpSequence);
    if NOT HexResp then
        OtpRespKey := KeyToSixWordFormat (Key64)
    else
        OtpRespKey := KeyToHex (Key64);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpCreateChallenge (OtpMethod: TOtpMethod; var OtpSequence: integer;
                                                    var OtpSeed: string): string;
begin
    result := '';
    if OtpMethod = OtpKeyNone then exit;
    OtpSeed := LowerCase (OtpSeed) ;
    if (OtpSequence < 0) or (NOT OtpIsValidSeed (OtpSeed)) then
    begin
        OtpSequence := InitialSequence;
        OtpSeed := OtpGetRandomSeed;
    end ;
    result := OtpKeyNames [Ord (OtpMethod)] + ' ' +
                                        IntToStr(OtpSequence) + ' ' + OtpSeed;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function OtpTestPassword (
    const OtpRespKey, OtpPassword: string;
    OtpMethod: TOtpMethod;
    var OtpSequence: integer;
    const OtpSeed: string): boolean;
var
    Key64: TOtp64bit;
    KeyHex, Key6W, RespKey: string;
begin
    Key64 := GenerateKey64 (OtpMethod, OtpSeed, OtpPassword, OtpSequence);
    RespKey := OtpLowNoSpace (OtpRespKey) ;
    KeyHex := OtpLowNoSpace (KeyToHex (Key64));
    Key6W := OtpLowNoSpace (KeyToSixWordFormat (Key64));
    result := (RespKey = Key6W);
    if not Result then
        Result := (RespKey = KeyHex);
    if (NOT result) and (OtpMethod = OtpKeyNone) then
                                         result := (OtpRespKey = OtpPassword) ;
    if Result then
        Dec (OtpSequence);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
