{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Sep 03, 2004
Version:      1.00
Description:  MD4 is an implementation of the MD4 Message-Digest Algorithm
              as described in RFC1320
Credit:       This unit is based on code written by David Barton
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Copyright (c) 1999-2002 David Barton (crypto@cityinthesky.co.uk)
  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
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

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsMD4;

{$I ICSDEFS.INC}

interface

uses
  Windows, Classes, Sysutils{, DCPcrypt2, DCPconst};

const
    IcsMD4Version          = 100;
    CopyRight : String     = ' IcsMD4 (c) 2004-2007 F. Piette V1.00 ';

type
{$IFDEF DELPHI3}
  LongWord = DWORD;
{$ENDIF}
  MD4Exception = class(Exception);
  TMD4Digest   = array [0..15] of Byte;
  PMD4Digest   = ^TMD4Digest;
  TMD4Context  = record
    FInitialized : Boolean;  { Whether or not the algorithm has been initialized }
    LenHi, LenLo : LongWord;
    Index        : DWord;
    CurrentHash  : array [0..3]  of DWord;
    HashBuffer   : array [0..63] of Byte;
  end;

function  MD4String(const Value : String) : String;
procedure MD4Init(var MD4Context : TMD4Context);
procedure MD4Burn(var MD4Context : TMD4Context);
procedure MD4Update(var MD4Context : TMD4Context; const Buffer; Size: LongWord);
procedure MD4UpdateStr(var MD4Context : TMD4Context; const Str: String);
procedure MD4Compress(var MD4Context : TMD4Context);
procedure MD4Final(var MD4Context : TMD4Context; var Digest : TMD4Digest);

implementation
{$R-}{$Q-}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LRot32(a, b: LongWord): LongWord;
begin
    Result:= (a shl b) or (a shr (32-b));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4Burn(var MD4Context : TMD4Context);
begin
    MD4Context.LenHi:= 0;
    MD4Context.LenLo:= 0;
    MD4Context.Index:= 0;
    FillChar(MD4Context.HashBuffer, Sizeof(MD4Context.HashBuffer), 0);
    FillChar(MD4Context.CurrentHash, Sizeof(MD4Context.CurrentHash), 0);
    MD4Context.FInitialized := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4Init(var MD4Context : TMD4Context);
begin
    MD4Burn(MD4Context);
    MD4Context.CurrentHash[0] := $67452301;
    MD4Context.CurrentHash[1] := $efcdab89;
    MD4Context.CurrentHash[2] := $98badcfe;
    MD4Context.CurrentHash[3] := $10325476;
    MD4Context.FInitialized   := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4Update(var MD4Context : TMD4Context; const Buffer; Size: LongWord);
var
    PBuf: ^Byte;
begin
    if not MD4Context.FInitialized then
        raise MD4Exception.Create('MD4 not initialized');

    Inc(MD4Context.LenHi, Size shr 29);
    Inc(MD4Context.LenLo, Size * 8);
    if MD4Context.LenLo < (Size * 8) then
        Inc(MD4Context.LenHi);

    PBuf:= @Buffer;
    while Size > 0 do begin
        if (Sizeof(MD4Context.HashBuffer) - MD4Context.Index) <= DWord(Size) then begin
            Move(PBuf^, MD4Context.HashBuffer[MD4Context.Index], Sizeof(MD4Context.HashBuffer) - MD4Context.Index);
            Dec(Size, Sizeof(MD4Context.HashBuffer) - MD4Context.Index);
            Inc(PBuf, Sizeof(MD4Context.HashBuffer) - MD4Context.Index);
            MD4Compress(MD4Context);
        end
        else begin
            Move(PBuf^, MD4Context.HashBuffer[MD4Context.Index], Size);
            Inc(MD4Context.Index, Size);
            Size := 0;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4Compress(var MD4Context : TMD4Context);
var
    Data : array [0..15] of DWord;
    A, B, C, D: DWord;
begin
    Move(MD4Context.HashBuffer, Data, Sizeof(Data));
    A:= MD4Context.CurrentHash[0];
    B:= MD4Context.CurrentHash[1];
    C:= MD4Context.CurrentHash[2];
    D:= MD4Context.CurrentHash[3];

    A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 0], 3);
    D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 1], 7);
    C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 2], 11);
    B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 3], 19);
    A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 4], 3);
    D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 5], 7);
    C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 6], 11);
    B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 7], 19);
    A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 8], 3);
    D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 9], 7);
    C:= LRot32(C + (B xor (D and (A xor B))) + Data[10], 11);
    B:= LRot32(B + (A xor (C and (D xor A))) + Data[11], 19);
    A:= LRot32(A + (D xor (B and (C xor D))) + Data[12], 3);
    D:= LRot32(D + (C xor (A and (B xor C))) + Data[13], 7);
    C:= LRot32(C + (B xor (D and (A xor B))) + Data[14], 11);
    B:= LRot32(B + (A xor (C and (D xor A))) + Data[15], 19);

    A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 0] + $5a827999, 3);
    D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 4] + $5a827999, 5);
    C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 8] + $5a827999, 9);
    B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[12] + $5a827999, 13);
    A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 1] + $5a827999, 3);
    D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 5] + $5a827999, 5);
    C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 9] + $5a827999, 9);
    B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[13] + $5a827999, 13);
    A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 2] + $5a827999, 3);
    D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 6] + $5a827999, 5);
    C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[10] + $5a827999, 9);
    B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[14] + $5a827999, 13);
    A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 3] + $5a827999, 3);
    D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 7] + $5a827999, 5);
    C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[11] + $5a827999, 9);
    B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[15] + $5a827999, 13);

    A:= LRot32(A + (B xor C xor D) + Data[ 0] + $6ed9eba1, 3);
    D:= LRot32(D + (A xor B xor C) + Data[ 8] + $6ed9eba1, 9);
    C:= LRot32(C + (D xor A xor B) + Data[ 4] + $6ed9eba1, 11);
    B:= LRot32(B + (C xor D xor A) + Data[12] + $6ed9eba1, 15);
    A:= LRot32(A + (B xor C xor D) + Data[ 2] + $6ed9eba1, 3);
    D:= LRot32(D + (A xor B xor C) + Data[10] + $6ed9eba1, 9);
    C:= LRot32(C + (D xor A xor B) + Data[ 6] + $6ed9eba1, 11);
    B:= LRot32(B + (C xor D xor A) + Data[14] + $6ed9eba1, 15);
    A:= LRot32(A + (B xor C xor D) + Data[ 1] + $6ed9eba1, 3);
    D:= LRot32(D + (A xor B xor C) + Data[ 9] + $6ed9eba1, 9);
    C:= LRot32(C + (D xor A xor B) + Data[ 5] + $6ed9eba1, 11);
    B:= LRot32(B + (C xor D xor A) + Data[13] + $6ed9eba1, 15);
    A:= LRot32(A + (B xor C xor D) + Data[ 3] + $6ed9eba1, 3);
    D:= LRot32(D + (A xor B xor C) + Data[11] + $6ed9eba1, 9);
    C:= LRot32(C + (D xor A xor B) + Data[ 7] + $6ed9eba1, 11);
    B:= LRot32(B + (C xor D xor A) + Data[15] + $6ed9eba1, 15);

    Inc(MD4Context.CurrentHash[0], A);
    Inc(MD4Context.CurrentHash[1], B);
    Inc(MD4Context.CurrentHash[2], C);
    Inc(MD4Context.CurrentHash[3], D);
    MD4Context.Index:= 0;
    FillChar(MD4Context.HashBuffer, Sizeof(MD4Context.HashBuffer), 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4Final(var MD4Context : TMD4Context; var Digest : TMD4Digest);
begin
    if not MD4Context.FInitialized then
        raise MD4Exception.Create('MD4 not initialized');
    MD4Context.HashBuffer[MD4Context.Index] := $80;
    if MD4Context.Index >= 56 then
      MD4Compress(MD4Context);
    PDWord(@(MD4Context.HashBuffer[56]))^ := MD4Context.LenLo;
    PDWord(@(MD4Context.HashBuffer[60]))^ := MD4Context.LenHi;
    MD4Compress(MD4Context);
    Move(MD4Context.CurrentHash, Digest, Sizeof(MD4Context.CurrentHash));
    MD4Burn(MD4Context);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MD4UpdateStr(var MD4Context : TMD4Context; const Str: String);
begin
    if Str = '' then
        MD4Update(MD4Context, PChar(0)^, 0)
    else
        MD4Update(MD4Context, Str[1], Length(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MD4String(const Value : String) : String;
var
    MD4Context: TMD4Context;
begin
    MD4Init(MD4Context);
    MD4UpdateStr(MD4Context, Value);
    SetLength(Result, 16);
    MD4Final(MD4Context, PMD4Digest(@Result[1])^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF SELFTEST}
function MD4SelfTest : Boolean;
const
  Test1Out: TMD4Digest =
    ($a4,$48,$01,$7a,$af,$21,$d8,$52,$5f,$c1,$0a,$e8,$7a,$a6,$72,$9d);
  Test2Out: TMD4Digest =
    ($d7,$9e,$1c,$30,$8a,$a5,$bb,$cd,$ee,$a8,$ed,$63,$df,$41,$2d,$a9);
var
    MD4Context : TMD4Context;
    TestOut    : TMD4Digest;
begin
    MD4Init(MD4Context);
    MD4UpdateStr(MD4Context, 'abc');
    MD4Final(MD4Context, TestOut);
    Result:= CompareMem(@TestOut, @Test1Out, Sizeof(Test1Out));
    MD4Init(MD4Context);
    MD4UpdateStr(MD4Context, 'abcdefghijklmnopqrstuvwxyz');
    MD4Final(MD4Context, TestOut);
    Result:= CompareMem(@TestOut, @Test2Out, Sizeof(Test2Out)) and Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
   MD4SelfTest;

{$ENDIF}

end.
