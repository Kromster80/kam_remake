{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Sep 03, 2004
Version:      8.00
Description:  DES encryption
Credit:       Implementation of the Data Encryption Standard (DES) as
              described in the book:
              Schneier, B., "Applied Cryptography - Protocols Algorithms, and
              source code in C", John Wiley & Sons, Inc. 1994.
              Original turbo-pascal implementation by Menno Victor van der Star
              <s795238@dutiws.twi.tudelft.nl>. His code is available at
              http://www.geocities.com/SiliconValley/2926/tpsrc/despas.txt
              F. Piette updated the code to use typed parameters.
EMail:        francois.piette@overbyte.be      http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2010 by François PIETTE
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
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDES;

{$R-}
{$Q-}
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}

interface

const
    IcsDESVersion          = 800;
    CopyRight : String     = ' IcsDES (c) 2004-2012 F. Piette V8.00 ';

type
    TArrayOf8Bytes  = array [0..7] of Byte;
    PArrayOf8Bytes  = ^TArrayOf8Bytes;

procedure DES(           { Function encrypt/decrypt data using DES algotithm }
    const Input  : TArrayOf8Bytes;       { 64 bit of input                   }
    var   Output : TArrayOf8Bytes;       { 64 bit output from DES algorithm  }
    const Key    : TArrayOf8Bytes;       { 64 bit key for DES algorithm      }
    Encrypt      : Boolean);             { TRUE to encrypt, FALSE to decrypt }

implementation

type
    TArrayOf16Bytes = array [1..16] of Byte;
    TArrayOf28Bytes = array [1..28] of Byte;
    TArrayOf32Bytes = array [1..32] of Byte;
    TArrayOf48Bytes = array [1..48] of Byte;
    TArrayOf56Bytes = array [1..56] of Byte;
    TArrayOf64Bytes = array [1..64] of Byte;

    TDesData = record
        InputValue             : TArrayOf64Bytes;
        OutputValue            : TArrayOf64Bytes;
        RoundKeys              : array [1..16] of TArrayOf48Bytes;
        L, R                   : TArrayOf32Bytes;
        FunctionResult         : TArrayOf32Bytes;
        C, D                   : TArrayOf28Bytes;
    end;

const
    { Initial Permutation }
    IP : TArrayOf64Bytes =      (58, 50, 42, 34, 26, 18, 10, 2, 
                                 60, 52, 44, 36, 28, 20, 12, 4, 
                                 62, 54, 46, 38, 30, 22, 14, 6, 
                                 64, 56, 48, 40, 32, 24, 16, 8,
                                 57, 49, 41, 33, 25, 17,  9, 1,
                                 59, 51, 43, 35, 27, 19, 11, 3,
                                 61, 53, 45, 37, 29, 21, 13, 5,
                                 63, 55, 47, 39, 31, 23, 15, 7);
    { Final Permutation }
    InvIP : TArrayOf64Bytes =   (40,  8, 48, 16, 56, 24, 64, 32,
                                 39,  7, 47, 15, 55, 23, 63, 31,
                                 38,  6, 46, 14, 54, 22, 62, 30,
                                 37,  5, 45, 13, 53, 21, 61, 29,
                                 36,  4, 44, 12, 52, 20, 60, 28,
                                 35,  3, 43, 11, 51, 19, 59, 27,
                                 34,  2, 42, 10, 50, 18, 58, 26,
                                 33,  1, 41,  9, 49, 17, 57, 25);
    { Expansion Permutation }
    E : TArrayOf48Bytes =       (32,  1,  2,  3,  4,  5,
                                  4,  5,  6,  7,  8,  9,
                                  8,  9, 10, 11, 12, 13,
                                 12, 13, 14, 15, 16, 17,
                                 16, 17, 18, 19, 20, 21,
                                 20, 21, 22, 23, 24, 25,
                                 24, 25, 26, 27, 28, 29,
                                 28, 29, 30, 31, 32,  1);
    { P-Box permutation }
    P : TArrayOf32Bytes =       (16,  7, 20, 21, 29, 12, 28, 17,
                                  1, 15, 23, 26,  5, 18, 31, 10,
                                  2,  8, 24, 14, 32, 27,  3,  9,
                                 19, 13, 30,  6, 22, 11,  4, 25);
    { Key Permutation }
    PC_1 : TArrayOf56Bytes =    (57, 49, 41, 33, 25, 17,  9,
                                  1, 58, 50, 42, 34, 26, 18,
                                 10,  2, 59, 51, 43, 35, 27,
                                 19, 11,  3, 60, 52, 44, 36,
                                 63, 55, 47, 39, 31, 23, 15,
                                  7, 62, 54, 46, 38, 30, 22,
                                 14,  6, 61, 53, 45, 37, 29,
                                 21, 13,  5, 28, 20, 12,  4);
    { Compression Permutation }
    PC_2 : TArrayOf48Bytes =    (14, 17, 11, 24,  1,  5,
                                  3, 28, 15,  6, 21, 10,
                                 23, 19, 12,  4, 26,  8,
                                 16,  7, 27, 20, 13,  2,
                                 41, 52, 31, 37, 47, 55,
                                 30, 40, 51, 45, 33, 48,
                                 44, 49, 39, 56, 34, 53,
                                 46, 42, 50, 36, 29, 32);
    { Number of key bits shifted per round }
    ST : TArrayOf16Bytes =      ( 1,  1,  2,  2,  2,  2,  2,  2,
                                  1,  2,  2,  2,  2,  2,  2,  1);
    { S-Boxes }
    SBoxes : array [1..8,  0..3,  0..15] of Byte =
          (((14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7),
            ( 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8),
            ( 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0),
            (15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13)),

            ((15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10),
            ( 3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5),
            ( 0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15),
            (13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9)),

            ((10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8),
            (13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1),
            (13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7),
            ( 1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12)),

            ((7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15),
            (13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9),
            (10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4),
            ( 3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14)),

            ((2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9),
            (14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6),
            ( 4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14),
            (11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3)),

            ((12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11),
            (10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8),
            ( 9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6),
            ( 4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13)),

            ((4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1),
            (13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6),
            ( 1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2),
            ( 6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12)),

            ((13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7),
            ( 1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2),
            ( 7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8),
            ( 2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)));



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetBit(const Bits: TArrayOf8Bytes; Index: Byte): Byte;
begin
    Dec(Index);
    if Bits[Index div 8] and (128 shr(Index mod 8)) > 0 then
        Result := 1
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SetBit(var Bits: TArrayOf8Bytes; Index, Value : Byte);
var
    Bit: Byte;
begin
    Dec(Index);
    Bit := 128 shr(Index mod 8);
    case Value of
    0: Bits[Index div 8] := Bits[Index div 8] and (not Bit);
    1: Bits[Index div 8] := Bits[Index div 8] or Bit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure F(
    var FR       : TArrayOf32Bytes;
    var FK       : TArrayOf48Bytes;
    var TotalOut : TArrayOf32Bytes);
var
    Temp1 : TArrayOf48Bytes;
    Temp2 : TArrayOf32Bytes;
    n, h, i, j, Row, Column : Integer;
begin
    for n := 1 to 48 do
        Temp1[n] := FR[E[n]] xor FK[n];
    for n := 1 to 8 do begin
        i      := (n - 1) * 6;
        j      := (n - 1) * 4;
        Row    := Temp1[i + 1] * 2 + Temp1[i + 6];
        Column := Temp1[i + 2] * 8 + Temp1[i + 3] * 4 +
                  Temp1[i + 4] * 2 + Temp1[i + 5];
        for h := 1 to 4 Do begin
            case h of
            1: Temp2[j + h] := (SBoxes[n, Row, Column] and 8) div 8;
            2: Temp2[j + h] := (SBoxes[n, Row, Column] and 4) div 4;
            3: Temp2[j + h] := (SBoxes[n, Row, Column] and 2) div 2;
            4: Temp2[j + h] := (SBoxes[n, Row, Column] and 1);
            end;
        end;
    end;
    for n := 1 to 32 do
        TotalOut[n] := Temp2[P[n]];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Shift(var SubKeyPart : TArrayOf28Bytes);
var
    n, b: Byte;
begin
    b := SubKeyPart[1];
    for n := 1 to 27 do
        SubKeyPart[n] := SubKeyPart[n + 1];
    SubKeyPart[28] := b;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SubKey(
    var DesData : TDesData;
    Round       : Byte;
    var SubKey  : TArrayOf48Bytes);
var
    n, b : Byte;
begin
    for n := 1 to ST[Round] do begin
        Shift(DesData.C);
        Shift(DesData.D);
    end;
    for n := 1 to 48 do begin
        b := PC_2[n];
        if b <= 28 then
            SubKey[n] := DesData.C[b]
        else
            SubKey[n] := DesData.D[b - 28];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DES(
    const Input  : TArrayOf8Bytes;
    var   Output : TArrayOf8Bytes;
    const Key    : TArrayOf8Bytes;
    Encrypt      : Boolean);
var
    n, b, Round : Byte;
    DesData : TDesData;
begin
    for n := 1 to 64 do
        DesData.InputValue[n] := GetBit(Input, n);
    for n := 1 to 28 do begin
        DesData.C[n] := GetBit(Key, PC_1[n]);
        DesData.D[n] := GetBit(Key, PC_1[n + 28]);
    end;
    for n := 1 to 16 do
        SubKey(DesData, n, DesData.RoundKeys[n]);
    for n := 1 to 64 do begin
        if n <= 32 then
            DesData.L[n]      := DesData.InputValue[IP[n]]
        else
            DesData.R[n - 32] := DesData.InputValue[IP[n]];
    end;
    for Round := 1 to 16 do begin
        if Encrypt then
            F(DesData.R, DesData.RoundKeys[Round], DesData.FunctionResult)
        else
            F(DesData.R, DesData.RoundKeys[17 - Round], DesData.FunctionResult);
        for n := 1 to 32 do
            DesData.FunctionResult[n] := DesData.FunctionResult[n] xor DesData.L[n];
        DesData.L := DesData.R;
        DesData.R := DesData.FunctionResult;
    end;
    for n := 1 to 64 do begin
        b := InvIP[n];
        if b <= 32 then
            DesData.OutputValue[n] := DesData.R[b]
        else
            DesData.OutputValue[n] := DesData.L[b-32];
    end;
    for n := 1 to 64 do
        SetBit(Output, n, DesData.OutputValue[n]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
