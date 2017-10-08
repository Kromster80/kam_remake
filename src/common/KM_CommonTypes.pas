unit KM_CommonTypes;
{$I KaM_Remake.inc}
interface

uses
  KM_Defaults;

type
  TBooleanArray = array of Boolean;
  TBoolean2Array = array of array of Boolean;
  TKMByteArray = array of Byte;
  TKMByte2Array = array of array of Byte;
  TKMByteSetArray = array of set of Byte;
  PKMByte2Array = ^TKMByte2Array;
  TKMWordArray = array of Word;
  TKMWord2Array = array of array of Word;
  PKMWordArray = ^TKMWordArray;
  TKMCardinalArray = array of Cardinal;
  PKMCardinalArray = ^TKMCardinalArray;
  TSmallIntArray = array of SmallInt;
  TIntegerArray = array of Integer;
  TInteger2Array = array of array of Integer;
  TSingleArray = array of Single;
  TSingle2Array = array of array of Single;
  TStringArray = array of string;
  TKMCharArray = array of Char;
  TRGBArray = array of record R,G,B: Byte end;
  TKMStaticByteArray = array [0..MaxInt - 1] of Byte;
  PKMStaticByteArray = ^TKMStaticByteArray;

  TEvent = procedure of object;
  TPointEvent = procedure (Sender: TObject; const X,Y: Integer) of object;
  TPointEventFunc = function (Sender: TObject; const X,Y: Integer): Boolean of object;
  TBooleanEvent = procedure (aValue: Boolean) of object;
  TIntegerEvent = procedure (aValue: Integer) of object;
  TSingleEvent = procedure (aValue: Single) of object;
  TUnicodeStringEvent = procedure (const aData: UnicodeString) of object;
  TUnicodeStringBoolEvent = procedure (const aData: UnicodeString; aBool: Boolean) of object;
  TGameStartEvent = procedure (const aData: UnicodeString; Spectating: Boolean) of object;
  TMapStartEvent = procedure (const aData: UnicodeString; aMapFolder: TMapFolder; aCRC: Cardinal; Spectating: Boolean) of object;
  TResyncEvent = procedure (aSender: ShortInt; aTick: cardinal) of object;
  TIntegerStringEvent = procedure (aValue: Integer; const aText: UnicodeString) of object;
  TBooleanFunc = function(Obj: TObject): Boolean of object;
  TBooleanFuncSimple = function: Boolean of object;

  TKMAnimLoop = packed record
                  Step: array [1 .. 30] of SmallInt;
                  Count: SmallInt;
                  MoveX, MoveY: Integer;
                end;

  //Message kind determines icon and available actions for Message
  TKMMessageKind = (
    mkText, //Mission text message
    mkHouse,
    mkUnit,
    mkQuill //Utility message (warnings in script loading)
    );


implementation


end.
