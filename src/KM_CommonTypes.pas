unit KM_CommonTypes;
{$I KaM_Remake.inc}
interface


type
  TKMByteArray = array of Byte;
  TKMByte2Array = array of array of Byte;
  PKMByte2Array = ^TKMByte2Array;
  TKMWordArray = array of Word;
  PKMWordArray = ^TKMWordArray;
  TKMCardinalArray = array of Cardinal;
  TIntegerArray = array of Integer;
  TStringArray = array of string;
  TRGBArray = array of record R,G,B: Byte end;

  TEvent = procedure of object;
  TPointEvent = procedure (Sender: TObject; const X,Y: Integer) of object;
  TIntegerEvent = procedure (aValue: Integer) of object;
  TUnicodeStringEvent = procedure (const aData: UnicodeString) of object;
  TGameStartEvent = procedure (const aData: UnicodeString; Spectating: Boolean) of object;
  TResyncEvent = procedure (aSender: ShortInt; aTick: cardinal) of object;
  TIntegerStringEvent = procedure (aValue: Integer; const aText: UnicodeString) of object;

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
