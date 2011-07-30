unit BGRADNetDeserial;

{$mode objfpc}{$H+}

interface

{ This unit allow to read .Net serialized classes with BinaryFormatter of
  namespace System.Runtime.Serialization.Formatters.Binary.

  Serialization is a process by which objects in memory are saved according
  to their structure.

  This unit is used by BGRAPaintNet to read Paint.NET images. }

uses
  Classes, SysUtils;

type
  TTypeCategory = (ftPrimitiveType = 0, ftString = 1, ftObjectType =
    2, ftRuntimeType = 3,
    ftGenericType = 4, ftArrayOfObject = 5, ftArrayOfString = 6,
    ftArrayOfPrimitiveType = 7);

  TPrimitiveType = (ptNone = 0, ptBoolean = 1, ptByte = 2, ptChar = 3, ptDecimal = 5,
    ptDouble = 6, ptInt16 = 7, ptInt32 = 8, ptInt64 = 9, ptSByte = 10, ptSingle = 11,
    ptDateTime = 13, ptUInt16 = 14, ptUInt32 = 15, ptUInt64 = 16, ptString = 18);

  ArrayOfNameValue = array of record
    Name: string;
    Value, valueType: string;
  end;

  TFieldType = record
    category: TTypeCategory;
    primitiveType: TPrimitiveType;
    refAssembly: longword;
    Name: string;
  end;

  TSerializedType = record
    ClassName:   string;
    nbFields:    integer;
    fieldNames:  array of string;
    fieldTypes:  array of TFieldType;
    refAssembly: longword;
  end;

  TAssemblyReference = record
    idAssembly: longword;
    Name: string;
  end;

  PSerializedObject = ^TSerializedObject;

  TSerializedObject = record
    idObject:   longword;
    numType:    integer;
    fields:     ArrayOfNameValue;
    refCount:   integer;
    inToString: boolean;
  end;

  { TDotNetDeserialization }
  TDotNetDeserialization = class
    objectTypes: array of TSerializedType;
    assemblies:  array of TAssemblyReference;
    objects:     array of TSerializedObject;

    function FindObject(typeName: string): PSerializedObject;
    function GetSimpleField(obj: TSerializedObject; Name: string): string;
    function FieldIndex(obj: TSerializedObject; Name: string): integer;
    function GetObjectField(obj: TSerializedObject; Name: string): PSerializedObject;
    function GetObject(id: string): PSerializedObject;
    function GetObject(id: longword): PSerializedObject;
    function GetObjectType(obj: PSerializedObject): string;
    function PrimitiveTypeName(pt: TPrimitiveType): string;
    function IsBoxedValue(obj: TSerializedObject; index: integer): boolean;
    function GetBoxedValue(obj: TSerializedObject; index: integer): string;
    function IsReferenceType(numType: integer; index: integer): boolean;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(filename: string);
    function ToString: ansistring;
    constructor Create;
  private
    EndOfStream:      boolean;
    ArrayFillerCount: integer;
    currentAutoObjectValue: longword;
    function nextAutoObjectId: longword;
    function LoadNextFromStream(Stream: TStream): longword;
    function LoadStringFromStream(Stream: TStream): string;
    function LoadTypeFromStream(Stream: TStream; IsRuntimeType: boolean): integer;
    function LoadValuesFromStream(Stream: TStream; numType: integer): ArrayOfNameValue;
    function LoadValueFromStream(Stream: TStream; fieldType: TFieldType): string;
    function GetTypeOfObject(idObject: longword): integer;
  end;

function WinReadWord(Stream: TStream): word;
function WinReadSmallInt(Stream: TStream): smallint;
function WinReadLongint(Stream: TStream): longint;
function WinReadLongword(Stream: TStream): longword;
function WinReadInt64(Stream: TStream): int64;
function WinReadQWord(Stream: TStream): QWord;

implementation

const
  //block types
  btRefTypeObject = 1;
  btRuntimeObject = 4;
  btExternalObject = 5;
  btString      = 6;
  btGenericArray = 7;
  btBoxedPrimitiveTypeValue = 8;
  btObjectReference = 9;
  btNullValue   = 10;
  btEndOfStream = 11;
  btAssembly    = 12;
  btArrayFiller8b = 13;
  btArrayFiller32b = 14;
  btArrayOfPrimitiveType = 15;
  btArrayOfObject = 16;
  btArrayOfString = 17;
  btMethodCall  = 21;
  btMethodResponse = 22;

  idArrayFiller = $80000000;

{$hints off}
function WinReadWord(Stream: TStream): word;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadSmallInt(Stream: TStream): smallint;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadLongint(Stream: TStream): longint;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadLongword(Stream: TStream): longword;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadInt64(Stream: TStream): int64;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

function WinReadQWord(Stream: TStream): QWord;
begin
  stream.Read(Result, sizeof(Result));
  Result := LEtoN(Result);
end;

{$hints on}

{ TDotNetDeserialization }

function TDotNetDeserialization.FindObject(typeName: string): PSerializedObject;
var
  i, numType:   integer;
  comparedType: string;
begin
  for i := 0 to high(objects) do
  begin
    numType := objects[i].numType;
    if numType >= 0 then
    begin
      comparedType := objectTypes[numType].ClassName;
      if (comparedType = typeName) or (length(typeName) <
        length(comparedType)) and
        (copy(comparedType, length(comparedType) - length(typeName),
        length(typeName) + 1) = '.' + typeName) then
      begin
        Result := @objects[i];
        exit;
      end;
    end;
  end;
  Result := nil;
end;

function TDotNetDeserialization.GetSimpleField(obj: TSerializedObject;
  Name: string): string;
var
  i: integer;
begin
  i := FieldIndex(obj, Name);
  if i = -1 then
    Result := ''
  else
  begin
    if IsBoxedValue(obj, i) then
      Result := GetBoxedValue(obj, i)
    else
      Result := obj.fields[i].Value;
  end;
end;

function TDotNetDeserialization.FieldIndex(obj: TSerializedObject;
  Name: string): integer;
var
  i: integer;
begin
  //case sensitive
  for i := 0 to high(obj.fields) do
    if obj.fields[i].Name = Name then
    begin
      Result := i;
      exit;
    end;
  //case insensitive
  for i := 0 to high(obj.fields) do
    if compareText(obj.fields[i].Name, Name) = 0 then
    begin
      Result := i;
      exit;
    end;
  //case sensitive inner member
  for i := 0 to high(obj.fields) do
    if (length(Name) < length(obj.fields[i].Name)) and
      (copy(obj.fields[i].Name, length(obj.fields[i].Name) - length(Name),
      length(Name) + 1) = '+' + Name) then
    begin
      Result := i;
      exit;
    end;
  //case insensitive inner member
  for i := 0 to high(obj.fields) do
    if (length(Name) < length(obj.fields[i].Name)) and
      (compareText(copy(obj.fields[i].Name, length(obj.fields[i].Name) -
      length(Name), length(Name) + 1), '+' + Name) = 0) then
    begin
      Result := i;
      exit;
    end;
  Result := -1;
end;

function TDotNetDeserialization.GetObjectField(obj: TSerializedObject;
  Name: string): PSerializedObject;
var
  i: integer;
begin
  i := FieldIndex(obj, Name);
  if i = -1 then
    Result := nil
  else
  begin
    if not IsReferenceType(obj.numType, i) then
      raise Exception.Create('GetObjectMember: Not a reference type');
    Result := GetObject(obj.fields[i].Value);
  end;
end;

function TDotNetDeserialization.GetObject(id: string): PSerializedObject;
var
  idObj: longword;
begin
  if copy(id, 1, 1) = '#' then
    Delete(id, 1, 1);
  idObj  := StrToInt(id);
  Result := GetObject(idObj);
end;

function TDotNetDeserialization.GetObject(id: longword): PSerializedObject;
var
  i: integer;
begin
  for i := 0 to high(objects) do
    if objects[i].idObject = id then
    begin
      Result := @objects[i];
      exit;
    end;
  Result := nil;
end;

function TDotNetDeserialization.GetObjectType(obj: PSerializedObject): string;
begin
  if (obj^.numType = -btString) then
    Result := 'String'
  else
  if (obj^.numType = -btArrayOfObject) then
    Result := 'Object[]'
  else
  if (obj^.numType = -btArrayOfString) then
    Result := 'String[]'
  else
  if (obj^.numType < 0) or (obj^.numType > high(objectTypes)) then
    Result := ''
  else
  begin
    Result := objectTypes[obj^.numType].ClassName;
  end;
end;

function TDotNetDeserialization.PrimitiveTypeName(pt: TPrimitiveType): string;
begin
  case pt of
    ptBoolean: Result  := 'Boolean';
    ptByte: Result     := 'Byte';
    ptChar: Result     := 'Char';
    ptDecimal: Result  := 'Decimal';
    ptDouble: Result   := 'Double';
    ptInt16: Result    := 'Int16';
    ptInt32: Result    := 'Int32';
    ptInt64: Result    := 'Int64';
    ptSByte: Result    := 'SByte';
    ptSingle: Result   := 'Single';
    ptDateTime: Result := 'DateTime';
    ptUInt16: Result   := 'UInt16';
    ptUInt32: Result   := 'UInt32';
    ptUInt64: Result   := 'UInt64';
    ptString: Result   := 'String';
    else
      raise Exception.Create('Unknown primitive type (' + IntToStr(byte(pt)) + ')');
  end;
end;

function TDotNetDeserialization.IsBoxedValue(obj: TSerializedObject;
  index: integer): boolean;
var
  subObj: PSerializedObject;
begin
  if not IsReferenceType(obj.numType, index) then
  begin
    Result := False;
    exit;
  end;
  subObj := GetObject(obj.fields[index].Value);
  if subObj = nil then
  begin
    Result := True;
    exit;
  end;
  Result := (length(subObj^.fields) = 1) and (subObj^.fields[0].Name = '');
end;

function TDotNetDeserialization.GetBoxedValue(obj: TSerializedObject;
  index: integer): string;
var
  subObj: PSerializedObject;
begin
  if not IsReferenceType(obj.numType, index) then
    raise Exception.Create('GetBoxedValue: Not a reference type');
  subObj := GetObject(obj.fields[index].Value);
  if subObj = nil then
  begin
    Result := ''; //empty value
    exit;
  end;
  if (length(subObj^.fields) = 1) and (subObj^.fields[0].Name = '') then
    Result := subObj^.fields[0].Value
  else
    raise Exception.Create('GetBoxedValue: Not a primitive type');
end;

function TDotNetDeserialization.IsReferenceType(numType: integer;
  index: integer): boolean;
begin
  if numType >= length(objectTypes) then
    raise Exception.Create('IsReferenceType: Type number out of bounds');

  if (numType < 0) then
  begin
    Result := (numType = -btArrayOfObject) or (numtype = -btArrayOfString);
  end
  else
  begin
    if (index < 0) or (index >= objecttypes[numType].nbFields) then
      raise Exception.Create('IsReferenceType: Index out of bounds');

    Result := (objecttypes[numType].fieldTypes[index].category <> ftPrimitiveType);
  end;
end;

procedure TDotNetDeserialization.LoadFromStream(Stream: TStream);
var
  header: packed record
    blockId: byte;
    value1, value2, value3, value4: longint;
  end;
  curStreamPosition, prevStreamPosition: int64;
begin
  {$hints off}
  if Stream.Read(header, sizeof(header)) <> sizeof(header) then
    raise Exception.Create('Invalid header size');
  if (header.blockId <> 0) or (header.value1 <> 1) or (header.value2 <> -1) or
    (header.value3 <> 1) or (header.value4 <> 0) then
    raise Exception.Create('Invalid header format');
  {$hints on}

  EndOfStream := False;
  curStreamPosition := Stream.Position;
  try
    while (Stream.Position < Stream.Size) and not EndOfStream do
    begin
      prevStreamPosition := curStreamPosition;
      curStreamPosition  := Stream.Position;
      LoadNextFromStream(Stream);
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while loading serialized data at position ' +
        IntToStr(stream.Position) + ' (block starting at ' +
        IntToStr(curStreamPosition) + ', previous block at ' +
        IntToStr(prevStreamPosition) + '). ' + ex.message);
  end;
end;

procedure TDotNetDeserialization.LoadFromFile(filename: string);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

function TDotNetDeserialization.ToString: string;

  function ObjectToString(num: integer; expectedType: string;
    tab: string; main: boolean): string;
  var
    j, k:   integer;
    subId:  longword;
    subNum: integer;
    objType, subExpectedType: string;
  begin
    Result := '';
    if (num < 0) or (num > high(objects)) then
      raise Exception.Create('Index out of bounds');
    with objects[num] do  //here array is not changed so it won't move
    begin
      if inToString then
      begin
        if main then
          Result := ''
        else
          Result := '#' + IntToStr(idObject) + LineEnding;
        exit;
      end;
      inToString := True;
      if main then
      begin
        if numType < 0 then
          objType := ''
        else
          objType := objectTypes[numType].ClassName;
        Result += tab + 'Object';
        if refCount > 0 then
          Result += ' #' + IntToStr(idObject);
        if (objType = '') or (objType = expectedType) then
          Result += ' = '
        else
          Result += ' As ' + objType + ' = ';
      end
      else
      begin
        objType := GetObjectType(@objects[num]);
        if (objType = '') or (objType = expectedType) then
          Result := ''
        else
          Result := '(' + objType + ') ';
        if (idObject < idArrayFiller) and (refCount > 0) then
          Result += '#' + IntToStr(idObject) + ' = ';
      end;
      if (length(objType) > 2) and (copy(objType, length(objType) - 1, 2) = '[]') then
        subExpectedType := copy(objType, 1, length(objType) - 2)
      else
        subExpectedType := '';

      if not main and (length(fields) = 1) and (fields[0].Name = '') then
      begin
        Result += fields[0].Value + LineEnding;
      end
      else
      if (length(fields) = 0) then
      begin
        Result += '{}' + LineEnding;
      end
      else
      begin
        Result += '{' + LineEnding;
        for j := 0 to High(fields) do
        begin
          Result += tab + '  ' + fields[j].Name;
          if (fields[j].valueType <> '') and (fields[j].valueType <> subExpectedType) and
            not ((subExpectedType = '') and ((fields[j].valueType = 'Int32') or
            (fields[j].valueType = 'Boolean'))) then
            Result += ' As ' + fields[j].valueType;
          Result   += ' = ';
          if not IsReferenceType(numType, j) or (copy(fields[j].Value, 1, 1) <> '#') or
            (fields[j].Value = '#0') then
            Result += fields[j].Value + lineending
          else
          begin
            subId  := StrToInt(copy(fields[j].Value, 2, length(fields[j].Value) - 1));
            subNum := -1;
            for k := 0 to high(objects) do
              if (objects[k].idObject = subId) then
              begin
                subNum := k;
                break;
              end;
            if subNum = -1 then
              Result += '#' + IntToStr(subId) + '!' + LineEnding
            else
              Result += objectToString(subNum, fields[j].valueType, tab + '  ', False);
          end;
        end;
        Result += tab + '}' + LineEnding;
        if main then
          Result += LineEnding;
      end;
    end;
  end;

var
  i: integer;
begin
  Result := '';
  for i := 0 to high(assemblies) do
    Result += 'Imports ' + assemblies[i].Name + LineEnding;
  Result   += lineEnding;
  for i := 0 to high(objects) do
    objects[i].inToString := False;
  for i := 0 to high(objects) do
    Result += ObjectToString(i, 'Object', '', True);
end;

constructor TDotNetDeserialization.Create;
begin
  currentAutoObjectValue := idArrayFiller + 1;
end;

function TDotNetDeserialization.nextAutoObjectId: longword;
begin
  Inc(currentAutoObjectValue);
  Result := currentAutoObjectValue;
end;

function TDotNetDeserialization.LoadNextFromStream(Stream: TStream): longword;
var
  blockType:    byte;
  idRefObject, tempIdObject: longword;
  tempType:     TFieldType;
  arrayCount, i, idx, FillZeroCount: integer;
  tempObj:      TSerializedObject;
  tempTypeName: string;
  tempPObj:     PSerializedObject;
begin
  Result := 0; //idObject or zero
   {$hints off}
  Stream.Read(blockType, sizeof(blockType));
   {$hints on}
  case blockType of

    btAssembly:
    begin
      setlength(assemblies, length(assemblies) + 1);
      with assemblies[high(assemblies)] do
      begin
        Stream.Read(idAssembly, 4);
        Name := LoadStringFromStream(Stream);
      end;
    end;

    btRuntimeObject, btExternalObject:
    begin
      setlength(objects, length(objects) + 1);
      idx := high(objects);
      with tempObj do  //use temp because array address may change
      begin
        refCount := 0;
        Stream.Read(idObject, 4);
        Result  := idObject;
        numType := LoadTypeFromStream(Stream, blockType = btRuntimeObject);
      end;
      objects[idx]   := tempObj;
      tempObj.fields := LoadValuesFromStream(Stream, objects[idx].numType);
      objects[idx].fields := tempObj.fields;
    end;

    btRefTypeObject:
    begin
      setlength(objects, length(objects) + 1);
      idx := high(objects);
      with tempObj do  //use temp because array address may change
      begin
        refCount    := 0;
        idObject    := WinReadLongword(Stream);
        Result      := idObject;
        idRefObject := WinReadLongword(Stream);
        numType     := GetTypeOfObject(idRefObject);
      end;
      objects[idx]   := tempObj;
      tempObj.fields := LoadValuesFromStream(Stream, objects[idx].numType);
      objects[idx].fields := tempObj.fields;
    end;

    btString:
    begin
      setlength(objects, length(objects) + 1);
      idx := high(objects);
      with tempObj do  //use temp because array address may change
      begin
        refCount := 0;
        Stream.Read(idObject, 4);
        Result  := idObject;
        numType := -blockType;
        setlength(fields, 1);
        fields[0].Name      := '';
        fields[0].valueType := 'String';
        fields[0].Value     := LoadStringFromStream(Stream);
      end;
      objects[idx] := tempObj;
    end;

    btBoxedPrimitiveTypeValue:
    begin
      try
        setlength(objects, length(objects) + 1);
        idx := high(objects);
        with tempObj do  //use temp because array address may change
        begin
          refCount := 0;
          idObject := nextAutoObjectId;
          Result   := idObject;
          numType  := -blockType;

          tempType.category    := ftPrimitiveType;
          tempType.refAssembly := 0;
          Stream.Read(tempType.primitiveType, 1);
          tempType.Name := PrimitiveTypeName(tempType.primitiveType);

          setlength(fields, 1);
          fields[0].Name      := '';
          fields[0].Value     := LoadValueFromStream(Stream, tempType);
          fields[0].valueType := tempType.Name;
        end;
        objects[idx] := tempObj;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading boxed primitive values. ' +
            ex.Message);
      end;
    end;

    btObjectReference:
    begin
      Stream.Read(Result, 4);
      tempPObj := GetObject(Result);
      if tempPObj <> nil then
        Inc(tempPObj^.refCount);
    end;

    btNullValue: Result := 0;

    btArrayOfPrimitiveType:
    begin
      try
        setlength(objects, length(objects) + 1);
        idx := high(objects);
        with tempObj do  //use temp because array address may change
        begin
          refCount := 0;
          Stream.Read(idObject, 4);
          Result     := idObject;
          arrayCount := WinReadLongint(Stream);

          tempType.category    := ftPrimitiveType;
          tempType.refAssembly := 0;
          Stream.Read(tempType.primitiveType, 1);
          tempType.Name := PrimitiveTypeName(tempType.primitiveType);

          setlength(fields, arrayCount);
          for i := 0 to arrayCount - 1 do
          begin
            fields[i].Name      := '[' + IntToStr(i) + ']';
            fields[i].Value     := LoadValueFromStream(Stream, tempType);
            fields[i].valueType := tempType.Name;
          end;

          setlength(objectTypes, length(objecttypes) + 1);
          numType := high(objectTypes);
          with objectTypes[numType] do
          begin
            ClassName := tempType.Name + '[]';
            nbFields  := arrayCount;
            setlength(fieldNames, nbFields);
            setlength(fieldTypes, nbFields);
            for i := 0 to arrayCount - 1 do
            begin
              fieldNames[i] := fields[i].Name;
              fieldTypes[i] := tempType;
            end;
            refAssembly := 0;
          end;
        end;
        objects[idx] := tempObj;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading array of primitive values. ' +
            ex.Message);
      end;
    end;

    btArrayOfObject, btArrayOfString:
    begin
      try
        setlength(objects, length(objects) + 1);
        idx := high(objects);
        with tempObj do  //use temp because array address may change
        begin
          refCount := 0;
          Stream.Read(idObject, 4);
          Result  := idObject;
          numType := -blockType;
          Stream.Read(arrayCount, 4);
        end;
        objects[idx] := tempObj;
        with tempObj do
        begin
          setlength(fields, arrayCount);
          FillZeroCount := 0;
          if blockType = btArrayOfObject then
            tempTypeName := 'Object'
          else
            tempTypeName := 'String';
          for i := 0 to arrayCount - 1 do
          begin
            fields[i].Name      := '[' + IntToStr(i) + ']';
            fields[i].valueType := tempTypeName;
            if FillZeroCount > 0 then
            begin
              fields[i].Value := '#0';
              Dec(FillZeroCount);
            end
            else
            begin
              tempIdObject := LoadNextFromStream(Stream);
              if tempIdObject = idArrayFiller then
              begin
                tempIdObject     := 0;
                FillZeroCount    := ArrayFillerCount;
                ArrayFillerCount := 0;
              end;
              if FillZeroCount > 0 then
              begin
                fields[i].Value := '#0';
                Dec(FillZeroCount);
              end
              else
              begin
                fields[i].Value := '#' + IntToStr(tempIdObject);
              end;
            end;
          end;
        end;
        objects[idx].fields := tempObj.fields;
      except
        on ex: Exception do
          raise Exception.Create('Error while reading array of object. ' + ex.Message);
      end;
    end;

    btArrayFiller8b, btArrayFiller32b:
    begin
      Result     := idArrayFiller;
      arrayCount := 0;
      if blockType = btArrayFiller8b then
      begin
        Stream.Read(arrayCount, 1);
      end
      else
        Stream.Read(arrayCount, 3);
      arrayCount := LEtoN(arrayCount);
      ArrayFillerCount := arraycount;
    end;

    btGenericArray:
      raise Exception.Create('Generic array not supported');

    btMethodCall, btMethodResponse:
      raise Exception.Create('Method or not supported');

    btEndOfStream: EndOfStream := True;

    else
      raise Exception.Create('Unknown block type (' + IntToStr(blockType) + ')');
  end;
end;

function TDotNetDeserialization.LoadStringFromStream(Stream: TStream): string;
var
  byteLength, shift: byte;
  fullLength: integer;
  utf8value:  string;
begin
  fullLength := 0;
  shift      := 0;
     {$hints off}
  repeat
    Stream.Read(byteLength, 1);
    Inc(fullLength, (byteLength and 127) shl shift);
    shift := shift + 7;
  until (byteLength < 128) or (shift > 24);
     {$hints on}
  setlength(utf8value, fullLength);
  if Stream.Read(utf8value[1], fullLength) <> fullLength then
    raise Exception.Create('String length error');
  Result := Utf8ToAnsi(utf8value);
end;

function TDotNetDeserialization.LoadTypeFromStream(Stream: TStream;
  IsRuntimeType: boolean): integer;
var
  i: integer;
begin
  try
    setlength(objectTypes, length(objectTypes) + 1);
    Result := high(objectTypes);
    with objectTypes[Result] do
    begin
      ClassName := LoadStringFromStream(Stream);
      Stream.Read(nbFields, 4);
      setlength(fieldNames, nbFields);
      setlength(fieldTypes, nbFields);
      for i := 0 to nbFields - 1 do
        fieldNames[i] := LoadStringFromStream(Stream);
      for i := 0 to nbFields - 1 do
        Stream.Read(fieldTypes[i].category, 1);
      for i := 0 to nbFields - 1 do
      begin
        fieldTypes[i].Name := '';
        fieldTypes[i].refAssembly := 0;
        fieldTypes[i].primitiveType := ptNone;
        case fieldTypes[i].category of
          ftPrimitiveType, ftArrayOfPrimitiveType:
          begin
            Stream.Read(fieldTypes[i].primitiveType, 1);
            fieldTypes[i].Name := PrimitiveTypeName(fieldTypes[i].primitiveType);
            if fieldTypes[i].category = ftArrayOfPrimitiveType then
              fieldTypes[i].Name += '[]';
          end;
          ftString: fieldTypes[i].Name      := 'String';
          ftObjectType: fieldTypes[i].Name  := 'Object';
          ftRuntimeType: fieldTypes[i].Name := LoadStringFromStream(Stream);
          ftGenericType:
          begin
            fieldTypes[i].Name := LoadStringFromStream(Stream);
            Stream.Read(fieldTypes[i].refAssembly, 4);
          end;
          ftArrayOfObject: fieldTypes[i].Name := 'Object[]';
          ftArrayOfString: fieldTypes[i].Name := 'String[]';
          else
            raise Exception.Create('Unknown field type tag (' + IntToStr(
              byte(fieldTypes[i].category)) + ')');
        end;
      end;
      if isRuntimeType then
        refAssembly := 0
      else
        Stream.Read(refAssembly, 4);
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while reading object type definition. ' +
        ex.Message);
  end;
end;

function TDotNetDeserialization.LoadValuesFromStream(Stream: TStream;
  numType: integer): ArrayOfNameValue;
var
  i:  integer;
  ot: TSerializedType;
begin
  if (numType < 0) or (numType > high(objectTypes)) then
    raise Exception.Create('Type number out of bounds (' + IntToStr(numType) + ')');
  ot := objectTypes[numType]; //use temp because array address may change
  try
    with ot do
    begin
      setlength(Result, nbFields);
      for i := 0 to nbFields - 1 do
      begin
        Result[i].Name      := fieldNames[i];
        Result[i].valueType := fieldTypes[i].Name;
        Result[i].Value     := LoadValueFromStream(Stream, fieldTypes[i]);
      end;
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while reading values of object of type ' +
        ot.ClassName + '. ' + ex.Message);
  end;
end;

function TDotNetDeserialization.LoadValueFromStream(Stream: TStream;
  fieldType: TFieldType): string;
var
  utf8value:    string;
  utf8len:      byte;
  tempByte:     byte;
  tempDouble:   double;
  tempSingle:   single;
  tempSByte:    shortint;
  tempUInt64:   QWord;
  tempIdObject: longword;
begin
  try
    case fieldType.category of
      ftPrimitiveType: case fieldType.primitiveType of
          ptBoolean:
          begin
                 {$hints off}
            Stream.Read(tempByte, 1);
                 {$hints on}
            if tempByte = 0 then
              Result := 'False'
            else
            if tempByte = 1 then
              Result := 'True'
            else
              raise Exception.Create('Invalid boolean value (' +
                IntToStr(tempByte) + ')');
          end;
          ptByte:
          begin
                 {$hints off}
            Stream.Read(tempByte, 1);
                 {$hints on}
            Result := IntToStr(tempByte);
          end;
          ptChar:
          begin
                 {$hints off}
            Stream.Read(tempByte, 1);
                 {$hints on}
            if tempByte and $80 = 0 then
              utf8len := 1
            else
            if tempByte and $E0 = $C0 then
              utf8len := 2
            else
            if tempByte and $F0 = $E0 then
              utf8len := 3
            else
            if tempByte and $F8 = $F0 then
              utf8len := 4
            else
              raise Exception.Create('Invalid UTF8 char');
            setlength(utf8value, utf8len);
            utf8value[1] := char(tempByte);
            Stream.Read(utf8value[2], utf8len - 1);
            Result := Utf8ToAnsi(utf8value);
          end;
          ptString, ptDecimal: Result := LoadStringFromStream(Stream);
          ptDouble:
          begin
              {$hints off}
            stream.Read(tempDouble, sizeof(tempDouble));
              {$hints on}
            Result := FloatToStr(tempDouble);
          end;
          ptInt16:
          begin
            Result := IntToStr(WinReadSmallInt(stream));
          end;
          ptInt32:
          begin
            Result := IntToStr(WinReadLongInt(stream));
          end;
          ptInt64:
          begin
            Result := IntToStr(WinReadInt64(stream));
          end;
          ptSByte:
          begin
              {$hints off}
            stream.Read(tempSByte, sizeof(tempSByte));
              {$hints on}
            Result := IntToStr(tempSByte);
          end;
          ptSingle:
          begin
              {$hints off}
            stream.Read(tempSingle, sizeof(tempSingle));
              {$hints on}
            Result := FloatToStr(tempSingle);
          end;
          ptUInt16:
          begin
            Result := IntToStr(WinReadWord(stream));
          end;
          ptUInt32:
          begin
            Result := IntToStr(WinReadLongword(stream));
          end;
          ptUInt64:
          begin
            Result := IntToStr(WinReadQWord(stream));
          end;
          ptDateTime:
          begin
            tempUInt64 := WinReadQWord(stream);
            Result     := DateTimeToStr(
              (tempUInt64 and $7FFFFFFFFFFFFFFF - 599264352000000000) / 864000000000);
          end;
          else
            raise Exception.Create('Unknown primitive type (' + IntToStr(
              byte(fieldType.primitiveType)) + ')');
        end;
      ftString, ftObjectType, ftRuntimeType, ftGenericType, ftArrayOfObject,
      ftArrayOfString, ftArrayOfPrimitiveType:
      begin
        tempIdObject := LoadNextFromStream(stream);
        Result := '#' + IntToStr(tempIdObject);

      end;
      else
        raise Exception.Create('Unknown field type (' + IntToStr(
          byte(fieldType.category)) + ')');
    end;
  except
    on ex: Exception do
      raise Exception.Create('Error while reading object value. ' + ex.Message);
  end;
end;

function TDotNetDeserialization.GetTypeOfObject(idObject: longword): integer;
var
  i: integer;
begin
  for i := 0 to high(objects) do
    if objects[i].idObject = idObject then
    begin
      Result := objects[i].numType;
      exit;
    end;
  raise Exception.Create('Object not found (' + IntToStr(idObject) + ')');
end;

initialization


end.

