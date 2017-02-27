unit KM_NetFileTransfer;
{$I KaM_Remake.inc}
interface
uses
  SysUtils, Math, KM_Defaults, KM_CommonClasses
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};

const MAX_TRANSFERS = 8; //One for each player
type
  TTransferEvent = procedure(aClientIndex: Integer) of object;
  TTransferPacketEvent = procedure(aClientIndex: Integer; aStream: TKMemoryStream; out SendBufferEmpty: Boolean) of object;
  TTransferProgressEvent = procedure(Total, Progress: Cardinal) of object;
  TKMTransferType = (kttMap, kttSave);

  TKMFileSender = class
  private
    fReceiverIndex: Integer;
    fChunksInFlight: Byte;
    fSendStream: TKMemoryStream;
    procedure AddFileToStream(aFileName, aPostFix, aExt: UnicodeString);
  public
    constructor Create(aType: TKMTransferType; aName: UnicodeString; aMapFolder: TMapFolder; aReceiverIndex: Integer);
    destructor Destroy; override;
    procedure WriteChunk(aStream: TKMemoryStream; aLength: Cardinal);
    procedure AckReceived;
    function StreamEnd: Boolean;
    property ReceiverIndex: Integer read fReceiverIndex;
  end;

  TKMFileReceiver = class
  private
    fReceiveStream: TKMemoryStream;
    fType: TKMTransferType;
    fName: UnicodeString;
    fMapCRC: Cardinal;
    fTotalSize: Cardinal;
    fReceivedSize: Cardinal;
    procedure ClearExistingFiles;
    function ValidExtension(Ext: UnicodeString): Boolean;
  public
    constructor Create(aType: TKMTransferType; aName: UnicodeString; aMapCRC: Cardinal = 0);
    destructor Destroy; override;
    procedure DataReceived(aStream: TKMemoryStream);
    property Name: UnicodeString read fName;
    property TotalSize: Cardinal read fTotalSize;
    property ReceivedSize: Cardinal read fReceivedSize;
    function ProcessTransfer: Boolean;
  end;

  TKMFileSenderManager = class
  private
    fSenders: array[1..MAX_TRANSFERS] of TKMFileSender;
    fOnTransferCompleted: TTransferEvent;
    fOnTransferPacket: TTransferPacketEvent;
    function ActiveTransferCount: Byte;
  public
    destructor Destroy; override;
    function StartNewSend(aType: TKMTransferType; aName: UnicodeString; aMapFolder: TMapFolder; aReceiverIndex: Integer): Boolean;
    procedure AbortAllTransfers;
    procedure AckReceived(aReceiverIndex: Integer);
    procedure ClientDisconnected(aReceiverIndex: Integer);
    procedure UpdateStateIdle(SendBufferEmpty: Boolean);
    property OnTransferCompleted: TTransferEvent write fOnTransferCompleted;
    property OnTransferPacket: TTransferPacketEvent write fOnTransferPacket;
  end;

implementation
uses
  KM_NetworkTypes, KM_Maps;

const
  //TODO: Add LIBX and WAV support for maps
  VALID_MAP_EXTENSIONS:  array[1..5] of UnicodeString =         ('map','dat','script','txt','pdf');
  VALID_MAP_EXTENSIONS_POSTFIX:  array[1..2] of UnicodeString = ('libx','wav');
  VALID_SAVE_EXTENSIONS: array[1..3] of UnicodeString =         ('sav','bas','rpl');


function GetFullSourceFileName(aType: TKMTransferType; aName: UnicodeString; aMapFolder: TMapFolder; Postfix, aExt: UnicodeString): UnicodeString;
begin
  case aType of
    kttMap:  Result := TKMapsCollection.FullPath(aName, Postfix + '.' + aExt, aMapFolder);
    kttSave: Result := ExeDir + SAVES_MP_FOLDER_NAME + PathDelim + aName + '.' + aExt;
  end;
end;


function GetFullDestFileName(aType: TKMTransferType; aName, Postfix, aExt: UnicodeString): UnicodeString;
begin
  case aType of
    kttMap:  Result := TKMapsCollection.FullPath(aName, Postfix + '.' + aExt, mfDL);
    kttSave: Result := ExeDir + SAVES_MP_FOLDER_NAME + PathDelim + DOWNLOADED_LOBBY_SAVE + '.' + aExt;
  end;
end;


{ TKMFileSender }
constructor TKMFileSender.Create(aType: TKMTransferType; aName: UnicodeString; aMapFolder: TMapFolder; aReceiverIndex: Integer);
var
  I: Integer;
  FileName: UnicodeString;
  F: TSearchRec;
  SourceStream: TKMemoryStream;
  CompressionStream: TCompressionStream;
begin
  inherited Create;
  fReceiverIndex := aReceiverIndex;
  fSendStream := TKMemoryStream.Create;
  fSendStream.WriteA('TransferCompressed');
  fSendStream.Write(aType, SizeOf(aType));
  fSendStream.WriteW(aName);
  //Fill stream with data to be sent
  case aType of
    kttMap:  begin
               for I := Low(VALID_MAP_EXTENSIONS) to High(VALID_MAP_EXTENSIONS) do
               begin
                 FileName := GetFullSourceFileName(aType, aName, aMapFolder, '', VALID_MAP_EXTENSIONS[I]);
                 if FileExists(FileName) then
                   AddFileToStream(FileName, '', VALID_MAP_EXTENSIONS[I]);
               end;
               for I := Low(VALID_MAP_EXTENSIONS_POSTFIX) to High(VALID_MAP_EXTENSIONS_POSTFIX) do
               begin
                 FileName := GetFullSourceFileName(aType, aName, aMapFolder, '.*', VALID_MAP_EXTENSIONS_POSTFIX[I]);
                 if FindFirst(FileName, faAnyFile, F) = 0 then
                 begin
                   repeat
                     if (F.Attr and faDirectory = 0) then
                       AddFileToStream(ExtractFilePath(FileName) + F.Name, ExtractFileExt(ChangeFileExt(F.Name,'')), VALID_MAP_EXTENSIONS_POSTFIX[I]);
                   until FindNext(F) <> 0;
                   FindClose(F);
                 end;
               end;
             end;
    kttSave: for I := Low(VALID_SAVE_EXTENSIONS) to High(VALID_SAVE_EXTENSIONS) do
             begin
               FileName := GetFullSourceFileName(aType, aName, aMapFolder, '', VALID_SAVE_EXTENSIONS[I]);
               if FileExists(FileName) then
                 AddFileToStream(FileName, '', VALID_SAVE_EXTENSIONS[I]);
             end;
  end;
  //Compress fSendStream
  SourceStream := fSendStream;
  fSendStream := TKMemoryStream.Create;
  fSendStream.WriteA('Transfer');
  CompressionStream := TCompressionStream.Create(cldefault, fSendStream);
  CompressionStream.CopyFrom(SourceStream, 0);
  //fSendStream now contains the compressed data from SourceStream
  CompressionStream.Free;
  SourceStream.Free;
  fSendStream.Position := 0;
end;


destructor TKMFileSender.Destroy;
begin
  fSendStream.Free;
  inherited;
end;


procedure TKMFileSender.AckReceived;
begin
  Dec(fChunksInFlight);
end;


procedure TKMFileSender.AddFileToStream(aFileName, aPostFix, aExt: UnicodeString);
var FileStream: TKMemoryStream;
begin
  FileStream := TKMemoryStream.Create;
  FileStream.LoadFromFile(aFileName);

  fSendStream.WriteA('FileStart');
  fSendStream.WriteW(aPostFix);
  fSendStream.WriteW(aExt);
  fSendStream.Write(Cardinal(FileStream.Size));
  if FileStream.Size > 0 then
    fSendStream.CopyFrom(FileStream, FileStream.Size);

  FileStream.Free;
end;


procedure TKMFileSender.WriteChunk(aStream: TKMemoryStream; aLength: Cardinal);
begin
  if aLength > fSendStream.Size - fSendStream.Position then
    aLength := fSendStream.Size - fSendStream.Position;

  aStream.WriteA('FileChunk');
  aStream.Write(aLength);
  aStream.Write(Cardinal(fSendStream.Size)); //Every chunk includes the total transfer size
  aStream.CopyFrom(fSendStream, aLength);
  Inc(fChunksInFlight);
end;


function TKMFileSender.StreamEnd: Boolean;
begin
  Result := fSendStream.Position = fSendStream.Size;
end;


{ TKMFileReceiver }
constructor TKMFileReceiver.Create(aType: TKMTransferType; aName: UnicodeString; aMapCRC: Cardinal = 0);
begin
  inherited Create;
  fReceiveStream := TKMemoryStream.Create;
  fType := aType;
  fName := aName;
  fMapCRC := aMapCRC;
end;


destructor TKMFileReceiver.Destroy;
begin
  fReceiveStream.Free;
  inherited;
end;


procedure TKMFileReceiver.DataReceived(aStream: TKMemoryStream);
var ChunkSize: Cardinal;
begin
  aStream.ReadAssert('FileChunk');
  aStream.Read(ChunkSize);
  aStream.Read(fTotalSize); //Every chunk includes the total transfer size
  Assert(aStream.Size - aStream.Position = ChunkSize, 'Chunk corrupted');
  fReceiveStream.CopyFrom(aStream, ChunkSize);
  fReceivedSize := fReceivedSize + ChunkSize;
end;


procedure TKMFileReceiver.ClearExistingFiles;
var
  FileName: UnicodeString;
  F: TSearchRec;
  I: Integer;
begin
  //Prepare destination
  case fType of
    kttMap:  begin
               //Create downloads folder if it's missing
               FileName := ExeDir + MAPS_DL_FOLDER_NAME;
               if not DirectoryExists(FileName) then
                 CreateDir(FileName);
               //Create map folder if it is missing
               FileName := FileName + PathDelim + fName;
               if not DirectoryExists(FileName) then
                 CreateDir(FileName)
               else
                 //If any files already exist in the folder, delete them
                 if FindFirst(FileName + PathDelim + fName + '*.*', faAnyFile, F) = 0 then
                 begin
                   repeat
                     if (F.Attr and faDirectory = 0) then
                       DeleteFile(FileName + PathDelim + F.Name);
                   until FindNext(F) <> 0;
                   FindClose(F);
                 end;
             end;
    kttSave: begin
               if not DirectoryExists(ExeDir + SAVES_MP_FOLDER_NAME) then
                 CreateDir(ExeDir + SAVES_MP_FOLDER_NAME)
               else
               begin
                 FileName := ExeDir + SAVES_MP_FOLDER_NAME + PathDelim + DOWNLOADED_LOBBY_SAVE;
                 for I := Low(VALID_SAVE_EXTENSIONS) to High(VALID_SAVE_EXTENSIONS) do
                   if FileExists(FileName + '.' + VALID_SAVE_EXTENSIONS[I]) then
                     DeleteFile(FileName + '.' + VALID_SAVE_EXTENSIONS[I]);
               end;
             end;
  end;
end;


function TKMFileReceiver.ValidExtension(Ext: UnicodeString): Boolean;
var I: Integer;
begin
  case fType of
    kttMap: begin
               for I := Low(VALID_MAP_EXTENSIONS) to High(VALID_MAP_EXTENSIONS) do
                 if Ext = VALID_MAP_EXTENSIONS[I] then
                 begin
                   Result := True;
                   Exit;
                 end;
               for I := Low(VALID_MAP_EXTENSIONS_POSTFIX) to High(VALID_MAP_EXTENSIONS_POSTFIX) do
                 if Ext = VALID_MAP_EXTENSIONS_POSTFIX[I] then
                 begin
                   Result := True;
                   Exit;
                 end;
            end;
    kttSave: for I := Low(VALID_SAVE_EXTENSIONS) to High(VALID_SAVE_EXTENSIONS) do
               if Ext = VALID_SAVE_EXTENSIONS[I] then
               begin
                 Result := True;
                 Exit;
               end;
  end;
  Result := False;
end;


function TKMFileReceiver.ProcessTransfer: Boolean;
var
  ReadType: TKMTransferType;
  ReadName, Ext, Postfix, FileName: UnicodeString;
  ReadSize: Cardinal;
  FileStream: TKMemoryStream;
  DecompressionStream: TDecompressionStream;
  ReadStream: TKMemoryStream;
begin
  Result := False;
  if fReceiveStream.Size = 0 then Exit; //Transfer was aborted

  //Decompress the stream
  fReceiveStream.Position := 0;
  fReceiveStream.ReadAssert('Transfer');
  DecompressionStream := TDecompressionStream.Create(fReceiveStream);
  //We need custom methods like ReadAssert, ReadW, etc. so we need to read from a TKMemoryStream
  ReadStream := TKMemoryStream.Create;
  ReadStream.CopyFromDecompression(DecompressionStream);
  DecompressionStream.Free;
  ReadStream.Position := 0;

  //Read from the stream
  ReadStream.ReadAssert('TransferCompressed');
  ReadStream.Read(ReadType, SizeOf(ReadType));
  Assert(ReadType = fType, 'Unexpected transfer type received');
  ReadStream.ReadW(ReadName);
  if (ReadName <> fName) and (ReadName + '_' + IntToHex(fMapCRC, 8) <> fName) then
    raise Exception.Create('Unexpected transfer name received');

  ClearExistingFiles;

  //Load each file
  while ReadStream.Position < ReadStream.Size do
  begin
    ReadStream.ReadAssert('FileStart');
    ReadStream.ReadW(Postfix);
    ReadStream.ReadW(Ext);
    //Check EXT is valid (so we don't allow EXEs and stuff)
    Assert(ValidExtension(Ext), 'Unexpected file extension received');

    ReadStream.Read(ReadSize);
    FileStream := TKMemoryStream.Create;
    FileStream.CopyFrom(ReadStream, ReadSize);

    FileName := GetFullDestFileName(fType, fName, Postfix, Ext);
    Assert(not FileExists(FileName), 'Transfer file already exists');
    FileStream.SaveToFile(FileName);
    FileStream.Free;
  end;
  ReadStream.Free;
  Result := True;
end;

{ TKMFileSenderManager }
procedure TKMFileSenderManager.AbortAllTransfers;
var I: Integer;
begin
  for I := Low(fSenders) to High(fSenders) do
    FreeAndNil(fSenders[I]);
end;

procedure TKMFileSenderManager.AckReceived(aReceiverIndex: Integer);
var I: Integer;
begin
  for I := Low(fSenders) to High(fSenders) do
    if (fSenders[I] <> nil) and (fSenders[I].ReceiverIndex = aReceiverIndex) then
      fSenders[I].AckReceived;
end;

procedure TKMFileSenderManager.ClientDisconnected(aReceiverIndex: Integer);
var I: Integer;
begin
  for I := Low(fSenders) to High(fSenders) do
    if (fSenders[I] <> nil) and (fSenders[I].ReceiverIndex = aReceiverIndex) then
      FreeAndNil(fSenders[I]);
end;

destructor TKMFileSenderManager.Destroy;
var I: Integer;
begin
  for I := Low(fSenders) to High(fSenders) do
    FreeAndNil(fSenders[I]);
  inherited;
end;

function TKMFileSenderManager.StartNewSend(aType: TKMTransferType; aName: UnicodeString; aMapFolder: TMapFolder; aReceiverIndex: Integer): Boolean;
var I: Integer;
begin
  for I := Low(fSenders) to High(fSenders) do
    if (fSenders[I] = nil) or (fSenders[I].ReceiverIndex = aReceiverIndex) then
    begin
      if fSenders[I] <> nil then
        //There is an existing transfer to this client, so free it
        fSenders[I].Free;

      fSenders[I] := TKMFileSender.Create(aType, aName, aMapFolder, aReceiverIndex);
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TKMFileSenderManager.ActiveTransferCount: Byte;
var I: Integer;
begin
  Result := 0;
  for I := Low(fSenders) to High(fSenders) do
    if fSenders[I] <> nil then
      Inc(Result);
end;

procedure TKMFileSenderManager.UpdateStateIdle(SendBufferEmpty: Boolean);
var
  I: Integer;
  Stream: TKMemoryStream;
  ClientIndex: Integer;
  MaxChunksInFlightPerSender: Byte;
begin
  //Reserve some bandwidth for each sender
  MaxChunksInFlightPerSender := Max(1, MAX_CHUNKS_BEFORE_ACK div Max(1, ActiveTransferCount));
  for I := Low(fSenders) to High(fSenders) do
    while (fSenders[I] <> nil) and (fSenders[I].fChunksInFlight < MaxChunksInFlightPerSender) and SendBufferEmpty do
    begin
      Stream := TKMemoryStream.Create;
      fSenders[I].WriteChunk(Stream, FILE_CHUNK_SIZE);
      fOnTransferPacket(fSenders[I].ReceiverIndex, Stream, SendBufferEmpty); //Updates SendBufferEmpty
      Stream.Free;
      if fSenders[I].StreamEnd then
      begin
        ClientIndex := fSenders[I].ReceiverIndex;
        FreeAndNil(fSenders[I]); //We must free it before calling OnTransferCompleted
        fOnTransferCompleted(ClientIndex);
      end;
    end;
end;

end.
