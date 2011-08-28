unit KM_ServerQuery;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, Windows, KM_Defaults, KM_CommonTypes, KM_Utils, KM_MasterServer, KM_NetClient;

const
  MAX_QUERIES = 16; //The maximum number of individual server queries that can be happening at once
  QUERY_TIMEOUT = 5000; //The maximum amount of time for an individual query to take (will take at least 2*ping)

type
  TKMServerInfo = record
    Name: string;
    IP: string;
    Port: string;
    Ping: word;
    RoomCount:integer;
    Rooms: array of record
                      RoomID:integer;
                      PlayerCount:integer;
                      GameState: string;
                    end;
  end;

  TServerDataEvent = procedure(aServerID:integer; aData:string; aPingStarted:cardinal) of object;

  TKMQuery = class
  private
    fNetClient: TKMNetClient;
    fQueryIsDone: boolean;
    fPingStarted:cardinal;
    fQueryStarted:cardinal;
    fIndexOnServer:integer;
    fServerID:integer;
    fOnServerData: TServerDataEvent;
    fOnQueryDone: TNotifyEvent;
    procedure NetClientReceive(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal);
    procedure PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure PerformQuery(aAddress, aPort:string; aServerID:integer);
    procedure Disconnect;
    property OnServerData:TServerDataEvent write fOnServerData;
    property OnQueryDone:TNotifyEvent write fOnQueryDone;
    procedure UpdateStateIdle;
  end;

  TKMServerList = class
    private
      fCount:integer;
      fLastQueried:integer;
      fServers:array of TKMServerInfo;
      procedure AddServer(aIP, aPort, aName: string; aPing: word);
      function GetServer(aIndex:integer):TKMServerInfo;
      procedure Clear;
      procedure LoadFromText(const aText: string);
    public
      property Servers[aIndex:integer]:TKMServerInfo read GetServer; default;
      property Count:integer read fCount;
      procedure TakeNewQuery(aQuery:TKMQuery);
      procedure LoadData(aServerID:integer; M:TKMemoryStream; aPing:integer);
  end;

  TKMServerQuery = class
  private
    fMasterServer: TKMMasterServer;
    fServerList: TKMServerList;
    fQuery: array[0..MAX_QUERIES-1] of TKMQuery;

    fOnListUpdated: TNotifyEvent;
    fOnAnnouncements: TGetStrProc;

    procedure ReceiveServerList(const S: string);
    procedure ReceiveAnnouncements(const S: string);

    procedure ServerDataReceive(aServerID:integer; aData:string; aPingStarted:cardinal);
    procedure QueryDone(Sender:TObject);
    function GetCount:integer;
  public
    constructor Create(aMasterServerAddress:string);
    destructor Destroy; override;

    property OnListUpdated:TNotifyEvent write fOnListUpdated;
    property OnAnnouncements:TGetStrProc write fOnAnnouncements;
    property Count: integer read GetCount;
    function GetServer(aIndex:integer):TKMServerInfo;
    procedure RefreshList;
    procedure FetchAnnouncements(const aLang: string);
    procedure UpdateStateIdle;
  end;

implementation


procedure TKMServerList.AddServer(aIP, aPort, aName: string; aPing: word);
begin
  if Length(fServers) <= fCount then SetLength(fServers,fCount+16);
  fServers[fCount].Name := aName;
  fServers[fCount].IP := aIP;
  fServers[fCount].Port := aPort;
  fServers[fCount].Ping := aPing;
  fServers[fCount].RoomCount := 0;
  SetLength(fServers[fCount].Rooms,0);
  inc(fCount);
end;


function TKMServerList.GetServer(aIndex:integer):TKMServerInfo;
begin
  Result := fServers[aIndex];
end;


procedure TKMServerList.Clear;
begin
  fCount := 0;
  SetLength(fServers,0);
end;


procedure TKMServerList.LoadFromText(const aText: string);
var
  Strings, Items: TStringList;
  i: integer;
begin
  fLastQueried := -1; //First is 0
  Clear;
  Strings := TStringList.Create;
  Items := TStringList.Create;
  Strings.Text := aText; //Parse according to EOLs
  for i:=0 to Strings.Count-1 do
  begin
    ParseDelimited(Items, Strings[i], ','); //Automatically clears Items and loads each value
    if Items.Count = 3 then //Must have 3 parameters
      AddServer(Items[1], Items[2], Items[0], 0);
  end;

  Items.Free;
  Strings.Free;
end;


procedure TKMServerList.TakeNewQuery(aQuery:TKMQuery);
begin
  if fLastQueried < fCount-1 then
  begin
    inc(fLastQueried);
    aQuery.PerformQuery(Servers[fLastQueried].IP, Servers[fLastQueried].Port, fLastQueried);
  end
  else
    aQuery.Disconnect;
end;


procedure TKMServerList.LoadData(aServerID:integer; M:TKMemoryStream; aPing:integer);
var i: integer;
begin
  with fServers[aServerID] do
  begin
    Ping := aPing;
    M.Position := 0;
    M.Read(RoomCount);
    SetLength(Rooms,RoomCount);
    for i:=0 to RoomCount-1 do
    begin
      M.Read(Rooms[i].RoomID);
      M.Read(Rooms[i].PlayerCount);
      Rooms[i].GameState := 'Loaded'; //Debug
    end;
  end;
end;


constructor TKMQuery.Create;
begin
  Inherited;
  fNetClient := TKMNetClient.Create;
  fQueryIsDone := false;
end;


destructor TKMQuery.Destroy;
begin
  fNetClient.Free;
  Inherited;
end;


procedure TKMQuery.PerformQuery(aAddress, aPort:string; aServerID:integer);
begin
  fServerID := aServerID;
  fQueryStarted := GetTickCount;
  fNetClient.Disconnect;
  fNetClient.OnRecieveData := NetClientReceive;
  fNetClient.ConnectTo(aAddress,aPort);
end;


procedure TKMQuery.Disconnect;
begin
  fNetClient.Disconnect;
  fNetClient.OnRecieveData := nil;
end;


procedure TKMQuery.UpdateStateIdle;
begin
  fNetClient.UpdateStateIdle;
  if GetTickCount-fQueryStarted > QUERY_TIMEOUT then
    fQueryIsDone := true; //Give up
  if fQueryIsDone then
  begin
    fOnQueryDone(Self);
    fQueryIsDone := false;
  end;
end;


procedure TKMQuery.NetClientReceive(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal);
var
  Kind:TKMessageKind;
  M:TKMemoryStream;
  Param:integer;
  Msg:string;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(Kind, SizeOf(TKMessageKind)); //Depending on kind message contains either Text or a Number
  case NetPacketType[Kind] of
    pfNumber: M.Read(Param);
    pfText:   M.Read(Msg);
  end;
  M.Free;

  case Kind of
    mk_GameVersion:
      if Msg <> GAME_REVISION then
        fQueryIsDone := true;

    mk_IndexOnServer:
    begin
      fIndexOnServer := Param;
      fPingStarted := GetTickCount;
      PacketSend(NET_ADDRESS_SERVER,mk_GetServerInfo,'',0);
    end;

    mk_ServerInfo:
    begin
      fOnServerData(fServerID, Msg, fPingStarted);
      fQueryIsDone := true; //We cannot call fOnQueryDone now because that would disconnect the socket halfway through the receive procedure (crashes)
    end;
  end;
end;


procedure TKMQuery.PacketSend(aRecipient:integer; aKind:TKMessageKind; const aText:string; aParam:integer);
var M:TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.Write(aKind, SizeOf(TKMessageKind));

  case NetPacketType[aKind] of
    pfNumber: M.Write(aParam);
    pfText:   M.Write(aText);
  end;

  fNetClient.SendData(fIndexOnServer, aRecipient, M.Memory, M.Size);
  M.Free;
end;


constructor TKMServerQuery.Create(aMasterServerAddress:string);
var i: integer;
begin
  Inherited Create;
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress);
  fMasterServer.OnServerList := ReceiveServerList;
  fMasterServer.OnAnnouncements := ReceiveAnnouncements;
  fServerList := TKMServerList.Create;
  for i:=0 to MAX_QUERIES-1 do
  begin
    fQuery[i] := TKMQuery.Create;
    fQuery[i].OnServerData := ServerDataReceive;
    fQuery[i].OnQueryDone := QueryDone;
  end;
end;


destructor TKMServerQuery.Destroy;
var i:integer;
begin
  fMasterServer.Free;
  fServerList.Free;
  for i:=0 to MAX_QUERIES-1 do
    fQuery[i].Free;
  Inherited;
end;


function TKMServerQuery.GetServer(aIndex:integer):TKMServerInfo;
begin
  Result := fServerList.Servers[aIndex];
end;


procedure TKMServerQuery.RefreshList;
begin
  fMasterServer.QueryServer; //Start the query
end;


procedure TKMServerQuery.ReceiveServerList(const S: string);
var i: integer;
begin
  fServerList.LoadFromText(S);
  for i:=0 to MAX_QUERIES-1 do
    fServerList.TakeNewQuery(fQuery[i]);
  if Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then fOnAnnouncements(S);
end;


procedure TKMServerQuery.ServerDataReceive(aServerID:integer; aData:string; aPingStarted:cardinal);
var M: TKMemoryStream;
begin
  M := TKMemoryStream.Create;
  M.WriteAsText(aData);
  fServerList.LoadData(aServerID, M, GetTickCount-aPingStarted);
  M.Free;
  if Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.QueryDone(Sender:TObject);
begin
  fServerList.TakeNewQuery(TKMQuery(Sender));
end;


function TKMServerQuery.GetCount:integer;
begin
  Result := fServerList.Count;
end;


procedure TKMServerQuery.FetchAnnouncements(const aLang: string);
begin
  fMasterServer.FetchAnnouncements(aLang);
end;


procedure TKMServerQuery.UpdateStateIdle;
var i:integer;
begin
  fMasterServer.UpdateStateIdle;
  for i:=0 to MAX_QUERIES-1 do
    fQuery[i].UpdateStateIdle;
end;


end.

