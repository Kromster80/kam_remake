unit KM_ServerQuery;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonTypes, KM_Locales,
  KM_NetworkTypes, KM_Utils, KM_MasterServer, KM_NetClient;

const
  MAX_QUERIES = 16; //The maximum number of individual server queries that can be happening at once
  QUERY_TIMEOUT = 5000; //The maximum amount of time for an individual query to take (will take at least 2*ping)

type
  TKMServerInfo = record
    Name: string;
    IP: string;
    Port: string;
    ServerType: TKMServerType;
    Ping: Word;
  end;

  TKMRoomInfo = record
    ServerIndex: Integer; //Points to some TKMServerInfo in TKMServerList
    RoomID: Integer;
    OnlyRoom: Boolean; //Is this the only room in the server?
    GameInfo: TMPGameInfo;
  end;

  TServerDataEvent = procedure(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal) of object;

  TServerSortMethod = (
    ssmByTypeAsc, ssmByTypeDesc, //Client or dedicated
    ssmByPasswordAsc, ssmByPasswordDesc, //Passworded or not
    ssmByNameAsc, ssmByNameDesc, //Server names
    ssmByPingAsc, ssmByPingDesc, //Server pings
    ssmByStateAsc, ssmByStateDesc, //Room state
    ssmByPlayersAsc, ssmByPlayersDesc); //Player count in room

  TKMQuery = class
  private
    fNetClient: TKMNetClient;
    fQueryActive:boolean;
    fQueryIsDone: boolean;
    fPingStarted:cardinal;
    fQueryStarted:cardinal;
    fIndexOnServer:integer;
    fServerID:integer;
    fOnServerData: TServerDataEvent;
    fOnQueryDone: TNotifyEvent;
    procedure NetClientReceive(aNetClient:TKMNetClient; aSenderIndex:integer; aData:pointer; aLength:cardinal);
    procedure PacketSend(aRecipient: Integer; aKind: TKMessageKind); overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PerformQuery(aAddress, aPort:string; aServerID:integer);
    procedure Disconnect;
    property OnServerData:TServerDataEvent read fOnServerData write fOnServerData;
    property OnQueryDone:TNotifyEvent read fOnQueryDone write fOnQueryDone;
    procedure UpdateStateIdle;
  end;

  TKMRoomList = class
  private
    fCount:integer;
    fRooms:array of TKMRoomInfo;
    procedure AddRoom(aServerIndex, aRoomID: Integer; aOnlyRoom:Boolean; aGameInfoStream: TKMemoryStream);
    function GetRoom(aIndex: Integer): TKMRoomInfo;
    procedure Clear;
  public
    destructor Destroy; override;
    property Rooms[aIndex: Integer]: TKMRoomInfo read GetRoom; default;
    property Count: Integer read fCount;
    procedure LoadData(aServerID:integer; aStream: TKMemoryStream);
    procedure SwapRooms(A,B:Integer);
  end;

  TKMServerList = class
  private
    fCount:integer;
    fLastQueried:integer;
    fServers:array of TKMServerInfo;
    procedure AddServer(aIP, aPort, aName: string; aType: TKMServerType; aPing: word);
    function GetServer(aIndex: Integer): TKMServerInfo;
    procedure Clear;
    procedure LoadFromText(const aText: UnicodeString);
  public
    property Servers[aIndex: Integer]: TKMServerInfo read GetServer; default;
    property Count: Integer read fCount;
    procedure TakeNewQuery(aQuery:TKMQuery);
    procedure SetPing(aServerID:integer; aPing: Cardinal);
  end;

  //Handles the master-server querrying and carries ServerList
  TKMServerQuery = class
  private
    fMasterServer: TKMMasterServer;
    fServerList: TKMServerList; //List of servers fetch from master
    fRoomList: TKMRoomList; //Info about each room populated after query completed
    fQuery: array[0..MAX_QUERIES-1] of TKMQuery;
    fQueriesCompleted: Integer;
    fSortMethod: TServerSortMethod;

    fOnListUpdated: TNotifyEvent;
    fOnAnnouncements: TUnicodeStringEvent;

    procedure ReceiveServerList(const S: string);
    procedure ReceiveAnnouncements(const S: string);

    procedure ServerDataReceive(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal);
    procedure QueryDone(Sender:TObject);

    procedure Sort;
    procedure SetSortMethod(aMethod: TServerSortMethod);
  public
    constructor Create(aMasterServerAddress: string);
    destructor Destroy; override;

    property OnListUpdated: TNotifyEvent read fOnListUpdated write fOnListUpdated;
    property OnAnnouncements: TUnicodeStringEvent read fOnAnnouncements write fOnAnnouncements;
    property Servers: TKMServerList read fServerList;
    property Rooms: TKMRoomList read fRoomList;

    property SortMethod: TServerSortMethod read fSortMethod write SetSortMethod;

    procedure RefreshList;
    procedure FetchAnnouncements;
    procedure SendMapInfo(const aMapName: string; aPlayerCount: Integer);
    procedure UpdateStateIdle;
  end;


implementation


{ TKMRoomList }
destructor TKMRoomList.Destroy;
begin
  Clear; //Frees GameInfo
  inherited;
end;


procedure TKMRoomList.AddRoom(aServerIndex, aRoomID: Integer; aOnlyRoom:Boolean; aGameInfoStream: TKMemoryStream);
begin
  if Length(fRooms) <= fCount then SetLength(fRooms, fCount+16);
  fRooms[fCount].ServerIndex := aServerIndex;
  fRooms[fCount].RoomID := aRoomID;
  fRooms[fCount].OnlyRoom := aOnlyRoom;
  fRooms[fCount].GameInfo := TMPGameInfo.Create;
  fRooms[fCount].GameInfo.LoadFromStream(aGameInfoStream);
  inc(fCount);
end;


function TKMRoomList.GetRoom(aIndex:integer):TKMRoomInfo;
begin
  Result := fRooms[aIndex];
end;


procedure TKMRoomList.Clear;
var i: Integer;
begin
  for i := 0 to fCount - 1 do
    fRooms[i].GameInfo.Free;
  fCount := 0;
  SetLength(fRooms, 0);
end;


procedure TKMRoomList.LoadData(aServerID:integer; aStream: TKMemoryStream);
var
  i, RoomCount, RoomID: Integer;
begin
  aStream.Read(RoomCount);
  for i := 0 to RoomCount - 1 do //We don't actually use i as the server sends us RoomID (rooms might not be in order)
  begin
    aStream.Read(RoomID);
    AddRoom(aServerID, RoomID, (RoomCount = 1), aStream);
  end;
end;


procedure TKMRoomList.SwapRooms(A,B: Integer);
var TempRoomInfo: TKMRoomInfo;
begin
  TempRoomInfo := fRooms[A];
  fRooms[A] := fRooms[B];
  fRooms[B] := TempRoomInfo;
end;


{ TKMServerList }
procedure TKMServerList.AddServer(aIP, aPort, aName: string; aType: TKMServerType; aPing: word);
begin
  if Length(fServers) <= fCount then SetLength(fServers, fCount+16);
  fServers[fCount].Name := aName;
  fServers[fCount].IP := aIP;
  fServers[fCount].Port := aPort;
  fServers[fCount].ServerType := aType;
  fServers[fCount].Ping := aPing;
  inc(fCount);
end;


function TKMServerList.GetServer(aIndex:integer):TKMServerInfo;
begin
  Result := fServers[aIndex];
end;


procedure TKMServerList.Clear;
begin
  fCount := 0;
  SetLength(fServers, 0);
end;


procedure TKMServerList.LoadFromText(const aText: UnicodeString);

  function GetServerType(aDedicated, aOS: UnicodeString): TKMServerType;
  begin
    if aDedicated = '0' then
      Result := st_Client
    else
      Result := st_Dedicated;
  end;

var
  Strings, Items: TStringList;
  I: Integer;
begin
  fLastQueried := -1; //First is 0
  Clear;
  Strings := TStringList.Create;
  Items := TStringList.Create;
  Strings.Text := aText; //Parse according to EOLs
  for I := 0 to Strings.Count - 1 do
  begin
    ParseDelimited(Items, Strings[I], ','); //Automatically clears Items and loads each value
    if Items.Count = 5 then //Must have 5 parameters
      AddServer(Items[1], Items[2], Items[0], GetServerType(Items[3], Items[4]), 0);
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


procedure TKMServerList.SetPing(aServerID:Integer; aPing: Cardinal);
begin
  fServers[aServerID].Ping := aPing;
end;


{ TKMQuery }
constructor TKMQuery.Create;
begin
  inherited;
  fNetClient := TKMNetClient.Create;
  fQueryIsDone := false;
  fQueryActive := false;
end;


destructor TKMQuery.Destroy;
begin
  fNetClient.Free;
  inherited;
end;


procedure TKMQuery.PerformQuery(aAddress, aPort:string; aServerID:integer);
begin
  fQueryActive := true;
  fServerID := aServerID;
  fQueryStarted := TimeGet;
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
  if not fQueryActive then exit;
  fNetClient.UpdateStateIdle;
  if GetTimeSince(fQueryStarted) > QUERY_TIMEOUT then
    fQueryIsDone := true; //Give up
  if fQueryIsDone then
  begin
    fQueryIsDone := false;
    fQueryActive := false;
    fOnQueryDone(Self);
  end;
end;


procedure TKMQuery.NetClientReceive(aNetClient: TKMNetClient; aSenderIndex: Integer; aData: Pointer; aLength: Cardinal);
var
  M: TKMemoryStream;
  Kind: TKMessageKind;
  tmpInteger: Integer;
  tmpString: UnicodeString;
begin
  Assert(aLength >= 1, 'Unexpectedly short message'); //Kind, Message

  M := TKMemoryStream.Create;
  M.WriteBuffer(aData^, aLength);
  M.Position := 0;
  M.Read(Kind, SizeOf(TKMessageKind)); //Depending on kind message contains either Text or a Number

  case Kind of
    mk_GameVersion:
      begin
        M.Read(tmpString);
        if tmpString <> NET_PROTOCOL_REVISON then
          fQueryIsDone := True;
      end;

    mk_IndexOnServer:
      begin
        M.Read(tmpInteger);
        fIndexOnServer := tmpInteger;
        fPingStarted := TimeGet;
        PacketSend(NET_ADDRESS_SERVER, mk_GetServerInfo);
      end;

    mk_ServerInfo:
      begin
        fOnServerData(fServerID, M, fPingStarted);
        fQueryIsDone := true; //We cannot call fOnQueryDone now because that would disconnect the socket halfway through the receive procedure (crashes)
      end;
  end;

  M.Free;
end;


procedure TKMQuery.PacketSend(aRecipient: Integer; aKind: TKMessageKind);
var
  M: TKMemoryStream;
begin
  Assert(NetPacketType[aKind] = pfNoData);

  M := TKMemoryStream.Create;
  M.Write(aKind, SizeOf(TKMessageKind));

  fNetClient.SendData(fIndexOnServer, aRecipient, M.Memory, M.Size);
  M.Free;
end;


{ TKMServerQuery }
constructor TKMServerQuery.Create(aMasterServerAddress: string);
var i: integer;
begin
  inherited Create;
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress, False);
  fMasterServer.OnServerList := ReceiveServerList;
  fMasterServer.OnAnnouncements := ReceiveAnnouncements;

  fSortMethod := ssmByStateAsc; //Default sorting is by state

  fServerList := TKMServerList.Create;
  fRoomList := TKMRoomList.Create;
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
  fRoomList.Free;
  for i:=0 to MAX_QUERIES-1 do
    fQuery[i].Free;
  inherited;
end;


procedure TKMServerQuery.RefreshList;
begin
  if LOCAL_SERVER_LIST then
    ReceiveServerList('Localhost,127.0.0.1,56789,0,Windows') //For debugging
    //+#13+'Localhost,127.0.0.1,56788,1,Windows'+#13+'Localhost,127.0.0.1,56787,1,Unix'
  else
    fMasterServer.QueryServer; //Start the query
end;


procedure TKMServerQuery.ReceiveServerList(const S: string);
var i: integer;
begin
  fQueriesCompleted := 0;
  fServerList.LoadFromText(S);
  fRoomList.Clear; //Updated rooms list will be loaded soon
  for i:=0 to MAX_QUERIES-1 do
    fServerList.TakeNewQuery(fQuery[i]);
  //If there are no servers we should update the list now, otherwise wait for the first server
  if (fServerList.fCount = 0) and Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then fOnAnnouncements(S);
end;


procedure TKMServerQuery.ServerDataReceive(aServerID: Integer; aStream: TKMemoryStream; aPingStarted: Cardinal);
begin
  fRoomList.LoadData(aServerID, aStream); //Tell RoomsList to load data about rooms
  fServerList.SetPing(aServerID, GetTimeSince(aPingStarted)); //Tell ServersList ping

  Sort; //Apply sorting
  if Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.QueryDone(Sender: TObject);
begin
  inc(fQueriesCompleted);
  fServerList.TakeNewQuery(TKMQuery(Sender));
  //When we receive the last query we should update the list (this removes 'Refreshing...' when no servers respond)
  if (fQueriesCompleted = fServerList.Count) and Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


//Get the server announcements in specified language
procedure TKMServerQuery.FetchAnnouncements;
begin
  fMasterServer.FetchAnnouncements(fLocales.UserLocale);
end;


procedure TKMServerQuery.SendMapInfo(const aMapName: string; aPlayerCount: Integer);
begin
  fMasterServer.SendMapInfo(aMapName, aPlayerCount);
end;


procedure TKMServerQuery.UpdateStateIdle;
var i: Integer;
begin
  fMasterServer.UpdateStateIdle;
  for i:=0 to MAX_QUERIES-1 do
    fQuery[i].UpdateStateIdle;
end;


procedure TKMServerQuery.Sort;

  function Compare(A, B: TKMRoomInfo; aMethod: TServerSortMethod):Boolean;
  var AServerInfo, BServerInfo: TKMServerInfo;
  const StateSortOrder: array[TMPGameState] of byte = (4,1,2,3);
  begin
    Result := False; //By default everything remains in place
    AServerInfo := Servers[A.ServerIndex];
    BServerInfo := Servers[B.ServerIndex];
    case aMethod of
      ssmByPasswordAsc: Result := A.GameInfo.PasswordLocked and not B.GameInfo.PasswordLocked;
      ssmByPasswordDesc:Result := not A.GameInfo.PasswordLocked and B.GameInfo.PasswordLocked;
      ssmByTypeAsc:     Result := AServerInfo.ServerType > BServerInfo.ServerType;
      ssmByTypeDesc:    Result := AServerInfo.ServerType < BServerInfo.ServerType;
      ssmByNameAsc:     Result := CompareText(AServerInfo.Name, BServerInfo.Name) > 0;
      ssmByNameDesc:    Result := CompareText(AServerInfo.Name, BServerInfo.Name) < 0;
      ssmByPingAsc:     Result := AServerInfo.Ping > BServerInfo.Ping;
      ssmByPingDesc:    Result := AServerInfo.Ping < BServerInfo.Ping;
      ssmByStateAsc:    Result := StateSortOrder[A.GameInfo.GameState] > StateSortOrder[B.GameInfo.GameState];
      ssmByStateDesc:   Result := StateSortOrder[A.GameInfo.GameState] < StateSortOrder[B.GameInfo.GameState];
      ssmByPlayersAsc:  Result := A.GameInfo.PlayerCount > B.GameInfo.PlayerCount;
      ssmByPlayersDesc: Result := A.GameInfo.PlayerCount < B.GameInfo.PlayerCount;
    end;
  end;

var
  i, k: Integer;
begin
  for i:=0 to fRoomList.Count-1 do
  for k:=i to fRoomList.Count-1 do
  if Compare(Rooms[i], Rooms[k], fSortMethod) then
    fRoomList.SwapRooms(i,k);
end;


procedure TKMServerQuery.SetSortMethod(aMethod: TServerSortMethod);
begin
  fSortMethod := aMethod;
  Sort; //New sorting method has been set, we need to apply it
end;


end.
