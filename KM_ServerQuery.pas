unit KM_ServerQuery;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_Utils, KM_MasterServer;

type
  TKMServerInfo = record
    Name: string;
    IP: string;
    Port: string;
    Room:word;
    GameState: string;
    PlayerCount: word;
    Ping: word;
  end;

  TKMServerList = class
    private
      fCount:integer;
      fServers:array of TKMServerInfo;
      procedure AddServer(aIP, aPort, aName: string; aPing: word);
      function GetServer(aIndex:integer):TKMServerInfo;
      procedure Clear;
      procedure LoadFromText(const aText: string);
    public
      property Servers[aIndex:integer]:TKMServerInfo read GetServer; default;
      property Count:integer read fCount;
  end;

  TKMServerQuery = class
  private
    fMasterServer: TKMMasterServer;
    fServerList: TKMServerList;

    fOnListUpdated: TNotifyEvent;
    fOnAnnouncements: TGetStrProc;

    procedure ReceiveServerList(const S: string);
    procedure ReceiveAnnouncements(const S: string);
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
  fServers[fCount].GameState := 'Lobby';
  fServers[fCount].PlayerCount := 0;
  fServers[fCount].Room := 0;
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
  SetLength(fServers,0);
end;


procedure TKMServerList.LoadFromText(const aText: string);
var
  Strings, Items: TStringList;
  i: integer;
begin
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


constructor TKMServerQuery.Create(aMasterServerAddress:string);
begin
  Inherited Create;
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress);
  fMasterServer.OnServerList := ReceiveServerList;
  fMasterServer.OnAnnouncements := ReceiveAnnouncements;
  fServerList := TKMServerList.Create;
end;


destructor TKMServerQuery.Destroy;
begin
  fMasterServer.Free;
  fServerList.Free;
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
begin
  fServerList.LoadFromText(S);
  if Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then fOnAnnouncements(S);
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
begin
  fMasterServer.UpdateStateIdle;
end;


end.

