unit KM_ServerQuery;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_Utils, KM_MasterServer;

type
  TKMServerInfo = record
    IP: string;
    Port: string;
    Name: string;
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
  end;

  TKMServerQuery = class
  private
    fMasterServer: TKMMasterServer;
    fServerList: TKMServerList;

    fOnListUpdated: TNotifyEvent;

    procedure ReceiveServerList(const S: string);
  public
    constructor Create(aMasterServerAddress:string);
    destructor Destroy; override;

    property OnListUpdated:TNotifyEvent write fOnListUpdated;
    procedure RefreshList;
    procedure UpdateStateIdle;
  end;

implementation


procedure TKMServerList.AddServer(aIP, aPort, aName: string; aPing: word);
begin
  if Length(fServers) <= fCount then SetLength(fServers,fCount+16);
  fServers[fCount].IP := aIP;
  fServers[fCount].Port := aPort;
  fServers[fCount].Name := aName;
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
    if Items.Count = 2 then //Must have 2 parameters
      AddServer(Items[0], Items[1], '', 0);
  end;

  Items.Free;
  Strings.Free;
end;


constructor TKMServerQuery.Create(aMasterServerAddress:string);
begin
  Inherited Create;
  fMasterServer := TKMMasterServer.Create(aMasterServerAddress);
  fMasterServer.OnServerList := ReceiveServerList;
  fServerList := TKMServerList.Create;
end;


destructor TKMServerQuery.Destroy;
begin
  fMasterServer.Free;
  fServerList.Free;
  Inherited;
end;


procedure TKMServerQuery.RefreshList;
begin
  fMasterServer.QueryServer; //Start the query
end;


procedure TKMServerQuery.ReceiveServerList(const S: string);
begin
  if S = '' then exit; //No servers running
  fServerList.LoadFromText(S);
  if Assigned(fOnListUpdated) then fOnListUpdated(Self);
end;


procedure TKMServerQuery.UpdateStateIdle;
begin
  fMasterServer.UpdateStateIdle;
end;


end.

