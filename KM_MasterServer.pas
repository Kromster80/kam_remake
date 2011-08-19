unit KM_MasterServer;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_HTTPClient;

type
  TKMMasterServer = class
  private
    fHTTPClient: TKMHTTPClient;
    fMasterServerAddress: string;
    fOnError:TGetStrProc;
    fOnServerList:TGetStrProc;

    procedure Receive(const S: string);
    procedure Error(const S: string);
  public
    constructor Create(aMasterServerAddress:string);
    destructor Destroy; override;

    property OnError:TGetStrProc write fOnError;
    property OnServerList:TGetStrProc write fOnServerList;
    procedure AnnounceServer(aPort:string; aTTL:integer);
    procedure QueryServer;
    procedure UpdateStateIdle;
  end;

implementation


constructor TKMMasterServer.Create(aMasterServerAddress:string);
begin
  Inherited Create;
  fHTTPClient := TKMHTTPClient.Create;
  fHTTPClient.OnReceive := nil;
  fHTTPClient.OnError := Error;
  fMasterServerAddress := aMasterServerAddress;
end;


destructor TKMMasterServer.Destroy;
begin
  fHTTPClient.Free;
  Inherited;
end;


procedure TKMMasterServer.Error(const S: string);
begin
  if Assigned(fOnError) then fOnError(S);
end;


procedure TKMMasterServer.Receive(const S: string);
begin
  if Assigned(fOnServerList) then fOnServerList(S);
end;


procedure TKMMasterServer.AnnounceServer(aPort:string; aTTL:integer);
begin
  fHTTPClient.GetURL(fMasterServerAddress+'serveradd.php?port='+aPort+'&ttl='+IntToStr(aTTL));
  fHTTPClient.OnReceive := nil; //We don't care about the response
end;


procedure TKMMasterServer.QueryServer;
begin
  fHTTPClient.GetURL(fMasterServerAddress+'serverquery.php');
  fHTTPClient.OnReceive := Receive;
end;


procedure TKMMasterServer.UpdateStateIdle;
begin
  fHTTPClient.UpdateStateIdle;
end;


end.

