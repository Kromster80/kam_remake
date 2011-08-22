unit KM_MasterServer;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_Defaults, URLUtils, KM_HTTPClient;

type
  TKMMasterServer = class
  private
    fHTTPClient: TKMHTTPClient;
    fHTTPAnnouncementsClient: TKMHTTPClient; //To fetch the annoucenemnts at the same time as the server list
    fMasterServerAddress: string;
    fOnError:TGetStrProc;
    fOnServerList:TGetStrProc;
    fOnAnnouncements:TGetStrProc;

    procedure Receive(const S: string);
    procedure ReceiveAnnouncements(const S: string);
    procedure Error(const S: string);
  public
    constructor Create(const aMasterServerAddress:string);
    destructor Destroy; override;

    property OnError:TGetStrProc write fOnError;
    property OnServerList:TGetStrProc write fOnServerList;
    property OnAnnouncements:TGetStrProc write fOnAnnouncements;
    procedure AnnounceServer(aName, aPort:string; aTTL:integer);
    procedure QueryServer;
    procedure FetchAnnouncements(const aLang: string);
    procedure UpdateStateIdle;
  end;

implementation


constructor TKMMasterServer.Create(const aMasterServerAddress:string);
begin
  Inherited Create;
  fHTTPClient := TKMHTTPClient.Create;
  fHTTPAnnouncementsClient := TKMHTTPClient.Create;
  fHTTPClient.OnReceive := nil;
  fHTTPClient.OnError := Error;
  fMasterServerAddress := aMasterServerAddress;
end;


destructor TKMMasterServer.Destroy;
begin
  fHTTPClient.Free;
  fHTTPAnnouncementsClient.Free;
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


procedure TKMMasterServer.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then fOnAnnouncements(S);
end;


procedure TKMMasterServer.AnnounceServer(aName, aPort:string; aTTL:integer);
begin
  fHTTPClient.GetURL(fMasterServerAddress+'serveradd.php?name='+UrlEncode(aName)+'&port='+UrlEncode(aPort)+'&ttl='+UrlEncode(IntToStr(aTTL))+'&rev='+UrlEncode(GAME_REVISION));
  fHTTPClient.OnReceive := nil; //We don't care about the response
end;


procedure TKMMasterServer.QueryServer;
begin
  fHTTPClient.GetURL(fMasterServerAddress+'serverquery.php?rev='+UrlEncode(GAME_REVISION));
  fHTTPClient.OnReceive := Receive;
end;


procedure TKMMasterServer.FetchAnnouncements(const aLang: string);
begin
  fHTTPAnnouncementsClient.GetURL(fMasterServerAddress+'announcements.php?lang='+UrlEncode(aLang)+'&rev='+UrlEncode(GAME_REVISION));
  fHTTPAnnouncementsClient.OnReceive := ReceiveAnnouncements;
end;


procedure TKMMasterServer.UpdateStateIdle;
begin
  fHTTPClient.UpdateStateIdle;
  fHTTPAnnouncementsClient.UpdateStateIdle;
end;


end.

