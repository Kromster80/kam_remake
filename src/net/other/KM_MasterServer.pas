unit KM_MasterServer;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  URLUtils, //This is a common URLUtils file used by both Delphi and Lazarus for two reasons:
            //1. Library specific stuff should all be done in wrappers (e.g. KM_NetServer_Overbyte) so we can easily switch systems.
            //2. Lazarus' LNet library has broken/incorrectly implemented URLUtils at the moment, so we can't rely on them.
  KM_Defaults, KM_HTTPClient;


type
  //Interaction with MasterServer
  TKMMasterServer = class
  private
    fIsDedicated: Boolean;

    fHTTPClient: TKMHTTPClient; //To update server status and fetch server list
    fHTTPAnnouncementsClient: TKMHTTPClient; //To fetch the annoucenemnts at the same time as the server list
    fHTTPMapsClient: TKMHTTPClient; //To tell master server about the map we played
    fMasterServerAddress: string;
    fOnError: TGetStrProc;
    fOnServerList: TGetStrProc;
    fOnAnnouncements: TGetStrProc;

    procedure ReceiveServerList(const S: string);
    procedure ReceiveAnnouncements(const S: string);
    procedure Error(const S: string);
  public
    constructor Create(const aMasterServerAddress: string; aDedicated:Boolean);
    destructor Destroy; override;

    property OnError: TGetStrProc write fOnError;
    property OnServerList: TGetStrProc write fOnServerList;
    property OnAnnouncements: TGetStrProc write fOnAnnouncements;
    procedure AnnounceServer(const aName: string; aPort: Word; aPlayerCount, aTTL: Integer);
    procedure QueryServerList;
    procedure FetchAnnouncements(const aLang: AnsiString);
    procedure SendMapInfo(const aMapName: string; aCRC: Cardinal; aPlayerCount: Integer);
    procedure UpdateStateIdle;

    property MasterServerAddress: string write fMasterServerAddress;
  end;


implementation

const
  {$IFDEF MSWindows} OS = 'Windows'; {$ENDIF}
  {$IFDEF UNIX}      OS = 'Unix'; {$ENDIF}
  {$IFDEF WDC} COMPILER = 'WDC'; {$ENDIF}
  {$IFDEF FPC} COMPILER = 'FPC'; {$ENDIF}


constructor TKMMasterServer.Create(const aMasterServerAddress: string; aDedicated:Boolean);
begin
  inherited Create;
  fHTTPClient := TKMHTTPClient.Create;
  fHTTPAnnouncementsClient := TKMHTTPClient.Create;
  fHTTPMapsClient := TKMHTTPClient.Create;
  fHTTPClient.OnReceive := nil;
  fHTTPClient.OnError := Error;
  fMasterServerAddress := aMasterServerAddress;
  fIsDedicated := aDedicated;
end;


destructor TKMMasterServer.Destroy;
begin
  fHTTPClient.Free;
  fHTTPAnnouncementsClient.Free;
  fHTTPMapsClient.Free;
  inherited;
end;


procedure TKMMasterServer.Error(const S: string);
begin
  if Assigned(fOnError) then fOnError(S);
end;


procedure TKMMasterServer.ReceiveServerList(const S: string);
begin
  if Assigned(fOnServerList) then fOnServerList(S);
end;


procedure TKMMasterServer.ReceiveAnnouncements(const S: string);
begin
  if Assigned(fOnAnnouncements) then fOnAnnouncements(S);
end;


procedure TKMMasterServer.AnnounceServer(const aName: string; aPort: Word; aPlayerCount, aTTL: Integer);
begin
  fHTTPClient.OnReceive := nil; //We don't care about the response
  fHTTPClient.GetURL(fMasterServerAddress+'serveradd.php?name='+UrlEncode(aName)+'&port='+UrlEncode(IntToStr(aPort))
                     +'&playercount='+UrlEncode(IntToStr(aPlayerCount))+'&ttl='+UrlEncode(IntToStr(aTTL))
                     +'&rev='+UrlEncode(NET_PROTOCOL_REVISON)+'&coderev='+UrlEncode(GAME_REVISION)
                     +'&os='+UrlEncode(OS)+'&compiler='+UrlEncode(COMPILER)+'&dedicated='+UrlEncode(IntToStr(byte(fIsDedicated)))
                     , False); //Result doesn't matter so ANSI is fine
end;


procedure TKMMasterServer.QueryServerList;
begin
  fHTTPClient.OnReceive := ReceiveServerList;
  fHTTPClient.GetURL(fMasterServerAddress+'serverquery.php?rev='+UrlEncode(NET_PROTOCOL_REVISON)+'&coderev='+UrlEncode(GAME_REVISION)
                     , False); //For now server list is ANSI only
end;


procedure TKMMasterServer.FetchAnnouncements(const aLang: AnsiString);
begin
  fHTTPAnnouncementsClient.OnReceive := ReceiveAnnouncements;
  fHTTPAnnouncementsClient.GetURL(fMasterServerAddress+'announcements.php?lang='
       +UrlEncode(UnicodeString(aLang))+'&rev='+UrlEncode(NET_PROTOCOL_REVISON)
       +'&coderev='+UrlEncode(GAME_REVISION)
       , True); //Announcements are in UTF8
end;


procedure TKMMasterServer.SendMapInfo(const aMapName: string; aCRC: Cardinal; aPlayerCount: Integer);
begin
  fHTTPMapsClient.OnReceive := nil; //We don't care about the response
  fHTTPMapsClient.GetURL(fMasterServerAddress+'maps.php?map='+UrlEncode(aMapName)
                         +'&mapcrc='+IntToHex(aCRC, 8)
                         +'&playercount='+UrlEncode(IntToStr(aPlayerCount))
                         +'&rev='+UrlEncode(NET_PROTOCOL_REVISON)
                         +'&coderev='+UrlEncode(GAME_REVISION)
                         , False); //Result doesn't matter so ANSI is fine
end;


procedure TKMMasterServer.UpdateStateIdle;
begin
  fHTTPClient.UpdateStateIdle;
  fHTTPAnnouncementsClient.UpdateStateIdle;
  fHTTPMapsClient.UpdateStateIdle;
end;


end.

