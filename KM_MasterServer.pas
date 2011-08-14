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

    //procedure Receive(const S: string);
    procedure Error(const S: string);
  public
    constructor Create(aMasterServerAddress:string);
    destructor Destroy; override;

    property OnError:TGetStrProc write fOnError;
    procedure AnnounceServer(aPort:string; aTTL:integer);
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


procedure TKMMasterServer.AnnounceServer(aPort:string; aTTL:integer);
begin
  fHTTPClient.GetURL(fMasterServerAddress+'serveradd.php?port='+aPort+'&ttl='+IntToStr(aTTL));
  fHTTPClient.OnReceive := nil; //We don't care about the response
end;


procedure TKMMasterServer.UpdateStateIdle;
begin
  fHTTPClient.UpdateStateIdle;
end;


end.

