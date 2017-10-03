unit KM_HTTPClient;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils
  {$IFDEF WDC} ,KM_HTTPClientOverbyte {$ENDIF}
  {$IFDEF FPC} ,KM_HTTPClientLNet {$ENDIF};

type
  //General wrapper for Delphi/Lazarus implementations
  TKMHTTPClient = class
  private
    {$IFDEF WDC} fClient: TKMHTTPClientOverbyte; {$ENDIF}
    {$IFDEF FPC} fClient: TKMHTTPClientLNet;     {$ENDIF}
    fOnError: TGetStrProc;
    fOnReceive: TGetStrProc;
    procedure Error(const S: string);
    procedure GetCompleted(const S: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetURL(const aURL: string; aIsUTF8: Boolean);
    procedure UpdateStateIdle;

    property OnError: TGetStrProc write fOnError;
    property OnReceive: TGetStrProc write fOnReceive;
  end;


implementation


constructor TKMHTTPClient.Create;
begin
  inherited;
  {$IFDEF WDC} fClient := TKMHTTPClientOverbyte.Create; {$ENDIF}
  {$IFDEF FPC} fClient := TKMHTTPClientLNet.Create;     {$ENDIF}
  fClient.OnGetCompleted := GetCompleted;
  fClient.OnError := Error;
end;


destructor TKMHTTPClient.Destroy;
begin
  fClient.Free;
  inherited;
end;


procedure TKMHTTPClient.GetURL(const aURL: string; aIsUTF8: Boolean);
begin
  fClient.GetURL(aUrl, aIsUTF8);
end;


procedure TKMHTTPClient.GetCompleted(const S: string);
begin
  if Assigned(fOnReceive) then fOnReceive(S);
end;


procedure TKMHTTPClient.Error(const S: string);
begin
  if Assigned(fOnError) then fOnError(S);
end;


procedure TKMHTTPClient.UpdateStateIdle;
begin
  {$IFDEF FPC} fClient.UpdateStateIdle; {$ENDIF}
end;


end.

