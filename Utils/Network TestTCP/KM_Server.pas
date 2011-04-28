unit KM_Server;
{$I KaM_Remake.inc}
interface
uses Classes, SysUtils, KM_ServerOverbyte;


{ Contains basic items we need for smooth Net experience:

    - start the server
    - stop the server

    - optionaly report non-important status messages

  Everything except that whould be handled by Host (kick players, etc..)
}
type
  TKMServerControl = class
  private
    fClientList:TList; //Remember our clients in list
    fServer:TKMServer;
    fOnStatusMessage:TGetStrProc;
    procedure Error(const S: string);
    procedure ClientConnect(aHandle:integer);
    procedure ClientDisconnect(aHandle:integer);
    procedure DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartListening(aPort:string);
    procedure StopListening;
    property OnStatusMessage:TGetStrProc write fOnStatusMessage;
  end;


implementation


constructor TKMServerControl.Create;
begin
  Inherited;
  fClientList := TList.Create;
  fServer := TKMServer.Create;
end;


destructor TKMServerControl.Destroy;
begin
  fServer.Free;
  fClientList.Free;
  Inherited;
end;


//There's an error in fServer, perhaps fatal for multiplayer.
procedure TKMServerControl.Error(const S: string);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Error '+S);
end;


procedure TKMServerControl.StartListening(aPort:string);
begin
  fServer.OnError := Error;
  fServer.OnClientConnect := ClientConnect;
  fServer.OnClientDisconnect := ClientDisconnect;
  fServer.OnDataAvailable := DataAvailable;
  fServer.StartListening(aPort);
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Listening..');
end;


procedure TKMServerControl.StopListening;
begin
  fServer.StopListening;
end;


//Someone has connected to us. We can use supplied Handle to negotiate
procedure TKMServerControl.ClientConnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Got connection '+inttostr(aHandle));
  fClientList.Add(pointer(aHandle));
end;


//Someone has disconnected from us. We can use supplied Handle to negotiate
procedure TKMServerControl.ClientDisconnect(aHandle:integer);
begin
  if Assigned(fOnStatusMessage) then fOnStatusMessage('Server: Client has disconnected '+inttostr(aHandle));
  fClientList.Remove(pointer(aHandle));
end;


//Someone has send us something
//For now just repeat the message to everyone including Sender (to calculate ping)
procedure TKMServerControl.DataAvailable(aHandle:integer; aData:pointer; aLength:cardinal);
var i:integer;
begin
  for i:=0 to fClientList.Count-1 do
    //if aHandle<>cardinal(fClientList.Items[i]) then
      fServer.SendData(cardinal(fClientList.Items[i]), aData, aLength);
end;


end.
