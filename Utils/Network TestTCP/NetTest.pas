unit NetTest;
{$I KaM_Remake.inc}
interface

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  Classes, Controls, Forms, StdCtrls,
  KM_Server, KM_Client;

type
  TfrmNetTest = class(TForm)
    edtSend: TEdit;
    btnSend: TButton;
    edtServer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    btnHost: TButton;
    btnJoin: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    btnStop: TButton;
    btnQuit: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnHostClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
  private
    fServer:TKMServerControl;
    fClient: TKMClientControl;
  public
    procedure GetData(aData:pointer; aLength:cardinal);
    procedure GetStatusMsg(const S:string);
    procedure Foo(Sender: TObject);
    procedure Bar(const S:string);
  end;


const KAM_PORT = '56789'; //We can decide on something official later

var frmNetTest: TfrmNetTest;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TfrmNetTest.FormDestroy(Sender: TObject);
begin
  if fServer<>nil then fServer.Free;
  if fClient<>nil then fClient.Free;
end;


procedure TfrmNetTest.GetData(aData:pointer; aLength:cardinal);
var S:string;
begin
  SetString(S, PAnsiChar(aData), aLength);
  Memo1.Lines.Add(S);
end;


procedure TfrmNetTest.GetStatusMsg(const S:string);
begin
  Memo1.Lines.Add(S);
end;


procedure TfrmNetTest.Foo(Sender: TObject);
begin
  //
end;


procedure TfrmNetTest.Bar(const S:string);
begin
  //
end;


procedure TfrmNetTest.btnSendClick(Sender: TObject);
var i:integer;
begin
  for i:=1 to 1 do
  fClient.SendText(edtSend.Text);
end;


procedure TfrmNetTest.btnHostClick(Sender: TObject);
begin
  fServer := TKMServerControl.Create;
  fServer.OnStatusMessage := GetStatusMsg;
  fServer.StartListening(KAM_PORT);
  btnHost.Enabled := false;
  btnStop.Enabled := true;
end;


procedure TfrmNetTest.btnStopClick(Sender: TObject);
begin
  fServer.StopListening;
  fServer.Free;
  btnHost.Enabled := true;
  btnStop.Enabled := false;
end;


procedure TfrmNetTest.btnJoinClick(Sender: TObject);
begin
  fClient := TKMClientControl.Create;
  fClient.OnConnectSucceed := Foo;
  fClient.OnConnectFailed := Bar;
  fClient.OnForcedDisconnect := Bar;
  fClient.OnRecieveData := GetData;
  fClient.OnStatusMessage := GetStatusMsg;
  fClient.ConnectTo(edtServer.Text, KAM_PORT);
  btnJoin.Enabled := false;
  btnSend.Enabled := true;
  btnQuit.Enabled := true;
end;


procedure TfrmNetTest.btnQuitClick(Sender: TObject);
begin
  fClient.Disconnect;
  fClient.Free;
  btnJoin.Enabled := true;
  btnSend.Enabled := false;
  btnQuit.Enabled := false;
end;


end.
