unit NetTest;
{$I KaM_Remake.inc}
interface

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  Classes, Controls, Forms, StdCtrls,
  KM_NetServer, KM_NetClient;

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
    fNetServer:TKMNetServer;
    fNetClient: TKMNetClient;
  public
    procedure GetData(aData:pointer; aLength:cardinal);
    procedure GetStatusMsg(const S:string);
    procedure Foo(Sender: TObject);
    procedure Bar(const S:string);
  end;


const
    KAM_PORT = '56789'; //We can decide on something official later

var frmNetTest: TfrmNetTest;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TfrmNetTest.FormDestroy(Sender: TObject);
begin
  if fNetClient<>nil then fNetClient.Free;
  if fNetServer<>nil then fNetServer.Free;
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
  fNetClient.SendText(edtSend.Text);
end;


procedure TfrmNetTest.btnHostClick(Sender: TObject);
begin
  fNetServer := TKMNetServer.Create;
  fNetServer.OnStatusMessage := GetStatusMsg;
  fNetServer.StartListening(KAM_PORT);
  btnHost.Enabled := false;
  btnStop.Enabled := true;
end;


procedure TfrmNetTest.btnStopClick(Sender: TObject);
begin
  fNetServer.StopListening;
  fNetServer.Free;
  btnHost.Enabled := true;
  btnStop.Enabled := false;
end;


procedure TfrmNetTest.btnJoinClick(Sender: TObject);
begin
  fNetClient := TKMNetClient.Create;
  fNetClient.OnConnectSucceed := Foo;
  fNetClient.OnConnectFailed := Bar;
  fNetClient.OnForcedDisconnect := Bar;
  fNetClient.OnRecieveData := GetData;
  fNetClient.OnStatusMessage := GetStatusMsg;
  fNetClient.ConnectTo(edtServer.Text, KAM_PORT);
  btnJoin.Enabled := false;
  btnSend.Enabled := true;
  btnQuit.Enabled := true;
end;


procedure TfrmNetTest.btnQuitClick(Sender: TObject);
begin
  fNetClient.Disconnect;
  fNetClient.Free;
  btnJoin.Enabled := true;
  btnSend.Enabled := false;
  btnQuit.Enabled := false;
end;


end.
