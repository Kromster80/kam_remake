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
    procedure GetData(const S:string);
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


procedure TfrmNetTest.GetData(const S:string);
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
begin
  fClient.SendText(edtSend.Text);
end;


procedure TfrmNetTest.btnHostClick(Sender: TObject);
begin
  fServer := TKMServerControl.Create;
  fServer.OnStatusMessage := GetData;
  fServer.StartListening(KAM_PORT);
  btnHost.Enabled := false;
  btnStop.Enabled := true;
end;


procedure TfrmNetTest.btnJoinClick(Sender: TObject);
begin
  fClient := TKMClientControl.Create;
  fClient.OnConnectSucceed := Foo;
  fClient.OnConnectFailed := Bar;
  fClient.OnForcedDisconnect := Bar;
  fClient.OnRecieveText := GetData;
  fClient.OnStatusMessage := GetData;
  fClient.ConnectTo(edtServer.Text, KAM_PORT);
  btnJoin.Enabled := false;
  btnSend.Enabled := true;
  btnQuit.Enabled := true;
end;


procedure TfrmNetTest.btnStopClick(Sender: TObject);
begin
  fServer.StopListening;
  btnHost.Enabled := true;
  btnStop.Enabled := false;
end;


procedure TfrmNetTest.btnQuitClick(Sender: TObject);
begin
  fClient.Disconnect;
  btnJoin.Enabled := true;
  btnSend.Enabled := false;
  btnQuit.Enabled := false;
end;

end.
