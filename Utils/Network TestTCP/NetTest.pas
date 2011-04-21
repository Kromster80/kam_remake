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
    procedure FormDestroy(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure btnHostClick(Sender: TObject);
    procedure btnJoinClick(Sender: TObject);
  private
    fServer:TKMServerControl;
    fClient: TKMClientControl;
  public
    procedure GetData(const S:string);
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
end;


procedure TfrmNetTest.btnJoinClick(Sender: TObject);
begin
  fClient := TKMClientControl.Create;
  fClient.OnStatusMessage := GetData;
  fClient.OnRecieveStr := GetData;
  fClient.ConnectTo(edtServer.Text, KAM_PORT);
  btnJoin.Enabled := false;
  btnSend.Enabled := true;
end;


{$IFDEF FPC}
initialization
{$I NetTest.lrs}
{$ENDIF}

end.
