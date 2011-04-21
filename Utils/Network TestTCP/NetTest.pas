unit NetTest;
{$I KaM_Remake.inc}
interface

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  Classes, Controls, Forms, StdCtrls,
  KM_Server, KM_Client;

const
  MULTIPLE_COPIES: boolean = true; //Are we running mutliple copies on the one PC to test?

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
    fServer:TKMServer;
    fClient: TKMClient;
  public
    procedure GetData(const aData:string);
  end;


var
  frmNetTest: TfrmNetTest;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TfrmNetTest.FormDestroy(Sender: TObject);
begin
  if fServer<>nil then fServer.Free;
  if fClient<>nil then fClient.Free;
end;


procedure TfrmNetTest.GetData(const aData:string);
begin
  Memo1.Lines.Add(aData);
end;


procedure TfrmNetTest.btnSendClick(Sender: TObject);
begin
  fClient.SendText(edtSend.Text);
end;


procedure TfrmNetTest.btnHostClick(Sender: TObject);
begin
  fServer := TKMServer.Create;
  fServer.OnRecievePacket := GetData;
  fServer.StartListening;
  btnHost.Enabled := false;
end;


procedure TfrmNetTest.btnJoinClick(Sender: TObject);
begin
  fClient := TKMClient.Create;
  fClient.OnRecievePacket := GetData;
  fClient.ConnectTo(edtServer.Text);
  btnJoin.Enabled := false;
  btnSend.Enabled := true;
end;


{$IFDEF FPC}
initialization
{$I NetTest.lrs}
{$ENDIF}

end.
