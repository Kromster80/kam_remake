unit NetTest;
{$I KaM_Remake.inc}
interface

uses
  {$IFDEF FPC} LResources, {$ENDIF}
  Classes, Controls, Forms, StdCtrls,
  KM_Network;

const
  MULTIPLE_COPIES: boolean = true; //Are we running mutliple copies on the one PC to test?

type
  TfrmNetTest = class(TForm)
    lblLastPacket: TLabel;
    edtSend: TEdit;
    Button1: TButton;
    edtServer: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    fKMNetwork: TKMNetwork;
  public
    { Public declarations }
    procedure GetData(const aData: string);
  end;


var
  frmNetTest: TfrmNetTest;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TfrmNetTest.FormCreate(Sender: TObject);
begin
  fKMNetwork := TKMNetwork.Create(MULTIPLE_COPIES);
  fKMNetwork.OnRecieveKMPacket := GetData;
end;


procedure TfrmNetTest.FormDestroy(Sender: TObject);
begin
  fKMNetwork.Free;
end;


procedure TfrmNetTest.GetData(const aData: String);
begin
  lblLastPacket.Caption := aData;
end;


procedure TfrmNetTest.Button1Click(Sender: TObject);
begin
  fKMNetwork.SendTo(edtServer.Text,edtSend.Text);
end;


{$IFDEF FPC}
initialization
{$I NetTest.lrs}
{$ENDIF}

end.
