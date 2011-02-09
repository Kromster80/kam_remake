unit NetTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, KM_Network, StdCtrls;

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
    procedure GetData(const aString: String);
  public
    { Public declarations }
  end;

var
  frmNetTest: TfrmNetTest;

implementation

{$R *.dfm}

procedure TfrmNetTest.FormCreate(Sender: TObject);
begin
  fKMNetwork := TKMNetwork.Create(frmNetTest, MULTIPLE_COPIES);
  fKMNetwork.OnRecieveKMPacket := GetData;
end;

procedure TfrmNetTest.FormDestroy(Sender: TObject);
begin
  fKMNetwork.Free;
end;

procedure TfrmNetTest.GetData(const aString: String);
begin
  lblLastPacket.Caption := aString;
end;

procedure TfrmNetTest.Button1Click(Sender: TObject);
begin
  fKMNetwork.SendTo(edtServer.Text,edtSend.Text);
end;

end.
