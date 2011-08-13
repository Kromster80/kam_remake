unit HTTPTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HttpProt, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure RequestDone(Sender  : TObject; RqType  : THttpRequest; ErrCode : Word);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HTTPClient: THttpCli;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  HTTPClient := THttpCli.Create(nil);
end;

procedure TForm1.RequestDone(Sender  : TObject; RqType  : THttpRequest; ErrCode : Word);
var RcdText: string;
begin
  Label1.Caption := 'Size: '+IntToStr(HTTPClient.RcvdStream.Size);
  SetLength(RcdText,HTTPClient.RcvdStream.Size);
  Move(TMemoryStream(HTTPClient.RcvdStream).Memory^,RcdText[1],HTTPClient.RcvdStream.Size);
  Memo1.Text := RcdText;
  HTTPClient.RcvdStream.Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if HTTPClient.State <> httpReady then exit;
  HTTPClient.RcvdStream := TMemoryStream.Create;
  HTTPClient.URL := Edit1.Text;
  HTTPClient.OnRequestDone := RequestDone;
  HTTPClient.GetASync; //Asynchronous means non-blocking
end;

end.
