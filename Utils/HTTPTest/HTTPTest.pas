unit HTTPTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, KM_HTTPClient;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure Error(const S: string);
    procedure Receive(const S: string);
  public
    HTTPClient: TKMHTTPClient;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  HTTPClient := TKMHTTPClient.Create;
  HTTPClient.OnError := Error;
  HTTPClient.OnReceive := Receive;
  Application.OnIdle := ApplicationIdle;
end;


procedure TForm1.Error(const S: string);
begin
  Memo1.Text := S;
end;


procedure TForm1.Receive(const S: string);
begin
  Memo1.Text := S;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  HTTPClient.GetURL(Edit1.Text);
end;


procedure TForm1.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  HTTPClient.UpdateStateIdle;
  Sleep(1);
  Done := false;
end;

end.
