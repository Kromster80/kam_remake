unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  KM_Defaults, KM_Scripting, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    btnBrowse: TButton;
    Label1: TLabel;
    btnValidate: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Remake Script Validator (' + GAME_REVISION + ')';
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  fScripting := TKMScripting.Create;
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit1.Text := OpenDialog1.FileName;
end;

procedure TForm1.btnValidateClick(Sender: TObject);
begin
  if not FileExists(Edit1.Text) then
  begin
    Memo1.Text := 'File not found';
    Exit;
  end;
  fScripting.LoadFromFile(Edit1.Text);
  Memo1.Text := StringReplace(fScripting.ErrorString, '|', #13#10, [rfReplaceAll]);
  if Memo1.Text = '' then
    Memo1.Text := 'No errors :)';
end;

end.
