unit Unit1;
interface
uses
  Classes, Controls, Dialogs, Forms, StdCtrls, SysUtils,
  KM_Defaults, KM_Scripting;


type
  TForm1 = class(TForm)
    Edit1: TEdit;
    btnBrowse: TButton;
    Label1: TLabel;
    btnValidate: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fScripting: TKMScripting;

    procedure Validate(aPath: string; aReportGood: Boolean);
  end;


var
  Form1: TForm1;


implementation
uses
  KM_Maps;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Remake Script Validator (' + GAME_REVISION + ')';

  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);

  fScripting := TKMScripting.Create(nil);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fScripting.Free;
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit1.Text := OpenDialog1.FileName;
end;


procedure TForm1.btnValidateClick(Sender: TObject);
begin
  Validate(Edit1.Text, True);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  maps: TStringList;
  I: Integer;
begin
  maps := TStringList.Create;

  // Exe path
  TKMapsCollection.GetAllMapPaths(ExtractFilePath(Application.ExeName), maps);
  for I := 0 to maps.Count - 1 do
    Validate(ChangeFileExt(maps[I], '.script'), False);

  Memo1.Lines.Append('Checked ' + IntToStr(maps.Count) + ' in .\');

  // Utils path
  TKMapsCollection.GetAllMapPaths(ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\'), maps);
  for I := 0 to maps.Count - 1 do
    Validate(ChangeFileExt(maps[I], '.script'), False);

  Memo1.Lines.Append('Checked ' + IntToStr(maps.Count) + ' in ..\..\');
end;


procedure TForm1.Validate(aPath: string; aReportGood: Boolean);
var
  CampaignFile: UnicodeString;
  txt: string;
begin
  if not FileExists(aPath) and aReportGood then
  begin
    Memo1.Lines.Append('File not found ' + aPath);
    Exit;
  end;

  CampaignFile := ExtractFilePath(aPath) + '..\campaigndata.script';
  fScripting.LoadFromFile(aPath, CampaignFile, nil);

  txt := StringReplace(fScripting.ErrorString, '|', sLineBreak, [rfReplaceAll]);

  if fScripting.WarningsString <> '' then
  begin
    if txt <> '' then
      txt := txt + sLineBreak;
    txt := txt + 'Warnings:' + sLineBreak;
    txt := txt + StringReplace(fScripting.WarningsString, '|', sLineBreak, [rfReplaceAll]);
  end;

  if txt <> '' then
    Memo1.Lines.Append(aPath + sLineBreak + txt)
  else
  if aReportGood then
    Memo1.Lines.Append(aPath + ' - No errors :)');
end;


end.
