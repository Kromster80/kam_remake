unit Unit1;
interface
uses
  Windows, Messages, Classes, Controls, Dialogs, Forms, StdCtrls, StrUtils, SysUtils, FileCtrl,
  KM_Defaults, KM_Scripting, shellapi;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    btnBrowseFile: TButton;
    Label1: TLabel;
    btnValidate: TButton;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    Label2: TLabel;
    Button1: TButton;
    btnBrowsePath: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseFileClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure btnBrowsePathClick(Sender: TObject);
  private
    fScripting: TKMScripting;

    procedure FindFiles(Path: String; out List: TStringList);
    procedure Validate(aPath: string; aReportGood: Boolean; aValidatePath: Boolean);

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  end;


var
  Form1: TForm1;
  IsValidatePath : Boolean;

implementation
uses
  KM_Maps;

{$R *.dfm}

procedure TForm1.FindFiles(Path: String; out List: TStringList);
var SearchRec:TSearchRec;
begin
  FindFirst(Path+PathDelim+'*', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      if (SearchRec.Attr and faDirectory = faDirectory) then
        FindFiles(Path + PathDelim + SearchRec.Name, List)
      else
        if SameText(RightStr(SearchRec.Name, Length(EXT_FILE_SCRIPT)+1), ('.'+EXT_FILE_SCRIPT)) then
          List.Add(Path + PathDelim + SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'KaM Remake Script Validator (' + GAME_REVISION + ')';

  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);

  fScripting := TKMScripting.Create(nil);

  DragAcceptFiles(Handle, True);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fScripting.Free;
  DragAcceptFiles(Handle, False);
end;

procedure TForm1.btnBrowseFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  Edit1.Text := OpenDialog1.FileName;
end;


procedure TForm1.btnBrowsePathClick(Sender: TObject);
var DirValidate : String;
begin
  if not SelectDirectory('Select the folder to Validate scripts', '', DirValidate) then Exit;
  Edit1.Text := DirValidate;
end;

procedure TForm1.btnValidateClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  Validate(Edit1.Text, True, IsValidatePath);
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  maps: TStringList;
  I: Integer;
begin
  Memo1.Lines.Clear;
  maps := TStringList.Create;

  // Exe path
  TKMapsCollection.GetAllMapPaths(ExtractFilePath(Application.ExeName), maps);
  for I := 0 to maps.Count - 1 do
    Validate(ChangeFileExt(maps[I], '.' + EXT_FILE_SCRIPT), False, False);

  Memo1.Lines.Append('Checked ' + IntToStr(maps.Count) + ' in .\');

  // Utils path
  TKMapsCollection.GetAllMapPaths(ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\'), maps);
  for I := 0 to maps.Count - 1 do
    Validate(ChangeFileExt(maps[I], '.' + EXT_FILE_SCRIPT), False, False);

  Memo1.Lines.Append('Checked ' + IntToStr(maps.Count) + ' in ..\..\');
end;


procedure TForm1.Edit1Change(Sender: TObject);
begin
  If (GetFileAttributes(PWideCHar(TEdit(Sender).Text)) and FILE_ATTRIBUTE_DIRECTORY) = FILE_ATTRIBUTE_DIRECTORY then
  begin
    IsValidatePath := true;
    Memo1.Text := 'Selected folders!';
  end
  else
  begin
    IsValidatePath := false;
    Memo1.Text := 'Select Files!';
  end;
end;

procedure TForm1.Validate(aPath: string; aReportGood: Boolean; aValidatePath: Boolean);
var
  CampaignFile: UnicodeString;
  txt: string;
  SL : TStringList;
  I : Integer;
begin

  if aValidatePath then
  begin
    if SameText(RightStr(aPath, 1), PathDelim) then aPath := LeftStr(aPath, (Length(aPath) -1));

    if not DirectoryExists(aPath) and aReportGood then
    begin
      Memo1.Lines.Append('Directory not found ' + aPath);
      Exit;
    end;

    SL := TStringList.Create;
    FindFiles(aPath, SL);
    if SL.Count = 0 then
    begin
      Memo1.Lines.Append('No files in a directory ' + aPath);
      Exit;
    end;
    Memo1.Lines.Append('Files in the folder: '+IntToStr(SL.Count));
    for I := 0 to SL.Count-1 do
    begin
      CampaignFile := ExtractFilePath(SL.Strings[I]) + '..\campaigndata.' + EXT_FILE_SCRIPT;
      fScripting.LoadFromFile(SL.Strings[I], CampaignFile, nil);

      txt := StringReplace(fScripting.ErrorString, '|', sLineBreak, [rfReplaceAll]);

      if fScripting.WarningsString <> '' then
      begin
        if txt <> '' then
        txt := txt + sLineBreak;
        txt := txt + 'Warnings:' + sLineBreak;
        txt := txt + StringReplace(fScripting.WarningsString, '|', sLineBreak, [rfReplaceAll]);
      end;

      if txt <> '' then
        Memo1.Lines.Append(SL.Strings[I] + sLineBreak + txt)
      else
      if aReportGood then
        Memo1.Lines.Append(SL.Strings[I] + ' - No errors :)');
    end; 
  end else
  begin

    if not FileExists(aPath) and aReportGood then
    begin
      Memo1.Lines.Append('File not found ' + aPath);
      Exit;
    end;

    CampaignFile := ExtractFilePath(aPath) + '..\campaigndata.' + EXT_FILE_SCRIPT;
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
end;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  Filename: array[0 .. MAX_PATH] of Char;
begin
  DragQueryFile(Msg.Drop, 0, Filename, MAX_PATH);
  Edit1.Text := Filename;
  DragFinish(Msg.Drop);
end;

end.
