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
    btnValidateAll: TButton;
    btnBrowsePath: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseFileClick(Sender: TObject);
    procedure btnValidateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnValidateAllClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure btnBrowsePathClick(Sender: TObject);
  private
    fScripting: TKMScripting;
    IsValidatePath : Boolean;
    sListFileInFolder : TStringList;

    procedure FindFiles(aPath: String; out aList: TStringList);
    procedure Validate(aPath: string; aReportGood: Boolean);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure ComponentReEnabled(aEnabled : Boolean);
  end;

var
  Form1: TForm1;
implementation
uses
  KM_Maps;
const
  EXT_FILE_SCRIPT = '.script';

{$R *.dfm}

procedure TForm1.FindFiles(aPath: String; out aList: TStringList);
var
  SearchRec:TSearchRec;
begin
  FindFirst(aPath+PathDelim+'*', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      if (SearchRec.Attr and faDirectory = faDirectory) then
        FindFiles(aPath + PathDelim + SearchRec.Name, aList)
      else
        if SameText(ExtractFileExt(SearchRec.Name), EXT_FILE_SCRIPT) then
          aList.Add(aPath + PathDelim + SearchRec.Name);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption                := 'KaM Remake Script Validator (' + GAME_REVISION + ')';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  fScripting             := TKMScripting.Create(nil);

  sListFileInFolder := TStringList.Create;

  DragAcceptFiles(Handle, True);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fScripting.Free;
  sListFileInFolder.Free;
  DragAcceptFiles(Handle, False);
end;

procedure TForm1.btnBrowseFileClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  Edit1.Text := OpenDialog1.FileName;
end;


procedure TForm1.btnBrowsePathClick(Sender: TObject);
var
  DirValidate : String;
begin
  if not SelectDirectory('Select the folder to Validate scripts', '', DirValidate) then Exit;
  Edit1.Text := DirValidate;
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
    Memo1.Text := 'Selected Files!';
  end;
end;

procedure TForm1.ComponentReEnabled(aEnabled: Boolean);
begin
  btnBrowsePath.Enabled := aEnabled;
  btnBrowseFile.Enabled := aEnabled;
  btnValidate.Enabled := aEnabled;
  btnValidateAll.Enabled := aEnabled;
  Edit1.Enabled := aEnabled;
end;

procedure TForm1.btnValidateClick(Sender: TObject);
var
  I : Integer;
begin
  ComponentReEnabled(false);

  Memo1.Lines.Clear;
  if IsValidatePath then
  begin
    ExcludeTrailingPathDelimiter(Edit1.Text);
    if not DirectoryExists(Edit1.Text) then
      Memo1.Lines.Append('Directory not found ' + Edit1.Text)
    else
    begin
      Memo1.Lines.Append('Search for files in a folder ...');
      sListFileInFolder.Clear;
      FindFiles(Edit1.Text, sListFileInFolder);
      Memo1.Lines.Append('Check '+Edit1.Text);
      if sListFileInFolder.Count = 0 then
        Memo1.Lines.Append('No files in a directory ' + Edit1.Text)
      else
      begin
        Memo1.Lines.Append('Files in the folder: '+IntToStr(sListFileInFolder.Count));
        for I := 0 to sListFileInFolder.Count-1 do
          Validate(sListFileInFolder.Strings[I], True);
        Memo1.Lines.Append('Checked ' + IntToStr(sListFileInFolder.Count));
      end;
    end;
  end else
    Validate(Edit1.Text, True);

  ComponentReEnabled(true);
end;

procedure TForm1.btnValidateAllClick(Sender: TObject);
var
  I: Integer;
begin
  ComponentReEnabled(false);

  Memo1.Lines.Clear;

  Memo1.Lines.Append('Check ' + ExtractFilePath(ParamStr(0)));
  // Exe path
  TKMapsCollection.GetAllMapPaths(ExtractFilePath(ParamStr(0)), sListFileInFolder);
  if sListFileInFolder.Count = 0 then
    Memo1.Lines.Append('No files in a directory :(')
  else
  begin
    Memo1.Lines.Append('Files in the folder: '+IntToStr(sListFileInFolder.Count));
    for I := 0 to sListFileInFolder.Count - 1 do
      Validate(ChangeFileExt(sListFileInFolder[I], EXT_FILE_SCRIPT), False);

    Memo1.Lines.Append('Checked ' + IntToStr(sListFileInFolder.Count) + ' in .\');
  end;
  // Utils path
  Memo1.Lines.Append('Check ' + ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\'));
  TKMapsCollection.GetAllMapPaths(ExpandFileName(ExtractFilePath(ParamStr(0)) + '..\..\'), sListFileInFolder);
  if sListFileInFolder.Count = 0 then
    Memo1.Lines.Append('No files in a directory :(')
  else
  begin
    Memo1.Lines.Append('Files in the folder: '+IntToStr(sListFileInFolder.Count));
    for I := 0 to sListFileInFolder.Count - 1 do
      Validate(ChangeFileExt(sListFileInFolder[I], EXT_FILE_SCRIPT), False);

    Memo1.Lines.Append('Checked ' + IntToStr(sListFileInFolder.Count));
  end;
  ComponentReEnabled(true);
end;

procedure TForm1.Validate(aPath: string; aReportGood: Boolean);
var
  CampaignFile: UnicodeString;
  txt:          string;
begin
  if not FileExists(aPath) and aReportGood then
  begin
    Memo1.Lines.Append('File not found ' + aPath);
    Exit;
  end;

  CampaignFile := ExtractFilePath(aPath) + '..\campaigndata' + EXT_FILE_SCRIPT;
  fScripting.LoadFromFile(aPath, CampaignFile, nil);

  txt := StringReplace(fScripting.ErrorString, '|', sLineBreak, [rfReplaceAll]);

  if fScripting.WarningsString <> '' then
  begin
    if txt <> '' then
      txt := txt + sLineBreak;

    txt := txt + 'Warnings:' + sLineBreak +
           StringReplace(fScripting.WarningsString, '|', sLineBreak, [rfReplaceAll]);
  end;

  if txt <> '' then
    Memo1.Lines.Append(aPath + sLineBreak + txt)
  else
    if aReportGood then
      Memo1.Lines.Append(aPath + ' - No errors :)');
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
