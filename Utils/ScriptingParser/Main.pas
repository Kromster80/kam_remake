unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtDlgs,
  SysUtils, Variants, Classes,StdCtrls, StrUtils,
  FormHelp;


type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    btnMenuFile: TMenuItem;
    menuSep1: TMenuItem;
    btnMenuExit: TMenuItem;
    btnMenuSave: TMenuItem;
    btnMenuOpen: TMenuItem;
    OpenTxtDlg: TOpenTextFileDialog;
    SaveTxtDlg: TSaveTextFileDialog;
    txtParserOutput: TMemo;
    btnMenuHelp: TMenuItem;
    procedure btnMenuExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMenuOpenClick(Sender: TObject);
    procedure btnMenuSaveClick(Sender: TObject);
    procedure btnMenuHelpClick(Sender: TObject);
    procedure txtParserOutputKeyPress(Sender: TObject; var Key: Char);
  private
    frmHelpForm: ThelpForm;
    procedure txtParser(aFile: String; aList: TStringList);
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  OpenTxtDlg := TOpenTextFileDialog.Create(Self);
  SaveTxtDlg := TSaveTextFileDialog.Create(Self);
  OpenTxtDlg.Filter := 'Pascal files (*.pas)|*.PAS|Any file (*.*)|*.*';
  SaveTxtDlg.Filter := 'Text files (*.txt)|*.TXT|Any file (*.*)|*.*';
end;


// Scans file's contents and puts it all in proper formatting for most wikis.
procedure TForm1.txtParser(aFile: String; aList: TStringList);
var
  i, iPlus: Integer;
  versionStr, descStr, restStr, finalStr: String;
  SourceTxt: TStringList;
begin
  SourceTxt := TStringList.Create;
  try
    aList.Add('| Version | Name | Description | Parameters and types | Returns |');
    aList.Add('| ------- | ---- | ----------- | -------------------- | ------- |');
    SourceTxt.LoadFromFile(aFile);

    for i := 0 to SourceTxt.Count-1 do
    begin
      versionStr := '';
      descStr := ' |';
      restStr := '';
      finalStr := '';
      iPlus := 0;

      // Before anything it should start with "//* Version:"
      if SourceTxt[i].StartsWith('//* Version:') then
      begin
        versionStr := '| ' + SourceTxt[i].Substring(SourceTxt[i].IndexOf(':') + 2);
        Inc(iPlus);
      end;

      // Descriptions are only added by lines starting with "//* "
      if SourceTxt[i+iPlus].StartsWith('//* ') and not (versionStr = '') then
      begin
        // Repeat until no description tags are found
        while SourceTxt[i+iPlus].StartsWith('//* ') do
        begin
          descStr := descStr + ' ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('*') + 2);
          Inc(iPlus);
        end;
      end;

      // Skip empty or "faulty" lines
      while not ((SourceTxt[i+iPlus].StartsWith('procedure')) or
                 (SourceTxt[i+iPlus].StartsWith('function'))) and not (versionStr = '') do
        Inc(iPlus);

      // Format procedures
      if SourceTxt[i+iPlus].StartsWith('procedure') and not (versionStr = '') then
      begin
        restStr := ' | ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('.') + 1);
        restStr := ReplaceStr(restStr, 'Proc', 'On');

        if restStr.Contains('(') then
        begin
          restStr := ReplaceStr(restStr, '(', descStr + ' | `');
          restStr := restStr.TrimRight([')', ';']) + '` | |';
        end else
          restStr := restStr.TrimRight([';']) + descStr + ' | | |';

        finalStr := versionStr + restStr;
      end;

      // Format functions
      if SourceTxt[i+iPlus].StartsWith('function') and not (versionStr = '') then
      begin
        restStr := ' | ' + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].IndexOf('.') + 1);
        Delete(restStr, restStr.LastIndexOf(':')+1, restStr.Length);
        restStr := ReplaceStr(restStr, 'Proc', 'On');

        if restStr.Contains('(') then
        begin
          restStr := ReplaceStr(restStr, '(', descStr + ' | `');
          restStr := restStr.TrimRight([')', ';']) + '` | `';
        end else
          restStr := restStr + descStr + ' | | `';

        restStr := restStr + SourceTxt[i+iPlus].Substring(SourceTxt[i+iPlus].LastIndexOf(':') + 2);
        restStr := restStr.TrimRight([';']) + '` |';
        finalStr := versionStr + restStr;
      end;

      if not (finalStr = '') then
        aList.Add(finalStr);
    end;
  finally
    FreeAndNil(SourceTxt);
  end;
end;


procedure TForm1.txtParserOutputKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;


procedure TForm1.btnMenuHelpClick(Sender: TObject);
begin
  // Open form
  frmHelpForm:=ThelpForm.Create(Nil);
  frmHelpForm.ShowModal;
  FreeAndNil(frmHelpForm);
end;


procedure TForm1.btnMenuOpenClick(Sender: TObject);
var
  Filename: String;
  ParsedStringList: TStringList;
begin
  ParsedStringList := TStringList.Create;

  try
    if OpenTxtDlg.Execute(Self.Handle) then
    begin
      Filename := OpenTxtDlg.FileName;

      if FileExists(Filename) then
      begin
        txtParser(Filename, ParsedStringList);
        txtParserOutput.Lines.Assign(ParsedStringList);
      end else
        raise Exception.Create('File does not exist.');
    end;
  finally
    FreeAndNil(ParsedStringList);
  end;
end;


procedure TForm1.btnMenuSaveClick(Sender: TObject);
var
  Filename : String;
begin
  if txtParserOutput.Lines.Count > 0 then
    if SaveTxtDlg.Execute(Self.Handle) then
    begin
      Filename := SaveTxtDlg.FileName;

      if (SaveTxtDlg.FilterIndex = 0) and not (Filename.EndsWith('.txt')) then
        Filename := Filename + '.txt';

      txtParserOutput.Lines.SaveToFile(FileName);
    end;
end;


procedure TForm1.btnMenuExitClick(Sender: TObject);
begin
  Form1.Close;
end;

end.
