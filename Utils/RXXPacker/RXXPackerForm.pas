unit RXXPackerForm;
{$I RXXPacker.inc}
interface
uses
  SysUtils, Classes, Controls, Forms, StdCtrls, TypInfo,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_ResourcePalettes, KM_ResourceSprites;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;


  var
    fPalettes: TKMPalettes;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TRXXForm1.FormCreate(Sender: TObject);
var RT: TRXType;
begin
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '..\..\';

  USE_RXX_FILES := false; //We are packing the RXX files, so always load the data from the other sources
  //Although we don't need them in this tool, these are required to load sprites
  fLog := TKMLog.Create(ExeDir + 'log.txt');

  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;

  for RT := Low(TRXType) to High(TRXType) do
    ListBox1.Items.Add(GetEnumName(TypeInfo(TRXType), Integer(RT)));

  ListBox1.ItemIndex := 0;
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var fSprites: TKMSpritePack; RT: TRXType; I: Integer;
begin
  btnPackRXX.Enabled := False;

  for I := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[I] then
  begin
    RT := TRXType(I);

    fSprites := TKMSpritePack.Create(fPalettes, Rt);

    //Load
    {if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rxx') then
    begin
      fSprites.LoadFromRXXFile(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rxx');
      fSprites.OverloadFromFolder(ExeDir + 'Sprites\');
    end
    else}
    if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rx') then
    begin
      fSprites.LoadFromRXFile(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rx');
      fSprites.OverloadFromFolder(ExeDir + 'Sprites\');
    end
    else
    if DirectoryExists(ExeDir + 'Sprites\') then
      fSprites.LoadFromFolder(ExeDir + 'Sprites\');

    //Save
    ForceDirectories(ExeDir + 'Data\Sprites\');
    fSprites.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '.rxx');

    fSprites.Free;

    ListBox1.Selected[I] := False;
    ListBox1.Refresh;
  end;

  btnPackRXX.Enabled := True;
end;


initialization
{$IFDEF FPC}
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.
