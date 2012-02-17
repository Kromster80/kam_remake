unit RXXPackerForm;
{$I RXXPacker.inc}
interface
uses
  Classes, Controls, Forms, StdCtrls, SysUtils, TypInfo,
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
  RXXForm1: TRXXForm1;
  fPalettes: TKMPalettes;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}


procedure TRXXForm1.FormCreate(Sender: TObject);
var
  RT: TRXType;
begin
  ExeDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + '..\..\';

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

    fLog.AddToLog('Trimmed ' + IntToStr(fSprites.TrimSprites));

    //fSprites.ExportToBMP(ExeDir + 'Export\'+RXInfo[RT].FileName);

    //Save
    ForceDirectories(ExeDir + 'Data\Sprites\');
    fSprites.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '.rxx');

    fSprites.Free;

    ListBox1.Selected[I] := False;
    ListBox1.Refresh;
  end;

  btnPackRXX.Enabled := True;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.
