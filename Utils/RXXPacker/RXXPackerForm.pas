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
    ComboBox1: TComboBox;
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
  ExeDir := '.\';
  //Although we don't need them in this tool, these are required to load sprites
  fLog := TKMLog.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log.txt');

  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;

  for RT := Low(TRXType) to High(TRXType) do
    ComboBox1.Items.Add(GetEnumName(TypeInfo(TRXType), Integer(RT)));

  ComboBox1.ItemIndex := 0;
  USE_RXX_FILES := False; //We are packing the RXX files, so always load the data from the other sources
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var fSprites: TKMSpritePack; RT: TRXType;
begin
  btnPackRXX.Enabled := False;

  RT := TRXType(ComboBox1.ItemIndex);

  fSprites := TKMSpritePack.Create(fPalettes, Rt);

  fSprites.LoadFromRXFile(ExeDir + '..\..\data\gfx\res\' + RXInfo[RT].FileName + '.rx');
//  fSprites.LoadFromFolder();

  fSprites.SaveToRXXFile(ExeDir + RXInfo[RT].FileName + '.rxx');

  fSprites.Free;

  btnPackRXX.Enabled := True;
end;


initialization
{$IFDEF FPC}
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.
