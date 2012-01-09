unit RXXPackerForm;
{$I RXXPacker.inc}

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}
interface
uses
  SysUtils, Classes, Controls, Forms, StdCtrls, TypInfo,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_TextLibrary, KM_ResourceGFX, KM_ResourceSprites, KM_ResourcePalettes, KM_Log, KM_Render;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ComboBox1: TComboBox;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;


var
  RXXForm1: TRXXForm1;


implementation
{$IFDEF WDC}
  {$R *.dfm}
{$ENDIF}

procedure TRXXForm1.FormCreate(Sender: TObject);
var RT: TRXType;
begin
  ExeDir := '..\..\';
  //Although we don't need them in this tool, these are required to load sprites
  fLog := TKMLog.Create(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'log.txt');
  fTextLibrary := TTextLibrary.Create(ExeDir+'data\misc\', 'eng');
  fRender := TRender.Create(Handle,1024,768,false);
  fResource := TResource.Create(nil,nil);
  fResource.LoadMenuResources('eng');
  for RT := Low(TRXType) to High(TRXType) do
    ComboBox1.Items.Add( GetEnumName(TypeInfo(TRXType), Integer(RT)) );
  ComboBox1.ItemIndex := 0;
  USE_RXX_FILES := false; //We are packing the RXX files, so always load the data from the other sources
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
begin
  btnPackRXX.Enabled := false;
  fResource.Sprites.PackRXX(TRXType(ComboBox1.ItemIndex),
                            fResource.Sprites.FileName[TRXType(ComboBox1.ItemIndex)]+'.rxx');
  btnPackRXX.Enabled := true;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}

end.
