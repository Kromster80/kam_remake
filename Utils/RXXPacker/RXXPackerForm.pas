unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils, TypInfo, PNGImage, Windows,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_Pics, KM_ResourcePalettes, KM_ResourceSprites, KM_ResourceSpritesEdit;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fPalettes: TKMPalettes;
  end;


var
  RXXForm1: TRXXForm1;


implementation
{$R *.dfm}
uses KM_ResourceHouse;


procedure TRXXForm1.FormCreate(Sender: TObject);
var
  RT: TRXType;
begin
  ExeDir := ExtractFilePath(Application.ExeName) + '..\..\';

  //Although we don't need them in this tool, these are required to load sprites
  fLog := TKMLog.Create(ExeDir + 'RXXPacker.log');

  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;

  for RT := Low(TRXType) to High(TRXType) do
    ListBox1.Items.Add(GetEnumName(TypeInfo(TRXType), Integer(RT)));

  ListBox1.ItemIndex := 0;
end;


procedure TRXXForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPalettes);
  FreeAndNil(fLog);
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var
  SpritePack: TKMSpritePackEdit;
  RT: TRXType;
  I: Integer;
  Tick: Cardinal;
  RXName: string;
  HouseDat: TKMHouseDatCollection;
begin
  btnPackRXX.Enabled := False;
  Tick := GetTickCount;

  Assert(DirectoryExists(ExeDir + 'SpriteResource\'),
         'Cannot find ' + ExeDir + 'SpriteResource\ folder.'+#10#13+
         'Please make sure this folder exists.');

  for I := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[I] then
  begin
    RT := TRXType(I);
    RXName := ExeDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx';
    Assert((RT = rxTiles) or FileExists(RXName),
           'Cannot find ' + RXName + ' file.'+#10#13+
           'Please copy the file from your KaM\data\gfx\res\ folder.');

    SpritePack := TKMSpritePackEdit.Create(RT, fPalettes);
    try
      //Load
      if RT <> rxTiles then
      begin
        SpritePack.LoadFromRXFile(RXName);
        SpritePack.OverloadFromFolder(ExeDir + 'SpriteResource\');
      end
      else
      if DirectoryExists(ExeDir + 'SpriteResource\') then
        SpritePack.LoadFromFolder(ExeDir + 'SpriteResource\');

      //Tiles must stay the same size as they can't use pivots
      if RT <> rxTiles then
        fLog.AddToLog('Trimmed ' + IntToStr(SpritePack.TrimSprites));

      //Houses need some special treatment to adapt to GL_ALPHA_TEST that we use for construction steps
      if RT = rxHouses then
      begin
        HouseDat := TKMHouseDatCollection.Create;
        SpritePack.AdjoinHouseMasks(HouseDat);
        SpritePack.GrowHouseMasks(HouseDat);
        HouseDat.Free;
      end;

      //The idea was to blur the water and make it semitrasparent, but it did not worked out as expected
      //if RT = rxTiles then
      //  SpritePack.SoftWater(nil);

      //Save
      SpritePack.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '.rxx');

      //Generate alpha shadows for the following sprite packs
      if RT in [rxHouses,rxUnits,rxGui,rxTrees] then
      begin
        if RT = rxGui then
          SpritePack.SoftenShadows(251, 281) //House tablets only (shadow softening messes up other rxGui sprites)
        else
          SpritePack.SoftenShadows;

        SpritePack.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '_a.rxx');
      end;
    finally
      SpritePack.Free;
    end;

    ListBox1.Selected[I] := False;
    ListBox1.Refresh;
  end;

  Label1.Caption := IntToStr(GetTickCount - Tick) + ' ms';
  btnPackRXX.Enabled := True;
end;


{$IFDEF FPC}
initialization
  {$i RXXPackerForm.lrs}
{$ENDIF}


end.
