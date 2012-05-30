unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, TypInfo, PNGImage,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_Pics, KM_ResourcePalettes, KM_ResourceSprites,
  ExtCtrls;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    btnAdd: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnSaveRXX: TButton;
    lbSpritesList: TListBox;
    btnLoadRXX: TButton;
    btnDelete: TButton;
    btnImport: TButton;
    btnExport: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure SaveDialog1Show(Sender: TObject);
    procedure btnLoadRXXClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveRXXClick(Sender: TObject);
    procedure lbSpritesListClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fPalettes: TKMPalettes;
    fSprites: TKMSpritePack;
    procedure GetImageToBitmap(aIndex: Integer; aPNG: TPNGObject);
    procedure UpdateList;
  end;


var
  RXXForm1: TRXXForm1;


implementation
{$R *.dfm}


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
  FreeAndNil(fSprites);
  FreeAndNil(fPalettes);
  FreeAndNil(fLog);
end;


procedure TRXXForm1.GetImageToBitmap(aIndex: Integer; aPNG: TPNGObject);
var
  I, K, W, H: Integer;
  T: Cardinal;
begin
  W := fSprites.RXData.Size[aIndex].X;
  H := fSprites.RXData.Size[aIndex].Y;

  aPNG.Resize(W, H);

  for I := 0 to H - 1 do
  for K := 0 to W - 1 do
  begin
    T := fSprites.RXData.RGBA[aIndex, I * W + K];

    //RGB and Alpha components are stored in two separate places
    aPNG.Pixels[K,I] := T and $FFFFFF;
    aPNG.AlphaScanline[I]^[K] := T shr 24;
  end;
end;


procedure TRXXForm1.lbSpritesListClick(Sender: TObject);
var
  ID: Integer;
  PNG: TPNGObject;
begin
  Image1.Picture.Bitmap.Canvas.Brush.Color := 0;
  Image1.Picture.Bitmap.Canvas.FillRect(Image1.Picture.Bitmap.Canvas.ClipRect);

  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;
  if fSprites.RXData.Flag[ID] = 0 then Exit;

  PNG := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, 0, 0);
  try
    GetImageToBitmap(ID, PNG);

    Image1.Picture.Assign(PNG);
  finally
    PNG.Free;
  end;
end;


procedure TRXXForm1.OpenDialog1Show(Sender: TObject);
begin
  //Win7 needs InitialDir to be set OnShow after Execute
  OpenDialog1.InitialDir := ExeDir;
end;


procedure TRXXForm1.SaveDialog1Show(Sender: TObject);
begin
  //Win7 needs InitialDir to be set OnShow after Execute
  SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
end;


procedure TRXXForm1.btnLoadRXXClick(Sender: TObject);
var RT: TRXType;
begin
  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.Filter := 'RX, RXXX packages (*.rx;*.rxx)|*.rxx;*.rx;';
  OpenDialog1.InitialDir := ExeDir + 'data\sprites\';
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  //GuiMain/GuiMainH needs to know it's type to use special palette mappings
  if SameText(ExtractFileName(OpenDialog1.FileName), 'guimain.rx') then
    RT := rxGuiMain
  else
  if SameText(ExtractFileName(OpenDialog1.FileName), 'guimainh.rx') then
    RT := rxGuiMainH
  else
    RT := rxTrees;

  FreeAndNil(fSprites);
  fSprites := TKMSpritePack.Create(RT, fPalettes, nil);

  Label1.Caption := ExtractFileName(OpenDialog1.FileName);

  if SameText(ExtractFileExt(OpenDialog1.FileName), '.rx') then
    fSprites.LoadFromRXFile(OpenDialog1.FileName)
  else
  if SameText(ExtractFileExt(OpenDialog1.FileName), '.rxx') then
    fSprites.LoadFromRXXFile(OpenDialog1.FileName);

  UpdateList;
end;


procedure TRXXForm1.btnAddClick(Sender: TObject);
var I: Integer;
begin
  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.InitialDir := ExeDir;
  OpenDialog1.Filter := 'Supported images (*.bmp;*.png)|*.bmp;*.png';
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  for I := 0 to OpenDialog1.Files.Count - 1 do
  begin
    fSprites.AddImage(ExtractFilePath(OpenDialog1.Files[I]),
                      ExtractFileName(OpenDialog1.Files[I]), fSprites.RXData.Count+1);
  end;

  UpdateList;
end;


procedure TRXXForm1.btnImportClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.InitialDir := ExeDir;
  OpenDialog1.Filter := 'Supported images (*.bmp;*.png)|*.bmp;*.png';
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  fSprites.AddImage(ExtractFilePath(OpenDialog1.FileName),
                    ExtractFileName(OpenDialog1.FileName), ID);

  UpdateList;
end;


procedure TRXXForm1.btnDeleteClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  fSprites.Delete(ID);

  UpdateList;
end;


procedure TRXXForm1.btnExportClick(Sender: TObject);
var
  ID: Integer;
  PNG: TPNGObject;
begin
  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  SaveDialog1.InitialDir := ExeDir;
  SaveDialog1.Filter := 'PNG image (*.png)|*.png';
  SaveDialog1.Options := SaveDialog1.Options - [ofAllowMultiSelect];
  if not SaveDialog1.Execute then Exit;

  PNG := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, 1, 1);
  try
    GetImageToBitmap(ID, PNG);

    PNG.SaveToFile(SaveDialog1.FileName);
  finally
    PNG.Free;
  end;
end;


procedure TRXXForm1.btnSaveRXXClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then Exit;
  fLog.AddToLog('Trimmed ' + IntToStr(fSprites.TrimSprites));
  fSprites.SaveToRXXFile(SaveDialog1.FileName);
end;


procedure TRXXForm1.UpdateList;
var I: Integer;
begin
  lbSpritesList.Items.BeginUpdate;
  lbSpritesList.Items.Clear;

  for I := 1 to fSprites.RXData.Count do
  begin
    if fSprites.RXData.Flag[I] = 0 then
      lbSpritesList.Items.Add(IntToStr(I)+'.')
    else
      lbSpritesList.Items.Add(Format('%d. %dx%d', [I, fSprites.RXData.Size[I].X, fSprites.RXData.Size[I].Y]));
  end;

  lbSpritesList.Items.EndUpdate;
  lbSpritesListClick(Self);
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
  procedure SkipUnusedSprites(aPack: TKMSpritePack; RT: TRXType);
  const
    SkipGui:      array [0.. 9] of Word = (403,405,406,408,410,411,552,553,555,581);
    SkipGuiMain:  array [0..16] of Word = (1,2,7,8,12,17,18,19,20,22,31,32,33,34,35,36,37);
    SkipGuiMainH: array [0..10] of Word = (1,8,10,11,12,13,14,15,16,17,18);
  var I: Integer;
  begin
    case RT of
      rxGui:      for I := 0 to High(SkipGui) do
                    aPack.RXData.Flag[SkipGui[I]] := 0;
      rxGuiMain:  for I := 0 to High(SkipGuiMain) do
                    aPack.RXData.Flag[SkipGuiMain[I]] := 0;
      rxGuiMainH: for I := 1 to aPack.RXData.Count do
                    aPack.RXData.Flag[I] := 0;
    end;
  end;
var
  SpritePack: TKMSpritePack;
  RT: TRXType;
  I: Integer;
begin
  btnPackRXX.Enabled := False;

  for I := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[I] then
  begin
    RT := TRXType(I);

    SpritePack := TKMSpritePack.Create(RT, fPalettes, nil);

    //Load
    if FileExists(ExeDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx') then
    begin
      SpritePack.LoadFromRXFile(ExeDir + 'SpriteResource\' + RXInfo[RT].FileName + '.rx');
      SkipUnusedSprites(SpritePack, RT);
      SpritePack.OverloadFromFolder(ExeDir + 'SpriteResource\');
    end
    else
    if DirectoryExists(ExeDir + 'SpriteResource\') then
      SpritePack.LoadFromFolder(ExeDir + 'SpriteResource\');

    fLog.AddToLog('Trimmed ' + IntToStr(SpritePack.TrimSprites));

    //Save
    ForceDirectories(ExeDir + 'Data\Sprites\');
    SpritePack.SaveToRXXFile(ExeDir + 'Data\Sprites\' + RXInfo[RT].FileName + '.rxx');

    SpritePack.Free;

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
