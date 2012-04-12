unit RXXPackerForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs, Forms, Graphics, StdCtrls, SysUtils, TypInfo,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_Pics, KM_ResourcePalettes, KM_ResourceSprites,
  Vcl.ExtCtrls;


type
  TRXXForm1 = class(TForm)
    btnPackRXX: TButton;
    ListBox1: TListBox;
    btnAdd: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnSaveRXX: TButton;
    ListBox2: TListBox;
    btnLoadRXX: TButton;
    dlgOpenRXX: TOpenDialog;
    Image1: TImage;
    btnDelete: TButton;
    btnReplace: TButton;
    procedure btnPackRXXClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure SaveDialog1Show(Sender: TObject);
    procedure btnLoadRXXClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveRXXClick(Sender: TObject);
    procedure dlgOpenRXXShow(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
  private
    fPalettes: TKMPalettes;
    fSprites: TKMSpritePack;
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
  fSprites := TKMSpritePack.Create(fPalettes, rxGame);

  for RT := Low(TRXType) to High(TRXType) do
    ListBox1.Items.Add(GetEnumName(TypeInfo(TRXType), Integer(RT)));

  ListBox1.ItemIndex := 0;
end;


procedure TRXXForm1.ListBox2Click(Sender: TObject);
var
  ID: Integer;
  I, K: Integer;
  T: Cardinal;
  BM: TBitmap;
begin
  Image1.Picture.Bitmap.Canvas.Brush.Color := 0;
  Image1.Picture.Bitmap.Canvas.FillRect(Image1.Picture.Bitmap.Canvas.ClipRect);

  ID := ListBox2.ItemIndex;
  if ID = -1 then Exit;
  if fSprites.Data.Flag[ID] = 0 then Exit;

  BM := TBitmap.Create;
  try
    BM.PixelFormat := pf32bit;
    BM.Width := fSprites.Data.Size[ID].X;
    BM.Height := fSprites.Data.Size[ID].Y;
    BM.Transparent := True;
    BM.AlphaFormat := afDefined;

    for I := 0 to BM.Height - 1 do
    for K := 0 to BM.Width - 1 do
    begin
      T := fSprites.Data.RGBA[ID, I*BM.Width+K];
      //Invert Alpha
      T := (T and $FFFFFF) or ((255 - T shr 24) shl 24);
      BM.Canvas.Pixels[K,I] := T;
    end;

    Image1.Picture.Assign(BM);
  finally
    BM.Free;
  end;
end;


procedure TRXXForm1.dlgOpenRXXShow(Sender: TObject);
begin
  //Win7 needs InitialDir to be set OnShow after Execute
  dlgOpenRXX.InitialDir := ExeDir + 'data\sprites\';
end;


procedure TRXXForm1.OpenDialog1Show(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExeDir;
end;


procedure TRXXForm1.SaveDialog1Show(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
end;


procedure TRXXForm1.btnLoadRXXClick(Sender: TObject);
begin
  //WinXP needs InitialDir to be set before Execute
  dlgOpenRXX.InitialDir := ExeDir + 'data\sprites\';
  if not dlgOpenRXX.Execute then Exit;
  fSprites.LoadFromRXXFile(dlgOpenRXX.FileName);

  UpdateList;
end;


procedure TRXXForm1.btnAddClick(Sender: TObject);
var I: Integer;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
  if not OpenDialog1.Execute then Exit;

  for I := 0 to OpenDialog1.Files.Count - 1 do
  begin
    fSprites.AddImage(ExtractFilePath(OpenDialog1.Files[I]),
                      ExtractFileName(OpenDialog1.Files[I]), fSprites.Data.Count);
  end;

  UpdateList;
end;


procedure TRXXForm1.btnReplaceClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := ListBox2.ItemIndex;
  if ID = -1 then Exit;
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
  ID := ListBox2.ItemIndex;
  if ID = -1 then Exit;

  fSprites.Delete(ID);//Data.Flag[ID] := 0;

  UpdateList;
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
  ListBox2.Items.BeginUpdate;
  ListBox2.Items.Clear;

  for I := 0 to fSprites.Data.Count - 1 do
  begin
    if fSprites.Data.Flag[I] = 0 then
      ListBox2.Items.Add(IntToStr(I)+'.')
    else
      ListBox2.Items.Add(Format('%d. %dx%d', [I, fSprites.Data.Size[I].X, fSprites.Data.Size[I].Y]));
  end;

  ListBox2.Items.EndUpdate;
  ListBox2Click(Self);
end;


procedure TRXXForm1.btnPackRXXClick(Sender: TObject);
var SpritePack: TKMSpritePack; RT: TRXType; I: Integer;
begin
  btnPackRXX.Enabled := False;

  for I := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[I] then
  begin
    RT := TRXType(I);

    SpritePack := TKMSpritePack.Create(fPalettes, Rt);

    //Load
    {if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rxx') then
    begin
      fSprites.LoadFromRXXFile(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rxx');
      fSprites.OverloadFromFolder(ExeDir + 'Sprites\');
    end
    else}
    if FileExists(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rx') then
    begin
      SpritePack.LoadFromRXFile(ExeDir + 'data\gfx\res\' + RXInfo[RT].FileName + '.rx');
      SpritePack.OverloadFromFolder(ExeDir + 'Sprites\');
    end
    else
    if DirectoryExists(ExeDir + 'Sprites\') then
      SpritePack.LoadFromFolder(ExeDir + 'Sprites\');

    fLog.AddToLog('Trimmed ' + IntToStr(SpritePack.TrimSprites));

    //fSprites.ExportToBMP(ExeDir + 'Export\'+RXInfo[RT].FileName);

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
