unit RXXEditorForm;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Classes, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils, TypInfo, PNGImage,
  {$IFDEF FPC} LResources, {$ENDIF}
  KM_Defaults, KM_Log, KM_Pics, KM_ResourcePalettes, KM_ResourceSprites, KM_ResourceSpritesEdit;


type
  TRXXForm1 = class(TForm)
    btnAdd: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btnSaveRXX: TButton;
    lbSpritesList: TListBox;
    btnLoadRXX: TButton;
    btnDelete: TButton;
    btnReplace: TButton;
    btnExport: TButton;
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Panel2: TPanel;
    Image2: TImage;
    Label2: TLabel;
    Label3: TLabel;
    btnMaskReplace: TButton;
    btnMaskExport: TButton;
    edtPivotX: TSpinEdit;
    edtPivotY: TSpinEdit;
    chkHasMask: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure OpenDialog1Show(Sender: TObject);
    procedure SaveDialog1Show(Sender: TObject);
    procedure btnLoadRXXClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSaveRXXClick(Sender: TObject);
    procedure lbSpritesListClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnMaskReplaceClick(Sender: TObject);
    procedure btnMaskExportClick(Sender: TObject);
    procedure chkHasMaskClick(Sender: TObject);
    procedure PivotChange(Sender: TObject);
  private
    fPalettes: TKMPalettes;
    fSprites: TKMSpritePackEdit;
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
  fLog := TKMLog.Create(ExeDir + 'RXXEditor.log');

  fPalettes := TKMPalettes.Create;
  fPalettes.LoadPalettes;
end;


procedure TRXXForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fSprites);
  FreeAndNil(fPalettes);
  FreeAndNil(fLog);
end;


procedure TRXXForm1.lbSpritesListClick(Sender: TObject);
  procedure ToggleImageButtons(aState: Boolean);
  begin
    edtPivotX.Enabled := aState;
    edtPivotY.Enabled := aState;
    btnDelete.Enabled := aState;
    btnReplace.Enabled := aState;
    btnExport.Enabled := aState;
    {chkHasMask.Enabled := aState;
    btnMaskReplace.Enabled := aState;
    btnMaskExport.Enabled := aState;}
  end;
var
  ID: Integer;
  PNGBase: TPNGObject;
  PNGMask: TPNGObject;
begin
  ToggleImageButtons(False);
  Image1.Picture.Bitmap.Canvas.Brush.Color := 0;
  Image1.Picture.Bitmap.Canvas.FillRect(Image1.Picture.Bitmap.Canvas.ClipRect);
  Image2.Picture.Bitmap.Canvas.Brush.Color := 0;
  Image2.Picture.Bitmap.Canvas.FillRect(Image1.Picture.Bitmap.Canvas.ClipRect);
  chkHasMask.Checked := False;

  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;
  if fSprites.RXData.Flag[ID] = 0 then Exit;

  PNGBase := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, 0, 0);
  PNGMask := TPNGObject.CreateBlank(COLOR_GRAYSCALE, 8, 0, 0);
  try
    fSprites.GetImageToBitmap(ID, PNGBase, PNGMask);
    Image1.Picture.Assign(PNGBase);
    if PNGMask.Width * PNGMask.Height <> 0 then
      Image2.Picture.Assign(PNGMask);
  finally
    PNGBase.Free;
    PNGMask.Free;
  end;

  edtPivotX.Value := fSprites.RXData.Pivot[ID].x;
  edtPivotY.Value := fSprites.RXData.Pivot[ID].y;
  chkHasMask.Checked := fSprites.RXData.HasMask[ID];

  ToggleImageButtons(True);
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
  OpenDialog1.Filter := 'RX, RXX packages (*.rx;*.rxx)|*.rxx;*.rx;';
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
  fSprites := TKMSpritePackEdit.Create(RT, fPalettes);

  Label1.Caption := ExtractFileName(OpenDialog1.FileName);

  if SameText(ExtractFileExt(OpenDialog1.FileName), '.rx') then
    fSprites.LoadFromRXFile(OpenDialog1.FileName)
  else
  if SameText(ExtractFileExt(OpenDialog1.FileName), '.rxx') then
    fSprites.LoadFromRXXFile(OpenDialog1.FileName);

  btnSaveRXX.Enabled := fSprites.IsLoaded;
  btnAdd.Enabled := fSprites.IsLoaded;

  UpdateList;
end;


procedure TRXXForm1.btnMaskExportClick(Sender: TObject);
begin
  //
end;


procedure TRXXForm1.btnMaskReplaceClick(Sender: TObject);
begin
  //
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


procedure TRXXForm1.btnReplaceClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  //WinXP needs InitialDir to be set before Execute
  OpenDialog1.InitialDir := ExeDir;
  OpenDialog1.Filter := 'Supported images (*.png)|*.png';
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
  PNGBase: TPNGObject;
  PNGMask: TPNGObject;
  FileName, FileNameA: string;
begin
  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  SaveDialog1.InitialDir := ExeDir;
  SaveDialog1.Filter := 'PNG image (*.png)|*.png';
  SaveDialog1.Options := SaveDialog1.Options - [ofAllowMultiSelect];
  if not SaveDialog1.Execute then Exit;

  FileName := SaveDialog1.FileName;
  FileNameA := StringReplace(FileName, '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]);

  PNGBase := TPNGObject.CreateBlank(COLOR_RGBALPHA, 8, 0, 0);
  PNGMask := TPNGObject.CreateBlank(COLOR_GRAYSCALE, 8, 0, 0);
  try
    fSprites.GetImageToBitmap(ID, PNGBase, PNGMask);

    PNGBase.SaveToFile(FileName);
    if PNGMask.Width * PNGMask.Height <> 0 then
      PNGMask.SaveToFile(FileNameA);
  finally
    PNGBase.Free;
    PNGMask.Free;
  end;
end;


procedure TRXXForm1.btnSaveRXXClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then Exit;
  fLog.AddToLog('Trimmed ' + IntToStr(fSprites.TrimSprites));
  fSprites.SaveToRXXFile(SaveDialog1.FileName);
end;


procedure TRXXForm1.chkHasMaskClick(Sender: TObject);
begin
  //
end;


procedure TRXXForm1.PivotChange(Sender: TObject);
var
  ID: Integer;
begin
  //To avoid OnChange misfire when we change selected Item we disable controls
  if not TEdit(Sender).Enabled then Exit;
  if not TryStrToInt(TEdit(Sender).Text, ID) then Exit;

  ID := lbSpritesList.ItemIndex + 1;
  if ID = 0 then Exit;

  fSprites.RXData.Pivot[ID].x := edtPivotX.Value;
  fSprites.RXData.Pivot[ID].y := edtPivotY.Value;
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


{$IFDEF FPC}
initialization
  {$i RXXEditorForm.lrs}
{$ENDIF}


end.
