unit Unit1;
interface
uses
  Forms, StdCtrls, ExtCtrls, ComCtrls, Math, SysUtils,
  KM_Defaults, KM_Campaigns, Dialogs, Controls,
  Spin, Graphics, Classes;

type
  TForm1 = class(TForm)
    tvList: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Button7: TButton;
    seMapCount: TSpinEdit;
    Label1: TLabel;
    seNodeCount: TSpinEdit;
    Label2: TLabel;
    dlgOpenPicture: TOpenDialog;
    dlgOpenCampaign: TOpenDialog;
    dlgSaveCampaign: TSaveDialog;
    Label3: TLabel;
    cbImageID: TSpinEdit;
    cbImageLib: TComboBox;
    Bevel1: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    edtShortName: TEdit;
    edtFullName: TEdit;
    edtMapName: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    imgBlackFlag: TImage;
    imgRedFlag: TImage;
    imgNode: TImage;
    procedure Button7Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure seMapCountChange(Sender: TObject);
    procedure CampaignChange(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    imgFlags: array of TImage;
    fUpdating: Boolean;
    fSelectedMap: Integer;
    fSelectedNode: Integer;
    PrevX, PrevY: Integer;
  public
    procedure UpdateList;
    procedure UpdateFlagCount;

    procedure FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SelectMap;
    procedure RefreshFlags;
  end;


var
  Form1: TForm1;
  C: TKMCampaign;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
const
  RXFileName: array [TRXType] of string = (
    'Trees', 'Houses', 'Units', 'GUI',
    'GUIMain', 'GUIMainH', 'RemakeMenu', 'Tileset', 'RemakeGame');
var
  R: TRXType;
begin
  cbImageLib.Clear;
  for R := Low(TRXType) to High(TRXType) do
    cbImageLib.AddItem(RXFileName[R], nil);

  C := TKMCampaign.Create;
  fSelectedMap := -1;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {if fSelectedMap = -1 then Exit;

  case Key of
    VK_LEFT:  imgFlags[fSelectedMap].Left := imgFlags[fSelectedMap].Left - 1;
    VK_RIGHT: imgFlags[fSelectedMap].Left := imgFlags[fSelectedMap].Left + 1;
    VK_UP:    imgFlags[fSelectedMap].Top := imgFlags[fSelectedMap].Top - 1;
    VK_DOWN:  imgFlags[fSelectedMap].Top := imgFlags[fSelectedMap].Top + 1;
  end;}
end;


procedure TForm1.FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fSelectedMap := TImage(Sender).Tag;
    fSelectedNode := -1;

    PrevX := X;
    PrevY := Y;

    SelectMap;
  end;
end;


procedure TForm1.FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    TImage(Sender).Left := TImage(Sender).Left + (X - PrevX);
    TImage(Sender).Top  := TImage(Sender).Top  + (Y - PrevY);

    C.Maps[fSelectedMap].Flag.X := TImage(Sender).Left - Image1.Left;
    C.Maps[fSelectedMap].Flag.Y := TImage(Sender).Top  - Image1.Top;

    //PrevX := X;
    //PrevY := Y;
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  dlgSaveCampaign.InitialDir := ParamStr(0);
  if not dlgSaveCampaign.Execute then Exit;

  C.SaveToFile(dlgSaveCampaign.FileName);
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  dlgOpenCampaign.InitialDir := ParamStr(0);
  if not dlgOpenCampaign.Execute then Exit;

  C.LoadFromFile(dlgOpenCampaign.FileName);

  fSelectedMap := -1;
  fSelectedNode := -1;

  UpdateList;
  UpdateFlagCount;
  RefreshFlags;
end;


procedure TForm1.Button7Click(Sender: TObject);
begin
  dlgOpenPicture.InitialDir := ParamStr(0);
  if not dlgOpenPicture.Execute then Exit;

  Image1.Picture.LoadFromFile(dlgOpenPicture.FileName);
end;


procedure TForm1.CampaignChange(Sender: TObject);
var P: TPicID;
begin
  if fUpdating then Exit;

  C.FullTitle := edtFullName.Text;
  C.ShortTitle := edtShortName.Text;

  P.RX := TRXType(cbImageLib.ItemIndex);
  P.ID := cbImageID.Value;
  C.BackGroundPic := P;

  //Shortname may be used as mapname in List
  UpdateList;
end;


procedure TForm1.seMapCountChange(Sender: TObject);
begin
  if fUpdating then Exit;

  C.MapCount := seMapCount.Value;

  UpdateList;
  UpdateFlagCount;
  RefreshFlags;
end;


procedure TForm1.MapChange(Sender: TObject);
begin
  if fUpdating or (fSelectedMap = -1) then Exit;

  C.Maps[fSelectedMap].MapName := edtMapName.Text;
  C.Maps[fSelectedMap].NodeCount := seNodeCount.Value;

  UpdateList;
end;


procedure TForm1.RefreshFlags;
var
  I: Integer;
begin
  for I := 0 to C.MapCount - 1 do
  begin
    imgFlags[I].Left := C.Maps[I].Flag.X + Image1.Left;
    imgFlags[I].Top := C.Maps[I].Flag.Y + Image1.Top;
    if I > fSelectedMap then
      imgFlags[I].Picture.Bitmap := imgBlackFlag.Picture.Bitmap
    else
      imgFlags[I].Picture.Bitmap := imgRedFlag.Picture.Bitmap
  end;
end;


procedure TForm1.UpdateList;
var
  I, K: Integer;
  N, SN: TTreeNode;
  S: string;
begin
  fUpdating := True;

  edtFullName.Text  := C.FullTitle;
  edtShortName.Text := C.ShortTitle;

  cbImageLib.ItemIndex  := Byte(C.BackGroundPic.RX);
  cbImageID.Value       := C.BackGroundPic.ID;

  seMapCount.Value := C.MapCount;

  tvList.Items.Clear;

  for I := 0 to C.MapCount - 1 do
  begin
    if Trim(C.Maps[I].MapName) <> '' then
      S := C.Maps[I].MapName
    else
      S := C.ShortTitle + ' mission ' + IntToStr(I + 1);

    N := tvList.Items.AddChild(nil, S);
    if fSelectedMap = I then
      N.Selected := True;

    for K := 0 to C.Maps[I].NodeCount - 1 do
    begin
      SN := tvList.Items.AddChild(N, 'node ' + IntToStr(K));
      if fSelectedNode = K then
        SN.Selected := True;
    end;
  end;

  fUpdating := False;
end;


procedure TForm1.tvListChange(Sender: TObject; Node: TTreeNode);
begin
  if Node.Parent = nil then
  begin
    fSelectedMap := Node.Index;
    fSelectedNode := -1;
  end
  else
  begin
    fSelectedMap := Node.Parent.Index;
    fSelectedNode := Node.Index;
  end;

  SelectMap;
end;


procedure TForm1.UpdateFlagCount;
var
  I: Integer;
begin
  //Create more flags if needed
  if C.MapCount > Length(imgFlags) then
  begin
    SetLength(imgFlags, C.MapCount);

    for I := 0 to C.MapCount - 1 do
    if imgFlags[I] = nil then
    begin
      imgFlags[I] := TImage.Create(Image1);
      imgFlags[I].Parent := Self;
      imgFlags[I].AutoSize := True;
      imgFlags[I].Transparent := True;
      imgFlags[I].Tag := I;
      imgFlags[I].OnMouseDown := FlagDown;
      imgFlags[I].OnMouseMove := FlagMove;
    end;
  end;

  //Hide unused flags
  for I := 0 to Length(imgFlags) - 1 do
    imgFlags[I].Visible := (I <= C.MapCount - 1);
end;


procedure TForm1.SelectMap;
begin
  if fUpdating then Exit;

  fUpdating := True;

  //Select map in TreeList
  if InRange(fSelectedMap, 0, tvList.Items.Count - 1) then
  begin
    tvList.Items[fSelectedMap].Selected := True;

    if InRange(fSelectedNode, 0, tvList.Items[fSelectedMap].Count - 1) then
      tvList.Items[fSelectedMap][fSelectedNode].Selected := True
    else
      fSelectedNode := -1;
  end
  else
    fSelectedMap := -1;

  //Update map info
  Label7.Caption := 'Mission name (' + IntToStr(fSelectedMap) + '/' + IntToStr(fSelectedNode) + ')';
  edtMapName.Text := C.Maps[fSelectedMap].MapName;
  seNodeCount.Value := C.Maps[fSelectedMap].NodeCount;

  fUpdating := False;

  RefreshFlags;
end;


end.
