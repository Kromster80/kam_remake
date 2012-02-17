unit Unit1;
interface
uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics, Math, Spin, StdCtrls, SysUtils, Windows,
  KM_Defaults, KM_Campaigns;


type
  TForm1 = class(TForm)
    tvList: TTreeView;
    btnSaveCMP: TButton;
    btnLoadCMP: TButton;
    btnLoadPicture: TButton;
    seMapCount: TSpinEdit;
    Label1: TLabel;
    seNodeCount: TSpinEdit;
    Label2: TLabel;
    dlgOpenPicture: TOpenDialog;
    dlgOpenCampaign: TOpenDialog;
    dlgSaveCampaign: TSaveDialog;
    Bevel1: TBevel;
    edtShortName: TEdit;
    Label6: TLabel;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    imgBlackFlag: TImage;
    imgRedFlag: TImage;
    imgNode: TImage;
    RadioGroup1: TRadioGroup;
    procedure btnLoadPictureClick(Sender: TObject);
    procedure btnLoadCMPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure seMapCountChange(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure btnSaveCMPClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RadioGroup1Click(Sender: TObject);
    procedure edtShortNameChange(Sender: TObject);
  private
    imgFlags: array of TImage;
    imgNodes: array of TImage;
    fUpdating: Boolean;
    fSelectedMap: Integer;
    fSelectedNode: Integer;
    PrevX, PrevY: Integer;
  public
    procedure FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure SelectMap;
    procedure RefreshFlags;
    procedure UpdateList;
    procedure UpdateFlagCount;
    procedure UpdateNodeCount;
  end;


var
  Form1: TForm1;
  C: TKMCampaign;


implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  C := TKMCampaign.Create;
  fSelectedMap := -1;

  seMapCount.MaxValue := MAX_CAMP_MAPS;
  seNodeCount.MaxValue := MAX_CAMP_NODES;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Img: TImage;
begin
  Img := nil;

  if (fSelectedMap <> -1) then
    if (fSelectedNode <> -1) then
      Img := imgNodes[fSelectedNode]
    else
      Img := imgFlags[fSelectedMap];

  if Img <> nil then
  case Key of
    Ord('D'): Img.Left := Img.Left + 1;
    Ord('A'): Img.Left := Img.Left - 1;
    Ord('W'): Img.Top  := Img.Top  - 1;
    Ord('S'): Img.Top  := Img.Top  + 1;
  end;
end;


procedure TForm1.FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fSelectedMap := TImage(Sender).Tag;
    fSelectedNode := -1;
    SelectMap;

    PrevX := X;
    PrevY := Y;
  end;
end;


procedure TForm1.FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Img: TImage;
begin


  if (ssLeft in Shift) and (TImage(Sender).Tag = fSelectedMap) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), Image1.Left, Image1.Left + 1024);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), Image1.Top, Image1.Top + 768);

    C.Maps[fSelectedMap].Flag.X := Img.Left - Image1.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - Image1.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
end;


procedure TForm1.NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    fSelectedNode := TImage(Sender).Tag;
    SelectMap;

    PrevX := X;
    PrevY := Y;
  end;
end;


procedure TForm1.NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var Img: TImage;
begin
  if (ssLeft in Shift) and (fSelectedMap <> -1) and (fSelectedNode <> -1) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), Image1.Left, Image1.Left + 1024);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), Image1.Top, Image1.Top + 768);

    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left - Image1.Left;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  - Image1.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
end;


procedure TForm1.btnSaveCMPClick(Sender: TObject);
begin
  dlgSaveCampaign.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);
  if not dlgSaveCampaign.Execute then Exit;

  C.SaveToFile(dlgSaveCampaign.FileName);
end;


procedure TForm1.btnLoadCMPClick(Sender: TObject);
begin
  if DirectoryExists(ExtractFilePath(ParamStr(0)) + '..\..\Campaigns\') then
    dlgOpenCampaign.InitialDir := ExtractFilePath(ParamStr(0)) + '..\..\Campaigns\'
  else
    dlgOpenCampaign.InitialDir := ExtractFilePath(ParamStr(0));

  if not dlgOpenCampaign.Execute then Exit;

  C.LoadFromFile(dlgOpenCampaign.FileName);

  fSelectedMap := -1;
  fSelectedNode := -1;

  btnSaveCMP.Enabled := True;
  btnLoadPicture.Enabled := True;

  UpdateList;
  UpdateFlagCount;
  RefreshFlags;
end;


procedure TForm1.btnLoadPictureClick(Sender: TObject);
begin
  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);
  if not dlgOpenPicture.Execute then Exit;

  Image1.Picture.LoadFromFile(dlgOpenPicture.FileName);
end;


procedure TForm1.edtShortNameChange(Sender: TObject);
begin
  if fUpdating then Exit;

  C.ShortTitle := AnsiString(edtShortName.Text);

  //Shortname may be used as mapname in List
  UpdateList;
end;


procedure TForm1.seMapCountChange(Sender: TObject);
begin
  if fUpdating then Exit;

  C.MapCount := seMapCount.Value;

  if fSelectedMap > C.MapCount - 1 then
    fSelectedMap := -1;

  UpdateList;
  UpdateFlagCount;
  RefreshFlags;
end;


procedure TForm1.MapChange(Sender: TObject);
begin
  if fUpdating or (fSelectedMap = -1) then Exit;

  C.Maps[fSelectedMap].NodeCount := seNodeCount.Value;

  if fSelectedNode > C.Maps[fSelectedMap].NodeCount - 1 then
    fSelectedNode := -1;

  UpdateList;
  UpdateNodeCount;
  RefreshFlags;
end;


procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  C.Maps[fSelectedMap].TextPos := RadioGroup1.ItemIndex;
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

  if fSelectedMap = -1 then Exit;

  for I := 0 to C.Maps[fSelectedMap].NodeCount - 1 do
  begin
    imgNodes[I].Left := C.Maps[fSelectedMap].Nodes[I].X + Image1.Left;
    imgNodes[I].Top := C.Maps[fSelectedMap].Nodes[I].Y + Image1.Top;
  end;
end;


procedure TForm1.UpdateList;
var
  I, K: Integer;
  N, SN: TTreeNode;
begin
  fUpdating := True;

  edtShortName.Text := C.ShortTitle;
  seMapCount.Value := C.MapCount;

  tvList.Items.Clear;

  for I := 0 to C.MapCount - 1 do
  begin
    N := tvList.Items.AddChild(nil, C.ShortTitle + ' mission ' + IntToStr(I + 1));
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
  if fUpdating then Exit;

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
      imgFlags[I].Parent := ScrollBox1;
      imgFlags[I].AutoSize := True;
      imgFlags[I].Transparent := True;
      imgFlags[I].Tag := I;
      //imgFlags[I].OnClick := FlagClick; //Select
      imgFlags[I].OnMouseDown := FlagDown; //Start drag
      imgFlags[I].OnMouseMove := FlagMove; //Drag
    end;
  end;

  //Hide unused flags
  for I := 0 to Length(imgFlags) - 1 do
    imgFlags[I].Visible := (I <= C.MapCount - 1);
end;


procedure TForm1.UpdateNodeCount;
var
  I: Integer;
begin
  if fSelectedMap = -1 then Exit;

  //Create nodes
  if C.Maps[fSelectedMap].NodeCount > Length(imgNodes) then
  begin
    SetLength(imgNodes, C.Maps[fSelectedMap].NodeCount);

    for I := 0 to C.Maps[fSelectedMap].NodeCount - 1 do
    if imgNodes[I] = nil then
    begin
      imgNodes[I] := TImage.Create(Image1);
      imgNodes[I].Parent := ScrollBox1;
      imgNodes[I].AutoSize := True;
      imgNodes[I].Transparent := True;
      imgNodes[I].Picture.Bitmap := imgNode.Picture.Bitmap;
      imgNodes[I].Tag := I;
      //imgFlags[I].OnClick := NodeClick; //Select
      imgNodes[I].OnMouseDown := NodeDown; //Start drag
      imgNodes[I].OnMouseMove := NodeMove; //Drag
    end;
  end;

  //Hide unused nodes
  for I := 0 to Length(imgNodes) - 1 do
    imgNodes[I].Visible := (I <= C.Maps[fSelectedMap].NodeCount - 1);
end;


procedure TForm1.SelectMap;
var I, M, N: Integer;
begin
  if fUpdating then Exit;

  fUpdating := True;

  //Try to select map in TreeList
  M := -1;
  N := -1;
  for I := 0 to tvList.Items.Count - 1 do
  begin
    if tvList.Items[I].Parent = nil then
    begin
      Inc(M);
      N := -1; //Reset node count
    end
    else
      Inc(N);

    if (M = fSelectedMap) and (N = fSelectedNode) then
      tvList.Items[I].Selected := True;
  end;

  //Update map info
  StatusBar1.Panels[0].Text := 'Selected map: ' + IntToStr(fSelectedMap) + '/' + IntToStr(fSelectedNode);
  seNodeCount.Value := C.Maps[fSelectedMap].NodeCount;

  fUpdating := False;

  UpdateNodeCount;
  RefreshFlags;
end;


end.
