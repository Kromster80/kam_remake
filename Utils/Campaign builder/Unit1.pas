unit Unit1;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Mask, Math, Spin, StdCtrls, SysUtils,
  KM_Defaults, KM_Campaigns, KM_Pics, KM_ResSpritesEdit;


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
    Label6: TLabel;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    imgBlackFlag: TImage;
    imgRedFlag: TImage;
    imgNode: TImage;
    rgBriefingPos: TRadioGroup;
    edtShortName: TMaskEdit;
    shpBriefing: TShape;
    procedure btnLoadPictureClick(Sender: TObject);
    procedure btnLoadCMPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure seMapCountChange(Sender: TObject);
    procedure MapChange(Sender: TObject);
    procedure btnSaveCMPClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgBriefingPosClick(Sender: TObject);
    procedure edtShortNameChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    imgFlags: array of TImage;
    imgNodes: array of TImage;
    fUpdating: Boolean;
    fSelectedMap: Integer;
    fSelectedNode: Integer;
    PrevX, PrevY: Integer;
    fSprites: TKMSpritePackEdit;
  public
    procedure FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure SelectMap;
    procedure RefreshBackground;
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
  DoubleBuffered := True; //Makes images drag around smoothly
  ScrollBox1.DoubleBuffered := True;

  Caption := 'Campaign Builder (' + GAME_REVISION + ')';

  C := TKMCampaign.Create;
  fSelectedMap := -1;

  seMapCount.MaxValue := MAX_CAMP_MAPS;
  seNodeCount.MaxValue := MAX_CAMP_NODES;

  fSprites := TKMSpritePackEdit.Create(rxTrees, nil);

  seMapCountChange(nil); //Initialise it to 1 map
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fSprites.Free;
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

  if Img = nil then Exit;

  case Key of
    Ord('D'): Img.Left := Img.Left + 1;
    Ord('A'): Img.Left := Img.Left - 1;
    Ord('W'): Img.Top  := Img.Top  - 1;
    Ord('S'): Img.Top  := Img.Top  + 1;
  end;

  if (fSelectedNode <> -1) then
  begin
    //Position node centers, so that if someone changes the nodes they still look correct
    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left - Image1.Left + Img.Width div 2;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  - Image1.Top + Img.Height div 2;
  end
  else
  begin
    C.Maps[fSelectedMap].Flag.X := Img.Left - Image1.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - Image1.Top;
  end;

  StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
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

    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left + Img.Width div 2  - Image1.Left;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  + Img.Height div 2 - Image1.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
end;


procedure TForm1.btnSaveCMPClick(Sender: TObject);
begin
  if C.MapCount < 2 then
  begin
    ShowMessage('Campaign must have at least 2 missions');
    Exit;
  end;

  if Length(Trim(C.CampName)) <> 3 then
  begin
    ShowMessage('Campaign short title must be 3 characters');
    Exit;
  end;

  dlgSaveCampaign.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);
  if not dlgSaveCampaign.Execute then Exit;

  C.SaveToFile(dlgSaveCampaign.FileName);

  fSprites.SaveToRXXFile(ExtractFilePath(dlgSaveCampaign.FileName) + 'images.rxx');
end;


procedure TForm1.btnLoadCMPClick(Sender: TObject);
var I: Integer;
begin
  if DirectoryExists(ExtractFilePath(Application.ExeName) + '..\..\Campaigns\') then
    dlgOpenCampaign.InitialDir := ExtractFilePath(Application.ExeName) + '..\..\Campaigns\'
  else
    dlgOpenCampaign.InitialDir := ExtractFilePath(Application.ExeName);

  //Win7 workaround ?
  //dlgOpenCampaign.FileName := ExtractFilePath(Application.ExeName) + '..\..\Campaigns\';

  if not dlgOpenCampaign.Execute then Exit;

  C.LoadFromFile(dlgOpenCampaign.FileName);

  fSprites.Free;
  fSprites := TKMSpritePackEdit.Create(rxTrees, nil);
  if FileExists(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx') then
    fSprites.LoadFromRXXFile(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx')
  else
    ShowMessage('Campaign background image (images.rxx) could not be found');

  fSelectedMap := -1;
  fSelectedNode := -1;

  UpdateList;
  UpdateFlagCount;
  RefreshBackground;
  RefreshFlags;
  //Hide nodes that might have been open from the last campaign
  for I := 0 to Length(imgNodes) - 1 do
    imgNodes[I].Visible := False;
end;


procedure TForm1.btnLoadPictureClick(Sender: TObject);
begin
  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);
  if not dlgOpenPicture.Execute then Exit;

  fSprites.AddImage(ExtractFilePath(dlgOpenPicture.FileName),
                    ExtractFileName(dlgOpenPicture.FileName), 1);

  RefreshBackground;
end;


procedure TForm1.edtShortNameChange(Sender: TObject);
var
  cmp: TKMCampaignId;
begin
  if fUpdating then Exit;

  cmp[0] := Ord(edtShortName.Text[1]);
  cmp[1] := Ord(edtShortName.Text[2]);
  cmp[2] := Ord(edtShortName.Text[3]);
  C.CampaignId := cmp;

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


procedure TForm1.rgBriefingPosClick(Sender: TObject);
begin
  if fUpdating or (fSelectedMap = -1) then Exit;

  C.Maps[fSelectedMap].TextPos := TBriefingCorner(rgBriefingPos.ItemIndex);

  RefreshFlags;
end;


procedure TForm1.RefreshBackground;
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    fSprites.GetImageToBitmap(1, Bmp, nil);
    Image1.Picture.Assign(Bmp);
  finally
    Bmp.Free;
  end;
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
    //Position node centers, so that if someone changes the nodes they still look correct
    imgNodes[I].Left := Image1.Left + C.Maps[fSelectedMap].Nodes[I].X - imgNodes[I].Width div 2;
    imgNodes[I].Top := Image1.Top + C.Maps[fSelectedMap].Nodes[I].Y - imgNodes[I].Height div 2;
  end;

  shpBriefing.Top := Image1.Height - shpBriefing.Height;
  shpBriefing.Left := IfThen(C.Maps[fSelectedMap].TextPos = bcBottomRight, Image1.Width - shpBriefing.Width, 0);
end;


procedure TForm1.UpdateList;
var
  I, K: Integer;
  N, SN: TTreeNode;
begin
  fUpdating := True;

  edtShortName.Text := C.CampName;
  seMapCount.Value := C.MapCount;

  tvList.Items.Clear;

  for I := 0 to C.MapCount - 1 do
  begin
    N := tvList.Items.AddChild(nil, C.CampName + ' mission ' + IntToStr(I + 1));
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

  seNodeCount.Value := C.Maps[fSelectedMap].NodeCount;
  rgBriefingPos.ItemIndex := Byte(C.Maps[fSelectedMap].TextPos);

  //Update map info
  StatusBar1.Panels[0].Text := 'Selected map: ' + IntToStr(fSelectedMap) + '/' + IntToStr(fSelectedNode);

  fUpdating := False;

  UpdateNodeCount;
  RefreshFlags;
end;


end.
