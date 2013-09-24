unit KM_ReadGFX1;
{$I ..\KaM_Remake.inc}

interface

function ReadGFX(text: string): boolean;
procedure MakeObjectsGFX(Sender: TObject);
procedure MakeHousesGFX(Sender: TObject);

implementation

uses KM_Unit1, KM_Defaults, KM_Form_Loading, Graphics, Sysutils, Dialogs, Math,
{$IFDEF FPC} GL, {$ENDIF}
  dglOpenGL, KromUtils;

function ReadGFX(text: string): boolean;
var
  ii, id: integer;
  MyBitMap, MyBitMapA: TBitMap;
  x, y, sy, sx: integer;
  rw, row, col, ColWidth, W: integer;
begin
  Result := false;

  //=============================================
  //Reading pallete for trees/objects
  //=============================================
  FormLoading.Label1.Caption := 'Reading pallete';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  if fileexists(text + 'data\gfx\pal0.bbm') then
  begin
    assignfile(f, text + 'data\gfx\pal0.bbm');
    reset(f, 1);
    blockread(f, c, 48);
    blockread(f, Pal0, 768); //256*3
    closefile(f);
  end
  else
  begin
    ShowMessage('Unable to locate ".\data\gfx\pal0.bbm" file');
    ShowMessage('Pallete loading error.' + EolW + 'Place KaM Editor into KaM folder');
    exit;
  end;

  //=============================================
  //Reading trees/objects data
  //=============================================
  FormLoading.Label1.Caption := 'Reading objects';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  if fileexists(text + 'data\gfx\res\trees.rx') then
  begin
    assignfile(f, text + 'data\gfx\res\trees.rx');
    reset(f, 1);
    blockread(f, TreeQty, 4);
    blockread(f, TreePal, TreeQty);
    for ii := 1 to TreeQty do
      if TreePal[ii] = 1 then
      begin //entry used
        blockread(f, TreeSize[ii], 4);
        blockread(f, TreePivot[ii], 8);
        setlength(TreeData[ii], TreeSize[ii, 1] * TreeSize[ii, 2]);
        blockread(f, TreeData[ii, 0], TreeSize[ii, 1] * TreeSize[ii, 2]);
      end;
    closefile(f);
  end
  else
  begin
    ShowMessage('Unable to locate ".\data\gfx\res\trees.rx" file');
    ShowMessage('Objects loading error.' + EolW + 'Place KaM Editor into KaM folder');
    exit;
  end;

  //=============================================
  //Reading map elements (has animation data)
  //=============================================
  FormLoading.Label1.Caption := 'Reading map elements';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  if fileexists(text + 'data\defines\mapelem.dat') then
  begin
    assignfile(f, text + 'data\defines\mapelem.dat');
    reset(f, 1);
    //for ii:=1 to 254 do
    blockread(f, MapElem[1], MapElemQty * 99); //256*3
    closefile(f);
  end
  else
  begin
    ShowMessage('Unable to locate ".\data\defines\mapelem.dat" file');
    ShowMessage('Map Elements loading error.' + EolW + 'Place KaM Editor into KaM folder');
    exit;
  end;

  //=============================================
  //Adding all objects to ImageList in right order
  //=============================================
  FormLoading.Label1.Caption := 'Initializing objects';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  rw := 0;
  row := 1;
  col := 1; //row/col in ObjPallete
  Form1.ObjPallete.ColCount := length(ObjPalleteTable[1]) - 1;
  Form1.ObjPallete.DefaultColWidth := (Form1.ObjPallete.Width - 5) div (Form1.ObjPallete.ColCount);
  //(Area / count) - 1 for mortar
  ColWidth := Form1.ObjPallete.DefaultColWidth;
  for ii := 1 to length(ObjIndex) do
  begin
    //for ii:=1 to MapElemQty do begin //1..254
    MyBitMap := TBitMap.Create;
    MyBitMap.Height := 128;
    MyBitMap.Width := 128;
    MyBitMapA := TBitMap.Create;
    MyBitMapA.Height := 128;
    MyBitMapA.Width := 128;
    MyBitMap.PixelFormat := pf24bit;

    //id:=MapElem[ii].Tree[1]+1;
    id := ObjIndexGFX[ii];

    sy := TreeSize[id, 2];
    if sy > 128 then
      sy := 128;
    sx := TreeSize[id, 1];
    if sx > 128 then
      sx := 128;
    for y := 0 to sy - 1 do
      for x := 0 to sx - 1 do
      begin
        MyBitMap.Canvas.Pixels[x, y] := Pal0[TreeData[id, y * sx + x] + 1, 1] + Pal0[TreeData[id, y * sx + x] + 1, 2] *
          256 + Pal0[TreeData[id, y * sx + x] + 1, 3] * 65536;
        if TreeData[id, y * sx + x] <> 0 then
          MyBitMapA.Canvas.Pixels[x, y] := 255
        else
          MyBitMapA.Canvas.Pixels[x, y] := 0;
      end;
    if sy > 0 then
      Form1.ImageList1.Add(MyBitMap, MyBitMapA);

    //Number of used columns
    W := ((sx - 1) div ColWidth) + 1;
    inc(rw, W * (ColWidth));
    if (rw >= Form1.ObjPallete.Width - 5) or (col + W > Form1.ObjPallete.ColCount) or
      (col > length(ObjPalleteTable[1]) - 1) then
    begin
      inc(row);
      col := 1;
      rw := 0;
    end;

    ObjPalleteTable[row, col] := ii;
    ObjPalleteTable[row, 0] := math.max(ObjPalleteTable[row, 0], sy);
    for x := col + 1 to math.min(Form1.ObjPallete.ColCount, length(ObjPalleteTable[1]) - 1) do
      ObjPalleteTable[row, x] := ii;
    inc(col, W);

  end;
  Form1.ObjPallete.RowCount := row;
  Form1.ObjPalleteScroll.max := row - 1;

  for ii := 1 to row do
    Form1.ObjPallete.RowHeights[ii - 1] := ObjPalleteTable[ii, 0] + 4;

  //=============================================
  //Reading houses data
  //=============================================
  FormLoading.Label1.Caption := 'Reading houses';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  if fileexists(text + 'data\gfx\res\houses.rx') then
  begin
    assignfile(f, text + 'data\gfx\res\houses.rx');
    reset(f, 1);
    blockread(f, HouseQty, 4);
    blockread(f, HousePal, HouseQty);
    for ii := 1 to HouseQty do
      if HousePal[ii] = 1 then
      begin //entry used
        blockread(f, HouseSize[ii], 4);
        blockread(f, HousePivot[ii], 8);
        setlength(HouseData[ii], HouseSize[ii, 1] * HouseSize[ii, 2]);
        blockread(f, HouseData[ii, 0], HouseSize[ii, 1] * HouseSize[ii, 2], NumRead);
        if HouseSize[ii, 1] * HouseSize[ii, 2] <> NumRead then
          s := '0';
      end;
    closefile(f);
  end
  else
  begin
    ShowMessage('Unable to locate ".\data\gfx\res\houses.rx" file');
    ShowMessage('Houses loading error.' + EolW + 'Place KaM Editor into KaM folder');
    exit;
  end;

  //=============================================
  //Adding all houses to ImageList in right order
  //=============================================
  FormLoading.Label1.Caption := 'Initializing houses';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  row := 1; //row in ObjPallete
  Form1.HousePallete.ColCount := 1;
  Form1.HousePallete.DefaultColWidth := 256;
  for ii := 1 to 29 do
    if ii <> 27 then
    begin
      MyBitMap := TBitMap.Create;
      MyBitMap.Height := 256;
      MyBitMap.Width := 256;
      MyBitMapA := TBitMap.Create;
      MyBitMapA.Height := 256;
      MyBitMapA.Width := 256;
      MyBitMap.PixelFormat := pf24bit;

      id := HouseIndexGFX[ii];
      HouseID[row] := ii;

      sy := HouseSize[id, 2];
      if sy > 256 then
        sy := 256;
      sx := HouseSize[id, 1];
      if sx > 256 then
        sx := 256;
      for y := 0 to sy - 1 do
        for x := 0 to sx - 1 do
        begin
          MyBitMap.Canvas.Pixels[x, y] := Pal0[HouseData[id, y * HouseSize[id, 1] + x] + 1, 1] +
            Pal0[HouseData[id, y * HouseSize[id, 1] + x] + 1, 2] * 256 +
            Pal0[HouseData[id, y * HouseSize[id, 1] + x] + 1, 3] * 65536;
          if HouseData[id, y * HouseSize[id, 1] + x] <> 0 then
            MyBitMapA.Canvas.Pixels[x, y] := 255
          else
            MyBitMapA.Canvas.Pixels[x, y] := 0;
        end;
      if sy > 0 then
        Form1.ImageList2.Add(MyBitMap, MyBitMapA);

      Form1.HousePallete.RowCount := row;
      Form1.HousePallete.RowHeights[row - 1] := sy;

      inc(row);
    end;
  Form1.HousePallete.RowCount := row - 1;
  Form1.HousePalleteScroll.max := row - 2;

  Result := true;
end;

procedure MakeObjectsGFX(Sender: TObject);
var
  h, i, k, mx, my, id: integer;
  TD: Pointer;
  by: ^cardinal;
begin
  //=============================================
  //Making OpenGL textures out of objects
  //=============================================
  FormLoading.Label1.Caption := 'Preparing objects textures';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  for h := 1 to length(ObjIndex) do
  begin
    id := ObjIndexGFX[h];
    if id = 0 then
      id := 81;
    mx := TreeSize[id, 1];
    my := TreeSize[id, 2];
    if mx > 128 then
      mx := 128;
    if my > 128 then
      my := 128;

    TD := AllocMem(128 * 128 * 4); //GetMem+FillChar(0)
    for i := 0 to 127 do
      for k := 0 to 127 do
        if (i < my) and (k < mx) and (TreeData[id, i * TreeSize[id, 1] + k] <> 0) then
        begin
          by := Pointer(integer(TD) + ((i + 128 - my) * 128 + k) * 4); //Get pointer
          by^ := Pal0[TreeData[id, i * TreeSize[id, 1] + k] + 1, 1] //Assign color
            + Pal0[TreeData[id, i * TreeSize[id, 1] + k] + 1, 2] SHL 8 + Pal0[TreeData[id, i * TreeSize[id, 1] + k] + 1,
            3] SHL 16 OR $FF000000;
        end;

    glGenTextures(1, @Tree[id]);
    glBindTexture(GL_TEXTURE_2D, Tree[id]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, 128, 128, GL_RGBA, GL_UNSIGNED_BYTE, TD);
    FreeMem(TD);
  end;
end;

procedure MakeHousesGFX(Sender: TObject);
var
  h, i, k, mx, my, id: integer;
  TD: Pointer;
  by: ^cardinal;
begin
  //=============================================
  //Making OpenGL textures out of houses
  //=============================================
  FormLoading.Label1.Caption := 'Preparing houses textures';
  FormLoading.Bar1.StepIt;
  FormLoading.Refresh;
  for h := 1 to 29 do
    if h <> 27 then
    begin

      id := HouseIndexGFX[h];
      mx := HouseSize[id, 1];
      my := HouseSize[id, 2];
      if mx > 256 then
        mx := 256;
      if my > 256 then
        my := 256;

      TD := AllocMem(256 * 256 * 4); //GetMem+FillChar(0)
      for i := 0 to 255 do
        for k := 0 to 255 do
          if (i < my) and (k < mx) and (HouseData[id, i * HouseSize[id, 1] + k] <> 0) then
          begin
            by := Pointer(integer(TD) + ((i + 256 - my) * 256 + k) * 4); //Get pointer
            by^ := Pal0[HouseData[id, i * HouseSize[id, 1] + k] + 1, 1] //Assign color
              + Pal0[HouseData[id, i * HouseSize[id, 1] + k] + 1, 2] SHL 8 +
              Pal0[HouseData[id, i * HouseSize[id, 1] + k] + 1, 3] SHL 16 OR $FF000000;
          end;

      glGenTextures(1, @House[h]);
      glBindTexture(GL_TEXTURE_2D, House[h]);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, 256, 256, GL_RGBA, GL_UNSIGNED_BYTE, TD);
      FreeMem(TD);
    end;
end;

end.
