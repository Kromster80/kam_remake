unit Unit1;
{$I KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Spin, ExtCtrls, Classes, SysUtils, Graphics, Types, Math, Windows,
  Unit_Runner, KM_RenderControl,
  {$IFDEF WDC} Vcl.ComCtrls {$ELSE} ComCtrls {$ENDIF};


type
  TForm2 = class(TForm)
    Button1: TButton;
    seCycles: TSpinEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo2: TMemo;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TrackBar1: TTrackBar;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label3: TLabel;
    TabSheet4: TTabSheet;
    Memo1: TMemo;
    Render: TTabSheet;
    Panel1: TPanel;
    chkRender: TCheckBox;
    seDuration: TSpinEdit;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure TabSheetResize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    fY: array of TLabel;
    fX: array of TLabel;
    fResults: TKMRunResults;
    fRunTime: string;
    RenderArea: TKMRenderControl;
    procedure RunnerProgress(const aValue: UnicodeString);
    procedure RefreshResults(aImg: TImage);
    procedure RefreshDistribution(aImg: TImage);
    procedure RefreshTimes(aImg: TImage);
    procedure RefreshAxisLabels(aImg: TImage; aTopX, aTopY: Integer);
  end;


var
  Form2: TForm2;


implementation
{$R *.dfm}


const
  COLORS_COUNT = 8;
  LineCol: array [0..COLORS_COUNT - 1] of TColor =
    (clRed, clBlue, clGreen, clPurple, clMaroon, clGray, clBlack, clOlive);


{$IFDEF FPC}
function Point(X,Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;
{$ENDIF}


procedure TForm2.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  RenderArea := TKMRenderControl.Create(Panel1);
  RenderArea.Parent := Panel1;
  RenderArea.Align := alClient;
  RenderArea.Color := clMaroon;

  for I := 0 to High(RunnerList) do
    ListBox1.Items.Append(RunnerList[I].ClassName);
end;


procedure TForm2.ListBox1Click(Sender: TObject);
var
  ID: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;
  Button1.Enabled := True;
end;


procedure TForm2.PageControl1Change(Sender: TObject);
var
  I,J: Integer;
  S: string;
begin
  case PageControl1.ActivePageIndex of
    0: RefreshResults(Image1);
    1: RefreshDistribution(Image2);
    2: RefreshTimes(Image3);
  end;

  Memo1.Clear;
  Memo1.Lines.BeginUpdate;
  for I := 0 to fResults.ChartsCount - 1 do
  begin
    S := IntToStr(I) + '. ';
    for J := 0 to fResults.ValueCount - 1 do
      S := S + Format('%d-%d ', [J, fResults.Value[I,J]]);
    Memo1.Lines.Append(S);
  end;
  Memo1.Lines.EndUpdate;
  Memo1.Lines.Append(fRunTime);
end;


procedure TForm2.Button1Click(Sender: TObject);
var
  T: Cardinal;
  ID, Count: Integer;
  RunnerClass: TKMRunnerClass;
  Runner: TKMRunnerCommon;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;
  Count := seCycles.Value;
  if Count <= 0 then Exit;

  Memo1.Clear;
  Button1.Enabled := False;
  try
    RunnerClass := RunnerList[ID];

    if chkRender.Checked then
      Runner := RunnerClass.Create(RenderArea)
    else
      Runner := RunnerClass.Create(nil);

    Runner.OnProgress := RunnerProgress;
    try
      T := GetTickCount;
      Runner.Duration := seDuration.Value;
      fResults := Runner.Run(Count);
      fRunTime := 'Done in ' + IntToStr(GetTickCount - T) + ' ms';
    finally
      Runner.Free;
    end;

    PageControl1Change(nil);
  finally
    Button1.Enabled := True;
  end;
end;


procedure TForm2.RefreshAxisLabels(aImg: TImage; aTopX, aTopY: Integer);
var
  I: Integer;
  Steps: Integer;
  Step: Word;
begin
  for I := 0 to High(fX) do
    FreeAndNil(fX[I]);

  for I := 0 to High(fY) do
    FreeAndNil(fY[I]);

  Step := Max(Round(aTopY / aImg.Height / 20), 1);

  Steps := Min(aTopY, aImg.Height div 20 div Step);
  SetLength(fY, Steps+1);
  if Steps > 0 then
  for I := 0 to Steps do
  begin
    fY[I] := TLabel.Create(aImg.Parent);
    fY[I].Parent := aImg.Parent;
    fY[I].Alignment := taRightJustify;
    fY[I].Transparent := True;
    fY[I].Left := aImg.Left - 1;
    fY[I].Top := aImg.Top + aImg.Height - Round(aImg.Height / Steps * I) - fY[I].Height + 6;
    fY[I].Caption := IntToStr(Round(aTopY * I / Steps))+'-';
  end;

  Steps := Min(aTopX, aImg.Width div 40);
  SetLength(fX, Steps);
  if Steps > 1 then
  for I := 0 to High(fX) do
  begin
    fX[I] := TLabel.Create(aImg.Parent);
    fX[I].Parent := aImg.Parent;
    fX[I].Alignment := taRightJustify;
    fX[I].Left := aImg.Left + Round(aImg.Width / High(fX) * I);
    fX[I].Top := aImg.Top + aImg.Height + 4;
    fX[I].Caption := FloatToStr(Round(aTopX * I * 10 / High(fX)) / 10);
  end;
end;


procedure TForm2.RefreshDistribution(aImg: TImage);
var
  I,J: Integer;
  DotX, DotY: Word;
  TopX, TopY: Integer;
  StatMax: Integer;
  Stats: array of Integer;
begin
  if aImg.Picture.Bitmap <> nil then
    aImg.Picture.Bitmap.SetSize(aImg.Width, aImg.Height);
  aImg.Canvas.FillRect(aImg.Canvas.ClipRect);

  StatMax := 0;
  for I := 0 to fResults.ValueCount - 1 do
  begin
    SetLength(Stats, 0); //Erase
    SetLength(Stats, Round(fResults.ValueMax) - Round(fResults.ValueMin) + 1);
    for J := 0 to fResults.ChartsCount - 1 do
      Inc(Stats[Round(fResults.Value[J,I]) - Round(fResults.ValueMin)]);

    for J := 0 to High(Stats) do
      StatMax := Max(StatMax, Stats[J]);
  end;

  for I := 0 to fResults.ValueCount - 1 do
  begin
    aImg.Canvas.Pen.Color := LineCol[I mod COLORS_COUNT];

    SetLength(Stats, 0); //Erase
    SetLength(Stats, Round(fResults.ValueMax) - Round(fResults.ValueMin) + 1);
    for J := 0 to fResults.ChartsCount - 1 do
      Inc(Stats[Round(fResults.Value[J,I]) - Round(fResults.ValueMin)]);

    for J := Low(Stats) to High(Stats) do
    begin
      if Length(Stats) > 1 then
        DotX := Round((J - Low(Stats)) * aImg.Width / (Length(Stats) - 1))
      else
        DotX := aImg.Width div 2;

      DotY := aImg.Height - Round(aImg.Height * Stats[J] / StatMax);

      if DotY <> aImg.Height then
        aImg.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);

      if J = 0 then
        aImg.Canvas.PenPos := Point(DotX, DotY)
      else
        aImg.Canvas.LineTo(DotX, DotY);
    end;
  end;
  TopX := Length(Stats);
  TopY := StatMax;

  RefreshAxisLabels(aImg, TopX, TopY);
end;


procedure TForm2.RefreshResults(aImg: TImage);
var
  I,J: Integer;
  DotX, DotY: Word;
  TopX, TopY: Integer;
begin
  if aImg.Picture.Bitmap <> nil then
    aImg.Picture.Bitmap.SetSize(aImg.Width, aImg.Height);
  aImg.Canvas.FillRect(aImg.Canvas.ClipRect);

  for I := 0 to fResults.ValueCount - 1 do
  begin
    aImg.Canvas.Pen.Color := LineCol[I mod COLORS_COUNT];
    for J := 0 to fResults.ChartsCount - 1 do
    begin
      if fResults.ChartsCount > 1 then
        DotX := Round(J / (fResults.ChartsCount - 1) * aImg.Width)
      else
        DotX := aImg.Width div 2;
      if fResults.ValueMax <> 0 then
        DotY := aImg.Height - Round(fResults.Value[J,I] / fResults.ValueMax * aImg.Height)
      else
        DotY := aImg.Height;
      aImg.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);
      if J = 0 then
        aImg.Canvas.PenPos := Point(DotX, DotY)
      else
        aImg.Canvas.LineTo(DotX, DotY);
    end;
  end;
  TopX := fResults.ChartsCount;
  TopY := fResults.ValueMax;

  RefreshAxisLabels(aImg, TopX, TopY);
end;


procedure TForm2.RefreshTimes(aImg: TImage);
var
  I,J: Integer;
  CutOff: Byte;
  DotX, DotY: Word;
  TopX, TopY: Integer;
begin
  if aImg.Picture.Bitmap <> nil then
    aImg.Picture.Bitmap.SetSize(aImg.Width, aImg.Height);
  aImg.Canvas.FillRect(aImg.Canvas.ClipRect);

  CutOff := TrackBar1.Position;

  for I := 0 to fResults.ChartsCount - 1 do
  begin
    aImg.Canvas.Pen.Color := LineCol[I mod COLORS_COUNT];

    for J := 0 to fResults.TimesCount - 1 do
    if fResults.Times[I,J] >= CutOff then
    begin
      DotX := Round(J / (fResults.TimesCount - 1) * aImg.Width);
      DotY := aImg.Height - Round(fResults.Times[I,J] / fResults.TimeMax * aImg.Height);

      aImg.Canvas.PenPos := Point(DotX, aImg.Height);
      aImg.Canvas.LineTo(DotX, DotY);
    end;
  end;
  TopX := fResults.TimesCount;
  TopY := fResults.TimeMax;

  RefreshAxisLabels(aImg, TopX, TopY);
end;


procedure TForm2.RunnerProgress(const aValue: UnicodeString);
begin
  Label2.Caption := aValue;
  Label2.Refresh;
  Application.ProcessMessages;
end;



procedure TForm2.TabSheetResize(Sender: TObject);
begin
  PageControl1Change(nil);
end;


procedure TForm2.TrackBar1Change(Sender: TObject);
begin
  PageControl1Change(nil);
end;

end.
