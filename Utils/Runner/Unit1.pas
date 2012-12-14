unit Unit1;
{$I KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Spin, ExtCtrls, Classes, SysUtils, Graphics, Types, Math, Windows,
  Unit_Runner;


type
  TForm2 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Memo1: TMemo;
    seCycles: TSpinEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo2: TMemo;
    RadioGroup1: TRadioGroup;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    fY: array of TLabel;
    fX: array of TLabel;
    fResults: TKMRunResults;
    fRunTime: string;
    procedure RunnerProgress(const aValue: string);
  end;


var
  Form2: TForm2;


implementation
{$R *.dfm}


procedure TForm2.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(RunnerList) do
    ListBox1.Items.Append(RunnerList[I].ClassName);
end;


procedure TForm2.FormResize(Sender: TObject);
begin
  if Image1.Picture.Graphic = nil then Exit;

  Image1.Picture.Graphic.Width := Image1.Width;
  Image1.Picture.Graphic.Height := Image1.Height;

  RadioGroup1Click(nil);
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
    Runner := RunnerClass.Create;
    Runner.OnProgress := RunnerProgress;
    try
      T := GetTickCount;
      fResults := Runner.Run(Count);
      fRunTime := 'Done in ' + IntToStr(GetTickCount - T) + ' ms';
    finally
      Runner.Free;
    end;

    RadioGroup1Click(nil);
  finally
    Button1.Enabled := True;
  end;
end;


procedure TForm2.RadioGroup1Click(Sender: TObject);
const
  COLORS_COUNT = 8;
  LineCol: array [0..COLORS_COUNT - 1] of TColor =
    (clRed, clBlue, clGreen, clPurple, clMaroon, clGray, clBlack, clOlive);
var
  I,J: Integer;
  Steps: Integer;
  DotX, DotY: Word;
  StatMax: Integer;
  Stats: array of Integer;
  S: string;
  TopX, TopY: Integer;
begin
  Image1.Canvas.FillRect(Image1.Canvas.ClipRect);

  TopX := 0;
  TopY := 0;

  case RadioGroup1.ItemIndex of
    0:  begin
          for J := 0 to fResults.ValueCount - 1 do
          begin
            Image1.Canvas.Pen.Color := LineCol[J mod COLORS_COUNT];
            for I := 0 to fResults.ChartsCount - 1 do
            begin
              DotX := Round(I / (fResults.ChartsCount - 1) * Image1.Width);
              DotY := Image1.Height - Round(fResults.Value[I,J] / fResults.ValueMax * Image1.Height);
              Image1.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);
              if I = 0 then
                Image1.Canvas.PenPos := Point(DotX, DotY)
              else
                Image1.Canvas.LineTo(DotX, DotY);
            end;
          end;
          TopX := fResults.ChartsCount;
          TopY := fResults.ValueMax;
        end;
    1:  begin
          for J := 0 to fResults.ValueCount - 1 do
          begin
            Image1.Canvas.Pen.Color := LineCol[J mod COLORS_COUNT];

            SetLength(Stats, 0);
            SetLength(Stats, Round(fResults.ValueMax) - Round(fResults.ValueMin) + 1);
            for I := 0 to fResults.ChartsCount - 1 do
              Inc(Stats[Round(fResults.Value[I,J]) - Round(fResults.ValueMin)]);

            StatMax := Stats[Low(Stats)];
            for I := Low(Stats)+1 to High(Stats) do
              StatMax := Max(StatMax, Stats[I]);

            for I := Low(Stats) to High(Stats) do
            begin
              DotX := Round((I - Low(Stats)) / (Length(Stats) - 1) * Image1.Width);
              DotY := Image1.Height - Round(Stats[I] / StatMax * Image1.Height);

              if DotY <> Image1.Height then
                Image1.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);

              if I = 0 then
                Image1.Canvas.PenPos := Point(DotX, DotY)
              else
                Image1.Canvas.LineTo(DotX, DotY);
            end;
          end;
          TopX := Length(Stats);
          TopY := StatMax;
        end;
    2:  begin
          for I := 0 to fResults.ChartsCount - 1 do
          begin
            Image1.Canvas.Pen.Color := LineCol[I mod COLORS_COUNT];

            for J := 0 to fResults.TimesCount - 1 do
            begin
              DotX := Round(J / (fResults.TimesCount - 1) * Image1.Width);
              DotY := Image1.Height - Round(fResults.Times[I,J] / fResults.TimeMax * Image1.Height);

              Image1.Canvas.PenPos := Point(DotX, Image1.Height);
              Image1.Canvas.LineTo(DotX, DotY);
            end;
          end;
          TopX := fResults.TimesCount;
          TopY := fResults.TimeMax;
        end;
  end;

  for I := 0 to High(fX) do
    FreeAndNil(fX[I]);

  for I := 0 to High(fY) do
    FreeAndNil(fY[I]);

  Steps := Min(TopY, Image1.Height div 36);
  SetLength(fY, Steps);
  if Steps > 1 then
  for I := 0 to High(fY) do
  begin
    fY[I] := TLabel.Create(Self);
    fY[I].Parent := Self;
    fY[I].Alignment := taRightJustify;
    fY[I].Left := Image1.Left - 6;
    fY[I].Top := Image1.Top + Image1.Height - Round(Image1.Height / High(fY) * I) - fY[I].Height;
    fY[I].Caption := IntToStr(Round(TopY / High(fY) * I));
  end;

  Steps := Min(TopX, Image1.Width div 40);
  SetLength(fX, Steps);
  if Steps > 1 then
  for I := 0 to High(fX) do
  begin
    fX[I] := TLabel.Create(Self);
    fX[I].Parent := Self;
    fX[I].Alignment := taRightJustify;
    fX[I].Left := Image1.Left + Round(Image1.Width / High(fX) * I);
    fX[I].Top := Image1.Top + Image1.Height + 4;
    fX[I].Caption := FloatToStr(Round(TopX / High(fX) * I*10)/10);
  end;

  Memo1.Clear;
  for I := 0 to fResults.ChartsCount - 1 do
  begin
    S := IntToStr(I) + '. ';
    for J := 0 to fResults.ValueCount - 1 do
      S := S + Format('%d-%d ', [J, fResults.Value[I,J]]);
    Memo1.Lines.Append(S);
  end;
  Memo1.Lines.Append(fRunTime);
end;


procedure TForm2.RunnerProgress(const aValue: string);
begin
  Label2.Caption := aValue;
  Label2.Refresh;
  Application.ProcessMessages;
end;


end.
