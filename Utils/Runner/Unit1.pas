unit Unit1;
{$I KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Spin, ExtCtrls, Classes, SysUtils, Math, Windows,
  Unit_Runner;


type
  TForm2 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    Memo2: TMemo;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    fResults: TKMRunResults;
    fRunTime: string;
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


procedure TForm2.Button1Click(Sender: TObject);
var
  T: Cardinal;
  ID, Count: Integer;
  RunnerClass: TKMRunnerClass;
  Runner: TKMRunnerCommon;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;
  Count := SpinEdit1.Value;
  if Count <= 0 then Exit;

  Memo1.Clear;
  Button1.Enabled := False;
  try

    RunnerClass := RunnerList[ID];
    Runner := RunnerClass.Create;
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
var
  I, K: Integer;
  ValueMin, ValueMax: Single;
  DotX, DotY: Word;
  Stats: array of Integer;
begin
  Image1.Canvas.FillRect(Image1.Canvas.ClipRect);
  if Length(fResults) = 0 then Exit;

  ValueMin := fResults[0].Value;
  ValueMax := fResults[0].Value;
  for I := 1 to High(fResults) do
  begin
    ValueMin := Min(ValueMin, fResults[I].Value);
    ValueMax := Max(ValueMax, fResults[I].Value);
  end;

  case RadioGroup1.ItemIndex of
    0:  for I := 0 to High(fResults) do
        begin
          DotX := Round(I / Length(fResults) * Image1.Width);
          DotY := Image1.Height - Round(fResults[I].Value / ValueMax * Image1.Height);
          Image1.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);
        end;
    1:  begin
          SetLength(Stats, Round(ValueMax) - Round(ValueMin) + 1);
          for I := 0 to High(fResults) do
            Inc(Stats[Round(fResults[I].Value) - Round(ValueMin)]);

          ValueMin := Stats[Low(Stats)];
          ValueMax := Stats[Low(Stats)];
          for I := Low(Stats)+1 to High(Stats) do
          begin
            ValueMin := Min(ValueMin, Stats[I]);
            ValueMax := Max(ValueMax, Stats[I]);
          end;

          Image1.Canvas.PenPos := Point(0, Image1.Height);
          for I := Low(Stats) to High(Stats) do
          begin
            DotX := Round((I - Low(Stats)) / Length(Stats) * Image1.Width);
            DotY := Image1.Height - Round(Stats[I] / ValueMax * Image1.Height);
            Image1.Canvas.Ellipse(DotX-2, DotY-2, DotX+2, DotY+2);
            Image1.Canvas.LineTo(DotX, DotY);
          end;
        end;
  end;

  for I := 0 to High(fResults) do
    Memo1.Lines.Append(Format('%d. %.2f', [I, fResults[I].Value]));
  Memo1.Lines.Append(fRunTime);
end;


end.
