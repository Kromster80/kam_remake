unit Unit1;
{$I KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Spin, ExtCtrls, Classes, SysUtils, Math,
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
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure PlotResults(const aRes: TKMRunResults);
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
  ID, Count: Integer;
  RunnerClass: TKMRunnerClass;
  Runner: TKMRunnerCommon;
  Res: TKMRunResults;
  I: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;
  Count := SpinEdit1.Value;
  if Count <= 0 then Exit;

  Memo1.Clear;
  Button1.Enabled := False;

  RunnerClass := RunnerList[ID];
  Runner := RunnerClass.Create;
  try
    Res := Runner.Run(Count);
  finally
    Runner.Free;
  end;

  PlotResults(Res);
end;


procedure TForm2.PlotResults(const aRes: TKMRunResults);
var
  I: Integer;
  ValueMin, ValueMax: Single;
  DotX, DotY: Word;
begin
  ValueMin := aRes[0].Value;
  ValueMax := aRes[0].Value;
  for I := 1 to High(aRes) do
  begin
    ValueMin := Min(ValueMin, aRes[I].Value);
    ValueMax := Max(ValueMax, aRes[I].Value);
  end;

  for I := 0 to High(aRes) do
  begin
    DotX := Round(I / Length(aRes) * Image1.Width);
    DotY := Round(aRes[I].Value / ValueMax * Image1.Height);
    Image1.Canvas.Ellipse(DotX-1, DotY-1, DotX+1, DotY+1);
  end;

  for I := 0 to High(aRes) do
    Memo1.Lines.Append(Format('%.2f', [aRes[I].Value]));
end;


end.
