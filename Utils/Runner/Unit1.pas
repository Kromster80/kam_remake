unit Unit1;
{$I KaM_Remake.inc}
interface
uses
  Forms, Controls, StdCtrls, Spin, ExtCtrls, Classes, SysUtils, Math;


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

  end;


var
  Form2: TForm2;


implementation
{$R *.dfm}
uses Unit_Runner;


procedure TForm2.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(RunnerList) do
    ListBox1.Items.Append(RunnerList[I].ClassName);
end;


procedure TForm2.Button1Click(Sender: TObject);
var
  ID: Integer;
  RunnerClass: TKMRunnerClass;
  Runner: TKMRunnerCommon;
  Res: TKMRunResults;
  I: Integer;
begin
  ID := ListBox1.ItemIndex;
  if ID = -1 then Exit;

  Memo1.Clear;

  RunnerClass := RunnerList[ID];

  Runner := RunnerClass.Create;
  try
    Res := Runner.Run(2);

  finally
    Runner.Free;
  end;

  for I := 0 to High(Res) do
    Memo1.Lines.Append(IntToStr(Res[I].Value));
end;


end.
