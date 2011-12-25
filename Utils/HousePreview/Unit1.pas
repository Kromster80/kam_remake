unit Unit1;
interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Math, ComCtrls, ExtCtrls,  Unit_HouseInfo;

type
  TForm1 = class(TForm)
    Image1: TImage;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  end;

var
  Form1: TForm1;
  HI:TKMHouseInfo;
       

implementation
{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  HI := TKMHouseInfo.Create(Image1.Canvas);
  TrackBar1.Max := HI.WoodSteps + HI.StoneSteps;
end;


procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  HI.BuildStage := TrackBar1.Position;
end;


end.
