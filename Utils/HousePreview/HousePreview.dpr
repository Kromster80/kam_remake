program HousePreview;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_HouseInfo in 'Unit_HouseInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
