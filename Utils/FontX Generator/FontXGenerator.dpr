program FontXGenerator;
uses
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form1};

{$R *.res}
{$R FontXGenerator.rec}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
