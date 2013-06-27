program FontXGenerator;
uses
  //FastMM4,
  Forms,
  Unit_Main in 'Unit_Main.pas' {Form1},
  KM_ResourceFontsEdit;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
