program ScriptingParser;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  FormHelp in 'FormHelp.pas' {helpForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(ThelpForm, helpForm);
  Application.Run;
end.
