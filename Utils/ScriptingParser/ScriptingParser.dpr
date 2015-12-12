program ScriptingParser;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  Main in 'src\Main.pas' {Form1},
  FormHelp in 'src\FormHelp.pas' {helpForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(ThelpForm, helpForm);
  Application.Run;
end.
