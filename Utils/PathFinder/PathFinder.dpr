program PathFinder;

uses
  Forms,
  Unit_Form in 'Unit_Form.pas' {Form1},
  Unit_Finder in 'Unit_Finder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
