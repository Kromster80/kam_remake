program ScriptingParser;

uses 
  Vcl.Forms, 
  Main in 'Main.pas' {Form1}, 
  KM_Utils in '..\..\src\KM_Utils.pas', 
  KM_Defaults in '..\..\src\KM_Defaults.pas', 
  KM_Points in '..\..\src\KM_Points.pas', 
  KM_CommonTypes in '..\..\src\KM_CommonTypes.pas'; 

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
