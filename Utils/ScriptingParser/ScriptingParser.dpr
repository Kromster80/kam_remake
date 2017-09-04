program ScriptingParser;

uses 
  Vcl.Forms, 
  Main in 'Main.pas' {Form1}, 
  KM_CommonUtils in '..\..\src\utils\KM_CommonUtils.pas', 
  KM_Defaults in '..\..\src\common\KM_Defaults.pas', 
  KM_Points in '..\..\src\common\KM_Points.pas', 
  KM_CommonTypes in '..\..\src\common\KM_CommonTypes.pas'; 

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
