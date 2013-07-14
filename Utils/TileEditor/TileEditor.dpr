program TileEditor;
{$I ..\..\KaM_Remake.inc}
uses
  Forms,
  Unit1 in 'Unit1.pas', {Form1}
  KM_ResTileset in '..\..\KM_ResTileset.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
