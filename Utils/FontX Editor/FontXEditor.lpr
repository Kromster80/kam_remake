program FontXEditor;

{$MODE Delphi}

uses
  Forms, Interfaces,
  umain in 'umain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
